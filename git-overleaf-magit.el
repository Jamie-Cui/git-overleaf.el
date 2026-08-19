;;; git-overleaf-magit.el --- Overleaf sync section for magit-status -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds an Overleaf section to `magit-status' buffers showing the sync
;; state and lazy diffs of local and remote changes.

;;; Code:

(require 'cl-lib)
(require 'git-overleaf)
(eval-and-compile
  (require 'magit-section)
  (require 'transient))
(require 'magit nil t)

(declare-function git-overleaf--async-key-active-p "git-overleaf-core")
(declare-function git-overleaf--async-supported-p "git-overleaf-core")
(declare-function git-overleaf--classify-sync-state "git-overleaf-sync")
(declare-function git-overleaf--fetch-sync "git-overleaf")
(declare-function git-overleaf--repo-async-key "git-overleaf-core")
(declare-function magit--insert-diff "magit-diff" (keep-error &rest args))
(declare-function magit-fetch-arguments "magit-fetch" ())
(declare-function magit-git-fetch "magit-fetch" (remote args))
(declare-function magit-get-mode-buffer "magit-mode" (mode &optional value frame))
(declare-function magit-pull-arguments "magit-pull" ())
(declare-function magit-push-arguments "magit-push" ())
(declare-function magit-refresh "magit-mode" ())
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function magit-toplevel "magit-git" (&optional directory))

;;;; Customization

(defcustom git-overleaf-magit-auto-refresh-remote t
  "Whether Magit status refreshes may refresh the Overleaf remote.

When non-nil, an Overleaf-managed `magit-status' buffer starts remote
snapshot downloads in the background when Emacs thread support is
available.  Automatic downloads are throttled internally and never
block an ordinary Magit refresh.  This option is independent of
`git-overleaf-enable-async', which continues to control other commands."
  :type 'boolean
  :group 'git-overleaf)

(defconst git-overleaf-magit--auto-refresh-remote-interval 300
  "Minimum seconds between automatic Overleaf remote refresh attempts.")

;;;; Remote state

(cl-defstruct (git-overleaf-magit--remote-state
               (:constructor git-overleaf-magit--make-remote-state))
  "Cached remote state owned by one Magit status buffer."
  refreshing
  last-attempt-time
  last-success-time
  error)

(defvar-local git-overleaf-magit--remote-state nil
  "Remote Overleaf state cached for the current Magit status buffer.")

(put 'git-overleaf-magit--remote-state 'permanent-local t)

(defun git-overleaf-magit--remote-state ()
  "Return the current buffer's remote state, creating it if necessary."
  (or git-overleaf-magit--remote-state
      (setq git-overleaf-magit--remote-state
            (git-overleaf-magit--make-remote-state))))

;;;; Presentation

(defconst git-overleaf-magit--sync-presentations
  '((in-sync "in sync" magit-dimmed)
    (head-matches-remote "content matches" magit-dimmed)
    (remote-matches-base "local changes" warning)
    (head-matches-base "remote changes" warning)
    (diverged "local and remote changes" warning))
  "Display label and face for each core Overleaf sync state.")

(defun git-overleaf-magit--format-refresh-time (seconds)
  "Format refresh time SECONDS for a compact status heading."
  (format-time-string "%H:%M" (seconds-to-time seconds)))

(defun git-overleaf-magit--state-label
    (repo local-in-sync sync-status worktree-dirty remote-state)
  "Return (LABEL . FACE) describing the Overleaf state of REPO.
LOCAL-IN-SYNC means HEAD matches the recorded base.  SYNC-STATUS is a
status returned by `git-overleaf--classify-sync-state', or nil when the
remote is unknown.  WORKTREE-DIRTY means staged, unstaged, untracked, or
unmerged changes exist.  REMOTE-STATE describes refresh progress and
cached snapshot freshness."
  (let* ((pending (git-overleaf--pending-state repo))
         (remote-known (and sync-status t))
         (refreshing
          (git-overleaf-magit--remote-state-refreshing remote-state))
         (refresh-error
          (git-overleaf-magit--remote-state-error remote-state))
         (last-success
          (git-overleaf-magit--remote-state-last-success-time remote-state))
         (spec (assq sync-status git-overleaf-magit--sync-presentations))
         label
         face)
    (cond
     (pending
      (setq label
            (format "pending %s (%s)"
                    (plist-get pending :action)
                    (git-overleaf--pending-phase repo pending)))
      (setq face 'error))
     (remote-known
      (setq label (nth 1 spec))
      (setq face (nth 2 spec)))
     (refreshing
      (setq label (if local-in-sync
                      "checking remote..."
                    "local changes; checking remote..."))
      (setq face (if local-in-sync 'magit-dimmed 'warning)))
     (refresh-error
      (setq label (if local-in-sync
                      "remote refresh failed"
                    "local changes; remote refresh failed"))
      (setq face 'error))
     (t
      (setq label (if local-in-sync
                      "remote unchecked"
                    "local changes; remote unchecked"))
      (setq face (if local-in-sync 'magit-dimmed 'warning))))
    (when worktree-dirty
      (setq label (concat label "; working tree modified"))
      (unless (eq face 'error)
        (setq face 'warning)))
    (when remote-known
      (cond
       (refreshing
        (setq label (concat label "; refreshing...")))
       (refresh-error
        (setq label
              (concat
               label
               "; refresh failed"
               (if last-success
                   (format "; snapshot %s"
                           (git-overleaf-magit--format-refresh-time
                            last-success))
                 ""))))
       (last-success
        (setq label
              (format "%s; checked %s"
                      label
                      (git-overleaf-magit--format-refresh-time
                       last-success))))))
    (cons label face)))

(defun git-overleaf-magit--worktree-dirty-p (repo)
  "Return non-nil when REPO has staged or working-tree changes."
  (let ((status (git-overleaf--read-repo-status repo)))
    (or (git-overleaf--repo-status-staged status)
        (git-overleaf--repo-status-unstaged status)
        (git-overleaf--repo-status-unmerged status))))

(defun git-overleaf-magit--face-string (string face)
  "Return STRING with FACE applied for direct and font-lock display."
  (propertize string 'face face 'font-lock-face face))

(defun git-overleaf-magit--heading (name remote label face)
  "Return an Overleaf heading for project NAME and logical REMOTE.
Display LABEL using FACE."
  (concat
   (git-overleaf-magit--face-string
    (format "Overleaf: %s [%s] (" name (or remote "unregistered"))
    'magit-section-heading)
   (git-overleaf-magit--face-string label face)
   (git-overleaf-magit--face-string ")" 'magit-section-heading)))

(defun git-overleaf-magit--display-remote-name (repo)
  "Return the logical Overleaf remote name to display for REPO."
  (condition-case nil
      (git-overleaf--remote-name repo)
    (user-error "ambiguous")))

(defun git-overleaf-magit--diff-kinds (local-in-sync sync-status)
  "Return diff kinds to show for LOCAL-IN-SYNC and SYNC-STATUS."
  (pcase sync-status
    ('in-sync nil)
    ('head-matches-remote '(matching))
    ('remote-matches-base '(local))
    ('head-matches-base '(remote))
    ('diverged '(local remote))
    (_ (unless local-in-sync '(local)))))

;;;; Section interaction

(defvar-keymap git-overleaf-magit-command-map
  :doc "Commands available from an Overleaf Magit section."
  "b" #'git-overleaf-browse-remote
  "f" #'git-overleaf-fetch
  "g" #'git-overleaf-magit-refresh-remote
  "k" #'git-overleaf-force-stop
  "l" #'git-overleaf-pull
  "L" #'git-overleaf-log
  "p" #'git-overleaf-push
  "q" #'git-overleaf-pull-abort
  "R" #'git-overleaf-reset
  "r" #'git-overleaf-register-remote
  "s" #'git-overleaf-status)

(defvar-keymap git-overleaf-magit-section-map
  :doc "Keymap for the Overleaf Magit status section."
  "<remap> <magit-browse-thing>" #'git-overleaf-browse-remote
  "<remap> <magit-visit-thing>" #'git-overleaf-browse-remote
  "G" #'git-overleaf-magit-refresh-remote)

(keymap-set git-overleaf-magit-section-map
            "C-c C-c"
            git-overleaf-magit-command-map)

(defclass git-overleaf-magit-section (magit-section)
  ((type :initform 'git-overleaf-magit)
   (keymap :initform 'git-overleaf-magit-section-map))
  "Top-level Overleaf status section in a Magit buffer.")

(defclass git-overleaf-magit-diff-section (magit-section)
  ((type :initform 'git-overleaf-magit-diff))
  "Lazy local, remote, or matching Overleaf diff section.")

;;;; Section insertion

(defun git-overleaf-magit--insert-diff-section
    (repo kind base-rev remote-commit)
  "Insert the Overleaf diff KIND for REPO.
BASE-REV is the last synchronized revision and REMOTE-COMMIT is the
cached Overleaf snapshot commit."
  (pcase-let ((`(,heading ,from ,to)
               (pcase kind
                 ('local
                  (list "Local changes" base-rev "HEAD"))
                 ('remote
                  (list "Remote changes" base-rev remote-commit))
                 ('matching
                  (list "Matching local and remote changes"
                        base-rev
                        "HEAD")))))
    (magit-insert-section (git-overleaf-magit-diff-section kind t)
      (magit-insert-heading heading)
      (magit-insert-section-body
        (let ((default-directory repo))
          (magit--insert-diff nil "diff" from to "--no-prefix"))))))

(defun git-overleaf-magit-insert-status ()
  "Insert an Overleaf status section into the current Magit buffer."
  (when-let* ((repo (magit-toplevel))
              (managed (git-overleaf--managed-repo-p repo))
              (base-ref (git-overleaf--base-ref repo))
              (base-rev (git-overleaf--rev-parse-noerror repo base-ref)))
    (let* ((name (git-overleaf--project-name repo))
           (base-tree (git-overleaf--tree-id repo base-rev))
           (head-tree (git-overleaf--tree-id repo "HEAD"))
           (remote-commit
            (git-overleaf--rev-parse-noerror
             repo
             (git-overleaf--remote-ref repo)))
           (remote-tree
            (and remote-commit
                 (git-overleaf--tree-id repo remote-commit)))
           (local-in-sync (equal base-tree head-tree))
           (sync-status
            (and remote-tree
                 (git-overleaf--classify-sync-state
                  base-tree head-tree remote-tree)))
           (worktree-dirty (git-overleaf-magit--worktree-dirty-p repo))
           (presentation
            (git-overleaf-magit--state-label
             repo
             local-in-sync
             sync-status
             worktree-dirty
             (git-overleaf-magit--remote-state))))
      (magit-insert-section (git-overleaf-magit-section repo)
        (magit-insert-heading
          (git-overleaf-magit--heading
           name
           (git-overleaf-magit--display-remote-name repo)
           (car presentation)
           (cdr presentation)))
        (dolist (kind
                 (git-overleaf-magit--diff-kinds
                  local-in-sync sync-status))
          (git-overleaf-magit--insert-diff-section
           repo kind base-rev remote-commit))))))

;;;; Remote refresh

(defun git-overleaf-magit--auto-refresh-due-p ()
  "Return non-nil when this Magit buffer may auto-refresh the remote."
  (let ((last-attempt
         (git-overleaf-magit--remote-state-last-attempt-time
          (git-overleaf-magit--remote-state))))
    (or (null last-attempt)
        (>= (- (float-time) last-attempt)
            git-overleaf-magit--auto-refresh-remote-interval))))

(defun git-overleaf-magit--remote-refresh-succeeded (magit-buf _remote-commit)
  "Record a successful remote refresh and refresh MAGIT-BUF."
  (when (buffer-live-p magit-buf)
    (with-current-buffer magit-buf
      (let ((state (git-overleaf-magit--remote-state)))
        (setf (git-overleaf-magit--remote-state-refreshing state) nil
              (git-overleaf-magit--remote-state-last-success-time state)
              (float-time)
              (git-overleaf-magit--remote-state-error state) nil))
      (git-overleaf--message "Remote snapshot ready.")
      (magit-refresh))))

(defun git-overleaf-magit--remote-refresh-failed (magit-buf message)
  "Record refresh failure MESSAGE and refresh MAGIT-BUF.
Keep any last successful remote snapshot available."
  (when (buffer-live-p magit-buf)
    (with-current-buffer magit-buf
      (let ((state (git-overleaf-magit--remote-state)))
        (setf (git-overleaf-magit--remote-state-refreshing state) nil
              (git-overleaf-magit--remote-state-error state) message))
      (magit-refresh))))

(defun git-overleaf-magit--status-buffer-for-repo (repo)
  "Return the Magit status buffer for REPO, creating one if needed."
  (let ((default-directory repo))
    (or (magit-get-mode-buffer 'magit-status-mode)
        (let ((git-overleaf-magit-auto-refresh-remote nil))
          (magit-status-setup-buffer repo)))))

(defun git-overleaf-magit-refresh-remote (&optional background)
  "Download the remote Overleaf snapshot and refresh its Magit section.
With internal optional argument BACKGROUND non-nil, force background
execution independently of `git-overleaf-enable-async'.  Interactive
calls continue to follow that user option."
  (interactive)
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (_managed
          (unless (git-overleaf--managed-repo-p repo)
            (user-error "Not an Overleaf project")))
         (magit-buf (git-overleaf-magit--status-buffer-for-repo repo)))
    (with-current-buffer magit-buf
      (when (git-overleaf-magit--remote-state-refreshing
             (git-overleaf-magit--remote-state))
        (user-error "Remote refresh already in progress")))
    (git-overleaf--set-repo-url repo)
    (let* ((state
            (with-current-buffer magit-buf
              (git-overleaf-magit--remote-state))))
      (with-current-buffer magit-buf
        (setf (git-overleaf-magit--remote-state-refreshing state) t
              (git-overleaf-magit--remote-state-last-attempt-time state)
              (float-time)
              (git-overleaf-magit--remote-state-error state) nil))
      (git-overleaf--with-repo-log-context repo
        (condition-case err
            (let ((git-overleaf-enable-async
                   (or background git-overleaf-enable-async)))
              (git-overleaf--async-start
               "Overleaf Magit remote refresh"
               (lambda ()
                 (git-overleaf--set-repo-url repo)
                 (git-overleaf--fetch-sync repo t))
               :key (git-overleaf--repo-async-key repo)
               :on-success
               (lambda (commit)
                 (git-overleaf-magit--remote-refresh-succeeded
                  magit-buf commit))
               :on-error
               (lambda (message)
                 (git-overleaf-magit--remote-refresh-failed
                  magit-buf message)
                 (git-overleaf--warn
                  "Overleaf remote refresh failed: %s"
                  message))))
          (error
           (with-current-buffer magit-buf
             (setf (git-overleaf-magit--remote-state-refreshing state) nil
                   (git-overleaf-magit--remote-state-error state)
                   (error-message-string err)))
           (signal (car err) (cdr err))))))))

(defun git-overleaf-magit--maybe-auto-refresh-remote ()
  "Start a due background Overleaf refresh after a Magit refresh."
  (when (and git-overleaf-magit-auto-refresh-remote
             (git-overleaf--async-supported-p)
             (derived-mode-p 'magit-status-mode)
             (git-overleaf-magit--auto-refresh-due-p))
    (when-let* ((repo (magit-toplevel)))
      (let ((state (git-overleaf-magit--remote-state))
            (key (git-overleaf--repo-async-key repo)))
        (when (and (git-overleaf--managed-repo-p repo)
                   (not
                    (git-overleaf-magit--remote-state-refreshing state))
                   (not (git-overleaf--async-key-active-p key)))
          (condition-case err
              (git-overleaf-magit-refresh-remote t)
            (error
             (setf (git-overleaf-magit--remote-state-refreshing state) nil
                   (git-overleaf-magit--remote-state-last-attempt-time state)
                   (float-time)
                   (git-overleaf-magit--remote-state-error state)
                   (error-message-string err))
             (git-overleaf--debug
              "Skipping automatic Overleaf remote refresh: %s"
              (error-message-string err)))))))))

;;;; Magit operations

(defvar git-overleaf-magit--fetch-integration-installed nil
  "Non-nil when the Magit fetch integration has been installed.")

(defvar git-overleaf-magit--pull-integration-installed nil
  "Non-nil when the Magit pull integration has been installed.")

(defvar git-overleaf-magit--push-integration-installed nil
  "Non-nil when the Magit push integration has been installed.")

(defun git-overleaf-magit--require-managed-repo ()
  "Return the current managed Overleaf repository."
  (let ((repo (or (magit-toplevel)
                  (user-error "Not inside a Git repository"))))
    (unless (git-overleaf--managed-repo-p repo)
      (user-error "Not an Overleaf project"))
    repo))

(defun git-overleaf-magit--operation-description (_operation)
  "Return the transient description for Overleaf OPERATION."
  (condition-case nil
      (let ((repo (git-overleaf-magit--require-managed-repo)))
        (if-let* ((remote (git-overleaf--remote-name repo)))
            (format "Overleaf: %s" remote)
          (format "Overleaf: %s"
                  (git-overleaf--project-name repo))))
    (error "Overleaf")))

(defun git-overleaf-magit--fetch-description ()
  "Return the transient description for Overleaf fetch."
  (git-overleaf-magit--operation-description "fetch"))

(defun git-overleaf-magit--pull-description ()
  "Return the transient description for Overleaf pull."
  (git-overleaf-magit--operation-description "pull"))

(defun git-overleaf-magit--push-description ()
  "Return the transient description for Overleaf push."
  (git-overleaf-magit--operation-description "push"))

(defun git-overleaf-magit--reject-arguments (prefix)
  "Reject active Git arguments from transient PREFIX."
  (let ((args (transient-args prefix)))
    (when args
      (user-error
       "Git arguments do not apply to Overleaf: %s"
       (mapconcat #'identity args " ")))))

(defun git-overleaf-magit--push-mode (args)
  "Return the Overleaf push mode selected by Magit ARGS.
No arguments and `--force-with-lease' select the normal guarded push.
The Git `-f' and `--force' arguments select a force-push.  Signal
when ARGS contains any other argument or combines the two force modes."
  (cond
   ((null args) 'normal)
   ((equal args '("--force-with-lease")) 'normal)
   ((and (null (cdr args))
         (member (car args) '("-f" "--force")))
    'force)
   (t
    (user-error
     (concat
      "Only --force-with-lease and --force apply to Overleaf pushes; "
      "got: %s")
     (mapconcat #'identity args " ")))))

(defun git-overleaf-magit--fetch-mode (args)
  "Return the Overleaf fetch mode selected by Magit ARGS.
No arguments, `-f', and `--force' all select the same remote-ref refresh.
Fetch never moves HEAD, the index, or the working tree."
  (cond
   ((or (null args)
        (and (null (cdr args))
             (member (car args) '("-f" "--force"))))
    'normal)
   (t
    (user-error
     "Only --force applies to Overleaf fetches; got: %s"
     (mapconcat #'identity args " ")))))

(transient-define-suffix git-overleaf-magit-fetch (&optional args)
  "Fetch the branchless logical Overleaf remote."
  :description #'git-overleaf-magit--fetch-description
  (interactive (list (magit-fetch-arguments)))
  (git-overleaf-magit--fetch-mode args)
  (git-overleaf-magit--require-managed-repo)
  (git-overleaf-magit-refresh-remote t))

(transient-define-suffix git-overleaf-magit-pull ()
  "Pull the branchless logical Overleaf remote."
  :description #'git-overleaf-magit--pull-description
  (interactive)
  (git-overleaf-magit--reject-arguments 'magit-pull)
  (let ((repo (git-overleaf-magit--require-managed-repo)))
    (let ((default-directory repo)
          (git-overleaf-enable-async
           (git-overleaf--async-supported-p)))
      (call-interactively #'git-overleaf-pull))))

(transient-define-suffix git-overleaf-magit-push (args)
  "Push the branchless logical Overleaf remote."
  :description #'git-overleaf-magit--push-description
  (interactive (list (magit-push-arguments)))
  (let ((mode (git-overleaf-magit--push-mode args))
        (repo (git-overleaf-magit--require-managed-repo)))
    (let ((default-directory repo)
          (current-prefix-arg (and (eq mode 'force) '(4)))
          (git-overleaf-enable-async
           (git-overleaf--async-supported-p)))
      (call-interactively #'git-overleaf-push))))

(defun git-overleaf-magit--around-git-fetch (function remote args)
  "Route a branchless fetch of logical REMOTE through git-overleaf.
Call FUNCTION with REMOTE and ARGS for every ordinary Git remote."
  (let* ((repo (and remote (magit-toplevel)))
         (overleaf-remote
          (and repo
               (git-overleaf--managed-repo-p repo)
               (git-overleaf--remote-name repo))))
    (if (not (and overleaf-remote (equal remote overleaf-remote)))
        (funcall function remote args)
      (pcase (git-overleaf-magit--fetch-mode args)
        (_ (git-overleaf-magit-refresh-remote t))))))

(defun git-overleaf-magit--operation-succeeded (repo operation)
  "Refresh REPO's Magit status after a successful Overleaf OPERATION."
  (when (memq operation '(fetch pull pull-abort push reset))
    (let ((default-directory repo))
      (when-let* ((buffer (magit-get-mode-buffer 'magit-status-mode)))
        (with-current-buffer buffer
          (let ((state (git-overleaf-magit--remote-state))
                (now (float-time)))
            (setf (git-overleaf-magit--remote-state-refreshing state) nil
                  (git-overleaf-magit--remote-state-last-attempt-time state) now
                  (git-overleaf-magit--remote-state-last-success-time state) now
                  (git-overleaf-magit--remote-state-error state) nil))
          (magit-refresh))))))

(defun git-overleaf-magit--install-fetch-integration ()
  "Install the Overleaf action and routing in `magit-fetch'."
  (unless git-overleaf-magit--fetch-integration-installed
    (setq git-overleaf-magit--fetch-integration-installed t)
    (transient-append-suffix
      'magit-fetch "e" '("O" git-overleaf-magit-fetch))
    (advice-add 'magit-git-fetch :around
                #'git-overleaf-magit--around-git-fetch)))

(defun git-overleaf-magit--install-pull-integration ()
  "Install the Overleaf action in `magit-pull'."
  (unless git-overleaf-magit--pull-integration-installed
    (setq git-overleaf-magit--pull-integration-installed t)
    (transient-append-suffix
      'magit-pull "e" '("O" git-overleaf-magit-pull))))

(defun git-overleaf-magit--install-push-integration ()
  "Install the Overleaf action in `magit-push'."
  (unless git-overleaf-magit--push-integration-installed
    (setq git-overleaf-magit--push-integration-installed t)
    (transient-append-suffix
      'magit-push "e" '("O" git-overleaf-magit-push))))

;;;; Setup

;;;###autoload
(defun git-overleaf-magit-setup ()
  "Enable the Overleaf section in `magit-status' buffers."
  (unless (require 'magit nil t)
    (user-error "git-overleaf-magit-setup requires the `magit' package"))
  (magit-add-section-hook
   'magit-status-sections-hook
   #'git-overleaf-magit-insert-status
   'magit-insert-stashes
   nil)
  (add-hook 'magit-status-mode-hook
            #'git-overleaf-magit--enable-status-buffer-hooks)
  (add-hook 'git-overleaf--after-operation-functions
            #'git-overleaf-magit--operation-succeeded)
  (with-eval-after-load 'magit-fetch
    (git-overleaf-magit--install-fetch-integration))
  (with-eval-after-load 'magit-pull
    (git-overleaf-magit--install-pull-integration))
  (with-eval-after-load 'magit-push
    (git-overleaf-magit--install-push-integration)))

(defun git-overleaf-magit--enable-status-buffer-hooks ()
  "Enable Overleaf refresh hooks in the current Magit status buffer."
  (add-hook 'magit-refresh-buffer-hook
            #'git-overleaf-magit--maybe-auto-refresh-remote
            nil
            t))

(provide 'git-overleaf-magit)

;;; git-overleaf-magit.el ends here
