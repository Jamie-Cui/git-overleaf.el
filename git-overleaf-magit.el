;;; git-overleaf-magit.el --- Overleaf sync section for magit-status -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds an Overleaf section to `magit-status' buffers showing the sync
;; state and a diff of local changes against the last synced snapshot.

;;; Code:

(require 'git-overleaf)
(require 'magit nil t)

(declare-function git-overleaf--async-enabled-p "git-overleaf-core")

;;;; Customization

(defcustom git-overleaf-magit-auto-refresh-remote t
  "Whether `magit-status' refreshes should also refresh the Overleaf remote.

When non-nil and `git-overleaf-enable-async' is also non-nil,
refreshes of an Overleaf-managed `magit-status' buffer may start a
background download of the latest remote snapshot.  Automatic downloads
are throttled internally.  The follow-up refresh that displays the
downloaded snapshot does not start another remote download."
  :type 'boolean
  :group 'git-overleaf)

(defconst git-overleaf-magit--auto-refresh-remote-interval 300
  "Minimum seconds between automatic Overleaf remote refreshes in Magit.")

;;;; Buffer-local state

(defvar-local git-overleaf-magit--remote-commit nil
  "SHA of the remote snapshot commit after `git-overleaf-magit-refresh-remote'.")

(defvar-local git-overleaf-magit--remote-base-rev nil
  "Base revision used to create `git-overleaf-magit--remote-commit'.")

(defvar-local git-overleaf-magit--refreshing nil
  "Non-nil while a remote refresh is in progress.")

(defvar-local git-overleaf-magit--last-remote-refresh-time nil
  "Last time an Overleaf remote refresh started in this Magit buffer.")

(defvar-local git-overleaf-magit--suppress-next-auto-refresh nil
  "Non-nil means skip the next automatic remote refresh once.")

;;;; Helpers

(defun git-overleaf-magit--state-label
    (repo local-in-sync remote-known remote-in-sync local-matches-remote)
  "Return (LABEL . FACE) describing the sync state of REPO.
LOCAL-IN-SYNC is non-nil when the base tree matches HEAD.
REMOTE-KNOWN is non-nil when a fresh remote snapshot is available.
REMOTE-IN-SYNC is non-nil when that remote snapshot matches the base tree.
LOCAL-MATCHES-REMOTE is non-nil when HEAD matches the remote snapshot."
  (let ((pending (git-overleaf--pending-state repo)))
    (cond
     (pending
      (cons (format "pending %s" (plist-get pending :action)) 'error))
     ((and local-in-sync (or (not remote-known) remote-in-sync))
      (cons "in sync" 'magit-dimmed))
     ((and remote-known local-in-sync)
      (cons "remote changes" 'warning))
     ((and remote-known local-matches-remote)
      (cons "local matches remote" 'warning))
     ((and remote-known remote-in-sync)
      (cons "local changes" 'warning))
     (remote-known
      (cons "local and remote changes" 'warning))
     (t
      (cons "local changes" 'warning)))))

(defun git-overleaf-magit--fresh-remote-commit (base-rev)
  "Return the remote snapshot commit when it was created from BASE-REV.
Clear the buffer-local remote snapshot when it belongs to an older base."
  (when (and git-overleaf-magit--remote-commit
             (not (equal git-overleaf-magit--remote-base-rev base-rev)))
    (setq git-overleaf-magit--remote-commit nil)
    (setq git-overleaf-magit--remote-base-rev nil)
    (setq git-overleaf-magit--last-remote-refresh-time nil))
  git-overleaf-magit--remote-commit)

(defun git-overleaf-magit--status-buffer-for-repo (repo)
  "Return the `magit-status-mode' buffer for REPO, creating one if needed."
  (let ((default-directory repo))
    (or (magit-get-mode-buffer 'magit-status-mode)
        (let ((git-overleaf-magit-auto-refresh-remote nil))
          (magit-status-setup-buffer repo)))))

;;;; Section insertion

(defun git-overleaf-magit-insert-status ()
  "Insert an Overleaf section into the current `magit-status' buffer."
  (when-let* ((repo (magit-toplevel))
              (managed (git-overleaf--managed-repo-p repo))
              (base-ref (git-overleaf--base-ref repo))
              (base-rev (git-overleaf--rev-parse-noerror repo base-ref)))
    (let* ((name (git-overleaf--project-name repo))
           (base-tree (git-overleaf--tree-id repo base-ref))
           (head-tree (git-overleaf--tree-id repo "HEAD"))
           (remote-commit
            (git-overleaf-magit--fresh-remote-commit base-rev))
           (remote-tree
            (and remote-commit
                 (git-overleaf--tree-id repo remote-commit)))
           (local-in-sync (equal base-tree head-tree))
           (remote-known remote-tree)
           (remote-in-sync (and remote-known (equal base-tree remote-tree)))
           (local-matches-remote
            (and remote-known (equal head-tree remote-tree)))
           (state (git-overleaf-magit--state-label
                   repo
                   local-in-sync
                   remote-known
                   remote-in-sync
                   local-matches-remote))
           (label (car state))
           (face (cdr state)))
      (magit-insert-section (overleaf)
		(magit-insert-heading
		  (format "Overleaf: %s (%s%s)"
				  (propertize name 'font-lock-face 'magit-section-heading)
				  (propertize label 'font-lock-face face)
				  (if git-overleaf-magit--refreshing
					  (propertize ", refreshing..." 'font-lock-face 'magit-dimmed)
				    "")))
		;; Local changes diff (base..HEAD), collapsed by default
		(unless local-in-sync
		  (magit-insert-section (overleaf-local nil t)
			(magit-insert-heading "Local changes (base..HEAD):")
			(magit-insert-section-body
			  (let ((default-directory repo))
				(magit--insert-diff nil
				  "diff" base-rev "HEAD" "--no-prefix")))))
		;; Remote changes (shown after r refresh)
		(when (and remote-commit (not remote-in-sync))
		  (magit-insert-section (overleaf-remote nil t)
			(magit-insert-heading "Remote changes (base..remote):")
			(magit-insert-section-body
			  (let ((default-directory repo))
				(magit--insert-diff nil
				  "diff" base-rev remote-commit
				  "--no-prefix")))))))))

;;;; Remote refresh

(defun git-overleaf-magit--auto-refresh-due-p ()
  "Return non-nil when this Magit buffer may auto-refresh the remote."
  (or (null git-overleaf-magit--last-remote-refresh-time)
      (>= (- (float-time) git-overleaf-magit--last-remote-refresh-time)
          git-overleaf-magit--auto-refresh-remote-interval)))

(defun git-overleaf-magit--maybe-auto-refresh-remote ()
  "Refresh the Overleaf remote after `magit-status' buffer refreshes."
  (cond
   (git-overleaf-magit--suppress-next-auto-refresh
    (setq git-overleaf-magit--suppress-next-auto-refresh nil))
   ((and git-overleaf-magit-auto-refresh-remote
         (git-overleaf--async-enabled-p)
         (derived-mode-p 'magit-status-mode)
         (not git-overleaf-magit--refreshing)
         (git-overleaf-magit--auto-refresh-due-p))
    (when-let* ((repo (magit-toplevel)))
      (when (git-overleaf--managed-repo-p repo)
        (condition-case err
            (git-overleaf-magit-refresh-remote)
          (error
	       (git-overleaf--debug
	        "Skipping automatic Overleaf remote refresh: %s"
	        (error-message-string err)))))))))

(defun git-overleaf-magit--finish-remote-refresh
    (magit-buf remote-commit base-rev &optional message)
  "Update MAGIT-BUF after a remote refresh.
REMOTE-COMMIT is the downloaded snapshot commit, or nil on failure.
BASE-REV is the base revision used to create REMOTE-COMMIT.
MESSAGE is displayed before refreshing the Magit buffer when non-nil."
  (when (buffer-live-p magit-buf)
    (with-current-buffer magit-buf
      (setq git-overleaf-magit--remote-commit remote-commit)
      (setq git-overleaf-magit--remote-base-rev
            (and remote-commit base-rev))
      (setq git-overleaf-magit--refreshing nil)
      (setq git-overleaf-magit--suppress-next-auto-refresh t)
      (when message
        (git-overleaf--message "%s" message))
      (magit-refresh))))

(defun git-overleaf-magit--enable-status-buffer-hooks ()
  "Enable Overleaf Magit hooks in the current `magit-status' buffer."
  (add-hook 'magit-refresh-buffer-hook
            #'git-overleaf-magit--maybe-auto-refresh-remote
            nil
            t))

(defun git-overleaf-magit-refresh-remote ()
  "Download the remote Overleaf snapshot and show remote changes.
Downloads, extracts, creates a temporary commit, and refreshes the
Magit buffer.  When `git-overleaf-enable-async' is non-nil, run the
heavy work in the background."
  (interactive)
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (_ (unless (git-overleaf--managed-repo-p repo)
              (user-error "Not an Overleaf project")))
         (magit-buf (git-overleaf-magit--status-buffer-for-repo repo)))
    (with-current-buffer magit-buf
      (when git-overleaf-magit--refreshing
        (user-error "Remote refresh already in progress")))
    (git-overleaf--set-repo-url repo)
    (let* ((base-ref (git-overleaf--base-ref repo))
           (base-rev (or (git-overleaf--rev-parse-noerror repo base-ref)
                         (user-error "Base ref %s does not exist" base-ref)))
           (project-id (git-overleaf--project-id repo))
           (previous-last-refresh-time
            (with-current-buffer magit-buf
              git-overleaf-magit--last-remote-refresh-time)))
      (git-overleaf--with-repo-log-context repo
        (with-current-buffer magit-buf
          (setq git-overleaf-magit--refreshing t)
          (setq git-overleaf-magit--last-remote-refresh-time (float-time)))
        (condition-case err
            (git-overleaf--async-start
             "Overleaf Magit remote refresh"
             (lambda ()
               (git-overleaf--set-repo-url repo)
               (git-overleaf--with-downloaded-snapshot
                project-id
                (lambda (root)
                  (git-overleaf--commit-directory
                   repo
                   root
                   base-rev
                   "overleaf remote snapshot"))))
             :key (format "magit-remote:%s"
                          (directory-file-name (expand-file-name repo)))
             :on-success
             (lambda (commit)
               (git-overleaf-magit--finish-remote-refresh
                magit-buf
                commit
                base-rev
                "Remote snapshot ready."))
             :on-error
             (lambda (message)
               (git-overleaf-magit--finish-remote-refresh
                magit-buf nil nil)
               (git-overleaf--warn "Overleaf remote refresh failed: %s" message)))
          (error
           (with-current-buffer magit-buf
             (setq git-overleaf-magit--refreshing nil)
             (setq git-overleaf-magit--last-remote-refresh-time
                   previous-last-refresh-time))
           (signal (car err) (cdr err))))))))

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
            #'git-overleaf-magit--enable-status-buffer-hooks))

(provide 'git-overleaf-magit)

;;; git-overleaf-magit.el ends here
