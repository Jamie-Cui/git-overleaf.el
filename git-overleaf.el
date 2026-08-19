;;; git-overleaf.el --- Clone, push, and pull full Overleaf projects with Git -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-5.5
;; Created: April 14, 2026
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Package-Requires: ((emacs "29.4") (websocket "1.15") (webdriver "0.1") (magit-section "4.5") (transient "0.7.2"))
;; Version: 3.0.0
;; Keywords: hypermedia, tex, tools
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides project-level Overleaf integration:
;;
;; - clone a full Overleaf project to a local Git repository
;; - push committed local changes to Overleaf and pull remote updates back
;; - detect remote divergence and resolve it with normal Git merges
;;
;; Conflict resolution intentionally happens in Git, not ediff.  When
;; both local and remote changed, `git-overleaf-pull' merges the
;; downloaded remote snapshot into the current branch and leaves
;; conflicts to Magit or plain Git.

;;; Code:

(require 'git-overleaf-auth)
(require 'git-overleaf-core)
(require 'git-overleaf-http)
(require 'git-overleaf-sync)

(declare-function git-overleaf--ensure-no-active-merge
                  "git-overleaf-sync" (repo action))

;;;; Command helpers

(defun git-overleaf--ensure-authenticated-async (op-desc continuation)
  "Ensure cookies are usable before OP-DESC, then call CONTINUATION.
OP-DESC is a user-facing operation description used in authentication
error messages.
When authentication is needed, run browser authentication in the
background before calling CONTINUATION."
  (let ((state (git-overleaf--cookie-state)))
    (if (eq (plist-get state :status) 'valid)
	    (funcall continuation)
      (let ((reason (git-overleaf--authentication-needed-reason state)))
	    (if (or noninteractive
		        (not
		         (let ((use-dialog-box nil))
                   (y-or-n-p
                    (format "%s Re-run `git-overleaf-authenticate` now? "
                            reason)))))
            (user-error
             "%s Run `git-overleaf-authenticate` before %s"
             reason
             (or op-desc "continuing"))
          (git-overleaf--start-authentication-async
           git-overleaf-url
           (lambda (_full-cookies)
             (funcall continuation))))))))

(defun git-overleaf--read-project-async (url continuation)
  "Fetch projects from URL in the background, then call CONTINUATION."
  (git-overleaf--async-start
   (format "Overleaf project list from %s" (git-overleaf--url-host))
   (lambda ()
     (git-overleaf-list url))
   :key (format "project-list:%s" url)
   :on-success
   (lambda (projects)
     (funcall continuation (git-overleaf--select-project projects)))))

(defun git-overleaf--clone-target-directory (project target-directory)
  "Return the TARGET-DIRECTORY for cloning PROJECT."
  (directory-file-name
   (expand-file-name
    (or target-directory
        (read-file-name
         "Clone to directory: "
         default-directory
         (expand-file-name
          (git-overleaf--sanitize-name (plist-get project :name))
          default-directory)
         nil
         (git-overleaf--sanitize-name (plist-get project :name)))))))

(defun git-overleaf--validate-clone-target (target)
  "Signal if TARGET is not a valid clone target."
  (when (and (file-exists-p target)
             (not (file-directory-p target)))
    (user-error "Target path %s exists and is not a directory" target))
  (unless (git-overleaf--directory-empty-p target)
    (user-error "Target directory %s is not empty" target)))

(defun git-overleaf--clone-selected-project (url project target)
  "Synchronously clone PROJECT from URL into TARGET."
  (let ((git-overleaf-url url)
        (target (directory-file-name (expand-file-name target)))
        (repo nil))
    (git-overleaf-log-with-context
     (git-overleaf--log-context-for-project project target)
     (git-overleaf--validate-clone-target target)
     (git-overleaf--with-downloaded-snapshot
      (plist-get project :id)
      (lambda (snapshot-root)
        (make-directory target t)
        (git-overleaf--copy-directory-contents snapshot-root target)
        (setq repo target)
        (git-overleaf--git-output repo "init")
        (git-overleaf--write-repo-metadata repo project)
        (git-overleaf--prepare-sync-metadata-repo repo)
        (git-overleaf--git-output repo "add" "--all" ".")
        (apply
         #'git-overleaf--git-output
         repo
        (append
          (git-overleaf--git-identity-args repo)
          '("commit" "-m" "chore: import project from Overleaf")))
        (git-overleaf--set-base-ref repo "HEAD")
        (git-overleaf--set-remote-ref repo "HEAD")
        (git-overleaf--record-remote-fetch-time repo)
        (git-overleaf--message
         "Cloned `%s' into %s"
         (plist-get project :name)
         target))))))

(defun git-overleaf--clone-sync (&optional url target-directory)
  "Synchronously clone a full Overleaf project from URL into TARGET-DIRECTORY."
  (let* ((url (or url (git-overleaf--url)))
         (project nil)
         (target nil))
    (setq git-overleaf-url url)
    (git-overleaf--ensure-authenticated "cloning from Overleaf")
    (setq project (git-overleaf--read-project url))
    (setq target
          (git-overleaf--clone-target-directory project target-directory))
    (git-overleaf--validate-clone-target target)
    (git-overleaf--clone-selected-project url project target)))

(defun git-overleaf--clone-async (&optional url target-directory)
  "Start an asynchronous clone from URL into TARGET-DIRECTORY."
  (let ((url (or url (git-overleaf--url))))
    (setq git-overleaf-url url)
    (git-overleaf--ensure-authenticated-async
     "cloning from Overleaf"
     (lambda ()
       (git-overleaf--read-project-async
        url
        (lambda (project)
          (let ((target
                 (git-overleaf--clone-target-directory
                  project
                  target-directory)))
            (git-overleaf--validate-clone-target target)
            (git-overleaf-log-with-context
             (git-overleaf--log-context-for-project project target)
             (git-overleaf--async-start
              (format "Overleaf clone `%s'" (plist-get project :name))
              (lambda ()
                (git-overleaf--clone-selected-project url project target))
              :key (format "clone:%s" target))))))))))

(defun git-overleaf--init-confirm-p (repo current-id current-name project)
  "Return non-nil if initializing REPO for PROJECT should continue.
CURRENT-ID is the Overleaf project id currently recorded for REPO, or
nil when REPO is not bound to a project.  CURRENT-NAME is the recorded
project name used in confirmation prompts."
  (or (not current-id)
      (yes-or-no-p
       (if (string= current-id (plist-get project :id))
           (format
            "Reinitialize the Overleaf base snapshot for `%s` against `%s'? "
            repo
            (or current-name current-id))
         (format
          "Rebind `%s' from Overleaf project `%s' to `%s'? "
          repo
          (or current-name current-id)
          (plist-get project :name))))))

(defun git-overleaf--init-selected-project (repo project)
  "Synchronously bind REPO to PROJECT and initialize its base snapshot."
  (git-overleaf-log-with-context
   (git-overleaf--log-context-for-project project repo)
   (git-overleaf--ensure-no-pending-action repo "reconfiguring the repository")
   (git-overleaf--prepare-sync-metadata-repo repo)
   (git-overleaf--with-downloaded-snapshot
    (plist-get project :id)
    (lambda (snapshot-root)
      (git-overleaf--initialize-base-ref repo project snapshot-root)
      (git-overleaf--message
       "Configured `%s' to track Overleaf project `%s' without pulling or pushing"
       repo
       (plist-get project :name))))))

(defun git-overleaf--init-sync (&optional directory url confirm)
  "Synchronously bind DIRECTORY to an Overleaf project on URL.
When CONFIRM is non-nil, ask before rebinding an existing project."
  (let* ((repo (git-overleaf--require-repo directory))
         (current-id nil)
         (current-name nil)
         (project nil))
    (git-overleaf--ensure-no-pending-action repo "reconfiguring the repository")
    (git-overleaf--set-repo-url repo url)
    (git-overleaf--prepare-sync-metadata-repo repo)
    (git-overleaf--ensure-authenticated "configuring the Overleaf project")
    (setq current-id (git-overleaf--git-config-get repo "git-overleaf.projectId"))
    (setq current-name (git-overleaf--git-config-get repo "git-overleaf.projectName"))
    (setq project (git-overleaf--read-project git-overleaf-url))
    (when (and confirm
               (not
                (git-overleaf--init-confirm-p
                 repo current-id current-name project)))
      (user-error "Aborted"))
    (git-overleaf--init-selected-project repo project)))

(defun git-overleaf--init-async (&optional directory url)
  "Start an asynchronous Overleaf project (from URL) initialization at DIRECTORY."
  (let* ((repo (git-overleaf--require-repo directory))
         (current-id nil)
         (current-name nil))
    (git-overleaf--ensure-no-pending-action repo "reconfiguring the repository")
    (git-overleaf--set-repo-url repo url)
    (setq current-id (git-overleaf--git-config-get repo "git-overleaf.projectId"))
    (setq current-name (git-overleaf--git-config-get repo "git-overleaf.projectName"))
    (git-overleaf--ensure-authenticated-async
     "configuring the Overleaf project"
     (lambda ()
       (git-overleaf--read-project-async
        git-overleaf-url
        (lambda (project)
          (when (not
                 (git-overleaf--init-confirm-p
                  repo current-id current-name project))
            (user-error "Aborted"))
          (git-overleaf-log-with-context
           (git-overleaf--log-context-for-project project repo)
           (git-overleaf--async-start
            (format "Overleaf init `%s'" repo)
            (lambda ()
              (git-overleaf--init-selected-project repo project))
            :key (git-overleaf--repo-async-key repo)))))))))

(defun git-overleaf--push-async (repo force noerror)
  "Start an asynchronous push for REPO.
When FORCE is non-nil, replace divergent remote content with HEAD.
When NOERROR is non-nil, demote setup and background errors to warnings."
  (git-overleaf--with-repo-log-context repo
	                                   (condition-case err
		                                   (let ((name nil))
		                                     (git-overleaf--set-repo-url repo)
		                                     (git-overleaf--ensure-no-active-merge
		                                      repo "pushing to Overleaf")
		                                     (setq name (format "Overleaf push `%s'"
							                                    (git-overleaf--project-name repo)))
		                                     (cl-labels
			                                     ((start ()
				                                    (git-overleaf--async-start
				                                     name
				                                     (lambda ()
					                                   (git-overleaf--push-sync repo force t))
				                                     :key (git-overleaf--repo-async-key repo)
				                                     :on-success
				                                     (lambda (_value)
					                                   (git-overleaf--notify-operation-succeeded
					                                    repo 'push)
					                                   (git-overleaf--message
					                                    "Finished %s" name))
				                                     :on-error
				                                     (lambda (message)
					                                   (if noerror
						                                   (git-overleaf--warn
						                                    "Automatic Overleaf push failed for %s: %s"
						                                    repo message)
					                                     (git-overleaf--warn "%s failed: %s" name message))))))
			                                   (if noerror
				                                   (progn
				                                     (git-overleaf--get-cookies)
				                                     (start))
			                                     (git-overleaf--ensure-authenticated-async
			                                      "pushing to Overleaf"
			                                      #'start))))
	                                    (error
	                                     (if noerror
	                                         (git-overleaf--warn "Automatic Overleaf push failed for %s: %s"
	                                                             repo (error-message-string err))
	                                       (signal (car err) (cdr err)))))))

(defun git-overleaf--confirm-force-push (repo)
  "Ask for confirmation before force-pushing REPO's HEAD to Overleaf."
  (unless
      (yes-or-no-p
       (format
        "Force-push local HEAD to Overleaf project `%s', replacing divergent remote content? "
        (git-overleaf--project-name repo)))
    (user-error "Aborted")))

(defun git-overleaf--pull-async (repo)
  "Start an asynchronous pull for REPO."
  (git-overleaf--with-repo-log-context repo
	                                   (git-overleaf--set-repo-url repo)
	                                   (git-overleaf--ensure-pull-startable repo)
	                                   (git-overleaf--ensure-authenticated-async
	                                    "pulling from Overleaf"
	                                    (lambda ()
	                                      (git-overleaf--async-start
		                                   (format "Overleaf pull `%s'" (git-overleaf--project-name repo))
		                                   (lambda ()
		                                     (git-overleaf--pull-sync repo t))
		                                   :key (git-overleaf--repo-async-key repo)
		                                   :on-success
		                                   (lambda (_value)
		                                     (git-overleaf--notify-operation-succeeded
		                                      repo 'pull)
		                                     (git-overleaf--message
		                                      "Finished Overleaf pull `%s'"
		                                      (git-overleaf--project-name repo))))))))

(defun git-overleaf--fetch-sync (repo &optional skip-auth)
  "Synchronously fetch the latest Overleaf snapshot for REPO.
Update only the hidden remote snapshot ref.  When SKIP-AUTH is non-nil,
assume the caller already checked authentication."
  (git-overleaf--with-repo-log-context repo
    (git-overleaf--set-repo-url repo)
    (unless skip-auth
      (git-overleaf--ensure-authenticated "fetching from Overleaf"))
    (git-overleaf--with-downloaded-snapshot
     (git-overleaf--project-id repo)
     (lambda (remote-root)
       (let ((commit
              (git-overleaf--record-remote-snapshot repo remote-root)))
         (git-overleaf--message
          "Fetched Overleaf project `%s'"
          (git-overleaf--project-name repo))
         commit)))))

(defun git-overleaf--fetch-async (repo)
  "Start an asynchronous Overleaf snapshot fetch for REPO."
  (git-overleaf--with-repo-log-context repo
    (git-overleaf--set-repo-url repo)
    (git-overleaf--ensure-authenticated-async
     "fetching from Overleaf"
     (lambda ()
       (git-overleaf--async-start
        (format "Overleaf fetch `%s'" (git-overleaf--project-name repo))
        (lambda () (git-overleaf--fetch-sync repo t))
        :key (git-overleaf--repo-async-key repo)
        :on-success
        (lambda (_commit)
          (git-overleaf--notify-operation-succeeded repo 'fetch)
          (git-overleaf--message
           "Finished Overleaf fetch `%s'"
           (git-overleaf--project-name repo))))))))

;;;; Interactive commands

;;;###autoload
(defun git-overleaf-register-remote (&optional directory name)
  "Register a branchless logical Overleaf remote for DIRECTORY.
NAME defaults to `git-overleaf-remote-name'.  This writes only local Git
configuration and initializes the hidden remote snapshot ref from the
existing synchronization base; it does not access Overleaf."
  (interactive)
  (let* ((repo (git-overleaf--require-managed-repo directory))
         (name
          (or name
              (read-string
               "Logical Overleaf remote name: "
               git-overleaf-remote-name)))
         (remote (git-overleaf--register-remote repo name))
         (base (git-overleaf--rev-parse repo (git-overleaf--base-ref repo))))
    (git-overleaf--git-config-set
     repo "git-overleaf.remoteRef" git-overleaf-remote-ref)
    (unless (git-overleaf--rev-parse-noerror repo (git-overleaf--remote-ref repo))
      (git-overleaf--set-remote-ref repo base))
    (git-overleaf--message
     "Registered branchless Overleaf remote `%s'" remote)
    remote))

;;;###autoload
(defun git-overleaf-fetch (&optional directory)
  "Fetch the latest Overleaf snapshot for the repository at DIRECTORY.
Only the hidden remote snapshot ref is updated; HEAD, the working tree,
and the last-synchronized base ref are left unchanged."
  (interactive)
  (let ((repo (git-overleaf--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (git-overleaf--async-enabled-p))
        (git-overleaf--fetch-async repo)
      (prog1 (git-overleaf--fetch-sync repo nil)
        (git-overleaf--notify-operation-succeeded repo 'fetch)))))

;;;###autoload
(defun git-overleaf-status (&optional directory)
  "Show cached synchronization status for the Overleaf repo at DIRECTORY.
This command does not access the network.  Return the same status as a
plist so Lisp callers can inspect it without parsing display text."
  (interactive)
  (let* ((repo (git-overleaf--require-managed-repo directory))
         (data (git-overleaf--status-data repo)))
    (when (called-interactively-p 'interactive)
      (with-output-to-temp-buffer "*Git Overleaf Status*"
        (princ (format "Project:    %s\n" (plist-get data :project)))
        (princ (format "Repository: %s\n" (plist-get data :repo)))
        (princ (format "Branch:     %s\n" (plist-get data :branch)))
        (princ (format "State:      %s\n" (plist-get data :state)))
        (let ((worktree (plist-get data :worktree)))
          (princ
           (format "Worktree:   %s\n"
                   (if worktree
                       (mapconcat #'symbol-name worktree ", ")
                     "clean"))))
        (princ
         (format "Pending:    %s\n"
                 (or (plist-get data :pending-phase) "none")))
        (princ
         (format "Fetched:    %s\n"
                 (or (plist-get data :remote-fetched-at) "unknown")))
        (princ
         (format "Next:       %s\n"
                 (plist-get data :recommendation)))))
    data))

;;;###autoload
(defun git-overleaf-reset (&optional directory mode)
  "Reset the local branch to the cached Overleaf snapshot.
MODE defaults to `mixed', moving HEAD and resetting the index while
preserving working-tree files.  With a prefix argument interactively,
use `hard', also resetting tracked working-tree files.  Neither mode
deletes untracked or ignored files.  Run `git-overleaf-fetch' first when
you need a fresh remote snapshot."
  (interactive (list nil (if current-prefix-arg 'hard 'mixed)))
  (let* ((repo (git-overleaf--require-managed-repo directory))
         (mode (or mode 'mixed)))
    (when (called-interactively-p 'interactive)
      (unless
          (yes-or-no-p
           (if (eq mode 'hard)
               (format
                "Reset `%s' --hard to the cached Overleaf snapshot? Tracked local changes will be discarded. "
                (git-overleaf--current-branch repo))
             (format
              "Reset `%s' to the cached Overleaf snapshot, preserving working-tree files? "
              (git-overleaf--current-branch repo))))
        (user-error "Aborted")))
    (prog1 (git-overleaf--reset-to-remote repo mode)
      (git-overleaf--notify-operation-succeeded repo 'reset))))

;;;###autoload
(defun git-overleaf-pull-abort (&optional directory)
  "Abort an active pending Overleaf pull in DIRECTORY.
If the Git merge was already aborted manually, clear only stale pending
metadata.  Refuse to rewrite history after the merge has been committed."
  (interactive)
  (let ((repo (git-overleaf--require-managed-repo directory)))
    (prog1 (git-overleaf--abort-pending-pull repo)
      (git-overleaf--notify-operation-succeeded repo 'pull-abort))))

;;;###autoload
(defun git-overleaf-clone (&optional url target-directory)
  "Clone a full Overleaf project into TARGET-DIRECTORY.
If URL is nil, use `git-overleaf-url'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (git-overleaf--async-enabled-p))
      (git-overleaf--clone-async url target-directory)
    (git-overleaf--clone-sync url target-directory)))

;;;###autoload
(defun git-overleaf-init (&optional directory url)
  "Bind the Git repo in DIRECTORY to a remote Overleaf project on URL.
The command stores project metadata and initializes the hidden base
snapshot used by later `git-overleaf-push' and
`git-overleaf-pull' runs, but does not automatically pull or push."
  (interactive)
  (let ((interactive-p (called-interactively-p 'interactive)))
    (if (and interactive-p
             (git-overleaf--async-enabled-p))
        (git-overleaf--init-async directory url)
      (git-overleaf--init-sync directory url interactive-p))))

;;;###autoload
(defun git-overleaf-push (&optional directory force noerror)
  "Push HEAD of the current Git repo to its configured Overleaf project.
The index and working tree are ignored: this command uploads committed
content only and never stages or commits files.

With FORCE non-nil, replace divergent Overleaf content with HEAD after
interactive confirmation.  A normal push refuses remote divergence.
Interrupted uploads are journaled; another normal push safely resumes
them when the remote contains only a partial application of the same
target, while pull reconciles independent remote changes.

Existing remote Overleaf text docs are updated through Overleaf's
real-time text OT path when possible, preserving document ids and web
history.  Non-doc files still use Overleaf upload/delete APIs.

When NOERROR is non-nil, silently return nil if DIRECTORY is not a
managed Overleaf repo, and demote push errors to warnings.  This is
useful for hooks such as `git-commit-post-finish-hook'."
  (interactive (list nil current-prefix-arg nil))
  (let* ((force (and force t))
         (interactive-p (called-interactively-p 'interactive))
         (repo (or (and directory (git-overleaf-root directory))
                   (git-overleaf-root default-directory))))
    (cond
     ((not (and repo (git-overleaf--managed-repo-p repo)))
      (if noerror
          nil
        (user-error "Repository %s is not configured as an Overleaf project"
                    (or repo default-directory))))
     ((or (and (called-interactively-p 'interactive)
               (git-overleaf--async-enabled-p))
          (and noerror
               (git-overleaf--async-enabled-p)))
      (when (and interactive-p force)
        (git-overleaf--confirm-force-push repo))
      (git-overleaf--push-async repo force noerror))
     (noerror
     (git-overleaf--with-repo-log-context repo
		                                   (condition-case err
			                                   (prog1
			                                       (git-overleaf--push-sync repo force)
			                                     (git-overleaf--notify-operation-succeeded
			                                      repo 'push))
		                                     (error
		                                      (git-overleaf--warn "Automatic Overleaf push failed for %s: %s"
							                                  repo (error-message-string err))))))
     (t
      (when (and interactive-p force)
        (git-overleaf--confirm-force-push repo))
      (prog1 (git-overleaf--push-sync repo force)
        (git-overleaf--notify-operation-succeeded repo 'push))))))

(defun git-overleaf--push-sync (repo &optional force skip-auth)
  "Internal: perform the actual push for managed REPO.
When FORCE is non-nil, replace divergent remote content.  When
SKIP-AUTH is non-nil, assume the caller already checked authentication."
  (git-overleaf--with-repo-log-context repo
	                                   (let ((project-id nil))
	                                     (git-overleaf--set-repo-url repo)
	                                     (git-overleaf--prepare-sync-metadata-repo repo)
	                                     (git-overleaf--ensure-no-active-merge
	                                      repo "pushing to Overleaf")
	                                     (unless skip-auth
		                                   (git-overleaf--ensure-authenticated "pushing to Overleaf"))
	                                     (setq project-id (git-overleaf--project-id repo))
	                                     (git-overleaf--with-remote-state
	                                      project-id
	                                      (lambda (remote-root remote-table)
		                                    (git-overleaf--push-with-remote-state
		                                     repo remote-root remote-table force))))))

;;;###autoload
(defun git-overleaf-pull (&optional directory)
  "Pull the latest Overleaf snapshot into the current Git repo at DIRECTORY.
Like `git pull', this command preserves staged, unstaged, and untracked
changes when Git can merge without overwriting them.  If local changes
overlap incoming changes, Git rejects the merge and leaves them intact.

When the local branch has diverged from the remote, performs a
`git merge --no-ff --no-edit' directly on the current branch.  If the
merge succeeds, records the downloaded remote snapshot as the base so a
later `git-overleaf-push' uploads the merged result.  If there are
conflicts, records a pending-pull state and prompts you to resolve them,
commit, then run `git-overleaf-push' to complete the sync.

If Overleaf changes again after a conflicted merge was committed, run
this command again: it integrates the newer snapshot instead of trapping
the repository in pending state.  It also reconciles a partially failed
push with independent remote changes."
  (interactive)
  (let* ((repo (git-overleaf--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (git-overleaf--async-enabled-p))
        (git-overleaf--pull-async repo)
      (prog1 (git-overleaf--pull-sync repo nil)
        (git-overleaf--notify-operation-succeeded repo 'pull)))))

(defun git-overleaf--pull-sync (repo &optional skip-auth)
  "Synchronously pull the latest Overleaf snapshot into REPO.
When SKIP-AUTH is non-nil, assume the caller already checked
authentication."
  (git-overleaf--with-repo-log-context repo
	                                   (git-overleaf--set-repo-url repo)
	                                   (git-overleaf--prepare-sync-metadata-repo repo)
	                                   (git-overleaf--ensure-pull-startable repo)
	                                   (unless skip-auth
	                                     (git-overleaf--ensure-authenticated
	                                      "pulling from Overleaf"))
	                                   (git-overleaf--with-downloaded-snapshot
	                                    (git-overleaf--project-id repo)
	                                    (lambda (remote-root)
	                                      (git-overleaf--pull-with-remote-state
	                                       repo remote-root)))))



;;;###autoload
(defun git-overleaf-browse-remote (&optional directory)
  "Open the configured Overleaf project at DIRECTORY in a browser."
  (interactive)
  (let* ((repo (or (and directory (git-overleaf-root directory))
                   (git-overleaf-root default-directory))))
    (if repo
        (progn
          (git-overleaf--set-repo-url repo)
          (browse-url
           (git-overleaf--project-page-url
            (git-overleaf--project-id repo))))
      (if (and (called-interactively-p 'interactive)
               (git-overleaf--async-enabled-p))
          (git-overleaf--ensure-authenticated-async
           "selecting an Overleaf project"
           (lambda ()
             (git-overleaf--read-project-async
              git-overleaf-url
              (lambda (project)
                (browse-url
                 (git-overleaf--project-page-url
                  (plist-get project :id)))))))
        (browse-url
         (git-overleaf--project-page-url
          (plist-get (git-overleaf--read-project) :id)))))))

;;;###autoload
(defun git-overleaf-force-stop ()
  "Stop all running background Overleaf operations.

This cancels tracked Emacs threads, interrupts external processes they
started, clears async operation locks, and discards pending foreground
callbacks.  The cancellation is best effort: external tools that have
already changed local or remote state are not rolled back."
  (interactive)
  (git-overleaf--force-stop))

;;;; Command map

;;;###autoload
(defvar-keymap git-overleaf-command-map
  "a" #'git-overleaf-authenticate
  "b" #'git-overleaf-browse-remote
  "c" #'git-overleaf-clone
  "f" #'git-overleaf-fetch
  "k" #'git-overleaf-force-stop
  "l" #'git-overleaf-pull
  "q" #'git-overleaf-pull-abort
  "p" #'git-overleaf-push
  "R" #'git-overleaf-reset
  "r" #'git-overleaf-register-remote
  "s" #'git-overleaf-status)


(provide 'git-overleaf)

;;; git-overleaf.el ends here
