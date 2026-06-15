;;; git-overleaf.el --- Clone, push, and pull full Overleaf projects with Git -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-5.5
;; Created: April 14, 2026
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Package-Requires: ((emacs "29.4") (websocket "1.15") (webdriver "0.1"))
;; Version: 2.0.0
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

;;;; Command helpers

(defun git-overleaf--repo-async-key (repo)
  "Return the async lock key for REPO."
  (format "repo:%s" (directory-file-name (expand-file-name repo))))

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

(defun git-overleaf--push-unstaged-action (repo noerror)
  "Return the unstaged-change action for pushing REPO.
When NOERROR is non-nil, do not prompt."
  (let ((status (git-overleaf--read-repo-status repo)))
    (when (git-overleaf--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before pushing"
       repo))
    (if (git-overleaf--repo-status-unstaged status)
        (progn
          (when noerror
            (user-error
             "Overleaf push requires a clean working tree; stage all changes or stash them first"))
          (unless
              (y-or-n-p
               (format
                "Repository %s has unstaged changes.  Stage all changes and continue with Overleaf push? "
                repo))
            (user-error
             "Overleaf push requires a clean working tree; stage all changes or stash them first"))
          'stage)
      'error)))

(defun git-overleaf--push-async (repo noerror)
  "Start an asynchronous push for REPO.
When NOERROR is non-nil, demote setup and background errors to warnings."
  (git-overleaf--with-repo-log-context repo
	                                   (condition-case err
		                                   (let* ((pending nil)
			                                      (unstaged-action nil)
			                                      (name nil))
		                                     (git-overleaf--set-repo-url repo)
		                                     (setq pending (git-overleaf--pending-state repo))
		                                     (if pending
			                                     (git-overleaf--ensure-clean-working-tree
			                                      repo
			                                      "finishing the pending Overleaf operation")
			                                   (setq unstaged-action
				                                     (git-overleaf--push-unstaged-action repo noerror)))
		                                     (setq name (format "Overleaf push `%s'"
							                                    (git-overleaf--project-name repo)))
		                                     (cl-labels
			                                     ((start ()
				                                    (git-overleaf--async-start
				                                     name
				                                     (lambda ()
					                                   (git-overleaf--push-sync repo unstaged-action t))
				                                     :key (git-overleaf--repo-async-key repo)
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

(defun git-overleaf--overwrite-remote-async (repo)
  "Start an asynchronous remote overwrite for REPO."
  (git-overleaf--with-repo-log-context repo
	                                   (git-overleaf--ensure-no-pending-action repo "overwriting the Overleaf remote")
	                                   (git-overleaf--set-repo-url repo)
	                                   (when (not
		                                      (yes-or-no-p
			                                   (format
			                                    "Overwrite Overleaf project `%s' with local HEAD?"
			                                    (git-overleaf--project-name repo))))
	                                     (user-error "Aborted"))
	                                   (let ((unstaged-action
		                                      (git-overleaf--push-unstaged-action repo nil)))
	                                     (git-overleaf--ensure-authenticated-async
	                                      "overwriting the Overleaf remote"
	                                      (lambda ()
		                                    (git-overleaf--async-start
		                                     (format "Overleaf remote overwrite `%s'"
				                                     (git-overleaf--project-name repo))
		                                     (lambda ()
			                                   (git-overleaf--overwrite-remote-sync repo unstaged-action t))
		                                     :key (git-overleaf--repo-async-key repo)))))))

(defun git-overleaf--pull-async (repo)
  "Start an asynchronous pull for REPO."
  (git-overleaf--with-repo-log-context repo
	                                   (git-overleaf--set-repo-url repo)
	                                   (let ((pending (git-overleaf--pending-state repo)))
	                                     (when pending
		                                   (pcase (plist-get pending :action)
		                                     ('pull
		                                      (user-error
			                                   "Unresolved merge conflicts from a previous pull; resolve them, commit, then run `git-overleaf-push'"))
		                                     (action
		                                      (user-error "Unsupported pending Overleaf action `%s'" action))))
	                                     (git-overleaf--ensure-clean-working-tree repo "pulling from Overleaf"))
	                                   (git-overleaf--ensure-authenticated-async
	                                    "pulling from Overleaf"
	                                    (lambda ()
	                                      (git-overleaf--async-start
		                                   (format "Overleaf pull `%s'" (git-overleaf--project-name repo))
		                                   (lambda ()
		                                     (git-overleaf--pull-sync repo t))
		                                   :key (git-overleaf--repo-async-key repo))))))

;;;; Interactive commands

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
(defun git-overleaf-push (&optional directory noerror)
  "Push the current Git repo to its configured Overleaf project.
Staged changes are committed automatically before the remote snapshot is
fetched.  When unstaged changes exist, prompt whether to stage them
first.

If a pending pull exists (merge conflict from a previous pull), verifies
the merge is complete and uploads the merged result.  If the remote has
diverged and no pending pull exists, signals an error asking you to run
`git-overleaf-pull' first.

Existing remote Overleaf text docs are updated through Overleaf's
real-time text OT path when possible, preserving document ids and web
history.  Non-doc files still use Overleaf upload/delete APIs.

When NOERROR is non-nil, silently return nil if DIRECTORY is not a
managed Overleaf repo, and demote push errors to warnings.  This is
useful for hooks such as `git-commit-post-finish-hook'."
  (interactive)
  (let* ((repo (or (and directory (git-overleaf-root directory))
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
      (git-overleaf--push-async repo noerror))
     (noerror
      (git-overleaf--with-repo-log-context repo
		                                   (condition-case err
			                                   (git-overleaf--push-sync repo)
		                                     (error
		                                      (git-overleaf--warn "Automatic Overleaf push failed for %s: %s"
								                                  repo (error-message-string err))))))
     (t
      (git-overleaf--push-sync repo)))))

(defun git-overleaf--push-sync (repo &optional unstaged-action skip-auth)
  "Internal: perform the actual push for managed REPO.
UNSTAGED-ACTION is passed to
`git-overleaf--prepare-working-tree-for-sync'.  When SKIP-AUTH is
non-nil, assume the caller already checked authentication."
  (git-overleaf--with-repo-log-context repo
	                                   (let ((pending nil)
		                                     (project-id nil))
	                                     (git-overleaf--set-repo-url repo)
	                                     (git-overleaf--prepare-sync-metadata-repo repo)
	                                     (setq pending (git-overleaf--pending-state repo))
	                                     (unless skip-auth
		                                   (git-overleaf--ensure-authenticated "pushing to Overleaf"))
	                                     (if pending
		                                     (git-overleaf--ensure-clean-working-tree repo "finishing the pending Overleaf operation")
		                                   (git-overleaf--prepare-working-tree-for-sync repo unstaged-action))
	                                     (setq project-id (git-overleaf--project-id repo))
	                                     (git-overleaf--with-remote-state
	                                      project-id
	                                      (lambda (remote-root remote-table)
		                                    (pcase (and pending (plist-get pending :action))
		                                      ('pull
			                                   (git-overleaf--finalize-pending-pull
			                                    repo pending remote-root remote-table))
		                                      (_
			                                   (when pending
			                                     (user-error "Unknown pending Overleaf action `%s'"
						                                     (plist-get pending :action)))
			                                   (git-overleaf--fresh-push repo remote-root remote-table))))))))

;;;###autoload
(defun git-overleaf-overwrite-remote (&optional directory)
  "Overwrite the configured Overleaf project with the current Git repo.

Optional DIRECTORY is the repository root.  Like `git-overleaf-push',
staged changes are committed automatically before upload.  Unlike
`git-overleaf-push', remote Overleaf changes are replaced by the
local \"HEAD\" snapshot.  Existing remote Overleaf text docs are
updated through text OT when possible, preserving document ids and
web history."
  (interactive)
  (let* ((repo (git-overleaf--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (git-overleaf--async-enabled-p))
        (git-overleaf--overwrite-remote-async repo)
      (git-overleaf--overwrite-remote-sync repo nil nil))))

;;;###autoload
(define-obsolete-function-alias
  'git-overleaf-push-force
  #'git-overleaf-overwrite-remote
  "2.0.0")

(defun git-overleaf--overwrite-remote-sync
    (repo &optional unstaged-action skip-auth)
  "Synchronously overwrite Overleaf with REPO.
UNSTAGED-ACTION is passed to
`git-overleaf--prepare-working-tree-for-sync'.  When SKIP-AUTH is
non-nil, assume the caller already checked authentication."
  (git-overleaf--with-repo-log-context repo
	                                   (let ((project-id nil)
		                                     (context nil))
	                                     (git-overleaf--ensure-no-pending-action repo "overwriting the Overleaf remote")
	                                     (git-overleaf--set-repo-url repo)
	                                     (git-overleaf--prepare-sync-metadata-repo repo)
	                                     (setq project-id (git-overleaf--project-id repo))
	                                     (unless skip-auth
		                                   (git-overleaf--ensure-authenticated "overwriting the Overleaf remote"))
	                                     (git-overleaf--prepare-working-tree-for-sync repo unstaged-action)
	                                     (git-overleaf--with-remote-state
	                                      project-id
	                                      (lambda (remote-root remote-table)
		                                    (setq context (git-overleaf--read-sync-state repo remote-root))
		                                    (if (memq (plist-get context :status) '(in-sync head-matches-remote))
			                                    (git-overleaf--note-matching-sync-state
			                                     repo
			                                     (plist-get context :head)
			                                     project-id
			                                     remote-table)
		                                      (git-overleaf--upload-head-and-set-base
			                                   repo
			                                   (plist-get context :head)
			                                   project-id
			                                   remote-root
			                                   remote-table
			                                   "Overwrote Overleaf project `%s' with local HEAD"
			                                   (git-overleaf--project-name repo))))))))

;;;###autoload
(defun git-overleaf-pull (&optional directory)
  "Pull the latest Overleaf snapshot into the current Git repo at DIRECTORY.
The working tree must be clean before pulling.

When the local branch has diverged from the remote, performs a
`git merge --no-ff --no-edit' directly on the current branch.  If the
merge succeeds, records the downloaded remote snapshot as the base so a
later `git-overleaf-push' uploads the merged result.  If there are
conflicts, records a pending-pull state and prompts you to resolve them,
commit, then run `git-overleaf-push' to complete the sync."
  (interactive)
  (let* ((repo (git-overleaf--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (git-overleaf--async-enabled-p))
        (git-overleaf--pull-async repo)
      (git-overleaf--pull-sync repo nil))))

(defun git-overleaf--pull-sync (repo &optional skip-auth)
  "Synchronously pull the latest Overleaf snapshot into REPO.
When SKIP-AUTH is non-nil, assume the caller already checked
authentication."
  (git-overleaf--with-repo-log-context repo
	                                   (let ((pending (git-overleaf--pending-state repo)))
	                                     (git-overleaf--set-repo-url repo)
	                                     (git-overleaf--prepare-sync-metadata-repo repo)
	                                     (when pending
		                                   (pcase (plist-get pending :action)
		                                     ('pull
		                                      (user-error
			                                   "Unresolved merge conflicts from a previous pull; resolve them, commit, then run `git-overleaf-push'"))
		                                     (action
		                                      (user-error "Unsupported pending Overleaf action `%s'" action))))
	                                     (git-overleaf--ensure-clean-working-tree repo "pulling from Overleaf")
	                                     (unless skip-auth
		                                   (git-overleaf--ensure-authenticated "pulling from Overleaf"))
	                                     (git-overleaf--with-downloaded-snapshot
	                                      (git-overleaf--project-id repo)
	                                      (lambda (remote-root)
		                                    (git-overleaf--fresh-pull repo remote-root))))))



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
  "k" #'git-overleaf-force-stop
  "l" #'git-overleaf-pull
  "p" #'git-overleaf-push
  "s" #'git-overleaf-push)


(provide 'git-overleaf)

;;; git-overleaf.el ends here
