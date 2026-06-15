;;; git-overleaf-sync.el --- Snapshot sync internals for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Remote sync metadata, local snapshots, and Git-backed sync logic.

;;; Code:

(require 'json)
(require 'git-overleaf-core)
(require 'git-overleaf-http)

;;;; Remote sync metadata helpers

(defun git-overleaf--sync-metadata-relative-path ()
  "Return the configured root-level sync metadata path."
  (when git-overleaf-sync-metadata-enabled
    (let ((path (string-trim git-overleaf-sync-metadata-file)))
      (cond
       ((string-empty-p path)
        (user-error "`git-overleaf-sync-metadata-file' cannot be empty"))
       ((file-name-absolute-p path)
        (user-error "`git-overleaf-sync-metadata-file' must be relative"))
       ((string-match-p "/" path)
        (user-error "`git-overleaf-sync-metadata-file' must be root-level"))
       ((string-match-p "\\`\\.\\.\\'" path)
        (user-error "`git-overleaf-sync-metadata-file' cannot be `..'"))
       (t path)))))

(defun git-overleaf--sync-metadata-path-p (path)
  "Return non-nil if PATH is the reserved sync metadata path."
  (and git-overleaf-sync-metadata-enabled
       (string= path (git-overleaf--sync-metadata-relative-path))))

(defun git-overleaf--sync-metadata-file-in-root (root)
  "Return the sync metadata file path inside ROOT."
  (expand-file-name (git-overleaf--sync-metadata-relative-path) root))

(defun git-overleaf--read-sync-metadata-file (file)
  "Read sync metadata from FILE, returning nil if FILE is invalid."
  (when (file-regular-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer :object-type 'plist :array-type 'list))
      (error
       (git-overleaf--warn
        "Ignoring invalid Overleaf sync metadata file %s: %s"
        file
        (error-message-string err))
       nil))))

(defun git-overleaf--read-sync-metadata-file-text (file)
  "Read sync metadata FILE as UTF-8 text for in-place remote updates."
  (condition-case err
      (git-overleaf--file-utf8-string file)
    (error
     (git-overleaf--warn
      "Could not preserve existing Overleaf sync metadata for OT update: %s"
      (error-message-string err))
     nil)))

(defun git-overleaf--extract-remote-sync-metadata (root)
  "Read and remove the remote sync metadata file from ROOT.
The file is removed so downloaded snapshots compare only user project
content."
  (when git-overleaf-sync-metadata-enabled
    (let ((file (git-overleaf--sync-metadata-file-in-root root)))
      (cond
       ((file-regular-p file)
        (setq git-overleaf--remote-sync-metadata-text
              (git-overleaf--read-sync-metadata-file-text file))
        (prog1 (git-overleaf--read-sync-metadata-file file)
          (delete-file file)))
       ((file-exists-p file)
        (git-overleaf--warn
         "Ignoring reserved Overleaf sync metadata path because it is not a file: %s"
         file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))
        nil)))))

(defun git-overleaf--git-object-id-p (value)
  "Return non-nil if VALUE looks like a full Git object id."
  (and (stringp value)
       (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" value)))

(defun git-overleaf--remote-sync-metadata-commit (repo remote-tree)
  "Return the Git commit recorded by REPO metadata if it matches REMOTE-TREE."
  (let* ((metadata git-overleaf--remote-sync-metadata)
         (commit (plist-get metadata :localCommit))
         (tree (plist-get metadata :localTree)))
    (when (and git-overleaf-sync-metadata-enabled
               (git-overleaf--git-object-id-p commit)
               (git-overleaf--git-object-id-p tree)
               (string= tree remote-tree))
      (when-let* ((resolved
                   (git-overleaf--git-output-noerror
                    repo
                    "rev-parse"
                    "--verify"
                    (format "%s^{commit}" commit))))
        (when (string= (git-overleaf--tree-id repo resolved) remote-tree)
          (git-overleaf--debug
           "Remote sync metadata maps snapshot to local commit %s"
           resolved)
          resolved)))))

(defun git-overleaf--sync-metadata-json (repo revision project-id)
  "Return JSON sync metadata for REVISION in REPO and PROJECT-ID."
  (let* ((commit (git-overleaf--rev-parse repo revision))
         (tree (git-overleaf--tree-id repo commit)))
    (concat
     (json-encode
      `(:schema 1
		        :tool "git-overleaf"
		        :projectId ,project-id
		        :overleafUrl ,(git-overleaf--url)
		        :localCommit ,commit
		        :localTree ,tree
		        :syncedAt ,(format-time-string
			                "%Y-%m-%dT%H:%M:%SZ"
			                (current-time)
			                t)))
     "\n")))

(defun git-overleaf--ensure-sync-metadata-ignored (repo)
  "Add the reserved sync metadata file to REPO's local Git exclude file."
  (when git-overleaf-sync-metadata-enabled
    (let* ((path (git-overleaf--sync-metadata-relative-path))
           (git-dir (expand-file-name ".git" repo))
           (exclude-file (expand-file-name "info/exclude" git-dir)))
      (when (file-directory-p git-dir)
        (make-directory (file-name-directory exclude-file) t)
        (with-temp-buffer
          (when (file-readable-p exclude-file)
            (insert-file-contents exclude-file))
          (goto-char (point-min))
          (unless (re-search-forward
                   (format "^%s$" (regexp-quote path))
                   nil
                   t)
            (goto-char (point-max))
            (unless (or (bobp) (= (char-before) ?\n))
              (insert "\n"))
            (insert path "\n")
            (write-region (point-min) (point-max) exclude-file nil 'silent)))))))

(defun git-overleaf--ensure-sync-metadata-untracked (repo)
  "Signal if REPO tracks the reserved sync metadata file."
  (when git-overleaf-sync-metadata-enabled
    (let ((path (git-overleaf--sync-metadata-relative-path)))
      (when (git-overleaf--git-output-noerror
             repo
             "ls-files"
             "--error-unmatch"
             "--"
             path)
        (user-error
         "`%s' is reserved for Overleaf sync metadata; remove it from Git tracking"
         path)))))

(defun git-overleaf--prepare-sync-metadata-repo (repo)
  "Prepare REPO for remote sync metadata bookkeeping."
  (git-overleaf--ensure-sync-metadata-ignored repo)
  (git-overleaf--ensure-sync-metadata-untracked repo))

(defun git-overleaf--with-downloaded-snapshot (project-id function)
  "Download PROJECT-ID, call FUNCTION with the snapshot root, then clean up."
  (git-overleaf-log-with-context
      (git-overleaf-log-make-context
       :project-id project-id
       :url (git-overleaf--url))
    (let ((snapshot nil))
      (unwind-protect
          (progn
            (setq snapshot (git-overleaf--download-snapshot project-id))
            (let ((git-overleaf--remote-sync-metadata nil)
                  (git-overleaf--remote-sync-metadata-text nil))
              (setq git-overleaf--remote-sync-metadata
                    (git-overleaf--extract-remote-sync-metadata
                     (git-overleaf--snapshot-root snapshot)))
              (funcall function (git-overleaf--snapshot-root snapshot))))
        (when snapshot
          (ignore-errors
            (delete-directory
             (git-overleaf--snapshot-temp-dir snapshot)
             t)))))))
(defun git-overleaf--with-remote-state (project-id function)
  "Download PROJECT-ID and call FUNCTION with the remote root and entity table."
  (git-overleaf--with-downloaded-snapshot
   project-id
   (lambda (remote-root)
     (funcall function
              remote-root
              (git-overleaf--fetch-remote-table project-id)))))
;;;; Local snapshot helpers

(defun git-overleaf--scan-local-tree (root)
  "Return local directory and file tables rooted at ROOT."
  (let ((dirs (make-hash-table :test #'equal))
        (files (make-hash-table :test #'equal)))
    (puthash "" root dirs)
    (cl-labels
        ((walk (dir)
           (dolist (entry (directory-files dir t nil t))
             (unless (member (file-name-nondirectory entry) '("." ".." ".git"))
               (let ((relative (file-relative-name entry root)))
                 (unless (git-overleaf--sync-metadata-path-p relative)
                   (if (file-directory-p entry)
                       (progn
                         (puthash relative entry dirs)
                         (walk entry))
                     (puthash relative entry files))))))))
      (walk root))
    `(:dirs ,dirs :files ,files)))

(defun git-overleaf--make-temp-index-path ()
  "Return a fresh path for a temporary Git index file.
The path itself does not exist yet, because Git expects to create the
index file on first use."
  (let ((path (make-temp-file "overleaf-index.")))
    (delete-file path)
    path))

(defun git-overleaf--materialize-commit (repo revision)
  "Write REVISION from REPO to a temporary directory and return it."
  (let* ((temp-dir (make-temp-file "overleaf-materialized." t))
         (index-file (git-overleaf--make-temp-index-path))
         (env (list (concat "GIT_INDEX_FILE=" index-file))))
    (unwind-protect
        (progn
          (git-overleaf--git-run repo (list "read-tree" revision) env)
          (git-overleaf--git-run
           repo
           (list
            "checkout-index"
            "-a"
            "-f"
            (format "--prefix=%s/" (file-name-as-directory temp-dir)))
           env)
          temp-dir)
      (ignore-errors (delete-file index-file)))))

(defun git-overleaf--commit-directory (repo directory parent message)
  "Create a Git commit in REPO from DIRECTORY with PARENT and MESSAGE.
Return the created commit id."
  (let* ((index-file (git-overleaf--make-temp-index-path))
         (env (list (concat "GIT_INDEX_FILE=" index-file)))
         (tree nil))
    (unwind-protect
        (progn
          (git-overleaf--git-run
           repo
           (list
            "--git-dir" (expand-file-name ".git" repo)
            "--work-tree" directory
            "add" "--all" ".")
           env)
          (setq tree
                (git-overleaf--command-result-output
                 (git-overleaf--git-run repo (list "write-tree") env)))
          (git-overleaf--command-result-output
           (git-overleaf--git-run
            repo
            (append
             (git-overleaf--git-identity-args repo)
             (list "commit-tree" tree)
             (when parent (list "-p" parent))
             (list "-m" message))
            env)))
      (ignore-errors (delete-file index-file)))))

(defun git-overleaf--git-identity-args (repo)
  "Return fallback Git identity args for REPO when necessary."
  (if (and (git-overleaf--git-output-noerror repo "config" "--get" "user.name")
           (git-overleaf--git-output-noerror repo "config" "--get" "user.email"))
      nil
    (git-overleaf--warn
     "Git identity is not configured for %s; using a repository-local placeholder author"
     repo)
    '("-c" "user.name=Overleaf Project"
      "-c" "user.email=git-overleaf@local")))

(defun git-overleaf--commit-working-tree (repo)
  "Commit staged changes in REPO before pushing."
  (apply
   #'git-overleaf--git-output
   repo
   (append
    (git-overleaf--git-identity-args repo)
    (if (git-overleaf--merge-in-progress-p repo)
        '("commit" "--no-edit")
      (list "commit" "-m" git-overleaf-sync-auto-commit-message)))))

(defun git-overleaf--prepare-working-tree-for-sync
    (repo &optional unstaged-action)
  "Stage and commit local changes in REPO when needed for pushing.

UNSTAGED-ACTION controls how unstaged or untracked changes are handled:
nil or `prompt' asks before staging all changes, `stage' stages all
changes without prompting, and `error' signals a user error."
  (let ((status (git-overleaf--read-repo-status repo)))
    (when (git-overleaf--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before pushing"
       repo))
    (when (git-overleaf--repo-status-unstaged status)
      (pcase (or unstaged-action 'prompt)
        ('stage nil)
        ('error
         (user-error
          "Overleaf push requires a clean working tree; stage all changes or stash them first"))
        (_
         (unless
             (y-or-n-p
              (format
               "Repository %s has unstaged changes.  Stage all changes and continue with Overleaf push? "
               repo))
           (user-error
            "Overleaf push requires a clean working tree; stage all changes or stash them first"))))
      (git-overleaf--git-output repo "add" "--all" ".")
      (setq status (git-overleaf--read-repo-status repo)))
    (when (git-overleaf--repo-status-staged status)
      (git-overleaf--create-local-backup-ref repo "before-auto-commit")
      (git-overleaf--message "Committing local changes before Overleaf push...")
      (git-overleaf--commit-working-tree repo)
      t)))

(defun git-overleaf--ensure-clean-working-tree (repo action)
  "Signal an error if REPO has local changes before ACTION."
  (let ((status (git-overleaf--read-repo-status repo)))
    (when (git-overleaf--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before %s"
       repo
       action))
    (when (or (git-overleaf--repo-status-staged status)
              (git-overleaf--repo-status-unstaged status))
      (user-error
       "Repository %s has local changes; commit or stash them before %s"
       repo
       action))))

;;;; Project sync internals

(defun git-overleaf--sync-local-tree
    (project-id local-root remote-root remote-table)
  "Synchronize LOCAL-ROOT into PROJECT-ID using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((local-state (git-overleaf--scan-local-tree local-root))
         (local-dirs (plist-get local-state :dirs))
         (local-files (plist-get local-state :files))
         (dir-paths nil)
         (file-paths nil)
         (delete-files nil)
         (delete-folders nil))
    (maphash
     (lambda (path _)
       (unless (string-empty-p path)
         (push path dir-paths)))
     local-dirs)
    (maphash
     (lambda (path _)
       (push path file-paths))
     local-files)

    (dolist (path
             (sort dir-paths
                   (lambda (left right)
                     (< (git-overleaf--path-depth left)
                        (git-overleaf--path-depth right)))))
      (let ((remote-entry (gethash path remote-table)))
        (when remote-entry
          (unless (eq (git-overleaf--entity-type remote-entry) 'folder)
            (git-overleaf--delete-entity project-id remote-entry)
            (git-overleaf--forget-entry remote-table path)
            (setq remote-entry nil)))
        (unless remote-entry
          (let* ((parent-path (git-overleaf--parent-path path))
                 (parent-entry (gethash parent-path remote-table))
                 (created
                  (git-overleaf--create-folder
                   project-id
                   (git-overleaf--entity-id parent-entry)
                   (file-name-nondirectory path))))
            (puthash
             path
             (make-git-overleaf--entity
              :path path
              :name (plist-get created :name)
              :id (plist-get created :_id)
              :type 'folder
              :parent-id (git-overleaf--entity-id parent-entry))
             remote-table)))))

    (dolist (path (sort file-paths #'string<))
      (let* ((local-file (gethash path local-files))
             (remote-entry (gethash path remote-table))
             (remote-file (expand-file-name path remote-root))
             (same-content
              (and remote-entry
                   (not (eq (git-overleaf--entity-type remote-entry) 'folder))
                   (git-overleaf--files-equal-p local-file remote-file))))
        (unless same-content
          (if (and remote-entry
                   (eq (git-overleaf--entity-type remote-entry) 'doc))
              (git-overleaf--update-doc-text
               project-id
               (git-overleaf--entity-id remote-entry)
               local-file
               remote-file)
            (when remote-entry
              (git-overleaf--delete-entity project-id remote-entry)
              (git-overleaf--forget-entry remote-table path))
            (let* ((parent-path (git-overleaf--parent-path path))
                   (parent-entry (gethash parent-path remote-table))
                   (response
                    (git-overleaf--curl-upload-file
                     project-id
                     (git-overleaf--entity-id parent-entry)
                     (file-name-nondirectory path)
                     local-file))
                   (entity-type
                    (pcase (plist-get response :entity_type)
                      ("doc" 'doc)
                      (_ 'file))))
              (puthash
               path
               (make-git-overleaf--entity
                :path path
                :name (file-name-nondirectory path)
                :id (plist-get response :entity_id)
                :type entity-type
                :parent-id (git-overleaf--entity-id parent-entry))
               remote-table))))))

    (maphash
     (lambda (path entity)
       (unless (string-empty-p path)
         (unless (or (git-overleaf--sync-metadata-path-p path)
                     (gethash path local-files)
                     (gethash path local-dirs))
           (if (eq (git-overleaf--entity-type entity) 'folder)
               (push path delete-folders)
             (push path delete-files)))))
     remote-table)

    (dolist (path
             (sort delete-files
                   (lambda (left right)
                     (> (git-overleaf--path-depth left)
                        (git-overleaf--path-depth right)))))
      (when-let* ((entity (gethash path remote-table)))
        (git-overleaf--delete-entity project-id entity)
        (remhash path remote-table)))

    (dolist (path
             (sort delete-folders
                   (lambda (left right)
                     (> (git-overleaf--path-depth left)
                        (git-overleaf--path-depth right)))))
      (when-let* ((entity (gethash path remote-table)))
        (git-overleaf--delete-entity project-id entity)
        (git-overleaf--forget-entry remote-table path)))))

(defun git-overleaf--upload-sync-metadata
    (repo revision project-id remote-table)
  "Update sync metadata for REVISION in REPO on PROJECT-ID."
  (when git-overleaf-sync-metadata-enabled
    (condition-case err
        (let* ((path (git-overleaf--sync-metadata-relative-path))
               (root-entry (gethash "" remote-table))
               (existing (gethash path remote-table))
               (metadata-text
                (git-overleaf--sync-metadata-json repo revision project-id))
	           (temp-file nil))
          (unless root-entry
            (user-error "Could not find remote Overleaf root folder"))
          (unwind-protect
              (if (and existing
                       (eq (git-overleaf--entity-type existing) 'doc))
                  (if git-overleaf--remote-sync-metadata-text
                      (progn
                        (git-overleaf--update-doc-text-content
                         project-id
                         (git-overleaf--entity-id existing)
                         git-overleaf--remote-sync-metadata-text
                         metadata-text)
                        (setq git-overleaf--remote-sync-metadata-text
                              metadata-text))
                    (git-overleaf--warn
                     "Could not update remote Overleaf sync metadata through text OT because the downloaded metadata text was unavailable"))
                (setq temp-file
                      (make-temp-file
                       "git-overleaf-sync-metadata."
                       nil
                       ".json"))
                (with-temp-file temp-file
                  (insert metadata-text))
                (when existing
                  (git-overleaf--delete-entity project-id existing)
                  (git-overleaf--forget-entry remote-table path))
                (let* ((response
                        (git-overleaf--curl-upload-file
                         project-id
                         (git-overleaf--entity-id root-entry)
                         path
                         temp-file))
                       (entity-type
                        (pcase (plist-get response :entity_type)
                          ("doc" 'doc)
                          (_ 'file))))
                  (puthash
                   path
                   (make-git-overleaf--entity
                    :path path
                    :name path
                    :id (plist-get response :entity_id)
                    :type entity-type
                    :parent-id (git-overleaf--entity-id root-entry))
                   remote-table)))
            (ignore-errors (delete-file temp-file))))
      (error
       (git-overleaf--warn
        "Could not update remote Overleaf sync metadata: %s"
        (error-message-string err))))))

(defun git-overleaf--sync-commit
    (repo revision project-id remote-root remote-table)
  "Synchronize REVISION from REPO into PROJECT-ID."
  (let ((local-root nil))
    (unwind-protect
        (progn
          (setq local-root (git-overleaf--materialize-commit repo revision))
          (git-overleaf--message "Uploading %s to Overleaf..." revision)
          (git-overleaf--sync-local-tree
           project-id local-root remote-root remote-table))
      (when local-root
        (ignore-errors (delete-directory local-root t))))))

(defun git-overleaf--record-remote-snapshot (repo remote-root)
  "Create a Git commit in REPO representing REMOTE-ROOT."
  (let* ((snapshot-commit
          (git-overleaf--commit-directory
           repo
           remote-root
           (git-overleaf--rev-parse-noerror
            repo
            (git-overleaf--base-ref repo))
           (format "overleaf: remote snapshot %s"
                   (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (snapshot-tree (git-overleaf--tree-id repo snapshot-commit)))
    (or (git-overleaf--remote-sync-metadata-commit repo snapshot-tree)
        snapshot-commit)))

(defun git-overleaf--initialize-base-ref (repo project remote-root)
  "Persist PROJECT in REPO and initialize the hidden Overleaf base ref.
REMOTE-ROOT must point at a downloaded snapshot of PROJECT.  This does
not modify the working tree or perform a pull/push."
  (let ((remote-commit
         (git-overleaf--commit-directory
          repo
          remote-root
          (git-overleaf--rev-parse-noerror
           repo
           (git-overleaf--base-ref repo))
          (format "overleaf: configured base snapshot %s"
                  (format-time-string "%Y-%m-%d %H:%M:%S")))))
    (git-overleaf--write-repo-metadata repo project)
    (git-overleaf--clear-pending-state repo)
    (git-overleaf--set-base-ref repo remote-commit)
    remote-commit))

(defun git-overleaf--classify-sync-state (base-tree head-tree remote-tree)
  "Classify the sync relationship between BASE-TREE, HEAD-TREE, and REMOTE-TREE."
  (cond
   ((and (string= head-tree base-tree)
         (string= remote-tree base-tree))
    'in-sync)
   ((string= head-tree remote-tree)
    'head-matches-remote)
   ((string= remote-tree base-tree)
    'remote-matches-base)
   ((string= head-tree base-tree)
    'head-matches-base)
   (t
    'diverged)))

(defun git-overleaf--read-sync-state (repo remote-root)
  "Return common sync state for REPO against REMOTE-ROOT."
  (let* ((base-ref (git-overleaf--base-ref repo))
         (base-commit (git-overleaf--rev-parse repo base-ref))
         (head (git-overleaf--rev-parse repo "HEAD"))
         (branch (git-overleaf--current-branch repo))
         (remote-commit (git-overleaf--record-remote-snapshot repo remote-root))
         (base-tree (git-overleaf--tree-id repo base-commit))
         (head-tree (git-overleaf--tree-id repo head))
         (remote-tree (git-overleaf--tree-id repo remote-commit)))
    `(:base-commit ,base-commit
		           :head ,head
		           :branch ,branch
		           :remote-commit ,remote-commit
		           :base-tree ,base-tree
		           :head-tree ,head-tree
		           :remote-tree ,remote-tree
		           :status ,(git-overleaf--classify-sync-state
			                 base-tree head-tree remote-tree))))

(defun git-overleaf--ensure-no-pending-action (repo command)
  "Signal if REPO still has a pending Overleaf sync before COMMAND."
  (when-let* ((pending (git-overleaf--pending-state repo)))
    (user-error
     "Pending Overleaf %s exists; finish it before %s"
     (plist-get pending :action)
     command)))

(defun git-overleaf--ensure-pending-remote-unchanged
    (repo remote-root remote-tree action)
  "Signal if REPO's REMOTE-ROOT no longer matches REMOTE-TREE for pending ACTION."
  (let ((current-remote-commit
         (git-overleaf--record-remote-snapshot repo remote-root)))
    (unless (string=
             (git-overleaf--tree-id repo current-remote-commit)
             remote-tree)
      (user-error
       "The remote project changed again while the %s branch was pending; start a new %s"
       action
       action))
    current-remote-commit))

(defun git-overleaf--note-matching-sync-state
    (repo head &optional project-id remote-table)
  "Update REPO base metadata after confirming HEAD already matches Overleaf.
When PROJECT-ID and REMOTE-TABLE are non-nil, also refresh remote sync
metadata."
  (when (and project-id remote-table)
    (git-overleaf--upload-sync-metadata repo head project-id remote-table))
  (git-overleaf--set-base-ref repo head)
  (git-overleaf--message "Local and remote content already match; base ref updated"))

(defun git-overleaf--upload-head-and-set-base
    (repo head project-id remote-root remote-table format-string &rest args)
  "Upload HEAD from REPO to Overleaf, update the base ref, and report success."
  (git-overleaf--sync-commit
   repo head project-id remote-root remote-table)
  (git-overleaf--upload-sync-metadata repo head project-id remote-table)
  (git-overleaf--set-base-ref repo head)
  (apply #'git-overleaf--message format-string args))

(defun git-overleaf--finalize-pending-pull
    (repo pending remote-root remote-table)
  "Finalize a pending pull in REPO: upload merged HEAD to Overleaf.
PENDING must have action=pull and a valid remote-commit.
REMOTE-ROOT and REMOTE-TABLE describe the current remote state."
  (let* ((remote-commit (plist-get pending :remote-commit)))
    (unless remote-commit
      (user-error "Pending pull metadata is incomplete"))
    (let* ((head (git-overleaf--rev-parse repo "HEAD"))
           (project-id (git-overleaf--project-id repo))
           (remote-tree (git-overleaf--tree-id repo remote-commit)))
      (unless (git-overleaf--is-ancestor-p repo remote-commit head)
        (user-error
         "Merge is not complete; resolve conflicts and commit before pushing"))
      (git-overleaf--ensure-pending-remote-unchanged
       repo remote-root remote-tree 'pull)
      (git-overleaf--upload-head-and-set-base
       repo head project-id remote-root remote-table
       "Pushed merged Overleaf pull for `%s'"
       (git-overleaf--project-name repo))
      (git-overleaf--clear-pending-state repo))))

(defun git-overleaf--fresh-push (repo remote-root remote-table)
  "Perform a fresh push of REPO using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((context (git-overleaf--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (project-id (git-overleaf--project-id repo))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (git-overleaf--upload-sync-metadata repo head project-id remote-table)
       (git-overleaf--message "Project `%s' is already in sync"
				                  (git-overleaf--project-name repo)))
      ('head-matches-remote
       (git-overleaf--note-matching-sync-state
        repo
        head
        project-id
        remote-table))
      ('remote-matches-base
       (git-overleaf--upload-head-and-set-base
        repo
        head
        project-id
        remote-root
        remote-table
        "Pushed `%s' to Overleaf"
        (git-overleaf--project-name repo)))
      ('head-matches-base
       (user-error
        "Remote Overleaf changes exist for `%s'; run `git-overleaf-pull` first"
        branch))
      (_
       (user-error
        "Remote Overleaf changes exist for `%s'; run `git-overleaf-pull' first"
        (git-overleaf--project-name repo))))))

(defun git-overleaf--fresh-pull (repo remote-root)
  "Perform a fresh pull of REPO using REMOTE-ROOT."
  (let* ((context (git-overleaf--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (git-overleaf--message "Project `%s' is already in sync"
				                  (git-overleaf--project-name repo)))
      ('head-matches-remote
       (git-overleaf--note-matching-sync-state repo head))
      ('remote-matches-base
       (git-overleaf--message "No remote Overleaf changes to pull into `%s'" branch))
      ('head-matches-base
       (git-overleaf--create-local-backup-ref repo "pull-ff")
       (git-overleaf--git-output repo "merge" "--ff-only" remote-commit)
       (git-overleaf--set-base-ref repo "HEAD")
       (git-overleaf--message "Pulled remote Overleaf changes into `%s'" branch))
      (_
       (git-overleaf--create-local-backup-ref repo "pull-merge")
       (let ((merge-result
              (git-overleaf--git-run
               repo
               (list "merge" "--no-ff" "--no-edit" remote-commit)
               nil
               t)))
         (if (and (integerp (git-overleaf--command-result-status merge-result))
                  (zerop (git-overleaf--command-result-status merge-result)))
             (progn
               (git-overleaf--set-base-ref repo remote-commit)
               (git-overleaf--message "Pulled Overleaf changes into `%s'" branch))
           (git-overleaf--create-local-backup-ref
            repo
            "pending-pull-remote"
            remote-commit)
           (git-overleaf--set-pending-pull-state repo remote-commit)
           (git-overleaf--warn
            "Merge conflict on `%s'. Resolve conflicts, commit, then run `git-overleaf-push'."
            branch)))))))


(provide 'git-overleaf-sync)

;;; git-overleaf-sync.el ends here
