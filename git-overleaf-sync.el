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

(defun git-overleaf--ensure-no-active-merge (repo action)
  "Signal if REPO has an unfinished Git merge before ACTION.
Staged, unstaged, and untracked changes are otherwise allowed."
  (let ((status (git-overleaf--read-repo-status repo)))
    (when (or (git-overleaf--merge-in-progress-p repo)
              (git-overleaf--repo-status-unmerged status))
      (user-error
       "Repository %s has an unfinished Git merge; complete or abort it before %s"
       repo
       action))))

;;;; Project sync internals

(cl-defstruct git-overleaf--upload-progress
  "Progress for one sequence of remote Overleaf mutations."
  project-id
  total
  (completed 0))

(defun git-overleaf--upload-progress-current (progress action path)
  "Show PROGRESS before performing ACTION on remote PATH."
  (when progress
    (let ((completed (git-overleaf--upload-progress-completed progress))
          (total (git-overleaf--upload-progress-total progress)))
      (git-overleaf--progress-message
       "Uploading content for project %s... %d%% (%d/%d: %s %s)"
       (git-overleaf--upload-progress-project-id progress)
       (floor (* 100.0 (/ (float completed) total)))
       (1+ completed)
       total
       action
       path))))

(defun git-overleaf--upload-progress-advance (progress)
  "Advance PROGRESS after one successful remote mutation."
  (when progress
    (cl-incf (git-overleaf--upload-progress-completed progress))
    (when (= (git-overleaf--upload-progress-completed progress)
             (git-overleaf--upload-progress-total progress))
      (git-overleaf--progress-message
       "Uploading content for project %s... 100%%"
       (git-overleaf--upload-progress-project-id progress)))))

(defun git-overleaf--with-upload-progress
    (progress action path function)
  "Run FUNCTION as remote ACTION on PATH while updating PROGRESS."
  (git-overleaf--upload-progress-current progress action path)
  (prog1 (funcall function)
    (git-overleaf--upload-progress-advance progress)))

(defun git-overleaf--path-below-any-p (path parents)
  "Return non-nil when PATH is below one of PARENTS."
  (cl-some
   (lambda (parent)
     (string-prefix-p (concat parent "/") path))
   parents))

(defun git-overleaf--plan-local-tree-sync
    (local-root remote-root remote-table)
  "Plan remote mutations needed to sync LOCAL-ROOT against REMOTE-ROOT.
REMOTE-TABLE is the current Overleaf entity table."
  (let* ((local-state (git-overleaf--scan-local-tree local-root))
         (local-dirs (plist-get local-state :dirs))
         (local-files (plist-get local-state :files))
         (dir-paths nil)
         (file-paths nil)
         (changed-files (make-hash-table :test #'equal))
         (replaced-folders nil)
         (delete-files nil)
         (delete-folders nil)
         (total 0))
    (maphash
     (lambda (path _)
       (unless (string-empty-p path)
         (push path dir-paths)
         (let ((remote-entry (gethash path remote-table)))
           (cond
            ((not remote-entry)
             (cl-incf total))
            ((not (eq (git-overleaf--entity-type remote-entry) 'folder))
             (cl-incf total 2))))))
     local-dirs)
    (maphash
     (lambda (path local-file)
       (push path file-paths)
       (let* ((remote-entry (gethash path remote-table))
              (remote-file (expand-file-name path remote-root))
              (same-content
               (and remote-entry
                    (not (eq (git-overleaf--entity-type remote-entry)
                             'folder))
                    (git-overleaf--files-equal-p local-file remote-file))))
         (unless same-content
           (puthash path t changed-files)
           (cond
            ((and remote-entry
                  (eq (git-overleaf--entity-type remote-entry) 'doc))
             (cl-incf total))
            (remote-entry
             (when (eq (git-overleaf--entity-type remote-entry) 'folder)
               (push path replaced-folders))
             (cl-incf total 2))
            (t
             (cl-incf total))))))
     local-files)
    (maphash
     (lambda (path entity)
       (unless (or (string-empty-p path)
                   (git-overleaf--sync-metadata-path-p path)
                   (gethash path local-files)
                   (gethash path local-dirs)
                   (git-overleaf--path-below-any-p path replaced-folders))
         (cl-incf total)
         (if (eq (git-overleaf--entity-type entity) 'folder)
             (push path delete-folders)
           (push path delete-files))))
     remote-table)
    `(:local-dirs ,local-dirs
      :local-files ,local-files
      :dir-paths
      ,(sort dir-paths
             (lambda (left right)
               (< (git-overleaf--path-depth left)
                  (git-overleaf--path-depth right))))
      :file-paths ,(sort file-paths #'string<)
      :changed-files ,changed-files
      :delete-files
      ,(sort delete-files
             (lambda (left right)
               (> (git-overleaf--path-depth left)
                  (git-overleaf--path-depth right))))
      :delete-folders
      ,(sort delete-folders
             (lambda (left right)
               (> (git-overleaf--path-depth left)
                  (git-overleaf--path-depth right))))
      :total ,total)))

(defun git-overleaf--sync-local-tree
    (project-id local-root remote-root remote-table)
  "Synchronize LOCAL-ROOT into PROJECT-ID using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((plan (git-overleaf--plan-local-tree-sync
                local-root remote-root remote-table))
         (local-files (plist-get plan :local-files))
         (progress
          (when (> (plist-get plan :total) 0)
            (make-git-overleaf--upload-progress
             :project-id project-id
             :total (plist-get plan :total)))))
    (dolist (path (plist-get plan :dir-paths))
      (let ((remote-entry (gethash path remote-table)))
        (when remote-entry
          (unless (eq (git-overleaf--entity-type remote-entry) 'folder)
            (git-overleaf--with-upload-progress
             progress
             "deleting"
             path
             (lambda ()
               (git-overleaf--delete-entity project-id remote-entry)))
            (git-overleaf--forget-entry remote-table path)
            (setq remote-entry nil)))
        (unless remote-entry
          (let* ((parent-path (git-overleaf--parent-path path))
                 (parent-entry (gethash parent-path remote-table))
                 (created
                  (git-overleaf--with-upload-progress
                   progress
                   "creating folder"
                   path
                   (lambda ()
                     (git-overleaf--create-folder
                      project-id
                      (git-overleaf--entity-id parent-entry)
                      (file-name-nondirectory path))))))
            (puthash
             path
             (make-git-overleaf--entity
              :path path
              :name (plist-get created :name)
              :id (plist-get created :_id)
              :type 'folder
              :parent-id (git-overleaf--entity-id parent-entry))
             remote-table)))))

    (dolist (path (plist-get plan :file-paths))
      (let* ((local-file (gethash path local-files))
             (remote-entry (gethash path remote-table)))
        (when (gethash path (plist-get plan :changed-files))
          (if (and remote-entry
                   (eq (git-overleaf--entity-type remote-entry) 'doc))
              (git-overleaf--with-upload-progress
               progress
               "updating"
               path
               (lambda ()
                 (git-overleaf--update-doc-text
                  project-id
                  (git-overleaf--entity-id remote-entry)
                  local-file
                  (expand-file-name path remote-root))))
            (when remote-entry
              (git-overleaf--with-upload-progress
               progress
               "deleting"
               path
               (lambda ()
                 (git-overleaf--delete-entity project-id remote-entry)))
              (git-overleaf--forget-entry remote-table path))
            (let* ((parent-path (git-overleaf--parent-path path))
                   (parent-entry (gethash parent-path remote-table))
                   (response
                    (git-overleaf--with-upload-progress
                     progress
                     "uploading"
                     path
                     (lambda ()
                       (git-overleaf--curl-upload-file
                        project-id
                        (git-overleaf--entity-id parent-entry)
                        (file-name-nondirectory path)
                        local-file))))
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

    (dolist (path (plist-get plan :delete-files))
      (when-let* ((entity (gethash path remote-table)))
        (git-overleaf--with-upload-progress
         progress
         "deleting"
         path
         (lambda ()
           (git-overleaf--delete-entity project-id entity)))
        (remhash path remote-table)))

    (dolist (path (plist-get plan :delete-folders))
      (when-let* ((entity (gethash path remote-table)))
        (git-overleaf--with-upload-progress
         progress
         "deleting"
         path
         (lambda ()
           (git-overleaf--delete-entity project-id entity)))
        (git-overleaf--forget-entry remote-table path)))))

(defun git-overleaf--upload-sync-metadata
    (repo revision project-id remote-table)
  "Update sync metadata for REVISION in REPO on PROJECT-ID."
  (when git-overleaf-sync-metadata-enabled
    (git-overleaf--progress-message "Finalizing Overleaf sync metadata...")
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

(defun git-overleaf--record-remote-snapshot
    (repo remote-root &optional parent)
  "Create and record a Git commit in REPO representing REMOTE-ROOT.
When PARENT is non-nil, make the snapshot its child and do not replace
it with a commit named by remote sync metadata."
  (let* ((snapshot-commit
          (git-overleaf--commit-directory
           repo
           remote-root
           (or parent
               (git-overleaf--rev-parse-noerror
                repo
                (git-overleaf--base-ref repo)))
           (format "overleaf: remote snapshot %s"
                   (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (snapshot-tree (git-overleaf--tree-id repo snapshot-commit))
         (remote-commit
          (or (and (not parent)
                   (git-overleaf--remote-sync-metadata-commit
                    repo snapshot-tree))
              snapshot-commit)))
    (git-overleaf--set-remote-ref repo remote-commit)
    (git-overleaf--record-remote-fetch-time repo)
    remote-commit))

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
    (git-overleaf--set-remote-ref repo remote-commit)
    (git-overleaf--record-remote-fetch-time repo)
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

(defun git-overleaf--read-sync-state (repo remote-root &optional remote-parent)
  "Return common sync state for REPO against REMOTE-ROOT.
When REMOTE-PARENT is non-nil, parent the new snapshot from it."
  (let* ((base-ref (git-overleaf--base-ref repo))
         (base-commit (git-overleaf--rev-parse repo base-ref))
         (head (git-overleaf--rev-parse repo "HEAD"))
         (branch (git-overleaf--current-branch repo))
         (remote-commit
          (git-overleaf--record-remote-snapshot
           repo remote-root remote-parent))
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
     "Repository %s has a pending Overleaf %s; finish it before %s"
     repo
     (plist-get pending :action)
     command)))

(defun git-overleaf--pending-phase (repo &optional pending)
  "Return the recovery phase of REPO's PENDING synchronization state."
  (when-let* ((state (or pending (git-overleaf--pending-state repo))))
    (pcase (plist-get state :action)
      ('pull
       (cond
        ((or (git-overleaf--merge-in-progress-p repo)
             (git-overleaf--repo-status-unmerged
              (git-overleaf--read-repo-status repo)))
         'merging)
        ((let ((remote (plist-get state :remote-commit)))
           (and remote
                (git-overleaf--rev-parse-noerror repo remote)
                (git-overleaf--is-ancestor-p repo remote "HEAD")))
         'committed)
        (t 'stale)))
      ('push
       (if (and (git-overleaf--rev-parse-noerror
                 repo (or (plist-get state :remote-commit) ""))
                (git-overleaf--rev-parse-noerror
                 repo (or (plist-get state :target-commit) "")))
           'resumable
         'invalid))
      (_ 'invalid))))

(defun git-overleaf--signal-pending-pull (repo)
  "Explain how to continue or abandon REPO's pending Overleaf pull."
  (pcase (git-overleaf--pending-phase repo)
    ('merging
     (user-error
      (concat
       "Repository %s has an active Overleaf merge; resolve and commit it, "
       "then run `git-overleaf-push', or run `git-overleaf-pull-abort'")
      repo))
    ('committed
     (user-error
      (concat
       "Repository %s has a committed Overleaf merge; run `git-overleaf-push', "
       "or run `git-overleaf-pull' again if the remote changed")
      repo))
    (_
     (user-error
      (concat
       "Repository %s has stale pending pull metadata; run "
       "`git-overleaf-pull-abort' before starting another operation")
      repo))))

(defun git-overleaf--ensure-pull-startable (repo)
  "Signal unless REPO can start or resume an Overleaf pull."
  (let ((pending (git-overleaf--pending-state repo)))
    (cond
     ((or (git-overleaf--merge-in-progress-p repo)
          (git-overleaf--repo-status-unmerged
           (git-overleaf--read-repo-status repo)))
      (if (eq (plist-get pending :action) 'pull)
          (git-overleaf--signal-pending-pull repo)
        (user-error
         "Repository %s has an unfinished Git merge; complete or abort it before pulling"
         repo)))
     ((and pending
           (eq (git-overleaf--pending-phase repo pending) 'stale))
      (git-overleaf--signal-pending-pull repo))
     ((and pending
           (eq (git-overleaf--pending-phase repo pending) 'invalid))
      (user-error
       "Repository %s has invalid pending Overleaf metadata; use `git-overleaf-reset' or `git-overleaf-push' with a force prefix"
       repo)))))

(defun git-overleaf--ensure-pending-remote-unchanged
    (repo remote-root remote-tree action)
  "Signal if REPO's REMOTE-ROOT no longer matches REMOTE-TREE for pending ACTION."
  (let ((current-remote-commit
         (git-overleaf--record-remote-snapshot repo remote-root)))
    (unless (string=
             (git-overleaf--tree-id repo current-remote-commit)
             remote-tree)
      (user-error
       "The remote project changed again while the pending %s was being finalized; run `git-overleaf-pull' to integrate it, or force `git-overleaf-push' to replace it"
       action))
    current-remote-commit))

(defun git-overleaf--tree-entry-map (repo revision)
  "Return a path-to-object-entry table for REVISION in REPO."
  (let ((table (make-hash-table :test #'equal))
        (output (git-overleaf--git-output
                 repo "ls-tree" "-r" "-z" "--full-tree" revision)))
    (dolist (record (split-string output "\0" t))
      (when (string-match
             "\\`\\([^ ]+ [^ ]+ [^\t]+\\)\t\\(.*\\)\\'"
             record)
        (puthash (match-string 2 record) (match-string 1 record) table)))
    table))

(defun git-overleaf--partial-push-compatible-p
    (repo base target current)
  "Return non-nil if CURRENT is a partial application of BASE to TARGET."
  (let ((base-map (git-overleaf--tree-entry-map repo base))
        (target-map (git-overleaf--tree-entry-map repo target))
        (current-map (git-overleaf--tree-entry-map repo current))
        (paths (make-hash-table :test #'equal))
        (compatible t))
    (dolist (table (list base-map target-map current-map))
      (maphash (lambda (path _entry) (puthash path t paths)) table))
    (maphash
     (lambda (path _)
       (let ((base-entry (gethash path base-map))
             (target-entry (gethash path target-map))
             (current-entry (gethash path current-map)))
         (unless (or (equal current-entry base-entry)
                     (equal current-entry target-entry))
           (setq compatible nil))))
     paths)
    compatible))

(defun git-overleaf--worktree-state (repo)
  "Return symbols describing REPO's index and working tree."
  (let ((status (git-overleaf--read-repo-status repo)))
    (delq nil
          (list
           (and (git-overleaf--repo-status-staged status) 'staged)
           (and (git-overleaf--repo-status-unstaged status) 'unstaged)
           (and (git-overleaf--repo-status-unmerged status) 'unmerged)))))

(defun git-overleaf--status-recommendation (state pending-phase merge-active)
  "Return a next-action string for STATE, PENDING-PHASE, and MERGE-ACTIVE."
  (cond
   ((eq pending-phase 'merging)
    "Resolve and commit, or run git-overleaf-pull-abort")
   ((eq pending-phase 'committed)
    "Run git-overleaf-push; run git-overleaf-pull first if Overleaf changed again")
   ((eq pending-phase 'stale)
    "Run git-overleaf-pull-abort")
   ((eq pending-phase 'resumable)
    "Run git-overleaf-push to resume, git-overleaf-pull to reconcile, or force push")
   ((eq pending-phase 'invalid)
    "Run git-overleaf-reset, or force git-overleaf-push")
   (merge-active
    "Complete or abort the active Git merge")
   ((eq state 'remote-unknown) "Run git-overleaf-fetch")
   ((memq state '(in-sync head-matches-remote)) "No synchronization needed")
   ((eq state 'remote-matches-base) "Run git-overleaf-push")
   ((memq state '(head-matches-base diverged)) "Run git-overleaf-pull")
   (t "Inspect the repository state")))

(defun git-overleaf--status-data (repo)
  "Return a read-only synchronization status plist for REPO."
  (let* ((base-ref (git-overleaf--base-ref repo))
         (remote-ref (git-overleaf--remote-ref repo))
         (base (git-overleaf--rev-parse repo base-ref))
         (head (git-overleaf--rev-parse repo "HEAD"))
         (remote (git-overleaf--rev-parse-noerror repo remote-ref))
         (pending (git-overleaf--pending-state repo))
         (pending-phase (git-overleaf--pending-phase repo pending))
         (merge-active (and (git-overleaf--merge-in-progress-p repo) t))
         (state
          (if remote
              (git-overleaf--classify-sync-state
               (git-overleaf--tree-id repo base)
               (git-overleaf--tree-id repo head)
               (git-overleaf--tree-id repo remote))
            'remote-unknown)))
    `(:repo ,repo
      :project ,(git-overleaf--project-name repo)
      :branch ,(or (git-overleaf--git-output-noerror
                    repo "branch" "--show-current")
                   "detached")
      :head ,head
      :base ,base
      :remote ,remote
      :state ,state
      :pending ,pending
      :pending-phase ,pending-phase
      :merge-active ,merge-active
      :worktree ,(git-overleaf--worktree-state repo)
      :remote-fetched-at ,(git-overleaf--remote-fetched-at repo)
      :recommendation
      ,(git-overleaf--status-recommendation
        state pending-phase merge-active))))

(defun git-overleaf--note-matching-sync-state
    (repo head &optional project-id remote-table)
  "Update REPO base metadata after confirming HEAD already matches Overleaf.
When PROJECT-ID and REMOTE-TABLE are non-nil, also refresh remote sync
metadata."
  (when (and project-id remote-table)
    (git-overleaf--upload-sync-metadata repo head project-id remote-table))
  (git-overleaf--set-base-ref repo head)
  (git-overleaf--set-remote-ref repo head)
  (git-overleaf--record-remote-fetch-time repo)
  (git-overleaf--message "Local and remote content already match; base ref updated"))

(defun git-overleaf--upload-head-and-set-base
    (repo head project-id remote-root remote-table success-message)
  "Upload HEAD from REPO to Overleaf, update the base ref, and report success."
  (git-overleaf--sync-commit
   repo head project-id remote-root remote-table)
  (git-overleaf--upload-sync-metadata repo head project-id remote-table)
  (git-overleaf--set-base-ref repo head)
  (git-overleaf--set-remote-ref repo head)
  (git-overleaf--record-remote-fetch-time repo)
  (git-overleaf--message "%s" success-message))

(defun git-overleaf--upload-target-with-journal
    (repo target remote-commit project-id remote-root remote-table
          success-message)
  "Upload TARGET and retain a resumable journal until all mutations succeed."
  (let ((pending (git-overleaf--pending-state repo)))
    (unless (and (eq (plist-get pending :action) 'push)
                 (equal (plist-get pending :target-commit) target))
      (git-overleaf--create-local-backup-ref
       repo "pending-push-target" target))
    (git-overleaf--set-pending-push-state repo remote-commit target)
    (git-overleaf--upload-head-and-set-base
     repo target project-id remote-root remote-table success-message)
    (git-overleaf--clear-pending-state repo)))

(defun git-overleaf--finalize-pending-pull
    (repo pending remote-root remote-table)
  "Finalize a pending pull in REPO: upload merged HEAD to Overleaf.
PENDING must have action=pull and a valid remote-commit.
REMOTE-ROOT and REMOTE-TABLE describe the current remote state."
  (let* ((remote-commit (plist-get pending :remote-commit)))
    (unless remote-commit
      (user-error
       "Pending pull metadata is incomplete; run `git-overleaf-pull-abort'"))
    (let* ((head (git-overleaf--rev-parse repo "HEAD"))
           (project-id (git-overleaf--project-id repo))
           (remote-tree (git-overleaf--tree-id repo remote-commit)))
      (unless (git-overleaf--is-ancestor-p repo remote-commit head)
        (git-overleaf--signal-pending-pull repo))
      (let ((current-remote
             (git-overleaf--ensure-pending-remote-unchanged
              repo remote-root remote-tree 'pull)))
        (git-overleaf--upload-target-with-journal
         repo head current-remote project-id remote-root remote-table
         (format "Pushed merged Overleaf pull for `%s'"
                 (git-overleaf--project-name repo)))))))

(defun git-overleaf--fresh-push (repo remote-root remote-table &optional force)
  "Push REPO using REMOTE-ROOT and REMOTE-TABLE.
When FORCE is non-nil, replace divergent remote content with HEAD."
  (let* ((context (git-overleaf--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (project-id (git-overleaf--project-id repo))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (if force
        (if (memq status '(in-sync head-matches-remote))
            (progn
              (git-overleaf--note-matching-sync-state
               repo head project-id remote-table)
              (git-overleaf--clear-pending-state repo))
          (git-overleaf--upload-target-with-journal
           repo head remote-commit project-id remote-root remote-table
           (format "Force-pushed `%s' to Overleaf"
                   (git-overleaf--project-name repo))))
      (pcase status
      ('in-sync
       (git-overleaf--upload-sync-metadata repo head project-id remote-table)
       (git-overleaf--set-base-ref repo head)
       (git-overleaf--set-remote-ref repo head)
       (git-overleaf--record-remote-fetch-time repo)
       (git-overleaf--message "Project `%s' is already in sync"
				                  (git-overleaf--project-name repo)))
      ('head-matches-remote
       (git-overleaf--note-matching-sync-state
        repo
        head
        project-id
        remote-table))
      ('remote-matches-base
       (git-overleaf--upload-target-with-journal
        repo head remote-commit project-id remote-root remote-table
        (format "Pushed `%s' to Overleaf"
                (git-overleaf--project-name repo))))
      ('head-matches-base
       (user-error
	        "Remote Overleaf changes exist for `%s'; run `git-overleaf-pull' first"
	        branch))
      (_
       (user-error
        "Remote Overleaf changes exist for `%s'; run `git-overleaf-pull' first"
        (git-overleaf--project-name repo)))))))

(defun git-overleaf--resume-pending-push
    (repo pending remote-root remote-table)
  "Safely resume PENDING push in REPO from the current REMOTE-ROOT."
  (let* ((base (plist-get pending :remote-commit))
         (target (plist-get pending :target-commit))
         (head (git-overleaf--rev-parse repo "HEAD")))
    (unless (and base target
                 (git-overleaf--rev-parse-noerror repo base)
                 (git-overleaf--rev-parse-noerror repo target))
      (user-error
       "Pending push metadata is incomplete; run `git-overleaf-reset' or force `git-overleaf-push'"))
    (unless (string= head target)
      (user-error
       "HEAD changed after the interrupted push; run `git-overleaf-pull' to reconcile, or force `git-overleaf-push' to replace Overleaf with the new HEAD"))
    (let ((current
           (git-overleaf--record-remote-snapshot repo remote-root base)))
      (unless (git-overleaf--partial-push-compatible-p
               repo base target current)
        (user-error
         "Overleaf changed independently after the interrupted push; run `git-overleaf-pull' to reconcile, or force `git-overleaf-push' to replace it"))
      (git-overleaf--upload-target-with-journal
       repo target current (git-overleaf--project-id repo)
       remote-root remote-table
       (format "Resumed push of `%s' to Overleaf"
               (git-overleaf--project-name repo))))))

(defun git-overleaf--push-with-remote-state
    (repo remote-root remote-table &optional force)
  "Push REPO against downloaded REMOTE-ROOT and REMOTE-TABLE."
  (let ((pending (git-overleaf--pending-state repo)))
    (cond
     (force
      (git-overleaf--fresh-push repo remote-root remote-table t))
     ((null pending)
      (git-overleaf--fresh-push repo remote-root remote-table))
     ((eq (plist-get pending :action) 'pull)
      (git-overleaf--finalize-pending-pull
       repo pending remote-root remote-table))
     ((eq (plist-get pending :action) 'push)
      (git-overleaf--resume-pending-push
       repo pending remote-root remote-table))
     (t
      (user-error "Unsupported pending Overleaf action `%s'"
                  (plist-get pending :action))))))

(defun git-overleaf--apply-pull-context (repo context)
  "Apply the pull decision described by CONTEXT to REPO."
  (let* ((head (plist-get context :head))
         (branch (plist-get context :branch))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (git-overleaf--message "Project `%s' is already in sync"
				                  (git-overleaf--project-name repo))
       'in-sync)
      ('head-matches-remote
       (git-overleaf--note-matching-sync-state repo head)
       'matching)
      ('remote-matches-base
       (git-overleaf--message "No remote Overleaf changes to pull into `%s'" branch)
       'no-remote-changes)
      ('head-matches-base
       (git-overleaf--create-local-backup-ref repo "pull-ff")
       (git-overleaf--git-output repo "merge" "--ff-only" remote-commit)
       (git-overleaf--set-base-ref repo "HEAD")
       (git-overleaf--message "Pulled remote Overleaf changes into `%s'" branch)
       'fast-forward)
      (_
       (git-overleaf--create-local-backup-ref repo "pull-merge")
       (let* ((merge-args
               (list "merge" "--no-ff" "--no-edit" remote-commit))
              (merge-result
               (git-overleaf--git-run repo merge-args nil t)))
         (if (and (integerp (git-overleaf--command-result-status merge-result))
                  (zerop (git-overleaf--command-result-status merge-result)))
             (progn
               (git-overleaf--set-base-ref repo remote-commit)
               (git-overleaf--message "Pulled Overleaf changes into `%s'" branch)
               'merged)
           (if (or (git-overleaf--merge-in-progress-p repo)
                   (git-overleaf--repo-status-unmerged
                    (git-overleaf--read-repo-status repo)))
               (progn
                 (git-overleaf--create-local-backup-ref
                  repo
                  "pending-pull-remote"
                  remote-commit)
                 (git-overleaf--set-pending-pull-state repo remote-commit)
                 (git-overleaf--warn
                  "Merge conflict on `%s'. Resolve conflicts, commit, then run `git-overleaf-push'."
                  branch)
                 'conflict)
             (error "%s"
                    (git-overleaf--command-error-message
                     (git-overleaf--ensure-executable
                      git-overleaf-git-executable)
                     merge-args
                     (git-overleaf--command-result-output
                      merge-result))))))))))

(defun git-overleaf--fresh-pull (repo remote-root &optional remote-parent)
  "Pull REPO using REMOTE-ROOT, optionally parenting it at REMOTE-PARENT."
  (git-overleaf--apply-pull-context
   repo
   (git-overleaf--read-sync-state repo remote-root remote-parent)))

(defun git-overleaf--resume-pending-pull (repo pending remote-root)
  "Continue a committed PENDING pull in REPO against REMOTE-ROOT."
  (let ((previous-remote (plist-get pending :remote-commit)))
    (unless (and previous-remote
                 (git-overleaf--rev-parse-noerror repo previous-remote)
                 (git-overleaf--is-ancestor-p
                  repo previous-remote "HEAD"))
      (git-overleaf--signal-pending-pull repo))
    (let* ((context
            (git-overleaf--read-sync-state
             repo remote-root previous-remote))
           (current-remote (plist-get context :remote-commit))
           (unchanged
            (string=
             (git-overleaf--tree-id repo current-remote)
             (git-overleaf--tree-id repo previous-remote))))
      (git-overleaf--set-base-ref repo previous-remote)
      (if unchanged
          (progn
            (git-overleaf--message
             "The committed pull already contains the latest Overleaf snapshot; run `git-overleaf-push'")
            'pending-ready)
        (let ((result (git-overleaf--apply-pull-context repo context)))
          (unless (eq result 'conflict)
            (git-overleaf--clear-pending-state repo))
          result)))))

(defun git-overleaf--reconcile-pending-push (repo pending remote-root)
  "Turn PENDING push into an ordinary pull from REMOTE-ROOT."
  (let ((base (plist-get pending :remote-commit)))
    (unless (and base (git-overleaf--rev-parse-noerror repo base))
      (user-error
       "Pending push metadata is incomplete; run `git-overleaf-reset' or force `git-overleaf-push'"))
    (git-overleaf--set-base-ref repo base)
    (let ((result (git-overleaf--fresh-pull repo remote-root base)))
      (unless (eq result 'conflict)
        (git-overleaf--clear-pending-state repo))
      result)))

(defun git-overleaf--pull-with-remote-state (repo remote-root)
  "Start or resume an Overleaf pull of REMOTE-ROOT into REPO."
  (let ((pending (git-overleaf--pending-state repo)))
    (pcase (plist-get pending :action)
      ('pull (git-overleaf--resume-pending-pull repo pending remote-root))
      ('push (git-overleaf--reconcile-pending-push repo pending remote-root))
      ('nil (git-overleaf--fresh-pull repo remote-root))
      (action (user-error "Unsupported pending Overleaf action `%s'" action)))))

(defun git-overleaf--abort-pending-pull (repo)
  "Abort or clear REPO's pending pull without guessing about committed history."
  (let* ((pending (git-overleaf--pending-state repo))
         (action (plist-get pending :action))
         (phase (git-overleaf--pending-phase repo pending)))
    (unless pending
      (user-error "Repository %s has no pending Overleaf pull" repo))
    (unless (eq action 'pull)
      (user-error
       "Repository %s has a pending Overleaf %s, not a pull; resume it with push or reconcile it with pull"
       repo action))
    (pcase phase
      ('merging
       (unless (git-overleaf--merge-in-progress-p repo)
         (user-error
          "Repository %s has unmerged entries but no active Git merge; resolve them before clearing pending state"
          repo))
       (git-overleaf--git-output repo "merge" "--abort")
       (git-overleaf--clear-pending-state repo)
       (git-overleaf--message "Aborted pending Overleaf pull in `%s'" repo))
      ('committed
       (user-error
        "The pending Overleaf merge is already committed; push it, pull newer remote changes, or reset explicitly"))
      (_
       (git-overleaf--clear-pending-state repo)
       (git-overleaf--message "Cleared stale pending Overleaf pull in `%s'" repo)))))

(defun git-overleaf--reset-to-remote (repo mode)
  "Reset REPO to its cached Overleaf remote ref using MODE.
MODE is `mixed' or `hard'.  Untracked and ignored files are preserved."
  (unless (memq mode '(mixed hard))
    (user-error "Unsupported Overleaf reset mode `%s'" mode))
  (let* ((branch (git-overleaf--current-branch repo))
         (remote-ref (git-overleaf--remote-ref repo))
         (remote-commit (git-overleaf--rev-parse-noerror repo remote-ref)))
    (unless remote-commit
      (user-error
       "No cached Overleaf snapshot exists; run `git-overleaf-fetch' first"))
    (git-overleaf--create-local-backup-ref
     repo (format "reset-%s" mode))
    (git-overleaf--git-output
     repo "reset" (format "--%s" mode) remote-commit)
    (git-overleaf--set-base-ref repo remote-commit)
    (git-overleaf--clear-pending-state repo)
    (git-overleaf--message
     "Reset local branch `%s' to cached Overleaf snapshot (%s)"
     branch mode)
    remote-commit))


(provide 'git-overleaf-sync)

;;; git-overleaf-sync.el ends here
