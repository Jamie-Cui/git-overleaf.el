;;; git-overleaf-git-test.el --- Git integration tests for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-core)
(require 'git-overleaf-http)
(require 'git-overleaf-sync)

(defmacro git-overleaf-git-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "git-overleaf-git-test." t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defmacro git-overleaf-git-test--with-repo (var &rest body)
  "Bind VAR to a temporary Git repository while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (git-overleaf-git-test--init-repo)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun git-overleaf-git-test--ensure-git ()
  "Skip the current test when Git is unavailable."
  (unless (executable-find git-overleaf-git-executable)
    (ert-skip "Git executable is unavailable")))

(defun git-overleaf-git-test--git (repo &rest args)
  "Run Git ARGS in REPO and return stdout."
  (apply #'git-overleaf--git-output repo args))

(defun git-overleaf-git-test--init-repo ()
  "Create and return a temporary Git repository."
  (git-overleaf-git-test--ensure-git)
  (let ((repo (make-temp-file "git-overleaf-repo." t)))
    (git-overleaf--run git-overleaf-git-executable '("init") repo)
    (git-overleaf-git-test--git repo "config" "user.name" "Overleaf Test")
    (git-overleaf-git-test--git
     repo "config" "user.email" "overleaf-test@example.invalid")
    repo))

(defun git-overleaf-git-test--write-file (root relative text)
  "Write TEXT to RELATIVE under ROOT and return the absolute path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert text))
    file))

(defun git-overleaf-git-test--read-file (root relative)
  "Return the contents of RELATIVE under ROOT."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative root))
    (buffer-string)))

(defun git-overleaf-git-test--commit-all (repo message)
  "Commit all changes in REPO with MESSAGE and return HEAD."
  (git-overleaf-git-test--git repo "add" "--all" ".")
  (git-overleaf-git-test--git repo "commit" "-m" message)
  (git-overleaf-git-test--git repo "rev-parse" "HEAD"))

(defun git-overleaf-git-test--remote-table ()
  "Return a minimal remote entity table with a root folder."
  (let ((table (make-hash-table :test #'equal)))
    (puthash
     ""
     (make-git-overleaf--entity
      :path ""
      :name "rootFolder"
      :id "root"
      :type 'folder)
     table)
    table))

(defun git-overleaf-git-test--mark-managed (repo)
  "Configure REPO as a managed Overleaf repository."
  (git-overleaf--git-config-set repo "git-overleaf.projectId" "project-id")
  (git-overleaf--git-config-set repo "git-overleaf.projectName" "Project")
  (git-overleaf--git-config-set
   repo "git-overleaf.url" "https://www.overleaf.com")
  (git-overleaf--git-config-set
   repo "git-overleaf.baseRef" git-overleaf-base-ref))

(defun git-overleaf-git-test--base-commit (repo text)
  "Create a base commit in REPO containing TEXT and set the base ref."
  (git-overleaf-git-test--write-file repo "main.tex" text)
  (let ((commit (git-overleaf-git-test--commit-all repo "base")))
    (git-overleaf-git-test--mark-managed repo)
    (git-overleaf--set-base-ref repo commit)
    commit))

(defmacro git-overleaf-git-test--without-remote-side-effects (&rest body)
  "Run BODY with Overleaf network side effects stubbed."
  (declare (indent 0) (debug t))
  `(let ((sync-calls nil)
         (metadata-calls nil)
         (messages nil)
         (warnings nil))
     (cl-letf (((symbol-function 'git-overleaf--sync-commit)
                (lambda (&rest args)
                  (push args sync-calls)))
               ((symbol-function 'git-overleaf--upload-sync-metadata)
                (lambda (&rest args)
                  (push args metadata-calls)))
               ((symbol-function 'git-overleaf--message)
                (lambda (&rest args)
                  (push args messages)))
               ((symbol-function 'git-overleaf--warn)
                (lambda (&rest args)
                  (push args warnings))))
       ,@body)))

(ert-deftest git-overleaf-git-test-commit-directory-and-materialize ()
  (git-overleaf-git-test--with-repo repo
    (git-overleaf-git-test--write-file repo "base.tex" "base\n")
    (let ((parent (git-overleaf-git-test--commit-all repo "base"))
          (materialized nil))
      (git-overleaf-git-test--with-temp-dir snapshot
        (git-overleaf-git-test--write-file snapshot "main.tex" "remote\n")
        (git-overleaf-git-test--write-file
         snapshot
         "chapters/intro.tex"
         "intro\n")
        (let ((commit (git-overleaf--commit-directory
                       repo
                       snapshot
                       parent
                       "remote snapshot")))
          (should (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" commit))
          (should (string-match-p
                   parent
                   (git-overleaf-git-test--git
                    repo
                    "rev-list"
                    "--parents"
                    "-n"
                    "1"
                    commit)))
          (unwind-protect
              (progn
                (setq materialized
                      (git-overleaf--materialize-commit repo commit))
                (should (equal (git-overleaf-git-test--read-file
                                materialized
                                "main.tex")
                               "remote\n"))
                (should (equal (git-overleaf-git-test--read-file
                                materialized
                                "chapters/intro.tex")
                               "intro\n"))
                (should-not (file-exists-p
                             (expand-file-name "base.tex" materialized))))
            (when materialized
              (ignore-errors (delete-directory materialized t)))))))))

(ert-deftest git-overleaf-git-test-initialize-base-ref-writes-metadata ()
  (let ((git-overleaf-url "https://example.overleaf.test")
        (git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--write-file repo "local.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf--set-pending-pull-state repo "pending")
      (git-overleaf-git-test--with-temp-dir snapshot
        (git-overleaf-git-test--write-file snapshot "remote.tex" "remote\n")
        (let ((commit (git-overleaf--initialize-base-ref
                       repo
                       '(:id "project-id" :name "Project")
                       snapshot))
              (materialized nil))
          (should (equal (git-overleaf--git-config-get
                          repo
                          "git-overleaf.projectId")
                         "project-id"))
          (should (equal (git-overleaf--git-config-get
                          repo
                          "git-overleaf.projectName")
                         "Project"))
          (should (equal (git-overleaf--git-config-get
                          repo
                          "git-overleaf.url")
                         "https://example.overleaf.test"))
          (should-not (git-overleaf--pending-state repo))
          (should (equal (git-overleaf--rev-parse
                          repo
                          (git-overleaf--base-ref repo))
                         commit))
          (unwind-protect
              (progn
                (setq materialized
                      (git-overleaf--materialize-commit repo commit))
                (should (equal (git-overleaf-git-test--read-file
                                materialized
                                "remote.tex")
                               "remote\n"))
                (should-not (file-exists-p
                             (expand-file-name "local.tex" materialized))))
            (when materialized
              (ignore-errors (delete-directory materialized t)))))))))

(ert-deftest git-overleaf-git-test-record-remote-snapshot-uses-metadata ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--write-file repo "main.tex" "same\n")
      (let* ((head (git-overleaf-git-test--commit-all repo "local"))
             (tree (git-overleaf--tree-id repo head))
             (git-overleaf--remote-sync-metadata
              `(:localCommit ,head :localTree ,tree))
             (git-overleaf-sync-metadata-enabled t))
        (git-overleaf-git-test--with-temp-dir snapshot
          (git-overleaf-git-test--write-file snapshot "main.tex" "same\n")
          (should (equal (git-overleaf--record-remote-snapshot
                          repo
                          snapshot)
                         head)))))))

(ert-deftest git-overleaf-git-test-fresh-push-uploads-local-head ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (let ((head (git-overleaf-git-test--commit-all repo "local")))
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "base\n")
          (git-overleaf-git-test--without-remote-side-effects
            (git-overleaf--fresh-push
             repo
             remote-root
             (git-overleaf-git-test--remote-table))
            (should (= (length sync-calls) 1))
            (should (= (length metadata-calls) 1))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--base-ref repo))
                           head))))))))

(ert-deftest git-overleaf-git-test-fresh-push-rejects-remote-changes ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf-git-test--without-remote-side-effects
          (should-error
           (git-overleaf--fresh-push
            repo
            remote-root
            (git-overleaf-git-test--remote-table))
           :type 'user-error)
          (should-not sync-calls)
          (should-not metadata-calls))))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf-git-test--without-remote-side-effects
          (should-error
           (git-overleaf--fresh-push
            repo
            remote-root
            (git-overleaf-git-test--remote-table))
           :type 'user-error)
          (should-not sync-calls)
          (should-not metadata-calls))))))

(ert-deftest git-overleaf-git-test-fresh-pull-fast-forwards ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf--fresh-pull repo remote-root)
        (should (equal (git-overleaf-git-test--read-file
                        repo
                        "main.tex")
                       "remote\n"))
        (should (equal (git-overleaf--rev-parse
                        repo
                        (git-overleaf--base-ref repo))
                       (git-overleaf--rev-parse repo "HEAD")))
        (should-not (git-overleaf--pending-state repo))
        (should-not (string-empty-p
                     (git-overleaf-git-test--git
                      repo
                      "for-each-ref"
                      "--format=%(refname)"
                      "refs/git-overleaf/backups")))))))

(ert-deftest git-overleaf-git-test-fresh-pull-records-pending-conflict ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf--fresh-pull repo remote-root)
        (let ((pending (git-overleaf--pending-state repo))
              (status (git-overleaf--read-repo-status repo)))
          (should (equal (plist-get pending :action) 'pull))
          (should (git-overleaf--git-object-id-p
                   (plist-get pending :remote-commit)))
          (should (git-overleaf--merge-in-progress-p repo))
          (should (git-overleaf--repo-status-unmerged status)))
        (should-not (string-empty-p
                     (git-overleaf-git-test--git
                      repo
                      "for-each-ref"
                      "--format=%(refname)"
                      "refs/git-overleaf/backups")))))))

(ert-deftest git-overleaf-git-test-working-tree-error-branches ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--write-file repo "base.tex" "base\n")
      (git-overleaf-git-test--commit-all repo "base")
      (should-error (git-overleaf--require-managed-repo repo)
                    :type 'user-error))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "dirty.tex" "dirty\n")
      (should-error (git-overleaf--ensure-clean-working-tree
                     repo
                     "testing")
                    :type 'user-error)
      (should-error (git-overleaf--prepare-working-tree-for-sync
                     repo
                     'error)
                    :type 'user-error)
      (git-overleaf-git-test--git repo "add" "dirty.tex")
      (should-error (git-overleaf--ensure-clean-working-tree
                     repo
                     "testing")
                    :type 'user-error))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--git repo "checkout" "--detach" "HEAD")
      (should-error (git-overleaf--current-branch repo)
                    :type 'user-error))))

;;; git-overleaf-git-test.el ends here
