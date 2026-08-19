;;; git-overleaf-git-test.el --- Git integration tests for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-core)
(require 'git-overleaf-http)
(require 'git-overleaf-sync)
(require 'git-overleaf)

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
   repo "git-overleaf.baseRef" git-overleaf-base-ref)
  (git-overleaf--git-config-set
   repo "git-overleaf.remoteRef" git-overleaf-remote-ref)
  (git-overleaf--configure-remote repo git-overleaf-remote-name "project-id"))

(defun git-overleaf-git-test--base-commit (repo text)
  "Create a base commit in REPO containing TEXT and set the base ref."
  (git-overleaf-git-test--write-file repo "main.tex" text)
  (let ((commit (git-overleaf-git-test--commit-all repo "base")))
    (git-overleaf-git-test--mark-managed repo)
    (git-overleaf--set-base-ref repo commit)
    (git-overleaf--set-remote-ref repo commit)
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
                  (push args sync-calls)
                  nil))
               ((symbol-function 'git-overleaf--upload-sync-metadata)
                (lambda (&rest args)
                  (push args metadata-calls)
                  nil))
               ((symbol-function 'git-overleaf--message)
                (lambda (&rest args)
                  (push args messages)
                  nil))
               ((symbol-function 'git-overleaf--warn)
                (lambda (&rest args)
                  (push args warnings)
                  nil)))
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
          (should (equal (git-overleaf--rev-parse
                          repo
                          (git-overleaf--remote-ref repo))
                         commit))
          (should (equal (git-overleaf--remote-name repo) "overleaf"))
          (should (equal
                   (git-overleaf--git-config-get
                    repo "remote.overleaf.skipFetchAll")
                   "true"))
          (should (equal
                   (git-overleaf--git-config-get
                    repo "remote.overleaf.skipDefaultUpdate")
                   "true"))
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
                         head))
          (should (equal
                   (git-overleaf--rev-parse repo git-overleaf-remote-ref)
                   head)))))))

(ert-deftest git-overleaf-git-test-logical-remote-rename-and-remove ()
  (git-overleaf-git-test--with-repo repo
    (git-overleaf--write-repo-metadata
     repo '(:id "project-id" :name "Project"))
    (should (equal (git-overleaf--remote-name repo) "overleaf"))
    (git-overleaf-git-test--git repo "remote" "rename" "overleaf" "paper")
    (should (equal (git-overleaf--remote-name repo) "paper"))
    (git-overleaf-git-test--git repo "remote" "remove" "paper")
    (should-not (git-overleaf--remote-name repo))))

(ert-deftest git-overleaf-git-test-logical-remote-name-collision ()
  (git-overleaf-git-test--with-repo repo
    (git-overleaf-git-test--git
     repo "remote" "add" "overleaf" "https://example.invalid/repo.git")
    (let (warnings)
      (cl-letf (((symbol-function 'git-overleaf--warn)
                 (lambda (&rest args) (push args warnings))))
        (git-overleaf--write-repo-metadata
         repo '(:id "project-id" :name "Project")))
      (should warnings))
    (should-not (git-overleaf--remote-name repo))
    (should (equal
             (git-overleaf--git-config-get repo "remote.overleaf.url")
             "https://example.invalid/repo.git"))
    (git-overleaf-git-test--write-file repo "main.tex" "base\n")
    (let ((head (git-overleaf-git-test--commit-all repo "base")))
      (git-overleaf--set-base-ref repo head)
      (should (equal
               (git-overleaf-register-remote repo "overleaf-project")
               "overleaf-project"))
      (should (equal
               (git-overleaf--remote-name repo)
               "overleaf-project"))
      (should (equal
               (git-overleaf--rev-parse repo (git-overleaf--remote-ref repo))
               head)))))

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
                           head))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--remote-ref repo))
                           head))))))))

(ert-deftest git-overleaf-git-test-fresh-push-normalizes-matching-refs ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "same\n")
      (git-overleaf-git-test--git repo "commit" "--allow-empty" "-m" "empty")
      (let ((head (git-overleaf--rev-parse repo "HEAD")))
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "same\n")
          (git-overleaf-git-test--without-remote-side-effects
            (git-overleaf--fresh-push
             repo
             remote-root
             (git-overleaf-git-test--remote-table))
            (should-not sync-calls)
            (should (= (length metadata-calls) 1))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--base-ref repo))
                           head))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--remote-ref repo))
                           head))))))))

(ert-deftest git-overleaf-git-test-force-push-clears-stale-pending-config ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (let ((head (git-overleaf-git-test--base-commit repo "base\n")))
        (git-overleaf--set-pending-pull-state repo head)
        (should (git-overleaf--pending-state repo))
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
          (git-overleaf-git-test--without-remote-side-effects
            (git-overleaf--fresh-push
             repo remote-root (git-overleaf-git-test--remote-table) t)
            (should (= (length sync-calls) 1))))
        (should-not (git-overleaf--pending-state repo))
        (should-not
         (git-overleaf--git-config-get
          repo "git-overleaf.pendingTargetCommit"))))))

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
        (should (equal (git-overleaf--rev-parse
                        repo
                        (git-overleaf--remote-ref repo))
                       (git-overleaf--rev-parse repo "HEAD")))
        (should-not (git-overleaf--pending-state repo))
        (should-not (string-empty-p
                     (git-overleaf-git-test--git
                      repo
                      "for-each-ref"
                      "--format=%(refname)"
                      "refs/git-overleaf/backups")))))))

(ert-deftest git-overleaf-git-test-pull-sync-preserves-unrelated-local-change ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "notes.tex" "base notes\n")
      (let ((base (git-overleaf-git-test--commit-all repo "add notes")))
        (git-overleaf--set-base-ref repo base)
        (git-overleaf--set-remote-ref repo base))
      (git-overleaf-git-test--write-file repo "notes.tex" "local notes\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf-git-test--write-file remote-root "notes.tex" "base notes\n")
        (cl-letf (((symbol-function 'git-overleaf--with-downloaded-snapshot)
                   (lambda (_project-id function)
                     (funcall function remote-root))))
          (git-overleaf--pull-sync repo t))
        (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                       "remote\n"))
        (should (equal (git-overleaf-git-test--read-file repo "notes.tex")
                       "local notes\n"))
        (should (git-overleaf--repo-status-unstaged
                 (git-overleaf--read-repo-status repo)))
        (should-not (git-overleaf--pending-state repo))))))

(ert-deftest git-overleaf-git-test-diverged-pull-preserves-unrelated-local-change ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "notes.tex" "base notes\n")
      (let ((base (git-overleaf-git-test--commit-all repo "add notes")))
        (git-overleaf--set-base-ref repo base)
        (git-overleaf--set-remote-ref repo base))
      (git-overleaf-git-test--write-file repo "local.tex" "committed local\n")
      (git-overleaf-git-test--commit-all repo "local change")
      (git-overleaf-git-test--write-file repo "notes.tex" "uncommitted local\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf-git-test--write-file remote-root "notes.tex" "base notes\n")
        (git-overleaf--fresh-pull repo remote-root)
        (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                       "remote\n"))
        (should (equal (git-overleaf-git-test--read-file repo "local.tex")
                       "committed local\n"))
        (should (equal (git-overleaf-git-test--read-file repo "notes.tex")
                       "uncommitted local\n"))
        (should (git-overleaf--repo-status-unstaged
                 (git-overleaf--read-repo-status repo)))
        (should-not (git-overleaf--merge-in-progress-p repo))
        (should-not (git-overleaf--pending-state repo))))))

(ert-deftest git-overleaf-git-test-diverged-pull-rejects-overlapping-local-change ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (let ((base (git-overleaf-git-test--base-commit repo "base\n")))
        (git-overleaf-git-test--write-file repo "local.tex" "committed local\n")
        (let ((local-head
               (git-overleaf-git-test--commit-all repo "local change")))
          (git-overleaf-git-test--write-file repo "main.tex" "uncommitted local\n")
          (git-overleaf-git-test--with-temp-dir remote-root
            (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
            (should-error (git-overleaf--fresh-pull repo remote-root))
            (should (equal (git-overleaf--rev-parse repo "HEAD") local-head))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--base-ref repo))
                           base))
            (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                           "uncommitted local\n"))
            (should-not (git-overleaf--merge-in-progress-p repo))
            (should-not (git-overleaf--repo-status-unmerged
                         (git-overleaf--read-repo-status repo)))
            (should-not (git-overleaf--pending-state repo))))))))

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

(ert-deftest git-overleaf-git-test-committed-pending-pull-integrates-new-remote ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--with-temp-dir remote-one
        (git-overleaf-git-test--write-file remote-one "main.tex" "remote\n")
        (should (eq (git-overleaf--fresh-pull repo remote-one) 'conflict)))
      (git-overleaf-git-test--write-file repo "main.tex" "merged\n")
      (git-overleaf-git-test--commit-all repo "resolve remote one")
      (should (eq (git-overleaf--pending-phase repo) 'committed))
      (git-overleaf-git-test--with-temp-dir remote-two
        (git-overleaf-git-test--write-file remote-two "main.tex" "remote\n")
        (git-overleaf-git-test--write-file remote-two "new.tex" "new remote\n")
        (should (eq (git-overleaf--pull-with-remote-state repo remote-two)
                    'merged))
        (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                       "merged\n"))
        (should (equal (git-overleaf-git-test--read-file repo "new.tex")
                       "new remote\n"))
        (should-not (git-overleaf--pending-state repo))))))

(ert-deftest git-overleaf-git-test-pull-abort-active-and-committed-states ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (let ((local-head (git-overleaf-git-test--commit-all repo "local")))
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
          (git-overleaf--fresh-pull repo remote-root))
        (git-overleaf--abort-pending-pull repo)
        (should-not (git-overleaf--pending-state repo))
        (should-not (git-overleaf--merge-in-progress-p repo))
        (should (equal (git-overleaf--rev-parse repo "HEAD") local-head))))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf--fresh-pull repo remote-root))
      (git-overleaf-git-test--write-file repo "main.tex" "resolved\n")
      (git-overleaf-git-test--commit-all repo "resolve")
      (should-error (git-overleaf--abort-pending-pull repo)
                    :type 'user-error)
      (should (git-overleaf--pending-state repo)))))

(ert-deftest git-overleaf-git-test-push-uploads-head-and-ignores-worktree ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "committed\n")
      (let ((head (git-overleaf-git-test--commit-all repo "local")))
        (git-overleaf-git-test--write-file repo "main.tex" "dirty\n")
        (git-overleaf-git-test--write-file repo "untracked.tex" "untracked\n")
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "base\n")
          (git-overleaf-git-test--without-remote-side-effects
            (git-overleaf--fresh-push
             repo remote-root (git-overleaf-git-test--remote-table))
            (should (equal (cadr (car sync-calls)) head))))
        (should (equal (git-overleaf--rev-parse repo "HEAD") head))
        (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                       "dirty\n"))
        (should (file-exists-p (expand-file-name "untracked.tex" repo)))))))

(ert-deftest git-overleaf-git-test-interrupted-push-journal-resumes ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "target\n")
      (let ((target (git-overleaf-git-test--commit-all repo "target")))
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "base\n")
          (cl-letf (((symbol-function 'git-overleaf--sync-commit)
                     (lambda (&rest _args) (error "upload interrupted"))))
            (should-error
             (git-overleaf--fresh-push
              repo remote-root (git-overleaf-git-test--remote-table))))
          (let ((pending (git-overleaf--pending-state repo)))
            (should (eq (plist-get pending :action) 'push))
            (should (equal (plist-get pending :target-commit) target)))
          (git-overleaf-git-test--write-file remote-root "main.tex" "target\n")
          (git-overleaf-git-test--without-remote-side-effects
            (git-overleaf--resume-pending-push
             repo (git-overleaf--pending-state repo) remote-root
             (git-overleaf-git-test--remote-table)))
          (should-not (git-overleaf--pending-state repo)))))))

(ert-deftest git-overleaf-git-test-interrupted-push-detects-independent-change ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (let ((base (git-overleaf-git-test--base-commit repo "base\n")))
        (git-overleaf-git-test--write-file repo "main.tex" "target\n")
        (let ((target (git-overleaf-git-test--commit-all repo "target")))
          (git-overleaf--set-pending-push-state repo base target)
          (git-overleaf-git-test--with-temp-dir remote-root
            (git-overleaf-git-test--write-file remote-root "main.tex" "external\n")
            (should-error
             (git-overleaf--resume-pending-push
              repo (git-overleaf--pending-state repo) remote-root
              (git-overleaf-git-test--remote-table))
             :type 'user-error)
            (should (git-overleaf--pending-state repo))))))))

(ert-deftest git-overleaf-git-test-pull-reconciles-pending-push ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (let ((base (git-overleaf-git-test--base-commit repo "base\n")))
        (git-overleaf-git-test--write-file repo "local.tex" "local\n")
        (let ((target (git-overleaf-git-test--commit-all repo "local")))
          (git-overleaf--set-pending-push-state repo base target)
          (git-overleaf-git-test--with-temp-dir remote-root
            (git-overleaf-git-test--write-file remote-root "main.tex" "base\n")
            (git-overleaf-git-test--write-file remote-root "remote.tex" "remote\n")
            (should (eq (git-overleaf--pull-with-remote-state repo remote-root)
                        'merged))
            (should-not (git-overleaf--pending-state repo))
            (should (equal (git-overleaf-git-test--read-file
                            repo "local.tex")
                           "local\n"))
            (should (equal (git-overleaf-git-test--read-file
                            repo "remote.tex")
                           "remote\n"))))))))

(ert-deftest git-overleaf-git-test-status-data-is-cached-and-actionable ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (let ((status (git-overleaf--status-data repo)))
        (should (eq (plist-get status :state) 'in-sync))
        (should-not (plist-get status :worktree))
        (should (equal (plist-get status :recommendation)
                       "No synchronization needed")))
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--write-file repo "untracked.tex" "dirty\n")
      (let ((status (git-overleaf--status-data repo)))
        (should (eq (plist-get status :state) 'remote-matches-base))
        (should (equal (plist-get status :worktree) '(unstaged)))
        (should (equal (plist-get status :recommendation)
                       "Run git-overleaf-push"))))))

(ert-deftest git-overleaf-git-test-reset-hard-replaces-tracked-worktree ()
  (let ((git-overleaf-log-echo nil)
        (git-overleaf-local-backups-enabled t))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local\n")
      (git-overleaf-git-test--write-file repo "local-only.tex" "local\n")
      (let ((local-head
             (git-overleaf-git-test--commit-all repo "local commit")))
        (git-overleaf-git-test--write-file repo "main.tex" "dirty\n")
        (git-overleaf-git-test--write-file repo "untracked.tex" "preserve\n")
        (git-overleaf--set-pending-pull-state repo local-head)
        (git-overleaf-git-test--with-temp-dir remote-root
          (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
          (let ((remote-commit
                 (git-overleaf--record-remote-snapshot repo remote-root)))
            (git-overleaf--reset-to-remote repo 'hard)
            (should (equal (git-overleaf--rev-parse repo "HEAD")
                           remote-commit))
            (should (equal (git-overleaf-git-test--read-file
                            repo
                            "main.tex")
                           "remote\n"))
            (should-not (file-exists-p
                         (expand-file-name "local-only.tex" repo)))
            (should (equal (git-overleaf-git-test--read-file
                            repo "untracked.tex")
                           "preserve\n"))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--base-ref repo))
                           remote-commit))
            (should (equal (git-overleaf--rev-parse
                            repo
                            (git-overleaf--remote-ref repo))
                           remote-commit))
            (should-not (git-overleaf--pending-state repo))
            (let ((backup-commits
                   (git-overleaf-git-test--git
                    repo
                    "for-each-ref"
                    "--format=%(objectname)"
                    "refs/git-overleaf/backups")))
              (should (string-match-p (regexp-quote local-head)
                                      backup-commits)))))))))

(ert-deftest git-overleaf-git-test-reset-hard-preserves-ignored-files ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo ".gitignore" "secret.txt\n")
      (git-overleaf-git-test--commit-all repo "ignore secret")
      (git-overleaf-git-test--write-file repo "secret.txt" "preserve\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (git-overleaf--record-remote-snapshot repo remote-root)
        (git-overleaf--reset-to-remote repo 'hard)
        (should (equal (git-overleaf-git-test--read-file
                        repo
                        "secret.txt")
                       "preserve\n"))))))

(ert-deftest git-overleaf-git-test-reset-mixed-preserves-worktree-content ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--write-file repo "main.tex" "local commit\n")
      (git-overleaf-git-test--commit-all repo "local")
      (git-overleaf-git-test--write-file repo "main.tex" "working copy\n")
      (git-overleaf-git-test--with-temp-dir remote-root
        (git-overleaf-git-test--write-file remote-root "main.tex" "remote\n")
        (let ((remote (git-overleaf--record-remote-snapshot repo remote-root)))
          (git-overleaf--reset-to-remote repo 'mixed)
          (should (equal (git-overleaf--rev-parse repo "HEAD") remote))
          (should (equal (git-overleaf-git-test--read-file repo "main.tex")
                         "working copy\n"))
          (should (git-overleaf--repo-status-unstaged
                   (git-overleaf--read-repo-status repo))))))))

(ert-deftest git-overleaf-git-test-working-tree-error-branches ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--write-file repo "base.tex" "base\n")
      (git-overleaf-git-test--commit-all repo "base")
      (should-error (git-overleaf--require-managed-repo repo)
                    :type 'user-error))
    (git-overleaf-git-test--with-repo repo
      (git-overleaf-git-test--base-commit repo "base\n")
      (git-overleaf-git-test--git repo "checkout" "--detach" "HEAD")
      (should-error (git-overleaf--current-branch repo)
                    :type 'user-error))))

;;; git-overleaf-git-test.el ends here
