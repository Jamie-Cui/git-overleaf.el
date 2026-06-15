;;; coverage.el --- Batch coverage runner for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defvar git-overleaf-coverage-min 0
  "Minimum total coverage percentage required by the batch coverage run.")

(defvar git-overleaf-coverage-directory "coverage"
  "Directory where batch coverage reports are written.")

(defconst git-overleaf-coverage--source-files
  '("git-overleaf-log.el"
    "git-overleaf-core.el"
    "git-overleaf-http.el"
    "git-overleaf-sync.el"
    "git-overleaf-firefox.el"
    "git-overleaf-auth.el"
    "git-overleaf.el"
    "git-overleaf-magit.el")
  "Source files instrumented by the batch coverage run.")

(defconst git-overleaf-coverage--test-files
  '("test/git-overleaf-test.el"
    "test/git-overleaf-git-test.el"
    "test/git-overleaf-sync-tree-test.el"
    "test/git-overleaf-command-test.el"
    "test/git-overleaf-http-auth-test.el"
    "test/git-overleaf-async-test.el"
    "test/git-overleaf-magit-test.el")
  "ERT files loaded by the batch coverage run.")

(defvar git-overleaf-coverage--instrumented nil
  "Alist mapping source file names to instrumented definition symbols.")

(defconst git-overleaf-coverage--uninstrumented-symbols
  '(("git-overleaf-core.el" . (git-overleaf--async-start)))
  "Definitions restored without `testcover' instrumentation.

Emacs 29's Edebug instrumentation can leave marker forms inside the
lexical environment of `make-thread' closures.  Keep the async thread
entry point uninstrumented so the batch coverage run remains portable
across the Emacs versions used locally and in CI.")

(defun git-overleaf-coverage--restore-definition (file symbol)
  "Restore SYMBOL from FILE without `testcover' instrumentation."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file default-directory))
    (goto-char (point-min))
    (let ((found nil)
          form)
      (condition-case nil
          (while (not found)
            (setq form (read (current-buffer)))
            (when (and (consp form)
                       (memq (car form) '(defun defmacro cl-defun cl-defmacro))
                       (eq (cadr form) symbol))
              (eval form t)
              (setq found t)))
        (end-of-file nil))
      (unless found
        (error "Could not restore %S from %s" symbol file)))))

(defun git-overleaf-coverage--instrument-file (file)
  "Instrument FILE with `testcover' and remember its definition symbols."
  (let ((uninstrumented
         (cdr (assoc file git-overleaf-coverage--uninstrumented-symbols)))
        symbols)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (testcover-start file))
    (setq symbols
          (cl-remove-if (lambda (symbol)
                          (memq symbol uninstrumented))
                        (mapcar #'car edebug-form-data)))
    (dolist (symbol uninstrumented)
      (git-overleaf-coverage--restore-definition file symbol))
    (push (cons file symbols)
          git-overleaf-coverage--instrumented)))

(defun git-overleaf-coverage--entry-covered-p (entry)
  "Return non-nil if testcover ENTRY is considered covered."
  (or (eq entry 'edebug-ok-coverage)
      (memq (car-safe entry) '(testcover-1value maybe noreturn))))

(defun git-overleaf-coverage--symbol-summary (symbol)
  "Return (TOTAL COVERED UNCOVERED) for SYMBOL's coverage vector."
  (let ((coverage (get symbol 'edebug-coverage))
        (total 0)
        (covered 0)
        (uncovered 0))
    (when (vectorp coverage)
      (dotimes (index (length coverage))
        (setq total (1+ total))
        (if (git-overleaf-coverage--entry-covered-p
             (aref coverage index))
            (setq covered (1+ covered))
          (setq uncovered (1+ uncovered)))))
    (list total covered uncovered)))

(defun git-overleaf-coverage--file-summary (file symbols)
  "Return a plist coverage summary for FILE and SYMBOLS."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (symbol symbols)
      (pcase-let ((`(,sym-total ,sym-covered ,sym-uncovered)
                   (git-overleaf-coverage--symbol-summary symbol)))
        (when (> sym-total 0)
          (setq defs (1+ defs))
          (setq total (+ total sym-total))
          (setq covered (+ covered sym-covered))
          (setq uncovered (+ uncovered sym-uncovered)))))
    (list :file file
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun git-overleaf-coverage--summaries ()
  "Return coverage summaries for all instrumented files."
  (mapcar (lambda (entry)
            (git-overleaf-coverage--file-summary
             (car entry)
             (cdr entry)))
          (nreverse git-overleaf-coverage--instrumented)))

(defun git-overleaf-coverage--total-summary (summaries)
  "Return total coverage summary for SUMMARIES."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (summary summaries)
      (setq defs (+ defs (plist-get summary :defs)))
      (setq total (+ total (plist-get summary :total)))
      (setq covered (+ covered (plist-get summary :covered)))
      (setq uncovered (+ uncovered (plist-get summary :uncovered))))
    (list :file "TOTAL"
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun git-overleaf-coverage--format-summary (summary)
  "Return a human-readable line for coverage SUMMARY."
  (format "%-28s defs=%3d forms=%5d covered=%5d missed=%5d %6.2f%%"
          (plist-get summary :file)
          (plist-get summary :defs)
          (plist-get summary :total)
          (plist-get summary :covered)
          (plist-get summary :uncovered)
          (plist-get summary :percent)))

(defun git-overleaf-coverage--write-tsv (summaries total)
  "Write SUMMARIES and TOTAL to the batch coverage TSV report."
  (make-directory git-overleaf-coverage-directory t)
  (let ((report (expand-file-name
                 "testcover-summary.tsv"
                 git-overleaf-coverage-directory)))
    (with-temp-file report
      (insert "file\tdefs\tforms\tcovered\tmissed\tpercent\n")
      (dolist (summary (append summaries (list total)))
        (insert
         (format "%s\t%d\t%d\t%d\t%d\t%.2f\n"
                 (plist-get summary :file)
                 (plist-get summary :defs)
                 (plist-get summary :total)
                 (plist-get summary :covered)
                 (plist-get summary :uncovered)
                 (plist-get summary :percent)))))
    report))

(defun git-overleaf-coverage-run ()
  "Run ERT tests under `testcover' and write a coverage summary."
  (setq git-overleaf-coverage--instrumented nil)
  (dolist (file git-overleaf-coverage--source-files)
    (git-overleaf-coverage--instrument-file file))
  (dolist (file git-overleaf-coverage--test-files)
    (load (expand-file-name file default-directory) nil t))
  (let* ((stats (ert-run-tests-batch t))
         (summaries (git-overleaf-coverage--summaries))
         (total (git-overleaf-coverage--total-summary summaries))
         (report (git-overleaf-coverage--write-tsv summaries total)))
    (princ "\nCoverage summary:\n")
    (dolist (summary summaries)
      (princ (concat (git-overleaf-coverage--format-summary summary)
                     "\n")))
    (princ (concat (git-overleaf-coverage--format-summary total)
                   "\n"))
    (princ (format "Coverage report: %s\n" report))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (kill-emacs 1))
    (when (< (plist-get total :percent) git-overleaf-coverage-min)
      (princ
       (format
        "Coverage %.2f%% is below required minimum %.2f%%\n"
        (plist-get total :percent)
        (float git-overleaf-coverage-min)))
      (kill-emacs 1))))

(git-overleaf-coverage-run)

;;; coverage.el ends here
