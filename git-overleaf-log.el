;;; git-overleaf-log.el --- Logging for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Global log buffer and logging helpers for git-overleaf.

;;; Code:

;;;; Customization

(defgroup git-overleaf nil
  "Clone, push, and pull full Overleaf projects."
  :prefix "git-overleaf-"
  :group 'tools)

(defcustom git-overleaf-debug nil
  "Whether to emit verbose debug messages."
  :type 'boolean
  :group 'git-overleaf)

(defcustom git-overleaf-log-echo t
  "Whether Overleaf project log entries are also echoed in the minibuffer."
  :type 'boolean
  :group 'git-overleaf)

;;;; Context

(defvar git-overleaf-log-context nil
  "Dynamic plist describing the Overleaf project currently being logged.
Supported keys include `:project-name', `:project-id', `:repo', and
`:url'.")

(defcustom git-overleaf-log-context-function
  'git-overleaf--log-default-context
  "Function returning a fallback Overleaf project log context plist.
It is called with no arguments and should return a plist like
`git-overleaf-log-context'.  The default function builds a context
from the current Git repository, when one is available."
  :type '(choice (const :tag "Use current Git repository"
                        git-overleaf--log-default-context)
                 (function :tag "Custom function"))
  :group 'git-overleaf)

(defconst git-overleaf-log--buffer-name "*git-overleaf-log*"
  "Name of the global Overleaf project log buffer.")

(defconst git-overleaf-log--time-format "%Y-%m-%d %H:%M:%S"
  "Time format used for entries in the Overleaf project log buffer.")

(defvar git-overleaf-log--mutex
  (and (fboundp 'make-mutex)
       (make-mutex "git-overleaf-log"))
  "Mutex protecting writes to the Overleaf project log buffer.")

(defun git-overleaf-log-make-context (&rest keys)
  "Return a normalized log context plist.
KEYS accepts `:project-name', `:project-id', `:repo', and `:url'."
  (let ((project-name (plist-get keys :project-name))
        (project-id (plist-get keys :project-id))
        (repo (plist-get keys :repo))
        (url (plist-get keys :url)))
    (append
     (when project-name
       (list :project-name project-name))
     (when project-id
       (list :project-id project-id))
     (when repo
       (list :repo (directory-file-name (expand-file-name repo))))
     (when url
       (list :url url)))))

(defun git-overleaf-log--merge-contexts (&rest contexts)
  "Merge context plists in CONTEXTS.
Later non-nil values replace earlier values."
  (let (merged)
    (dolist (context contexts)
      (when (listp context)
        (while context
          (let ((key (pop context))
                (value (pop context)))
            (when (and (keywordp key) value)
              (setq merged (plist-put merged key value)))))))
    merged))

(defun git-overleaf-log-current-context ()
  "Return the current effective Overleaf project log context."
  (git-overleaf-log--merge-contexts
   (and git-overleaf-log-context-function
        (ignore-errors (funcall git-overleaf-log-context-function)))
   git-overleaf-log-context))

(defmacro git-overleaf-log-with-context (context &rest body)
  "Run BODY with CONTEXT merged into the current log context."
  (declare (indent 1) (debug (form body)))
  `(let ((git-overleaf-log-context
          (git-overleaf-log--merge-contexts
           (git-overleaf-log-current-context)
           ,context)))
     ,@body))

(defun git-overleaf-log--context-label (context)
  "Return a display label for CONTEXT."
  (let* ((project-name (plist-get context :project-name))
         (project-id (plist-get context :project-id))
         (repo (plist-get context :repo))
         (url (plist-get context :url))
         (parts
          (delq
           nil
           (list
            (when project-name
              (format "project=%s" project-name))
            (when project-id
              (format "id=%s" project-id))
            (when repo
              (format "repo=%s"
                      (abbreviate-file-name
                       (directory-file-name (expand-file-name repo)))))
            (when url
              (format "url=%s" url))))))
    (if parts
        (mapconcat #'identity parts " ")
      "global")))

;;;; Buffer

(define-derived-mode git-overleaf-log-mode special-mode "Git-Overleaf-Log"
  "Major mode for the global Overleaf project log buffer."
  (setq-local truncate-lines t))

(defun git-overleaf-log--buffer ()
  "Return the global Overleaf project log buffer."
  (let ((buffer (get-buffer-create git-overleaf-log--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'git-overleaf-log-mode)
        (git-overleaf-log-mode)))
    buffer))

;;;###autoload
(defun git-overleaf-log ()
  "Display the global Overleaf project log buffer."
  (interactive)
  (display-buffer (git-overleaf-log--buffer)))

;;;###autoload
(defun git-overleaf-log-clear ()
  "Clear the global Overleaf project log buffer."
  (interactive)
  (let ((buffer (git-overleaf-log--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defmacro git-overleaf-log--with-mutex (&rest body)
  "Run BODY while holding the log mutex when available."
  (declare (indent 0) (debug t))
  `(if git-overleaf-log--mutex
       (progn
         (mutex-lock git-overleaf-log--mutex)
         (unwind-protect
             (progn ,@body)
           (mutex-unlock git-overleaf-log--mutex)))
     ,@body))

(defun git-overleaf-log--append (level text)
  "Append TEXT as LEVEL to the global Overleaf project log buffer."
  (let* ((context (git-overleaf-log-current-context))
         (timestamp (format-time-string git-overleaf-log--time-format))
         (prefix (format "%s %-5s [%s] "
                         timestamp
                         (upcase (symbol-name level))
                         (git-overleaf-log--context-label context)))
         (lines (split-string (or text "") "\n"))
         (buffer (git-overleaf-log--buffer)))
    (git-overleaf-log--with-mutex
     (with-current-buffer buffer
       (let ((inhibit-read-only t)
             (moving (eobp)))
         (save-excursion
           (goto-char (point-max))
           (insert prefix (car lines) "\n")
           (dolist (line (cdr lines))
             (insert (make-string (length prefix) ?\s) line "\n")))
         (when moving
           (goto-char (point-max))))))))

(defun git-overleaf-log--emit (level echo-prefix format-string args)
  "Log LEVEL entry and echo it with ECHO-PREFIX when enabled.
FORMAT-STRING and ARGS are passed to `format' to build the log text."
  (let ((text (apply #'format format-string args)))
    (git-overleaf-log--append level text)
    (when git-overleaf-log-echo
      (message "%s" (concat "[git-overleaf] " echo-prefix text)))
    text))

;;;; Logging helpers

(defun git-overleaf--message (format-string &rest args)
  "Log an Overleaf info message using FORMAT-STRING and ARGS."
  (git-overleaf-log--emit 'info "" format-string args))

(defun git-overleaf--warn (format-string &rest args)
  "Log an Overleaf warning using FORMAT-STRING and ARGS."
  (git-overleaf-log--emit 'warn "WARNING: " format-string args))

(defun git-overleaf--debug (format-string &rest args)
  "Log a debug message using FORMAT-STRING and ARGS."
  (when git-overleaf-debug
    (git-overleaf-log--emit 'debug "DEBUG: " format-string args)))

(provide 'git-overleaf-log)

;;; git-overleaf-log.el ends here
