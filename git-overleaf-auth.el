;;; git-overleaf-auth.el --- Browser authentication for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overleaf authentication backends.

;;; Code:

(require 'cl-lib)
(require 'url-expand)
(require 'webdriver)
(require 'webdriver-firefox)
(require 'git-overleaf-core)
(require 'git-overleaf-firefox)
(require 'git-overleaf-http)

(declare-function git-overleaf--async-enabled-p "git-overleaf-core")
(declare-function git-overleaf--async-register-process "git-overleaf-core")
(declare-function git-overleaf--async-unregister-process "git-overleaf-core")

;;;; Authentication

(defmacro git-overleaf--with-webdriver (&rest body)
  "Execute BODY if geckodriver is available."
  `(if (not (executable-find "geckodriver"))
       (progn
         (message-box
          "Please install geckodriver to authenticate with Overleaf.")
         (user-error "Required executable `geckodriver' was not found"))
     ,@body))

;; `webdriver-firefox' currently assumes geckodriver immediately prints a
;; specific "Listening on ..." line before `webdriver-service-start' asks for
;; the port.  Newer geckodriver builds can race with that lookup, which raises
;; a plain `search-failed' before authentication even opens Firefox.  Poll the
;; process buffer briefly and fall back to the already configured port.
(cl-defmethod webdriver-service-get-port ((self webdriver-service-firefox))
  "Return the port where SELF is listening, tolerating delayed log output."
  (let ((process (oref self process))
        (buffer (get-buffer (oref self buffer)))
        (fallback (oref self port))
        (deadline (+ (float-time) 3.0))
        port)
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (while (and (not port)
                      (process-live-p process)
                      (< (float-time) deadline))
            (goto-char (point-max))
            (when (re-search-backward
                   "Listening on .*:\\([0-9]+\\)\\(?:[[:space:]]*$\\|/\\)"
                   nil t)
              (setq port (string-to-number (match-string 1))))
            (unless port
              (accept-process-output process 0.1 nil t))))))
    (or port
        (and (integerp fallback) fallback)
        (error "Could not determine geckodriver listening port"))))

(defun git-overleaf--webdriver-service ()
  "Return a Firefox webdriver service using an available local port."
  (make-instance 'webdriver-service-firefox
                 :port (webdriver--get-free-port)
                 :buffer (generate-new-buffer
                          " *git-overleaf-geckodriver*")))

(defun git-overleaf--webdriver-session ()
  "Return a webdriver session for Overleaf authentication."
  (make-instance 'webdriver-session
                 :service (git-overleaf--webdriver-service)))

(defun git-overleaf--webdriver-session-stop (session)
  "Stop SESSION and its service without masking an earlier error."
  (let ((service (oref session service)))
    (condition-case err
        (when (oref session id)
          (webdriver-session-stop session))
      (error
       (git-overleaf--debug
        "Ignoring webdriver session cleanup error: %s"
        (error-message-string err))))
    (when service
      (condition-case err
          (webdriver-service-stop service)
        (error
         (git-overleaf--debug
          "Ignoring webdriver service cleanup error: %s"
          (error-message-string err)))))))

(defmacro git-overleaf--with-webdriver-direct-connection (&rest body)
  "Run BODY with local webdriver HTTP requests bypassing proxies."
  (declare (indent 0) (debug t))
  `(let ((url-proxy-services
          (cons '("no_proxy" . "\\`\\(?:localhost\\|127\\.0\\.0\\.1\\)\\'")
                (assq-delete-all "no_proxy" (copy-sequence url-proxy-services)))))
     ,@body))

(cl-defmacro git-overleaf--webdriver-wait-until-appears
    ((session xpath &optional (element-sym '_unused) (delay .1)) &rest body)
  "Wait until XPATH appears in SESSION, bind it to ELEMENT-SYM and run BODY."
  (let ((not-found (gensym))
        (selector (gensym)))
    `(let ((,selector
            (make-instance 'webdriver-by
                           :strategy "xpath"
                           :selector ,xpath))
           (,not-found t))
       (while ,not-found
         (condition-case nil
             (let ((,element-sym
                    (webdriver-find-element ,session ,selector)))
               (setq ,not-found nil)
               ,@body)
           (webdriver-error
            (sleep-for ,delay)))))))

(defun git-overleaf--webdriver-cookie-string (cookies)
  "Return an HTTP Cookie header string for webdriver COOKIES."
  (let ((pairs
         (cl-loop
          for cookie across cookies
          for name = (alist-get 'name cookie)
          for value = (alist-get 'value cookie)
          when (and (stringp name) (stringp value))
          collect (format "%s=%s" name value))))
    (unless pairs
      (user-error "No cookies were captured after Overleaf authentication"))
    (string-join pairs "; ")))

(defun git-overleaf--webdriver-project-url (href)
  "Return an absolute Overleaf project URL for HREF.
HREF may already be absolute or may be a relative path such as
\"/project/...\"."
  (and (stringp href)
       (url-expand-file-name href (concat (git-overleaf--url) "/"))))

(defun git-overleaf--webdriver-cookie-expiry (cookies)
  "Return the authenticated-session expiry for webdriver COOKIES, or nil.
Only cookie names matching `git-overleaf-auth-session-cookie-regexp' are
considered.  This avoids treating short-lived analytics cookies as the
expiry of the actual Overleaf login session."
  (let ((expiries
         (cl-loop
          for cookie across cookies
          for name = (alist-get 'name cookie)
          for expiry = (alist-get 'expiry cookie)
          when (and (stringp name)
                    (integerp expiry)
                    (string-match-p
                     git-overleaf-auth-session-cookie-regexp
                     name))
          collect expiry)))
    (when expiries
      (apply #'min expiries))))

(defun git-overleaf--apply-authenticated-cookies (full-cookies message)
  "Apply FULL-COOKIES to the current session and display MESSAGE.
MESSAGE is formatted with the current cookie domain when non-nil."
  (git-overleaf--clear-csrf-cache)
  (setq git-overleaf--current-cookies
        (git-overleaf--normalize-full-cookies full-cookies))
  (when message
    (git-overleaf--message message (git-overleaf--cookie-domain))))

(defun git-overleaf--save-and-apply-authenticated-cookies
    (full-cookies message)
  "Save and apply FULL-COOKIES, then display MESSAGE."
  (unless (and (boundp 'git-overleaf-save-cookies)
               git-overleaf-save-cookies)
    (user-error
     "`git-overleaf-save-cookies' needs to be configured"))
  (funcall git-overleaf-save-cookies (prin1-to-string full-cookies))
  (git-overleaf--apply-authenticated-cookies full-cookies message)
  full-cookies)

(defun git-overleaf--authenticate-with-firefox-cookies (&optional url)
  "Synchronously import Overleaf cookies from Firefox for URL."
  (setq git-overleaf-url (or url (git-overleaf--url)))
  (git-overleaf--save-and-apply-authenticated-cookies
   (git-overleaf-firefox-cookies git-overleaf-url)
   "Imported Overleaf cookies from Firefox for %s"))

(defun git-overleaf--start-authentication-async
    (&optional url on-success)
  "Start authentication for URL in the background.
ON-SUCCESS, when non-nil, is called after cookies are saved and applied."
  (when (and (eq git-overleaf-auth-backend 'webdriver)
             (not (executable-find "geckodriver")))
    (message-box
     "Please install geckodriver to authenticate with Overleaf.")
    (user-error "Required executable `geckodriver' was not found"))
  (setq git-overleaf-url (or url (git-overleaf--url)))
  (git-overleaf--async-start
   (format "Overleaf authentication for %s" (git-overleaf--url-host))
   (lambda ()
     (git-overleaf--authenticate-sync git-overleaf-url))
   :key (format "auth:%s" (git-overleaf--url))
   :on-success
   (lambda (full-cookies)
     (git-overleaf--apply-authenticated-cookies full-cookies nil)
     (git-overleaf--message "Authentication finished for %s"
                                (git-overleaf--cookie-domain))
     (when on-success
       (funcall on-success full-cookies)))))

(defun git-overleaf--authenticate-with-webdriver (&optional url)
  "Synchronously use selenium webdriver to log into URL and obtain cookies.
If URL is nil, use `git-overleaf-url'.  Return the saved full cookie alist."
  (git-overleaf--with-webdriver
   (unless (and (boundp 'git-overleaf-save-cookies)
                git-overleaf-save-cookies)
     (user-error
      "`git-overleaf-save-cookies' needs to be configured"))
   (setq git-overleaf-url (or url (git-overleaf--url)))
   (let ((session (git-overleaf--webdriver-session)))
     (unwind-protect
         ;; Re-authentication should not depend on previously saved cookies.
         ;; Using only the freshly captured cookie avoids failures from stale
         ;; or undecryptable cookie stores.
         (git-overleaf--with-webdriver-direct-connection
           (let ((full-cookies nil))
             (webdriver-service-start (oref session service))
             (git-overleaf--async-register-process
              (oref (oref session service) process))
             (webdriver-session-start session)
             (webdriver-goto-url session (concat (git-overleaf--url) "/login"))
             (git-overleaf--message "Log in using the browser window...")
             (git-overleaf--webdriver-wait-until-appears
              (session "//button[@id='new-project-button-sidebar']"))
             (let* ((project-link-selector
                     (make-instance 'webdriver-by
                                    :strategy "xpath"
                                    :selector "//a[contains(@href, '/project/')]"))
                    (first-project
                     (ignore-errors
                       (webdriver-find-element session project-link-selector)))
                    (first-project-path
                     (and first-project
                          (git-overleaf--webdriver-project-url
                           (webdriver-get-element-attribute
                            session
                            first-project
                            "href"))))
                    (cookies nil))
               (when first-project-path
                 (webdriver-goto-url session first-project-path))
               (setq cookies (webdriver-get-all-cookies session))
               (setf (alist-get (git-overleaf--cookie-domain) full-cookies nil nil #'string=)
                     (list (git-overleaf--webdriver-cookie-string cookies)
                           (git-overleaf--webdriver-cookie-expiry cookies)))
               (git-overleaf--save-and-apply-authenticated-cookies
                full-cookies
                "Saved Overleaf cookies for %s")
               full-cookies)))
       (git-overleaf--with-webdriver-direct-connection
         (when (oref (oref session service) process)
           (git-overleaf--async-unregister-process
            (oref (oref session service) process)))
         (git-overleaf--webdriver-session-stop session))))))

(defun git-overleaf--authenticate-sync (&optional url)
  "Synchronously authenticate to URL using `git-overleaf-auth-backend'."
  (pcase git-overleaf-auth-backend
    ('webdriver
     (git-overleaf--authenticate-with-webdriver url))
    ('firefox-cookies
     (git-overleaf--authenticate-with-firefox-cookies url))
    (_
     (user-error "Unsupported `git-overleaf-auth-backend': %S"
                 git-overleaf-auth-backend))))

;;;###autoload
(defun git-overleaf-authenticate (&optional url)
  "Obtain cookies for URL using `git-overleaf-auth-backend'.
If URL is nil, use `git-overleaf-url'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (git-overleaf--async-enabled-p))
      (git-overleaf--start-authentication-async url)
    (git-overleaf--authenticate-sync url)))


(provide 'git-overleaf-auth)

;;; git-overleaf-auth.el ends here
