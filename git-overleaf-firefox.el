;;; git-overleaf-firefox.el --- Firefox cookie import for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Imports Overleaf cookies from an existing Firefox profile.

;;; Code:

(require 'cl-lib)
(require 'git-overleaf-core)
(require 'sqlite)
(require 'subr-x)

;;;; Customization

(defcustom git-overleaf-firefox-profile nil
  "Firefox profile directory used for importing Overleaf cookies.

When nil, `git-overleaf-authenticate' with the `firefox-cookies'
backend reads Firefox's profiles.ini and uses its default profile."
  :type '(choice
          (directory :tag "Firefox profile directory")
          (const :tag "Auto-detect default profile" nil))
  :group 'git-overleaf)

;;;; Profile discovery

(defun git-overleaf-firefox--profiles-ini-candidates ()
  "Return possible Firefox profiles.ini paths for this system."
  (delq
   nil
   (list
    (expand-file-name "~/Library/Application Support/Firefox/profiles.ini")
    (expand-file-name "~/.mozilla/firefox/profiles.ini")
    (when-let* ((appdata (getenv "APPDATA")))
      (expand-file-name "Mozilla/Firefox/profiles.ini" appdata)))))

(defun git-overleaf-firefox--profiles-ini-file ()
  "Return the readable Firefox profiles.ini file, or nil."
  (cl-find-if #'file-readable-p
              (git-overleaf-firefox--profiles-ini-candidates)))

(defun git-overleaf-firefox--parse-profiles-ini (file)
  "Parse Firefox profiles.ini FILE and return section plists."
  (let ((sections nil)
        (current nil))
    (cl-labels
        ((finish-section ()
           (when current
             (push current sections)
             (setq current nil)))
         (plist-key (key)
           (intern (concat ":" (downcase key)))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (cond
             ((or (string-empty-p line)
                  (string-prefix-p ";" line)
                  (string-prefix-p "#" line)))
             ((string-match "\\`\\[\\([^]]+\\)\\]\\'" line)
              (finish-section)
              (setq current (list :section (match-string 1 line))))
             ((and current
                   (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line))
              (setf (plist-get current
                               (plist-key (string-trim (match-string 1 line))))
                    (string-trim (match-string 2 line))))))
          (forward-line 1)))
      (finish-section))
    (nreverse sections)))

(defun git-overleaf-firefox--profile-section-p (section)
  "Return non-nil if SECTION describes a Firefox profile."
  (string-prefix-p "Profile" (or (plist-get section :section) "")))

(defun git-overleaf-firefox--install-section-p (section)
  "Return non-nil if SECTION describes a Firefox install."
  (string-prefix-p "Install" (or (plist-get section :section) "")))

(defun git-overleaf-firefox--install-default-section (sections)
  "Return a profile section derived from a Firefox install default in SECTIONS.
Firefox's dedicated-profile feature records the active profile in an
\"[InstallXXX]\" section.  Its `Default' entry is a profile path,
relative to the profiles.ini directory."
  (when-let* ((install
               (cl-find-if
                (lambda (section)
                  (and (git-overleaf-firefox--install-section-p section)
                       (plist-get section :default)))
                sections)))
    (list :section (plist-get install :section)
          :path (plist-get install :default)
          :isrelative "1")))

(defun git-overleaf-firefox--legacy-default-section (sections)
  "Return the legacy top-level `Default=1' Firefox profile in SECTIONS."
  (cl-find-if
   (lambda (section)
     (and (git-overleaf-firefox--profile-section-p section)
          (string= (plist-get section :default) "1")))
   sections))

(defun git-overleaf-firefox--default-profile-section (sections)
  "Return the default Firefox profile section from SECTIONS.

Modern Firefox keeps a dedicated profile per install and records it in
an \"[InstallXXX]\" section, which it treats as authoritative.  Prefer
that profile over the legacy top-level `Default=1' profile, which can
point to a stale, empty profile that no longer holds the user's
cookies."
  (or (git-overleaf-firefox--install-default-section sections)
      (git-overleaf-firefox--legacy-default-section sections)))

(defun git-overleaf-firefox--resolve-profile-path (section base-directory)
  "Return the absolute Firefox profile path for SECTION under BASE-DIRECTORY."
  (let ((path (plist-get section :path)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "Firefox profiles.ini default profile has no Path entry"))
    (if (string= (plist-get section :isrelative) "1")
        (expand-file-name path base-directory)
      (expand-file-name path))))

(defun git-overleaf-firefox--profile-directory ()
  "Return the Firefox profile directory used for cookie import."
  (let ((profile
         (if git-overleaf-firefox-profile
             (expand-file-name git-overleaf-firefox-profile)
           (let* ((ini-file
                   (or (git-overleaf-firefox--profiles-ini-file)
                       (user-error
                        "Could not find Firefox profiles.ini.  Set `git-overleaf-firefox-profile' to a Firefox profile directory")))
                  (sections
                   (git-overleaf-firefox--parse-profiles-ini ini-file))
                  (section
                   (or (git-overleaf-firefox--default-profile-section sections)
                       (user-error
                        "Could not determine the default Firefox profile.  Set `git-overleaf-firefox-profile' manually"))))
             (git-overleaf-firefox--resolve-profile-path
              section
              (file-name-directory ini-file))))))
    (unless (file-directory-p profile)
      (user-error "Firefox profile directory does not exist: %s" profile))
    profile))

;;;; Cookie import

(defun git-overleaf-firefox--ensure-sqlite ()
  "Signal unless Emacs can open SQLite databases."
  (unless (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))
    (user-error
     "`git-overleaf-auth-backend' is `firefox-cookies', but this Emacs was built without SQLite support")))

(defun git-overleaf-firefox--copy-cookie-store (profile)
  "Copy Firefox cookie SQLite files from PROFILE and return (DIR DB-FILE)."
  (let* ((source (expand-file-name "cookies.sqlite" profile))
         (temp-dir (make-temp-file "overleaf-firefox-cookies." t))
         (target (expand-file-name "cookies.sqlite" temp-dir)))
    (unless (file-readable-p source)
      (delete-directory temp-dir t)
      (user-error "Firefox profile %s does not contain a readable cookies.sqlite file" profile))
    (copy-file source target t)
    (dolist (suffix '("-wal" "-shm"))
      (let ((sidecar (concat source suffix)))
        (when (file-readable-p sidecar)
          (copy-file sidecar (concat target suffix) t))))
    (list temp-dir target)))

(defun git-overleaf-firefox--cookie-query (hosts)
  "Return a Firefox cookie SQL query matching HOSTS."
  (format
   "select name, value, host, path, expiry from moz_cookies where host in (%s)"
   (string-join (make-list (length hosts) "?") ", ")))

(defun git-overleaf-firefox--cookie-rows (db)
  "Return Firefox cookie rows from DB matching the current Overleaf host."
  (let ((hosts (git-overleaf--cookie-key-candidates)))
    (sqlite-select
     db
     (git-overleaf-firefox--cookie-query hosts)
     hosts)))

(defun git-overleaf-firefox--cookie-expired-p (row now)
  "Return non-nil if Firefox cookie ROW is expired at NOW."
  (let ((expiry (nth 4 row)))
    (and (integerp expiry)
         (> expiry 0)
         (<= expiry now))))

(defun git-overleaf-firefox--session-cookie-p (row)
  "Return non-nil if Firefox cookie ROW is an Overleaf session cookie."
  (let ((name (nth 0 row)))
    (and (stringp name)
         (string-match-p git-overleaf-auth-session-cookie-regexp name))))

(defun git-overleaf-firefox--cookie-header (rows)
  "Return an HTTP Cookie header string from Firefox cookie ROWS."
  (let ((pairs
         (cl-loop
          for row in rows
          for name = (nth 0 row)
          for value = (nth 1 row)
          when (and (stringp name)
                    (not (string-empty-p name))
                    (stringp value))
          collect (format "%s=%s" name value))))
    (unless pairs
      (user-error "No usable Overleaf cookies found in Firefox profile"))
    (string-join pairs "; ")))

(defun git-overleaf-firefox--session-expiry (rows)
  "Return the earliest positive session-cookie expiry in ROWS."
  (let ((expiries
         (cl-loop
          for row in rows
          for expiry = (nth 4 row)
          when (and (git-overleaf-firefox--session-cookie-p row)
                    (integerp expiry)
                    (> expiry 0))
          collect expiry)))
    (when expiries
      (apply #'min expiries))))

(defun git-overleaf-firefox--format-time (seconds)
  "Return a user-facing timestamp for SECONDS since the epoch."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time seconds)))

(defun git-overleaf-firefox--full-cookies-from-rows (rows profile)
  "Return normalized Overleaf full cookies from Firefox ROWS in PROFILE."
  (let* ((now (time-convert nil 'integer))
         (session-rows
          (cl-remove-if-not #'git-overleaf-firefox--session-cookie-p rows))
         (valid-rows
          (cl-remove-if
           (lambda (row)
             (git-overleaf-firefox--cookie-expired-p row now))
           rows))
         (valid-session-rows
          (cl-remove-if
           (lambda (row)
             (git-overleaf-firefox--cookie-expired-p row now))
           session-rows)))
    (cond
     ((null rows)
      (user-error
       "No Overleaf cookies found in Firefox profile %s.  Log in to %s in Firefox, then run `git-overleaf-authenticate' again"
       profile
       (git-overleaf--url)))
     ((null session-rows)
      (user-error
       "Found Overleaf cookies in Firefox profile %s, but no authenticated session cookie.  Log in to %s in Firefox, then run `git-overleaf-authenticate' again"
       profile
       (git-overleaf--url)))
     ((null valid-session-rows)
      (let ((expiry (git-overleaf-firefox--session-expiry session-rows)))
        (user-error
         "Firefox Overleaf session cookies in profile %s are expired%s.  Log in to %s in Firefox, then run `git-overleaf-authenticate' again"
         profile
         (if expiry
             (format " since %s"
                     (git-overleaf-firefox--format-time expiry))
           "")
         (git-overleaf--url))))
     ((null valid-rows)
      (user-error
       "Firefox Overleaf cookies in profile %s are expired.  Log in to %s in Firefox, then run `git-overleaf-authenticate' again"
       profile
       (git-overleaf--url)))
     (t
      (let ((expiry (git-overleaf-firefox--session-expiry
                     valid-session-rows)))
        (list
         (list (git-overleaf--cookie-domain)
               (git-overleaf-firefox--cookie-header valid-rows)
               expiry)))))))

;;;###autoload
(defun git-overleaf-firefox-cookies (&optional url)
  "Return Overleaf cookies imported from an existing Firefox profile.
If URL is nil, use `git-overleaf-url'.  The Firefox profile must
already contain a valid Overleaf login session."
  (let ((git-overleaf-url (or url (git-overleaf--url)))
        (profile nil)
        (copy nil)
        (db nil))
    (git-overleaf-firefox--ensure-sqlite)
    (setq profile (git-overleaf-firefox--profile-directory))
    (unwind-protect
        (progn
          (setq copy (git-overleaf-firefox--copy-cookie-store profile))
          (setq db (sqlite-open (cadr copy)))
          (unless db
            (user-error "Could not open Firefox cookies database copied from %s" profile))
          (git-overleaf-firefox--full-cookies-from-rows
           (git-overleaf-firefox--cookie-rows db)
           profile))
      (when db
        (ignore-errors (sqlite-close db)))
      (when copy
        (ignore-errors (delete-directory (car copy) t))))))

(provide 'git-overleaf-firefox)

;;; git-overleaf-firefox.el ends here
