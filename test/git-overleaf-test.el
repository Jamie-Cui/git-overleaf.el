;;; git-overleaf-test.el --- Tests for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-core)
(require 'git-overleaf-http)
(require 'git-overleaf-sync)
(require 'git-overleaf-firefox)

(defmacro git-overleaf-test--with-url (url &rest body)
  "Run BODY with `git-overleaf-url' bound to URL."
  (declare (indent 1) (debug t))
  `(let ((git-overleaf-url ,url))
     ,@body))

(defmacro git-overleaf-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "git-overleaf-test." t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(ert-deftest git-overleaf-test-url-helpers ()
  (git-overleaf-test--with-url " https://Example.Overleaf.test/ "
    (should (equal (git-overleaf--url)
                   "https://Example.Overleaf.test"))
    (should (equal (git-overleaf--url-host)
                   "example.overleaf.test"))
    (should (equal (git-overleaf--cookie-domain)
                   "example.overleaf.test"))
    (should (equal (git-overleaf--project-page-url "abc123")
                   "https://Example.Overleaf.test/project/abc123")))
  (git-overleaf-test--with-url "not a url"
    (should-error (git-overleaf--url-host) :type 'user-error)))

(ert-deftest git-overleaf-test-cookie-key-candidates ()
  (git-overleaf-test--with-url "https://www.overleaf.com"
    (should (equal (git-overleaf--cookie-key-candidates)
                   '("www.overleaf.com"
                     ".www.overleaf.com"
                     "overleaf.com"
                     ".overleaf.com"))))
  (git-overleaf-test--with-url "https://overleaf.example"
    (should (equal (git-overleaf--cookie-key-candidates)
                   '("overleaf.example" ".overleaf.example")))))

(ert-deftest git-overleaf-test-sanitize-name ()
  (should (equal (git-overleaf--sanitize-name "  My Project v2.tex!! ")
                 "my-project-v2-tex"))
  (should (equal (git-overleaf--sanitize-name "---") "")))

(ert-deftest git-overleaf-test-redact-command-data ()
  (should (equal (git-overleaf--redact-sensitive-argument
                  "Cookie: session=secret")
                 "Cookie: <redacted>"))
  (should (equal (git-overleaf--redact-command-args
                  '("-H" "Cookie: session=secret"
                    "--header" "X-Csrf-Token: token"
                    "--cookie" "raw-cookie-secret"
                    "-H" "Accept: application/json"
                    "https://example.test"))
                 '("-H" "Cookie: <redacted>"
                   "--header" "X-Csrf-Token: <redacted>"
                   "--cookie" "<redacted>"
                   "-H" "Accept: application/json"
                   "https://example.test")))
  (let ((message (git-overleaf--command-error-message
                  "curl"
                  '("-H" "Cookie: session=secret" "https://example.test")
                  "X-Csrf-Token: secret-token-value\nbody")))
    (should (string-match-p "Cookie: <redacted>" message))
    (should (string-match-p "X-Csrf-Token: <redacted>" message))
    (should-not (string-match-p "session=secret\\|secret-token-value"
                                message))))

(ert-deftest git-overleaf-test-normalize-cookies ()
  (git-overleaf-test--with-url "https://www.overleaf.com"
    (should (equal (git-overleaf--normalize-cookie-entry
                    '("WWW.OVERLEAF.COM" "sid=1" 123))
                   '("www.overleaf.com" "sid=1" 123)))
    (should (equal (git-overleaf--normalize-cookie-entry
                    '("www.overleaf.com" "sid=1"))
                   '("www.overleaf.com" "sid=1" nil)))
    (should-error (git-overleaf--normalize-cookie-entry
                   '("www.overleaf.com" 42))
                  :type 'error)
    (should (equal (git-overleaf--normalize-full-cookies "sid=1")
                   '(("www.overleaf.com" "sid=1" nil))))
    (should (equal (git-overleaf--normalize-full-cookies
                    "((\".www.overleaf.com\" \"sid=1\" 999))")
                   '((".www.overleaf.com" "sid=1" 999))))
    (should (equal (git-overleaf--normalize-full-cookies "  ")
                   nil))
    (should-error (git-overleaf--normalize-full-cookies
                   "((\"broken\" 42))")
                  :type 'error)
    (should-error (git-overleaf--normalize-full-cookies 42)
                  :type 'error)))

(ert-deftest git-overleaf-test-cookie-state ()
  (let* ((now (time-convert nil 'integer))
         (git-overleaf--current-cookies nil)
         (git-overleaf-cookies
          `(("overleaf.com" "sid=valid" ,(+ now 3600)))))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (git-overleaf--cookie-state) :status)
                     'valid))
      (should (equal (git-overleaf--get-cookies) "sid=valid"))))
  (let* ((now (time-convert nil 'integer))
         (git-overleaf--current-cookies nil)
         (git-overleaf-cookies
          `(("www.overleaf.com" "sid=expired" ,(- now 3600)))))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (git-overleaf--cookie-state) :status)
                     'expired))
      (should-error (git-overleaf--get-cookies) :type 'user-error)))
  (let ((git-overleaf--current-cookies nil)
        (git-overleaf-cookies nil))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (git-overleaf--cookie-state) :status)
                     'missing))
      (should-error (git-overleaf--get-cookies) :type 'user-error)
      (should (string-match-p
               "not set locally"
               (git-overleaf--authentication-needed-reason))))))

(ert-deftest git-overleaf-test-curl-helper-arguments ()
  (should (equal (git-overleaf--format-curl-headers
                  '(("A" . "1") ("B" . "2")))
                 '("A: 1" "B: 2")))
  (let ((git-overleaf--curl-connect-timeout 2)
        (git-overleaf--curl-max-time nil))
    (should (equal (git-overleaf--curl-timeout-args)
                   '("--connect-timeout" "2"))))
  (let ((git-overleaf--curl-connect-timeout nil)
        (git-overleaf--curl-download-max-time 10)
        (git-overleaf--curl-download-speed-limit 256)
        (git-overleaf--curl-download-speed-time 5))
    (should (equal (git-overleaf--curl-download-timeout-args)
                   '("--max-time" "10"
                     "--speed-limit" "256"
                     "--speed-time" "5")))
    (should (member "--silent" (git-overleaf--curl-download-base-args)))
    (should-not (member "--silent"
                        (git-overleaf--curl-download-base-args t)))
    (should (member "--progress-bar"
                    (git-overleaf--curl-download-base-args t))))
  (should (equal (git-overleaf--curl-download-args
                  "https://example.test/project/id/download/zip"
                  "/tmp/project.zip"
                  '("Cookie: <redacted>")
                  t)
                 '("--fail" "--show-error" "--location" "--progress-bar"
                   "--connect-timeout" "10"
                   "--speed-limit" "1024"
                   "--speed-time" "30"
                   "-H" "Cookie: <redacted>"
                   "--output" "/tmp/project.zip"
                   "https://example.test/project/id/download/zip")))
  (should (equal (git-overleaf--curl-download-progress-percent
                  "\r######## 12.3%")
                 12))
  (should (equal (git-overleaf--curl-download-progress-percent
                  "\r#### 9.9%\r######################################################################## 100.0%")
                 100))
  (should-not (git-overleaf--curl-download-progress-percent "no progress"))
  (let ((result (make-git-overleaf--command-result
                 :status 22
                 :output "curl: returned error: 403")))
    (should (git-overleaf--curl-403-p result)))
  (let ((result (make-git-overleaf--command-result
                 :status 0
                 :output "")))
    (should-not (git-overleaf--curl-403-p result))))

(ert-deftest git-overleaf-test-csrf-cache-helpers ()
  (let ((git-overleaf--csrf-cache (make-hash-table :test #'equal)))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (let ((key-a (git-overleaf--csrf-cache-key "project-a" "cookie-a"))
            (key-b (git-overleaf--csrf-cache-key "project-b" "cookie-b")))
        (puthash key-a "token-a" git-overleaf--csrf-cache)
        (puthash key-b "token-b" git-overleaf--csrf-cache)
        (git-overleaf--clear-csrf-cache "project-a")
        (should-not (gethash key-a git-overleaf--csrf-cache))
        (should (equal (gethash key-b git-overleaf--csrf-cache)
                       "token-b"))
        (git-overleaf--clear-csrf-cache)
        (should (= (hash-table-count git-overleaf--csrf-cache) 0))))))

(ert-deftest git-overleaf-test-sync-metadata-path-validation ()
  (let ((git-overleaf-sync-metadata-enabled t)
        (git-overleaf-sync-metadata-file ".git-overleaf-sync.json"))
    (should (equal (git-overleaf--sync-metadata-relative-path)
                   ".git-overleaf-sync.json"))
    (should (git-overleaf--sync-metadata-path-p
             ".git-overleaf-sync.json")))
  (let ((git-overleaf-sync-metadata-enabled nil)
        (git-overleaf-sync-metadata-file ".git-overleaf-sync.json"))
    (should-not (git-overleaf--sync-metadata-relative-path))
    (should-not (git-overleaf--sync-metadata-path-p
                 ".git-overleaf-sync.json")))
  (dolist (path '("" "/absolute" "dir/file" ".."))
    (let ((git-overleaf-sync-metadata-enabled t)
          (git-overleaf-sync-metadata-file path))
      (should-error (git-overleaf--sync-metadata-relative-path)
                    :type 'user-error))))

(ert-deftest git-overleaf-test-sync-metadata-file-reading ()
  (let ((git-overleaf-log-echo nil))
    (git-overleaf-test--with-temp-dir dir
      (let ((valid (expand-file-name "valid.json" dir))
            (invalid (expand-file-name "invalid.json" dir)))
        (with-temp-file valid
          (insert "{\"schema\":1,\"localCommit\":\"abc\"}"))
        (with-temp-file invalid
          (insert "{not-json"))
        (should (equal (git-overleaf--read-sync-metadata-file valid)
                       '(:schema 1 :localCommit "abc")))
        (should-not (git-overleaf--read-sync-metadata-file invalid))))))

(ert-deftest git-overleaf-test-git-object-id-p ()
  (should (git-overleaf--git-object-id-p
           "0123456789abcdef0123456789abcdef01234567"))
  (should (git-overleaf--git-object-id-p
           "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
  (should-not (git-overleaf--git-object-id-p "short"))
  (should-not (git-overleaf--git-object-id-p "not-a-sha")))

(ert-deftest git-overleaf-test-classify-sync-state ()
  (should (eq (git-overleaf--classify-sync-state "a" "a" "a")
              'in-sync))
  (should (eq (git-overleaf--classify-sync-state "a" "b" "b")
              'head-matches-remote))
  (should (eq (git-overleaf--classify-sync-state "a" "b" "a")
              'remote-matches-base))
  (should (eq (git-overleaf--classify-sync-state "a" "a" "b")
              'head-matches-base))
  (should (eq (git-overleaf--classify-sync-state "a" "b" "c")
              'diverged)))

(ert-deftest git-overleaf-test-sharejs-text-op ()
  (should-not (git-overleaf--sharejs-text-op "same" "same"))
  (should (equal (git-overleaf--sharejs-text-op "ab" "aXb")
                 [(:i "X" :p 1)]))
  (should (equal (git-overleaf--sharejs-text-op "aXb" "ab")
                 [(:d "X" :p 1)]))
  (should (equal (git-overleaf--sharejs-text-op "ab" "aXYb")
                 [(:i "XY" :p 1)]))
  (should (equal (git-overleaf--sharejs-text-op "aXb" "aYb")
                 [(:d "X" :p 1) (:i "Y" :p 1)]))
  (should (equal (git-overleaf--sharejs-text-op
                  "a\U0001F600b"
                  "a\U0001F600Xb")
                 [(:i "X" :p 3)])))

(ert-deftest git-overleaf-test-socketio-doc-line-decoding ()
  (should (equal (git-overleaf--decode-socketio-doc-line "hello" "doc")
                 "hello"))
  (should-error (git-overleaf--decode-socketio-doc-line
                 (string #x100)
                 "doc")
                :type 'user-error)
  (should-error (git-overleaf--decode-socketio-doc-line
                 (string #xff)
                 "doc")
                :type 'user-error))

(ert-deftest git-overleaf-test-socketio-event-helpers ()
  (should (equal (git-overleaf--socketio-error-message nil) nil))
  (should (equal (git-overleaf--socketio-error-message "boom") "boom"))
  (should (equal (git-overleaf--socketio-error-message '(:message "boom"))
                 "boom"))
  (should (equal (git-overleaf--doc-update-event-doc-id
                  '(:name "otUpdateApplied" :args ((:doc "doc-a"))))
                 "doc-a"))
  (should (equal (git-overleaf--doc-update-event-doc-id
                  '(:name "otUpdateError" :args (nil (:doc_id "doc-b"))))
                 "doc-b"))
  (should (git-overleaf--source-doc-update-applied-p
           '(:name "otUpdateApplied" :args ((:doc "doc-a")))
           "doc-a"))
  (should-not (git-overleaf--source-doc-update-applied-p
               '(:name "otUpdateApplied" :args ((:doc "doc-a" :op [])))
               "doc-a")))

(ert-deftest git-overleaf-test-remote-doc-state-validation ()
  (cl-letf (((symbol-function 'git-overleaf--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line1" "line2") 7 nil nil "sharejs-text-ot"))))
    (should (equal (git-overleaf--remote-doc-state nil "doc")
                   '(:version 7 :text "line1\nline2"))))
  (cl-letf (((symbol-function 'git-overleaf--socketio-emit)
             (lambda (&rest _args)
               '("join failed" nil nil nil nil nil))))
    (should-error (git-overleaf--remote-doc-state nil "doc")
                  :type 'user-error))
  (cl-letf (((symbol-function 'git-overleaf--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line") 7 nil nil "other-type"))))
    (should-error (git-overleaf--remote-doc-state nil "doc")
                  :type 'user-error))
  (cl-letf (((symbol-function 'git-overleaf--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line") nil nil nil "sharejs-text-ot"))))
    (should-error (git-overleaf--remote-doc-state nil "doc")
                  :type 'user-error)))

(ert-deftest git-overleaf-test-entity-table-helpers ()
  (let* ((root '(:name "rootFolder"
                 :_id "root"
                 :docs ((:name "main.tex" :_id "doc-1"))
                 :fileRefs ((:name "figure.png" :_id "file-1"))
                 :folders ((:name "chapters"
                             :_id "folder-1"
                             :docs ((:name "intro.tex" :_id "doc-2"))))))
         (table (git-overleaf--build-entity-table root)))
    (should (equal (git-overleaf--entity-type (gethash "" table))
                   'folder))
    (should (equal (git-overleaf--entity-id (gethash "main.tex" table))
                   "doc-1"))
    (should (equal (git-overleaf--entity-type (gethash "figure.png" table))
                   'file))
    (should (equal (git-overleaf--entity-parent-id
                    (gethash "chapters/intro.tex" table))
                   "folder-1"))
    (git-overleaf--forget-entry table "chapters")
    (should-not (gethash "chapters" table))
    (should-not (gethash "chapters/intro.tex" table))
    (should (gethash "main.tex" table))))

(ert-deftest git-overleaf-test-firefox-profiles-ini ()
  ;; When an [Install] section is present, its default profile is
  ;; authoritative and wins over the legacy top-level `Default=1', which
  ;; may point to a stale, empty profile.
  (git-overleaf-test--with-temp-dir dir
    (let ((ini (expand-file-name "profiles.ini" dir)))
      (with-temp-file ini
        (insert "[Profile0]\n")
        (insert "Name=default\n")
        (insert "IsRelative=1\n")
        (insert "Path=Profiles/default-release\n")
        (insert "Default=1\n")
        (insert "\n[InstallABC]\n")
        (insert "Default=Profiles/install-default\n"))
      (let* ((sections (git-overleaf-firefox--parse-profiles-ini ini))
             (default (git-overleaf-firefox--default-profile-section
                        sections)))
        (should (equal (plist-get (car sections) :section) "Profile0"))
        (should (equal (plist-get default :path)
                       "Profiles/install-default"))
        (should (equal (git-overleaf-firefox--resolve-profile-path
                        default
                        dir)
                       (expand-file-name "Profiles/install-default" dir))))))
  ;; With no [Install] section, fall back to the legacy `Default=1' profile.
  (git-overleaf-test--with-temp-dir dir
    (let ((ini (expand-file-name "profiles.ini" dir)))
      (with-temp-file ini
        (insert "[Profile0]\n")
        (insert "Name=default\n")
        (insert "IsRelative=1\n")
        (insert "Path=Profiles/default-release\n")
        (insert "Default=1\n"))
      (let* ((sections (git-overleaf-firefox--parse-profiles-ini ini))
             (default (git-overleaf-firefox--default-profile-section
                        sections)))
        (should (equal (plist-get default :path)
                       "Profiles/default-release")))))
  (let ((install-only '((:section "InstallABC"
                        :default "Profiles/install-default"))))
    (should (equal (plist-get
                    (git-overleaf-firefox--default-profile-section
                     install-only)
                    :path)
                   "Profiles/install-default")))
  (should-error (git-overleaf-firefox--resolve-profile-path
                 '(:section "Profile0")
                 temporary-file-directory)
                :type 'user-error))

(ert-deftest git-overleaf-test-firefox-cookie-rows ()
  (should (equal (git-overleaf-firefox--cookie-query '("a" "b" "c"))
                 "select name, value, host, path, expiry from moz_cookies where host in (?, ?, ?)"))
  (should (git-overleaf-firefox--cookie-expired-p
           '("connect.sid" "value" ".overleaf.com" "/" 10)
           10))
  (should-not (git-overleaf-firefox--cookie-expired-p
               '("connect.sid" "value" ".overleaf.com" "/" 0)
               10))
  (should (git-overleaf-firefox--session-cookie-p
           '("connect.sid" "value" ".overleaf.com" "/" 100)))
  (should-not (git-overleaf-firefox--session-cookie-p
               '("GCLB" "value" ".overleaf.com" "/" 100)))
  (should (equal (git-overleaf-firefox--cookie-header
                  '(("connect.sid" "session" ".overleaf.com" "/" 100)
                    ("GCLB" "lb" ".overleaf.com" "/" 100)
                    (nil "ignored" ".overleaf.com" "/" 100)))
                 "connect.sid=session; GCLB=lb"))
  (should-error (git-overleaf-firefox--cookie-header
                 '((nil "ignored" ".overleaf.com" "/" 100)))
                :type 'user-error)
  (should (equal (git-overleaf-firefox--session-expiry
                  '(("connect.sid" "a" ".overleaf.com" "/" 50)
                    ("overleaf_session2" "b" ".overleaf.com" "/" 40)
                    ("GCLB" "lb" ".overleaf.com" "/" 10)))
                 40)))

(ert-deftest git-overleaf-test-firefox-full-cookies-from-rows ()
  (let* ((now (time-convert nil 'integer))
         (rows `(("connect.sid" "session" ".www.overleaf.com" "/" ,(+ now 100))
                 ("GCLB" "lb" ".www.overleaf.com" "/" ,(+ now 50)))))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (should (equal (git-overleaf-firefox--full-cookies-from-rows
                      rows
                      "/tmp/profile")
                     `(("www.overleaf.com"
                        "connect.sid=session; GCLB=lb"
                        ,(+ now 100)))))))
  (let ((now (time-convert nil 'integer)))
    (git-overleaf-test--with-url "https://www.overleaf.com"
      (should-error (git-overleaf-firefox--full-cookies-from-rows
                     nil
                     "/tmp/profile")
                    :type 'user-error)
      (should-error (git-overleaf-firefox--full-cookies-from-rows
                     `(("GCLB" "lb" ".www.overleaf.com" "/" ,(+ now 100)))
                     "/tmp/profile")
                    :type 'user-error)
      (should-error (git-overleaf-firefox--full-cookies-from-rows
                     `(("connect.sid" "session" ".www.overleaf.com" "/"
                        ,(- now 100)))
                     "/tmp/profile")
                    :type 'user-error))))

;;; git-overleaf-test.el ends here
