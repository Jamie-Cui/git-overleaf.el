;;; git-overleaf-http-auth-test.el --- HTTP/auth boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'url-util)
(require 'git-overleaf-auth)
(require 'git-overleaf-core)
(require 'git-overleaf-http)

(ert-deftest git-overleaf-http-auth-test-csrf-token-cache-and-failure ()
  (let ((git-overleaf-url "https://example.overleaf.test")
        (git-overleaf--csrf-cache (make-hash-table :test #'equal))
        (git-overleaf-cookies "sid=1")
        (git-overleaf--current-cookies nil)
        (requests nil))
    (cl-letf (((symbol-function 'git-overleaf--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "<html><meta name=\"ol-csrfToken\" content=\"token-1\"></html>")))
      (should (equal (git-overleaf--csrf-token "project-id")
                     "token-1"))
      (should (equal (git-overleaf--csrf-token "project-id")
                     "token-1"))
      (should (= (length requests) 1))
      (should (equal (caar requests) "GET")))
    (cl-letf (((symbol-function 'git-overleaf--curl-request)
               (lambda (&rest _args) "<html></html>")))
      (should-error (git-overleaf--csrf-token "other-project")
                    :type 'user-error))))

(ert-deftest git-overleaf-http-auth-test-socket-cookies-appends-gclb ()
  (let ((git-overleaf-url "https://example.overleaf.test")
        (git-overleaf-cookies "sid=1")
        (git-overleaf--current-cookies nil))
    (cl-letf (((symbol-function 'git-overleaf--curl-header-text)
               (lambda (&rest _args)
                 "HTTP/1.1 200 OK\r\nSet-Cookie: GCLB=abc; Path=/\r\n")))
      (should (equal (git-overleaf--socket-cookies)
                     "sid=1; GCLB=abc")))
    (cl-letf (((symbol-function 'git-overleaf--curl-header-text)
               (lambda (&rest _args) "HTTP/1.1 200 OK\r\n")))
      (should (equal (git-overleaf--socket-cookies) "sid=1")))))

(ert-deftest git-overleaf-http-auth-test-curl-upload-retries-after-403 ()
  (let ((git-overleaf-url "https://example.overleaf.test")
        (git-overleaf-cookies "sid=1")
        (git-overleaf--current-cookies nil)
        (git-overleaf--csrf-cache (make-hash-table :test #'equal))
        (git-overleaf-curl-executable "curl")
        (runs nil)
        (warnings nil))
    (cl-letf (((symbol-function 'git-overleaf--csrf-token)
               (lambda (_project-id &optional _refresh) "csrf-token"))
              ((symbol-function 'git-overleaf--clear-csrf-cache)
               (lambda (&optional project-id)
                 (push (list :clear-csrf project-id) warnings)))
              ((symbol-function 'git-overleaf--warn)
               (lambda (&rest args)
                 (push (cons :warn args) warnings)))
              ((symbol-function 'git-overleaf--ensure-executable)
               (lambda (program) program))
              ((symbol-function 'git-overleaf--run)
               (lambda (program args &optional _directory _env noerror)
                 (push (list program args noerror) runs)
                 (if (= (length runs) 1)
                     (make-git-overleaf--command-result
                      :status 22
                      :output "curl: returned error: 403")
                   (make-git-overleaf--command-result
                    :status 0
                    :output "{\"entity_id\":\"entity-1\",\"entity_type\":\"doc\"}")))))
      (should (equal (git-overleaf--curl-upload-file
                      "project-id"
                      "folder-id"
                      "main.tex"
                      "/tmp/main.tex")
                     '(:entity_id "entity-1" :entity_type "doc")))
      (should (= (length runs) 2))
      (should (member '(:clear-csrf "project-id") warnings)))))

(ert-deftest git-overleaf-http-auth-test-curl-upload-redacts-final-error ()
  (let ((git-overleaf-url "https://example.overleaf.test")
        (git-overleaf-cookies "sid=secret")
        (git-overleaf--current-cookies nil)
        (git-overleaf-curl-executable "curl"))
    (cl-letf (((symbol-function 'git-overleaf--csrf-token)
               (lambda (&rest _args) "csrf-secret"))
              ((symbol-function 'git-overleaf--ensure-executable)
               (lambda (program) program))
              ((symbol-function 'git-overleaf--run)
               (lambda (_program _args &optional _directory _env _noerror)
                 (make-git-overleaf--command-result
                  :status 22
                  :output "Cookie: sid=secret\nX-Csrf-Token: csrf-secret"))))
      (let ((err (should-error
                  (git-overleaf--curl-upload-file
                   "project-id"
                   "folder-id"
                   "main.tex"
                   "/tmp/main.tex")
                  :type 'error)))
        (should-not (string-match-p "sid=secret\\|csrf-secret"
                                    (error-message-string err)))
        (should (string-match-p "<redacted>"
                                (error-message-string err)))))))

(ert-deftest git-overleaf-http-auth-test-project-list-parses-prefetched-blob ()
  (let* ((git-overleaf-url "https://example.overleaf.test")
         (git-overleaf-cookies "sid=1")
         (git-overleaf--current-cookies nil)
         (json "{\"projects\":[{\"id\":\"p1\",\"name\":\"One\"}]}")
         (encoded (url-hexify-string json))
         (html (format "<input name=\"ol-prefetchedProjectsBlob\" content=\"%s\">"
                       encoded)))
    (cl-letf (((symbol-function 'git-overleaf--curl-request)
               (lambda (&rest _args) html)))
      (should (equal (git-overleaf-list)
                     '((:id "p1" :name "One")))))
    (cl-letf (((symbol-function 'git-overleaf--curl-request)
               (lambda (&rest _args) "<html></html>")))
      (should-error (git-overleaf-list)
                    :type 'user-error))))

(ert-deftest git-overleaf-http-auth-test-create-folder-and-delete-entity ()
  (let ((requests nil))
    (cl-letf (((symbol-function 'git-overleaf--url)
               (lambda () "https://example.overleaf.test"))
              ((symbol-function 'git-overleaf--project-headers)
               (lambda (&rest _args) '(("Cookie" . "sid=1"))))
              ((symbol-function 'git-overleaf--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "{\"_id\":\"folder-id\",\"name\":\"New\"}")))
      (should (equal (git-overleaf--create-folder
                      "project-id"
                      "root"
                      "New")
                     '(:_id "folder-id" :name "New")))
      (let ((request (car requests)))
        (should (equal (nth 0 request) "POST"))
        (should (equal (nth 1 request)
                       "https://example.overleaf.test/project/project-id/folder"))
        (should (string-match-p "parent_folder_id" (nth 3 request))))))
  (let ((requests nil))
    (cl-letf (((symbol-function 'git-overleaf--url)
               (lambda () "https://example.overleaf.test"))
              ((symbol-function 'git-overleaf--project-headers)
               (lambda (&rest _args) '(("Cookie" . "sid=1"))))
              ((symbol-function 'git-overleaf--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "")))
      (git-overleaf--delete-entity
       "project-id"
       (make-git-overleaf--entity
        :id "doc-id"
        :type 'doc))
      (should (equal (nth 0 (car requests)) "DELETE"))
      (should (equal (nth 1 (car requests))
                     "https://example.overleaf.test/project/project-id/doc/doc-id"))
      (should-error
       (git-overleaf--delete-entity
        "project-id"
        (make-git-overleaf--entity :id "bad" :type 'unknown))
       :type 'user-error))))

(ert-deftest git-overleaf-http-auth-test-auth-cookie-apply-and-save ()
  (let ((git-overleaf-url "https://www.overleaf.com")
        (git-overleaf--current-cookies nil)
        (git-overleaf--csrf-cache (make-hash-table :test #'equal))
        (saved nil)
        (messages nil))
    (puthash "x" "csrf" git-overleaf--csrf-cache)
    (cl-letf (((symbol-function 'git-overleaf--message)
               (lambda (&rest args) (push args messages))))
      (git-overleaf--apply-authenticated-cookies
       '(("www.overleaf.com" "sid=1" nil))
       "Saved cookies for %s")
      (should (= (hash-table-count git-overleaf--csrf-cache) 0))
      (should (equal git-overleaf--current-cookies
                     '(("www.overleaf.com" "sid=1" nil))))
      (should (equal (car messages)
                     '("Saved cookies for %s" "www.overleaf.com"))))
    (let ((git-overleaf-save-cookies
           (lambda (value) (setq saved value))))
      (should (equal (git-overleaf--save-and-apply-authenticated-cookies
                      '(("www.overleaf.com" "sid=2" nil))
                      nil)
                     '(("www.overleaf.com" "sid=2" nil))))
      (should (equal saved "((\"www.overleaf.com\" \"sid=2\" nil))"))))
  (let ((git-overleaf-save-cookies nil))
    (should-error
     (git-overleaf--save-and-apply-authenticated-cookies
      '(("www.overleaf.com" "sid=1" nil))
      nil)
     :type 'user-error)))

(ert-deftest git-overleaf-http-auth-test-authenticate-sync-selects-backend ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'git-overleaf--authenticate-with-webdriver)
               (lambda (&optional url) (push (list 'webdriver url) calls)))
              ((symbol-function 'git-overleaf--authenticate-with-firefox-cookies)
               (lambda (&optional url) (push (list 'firefox url) calls))))
      (let ((git-overleaf-auth-backend 'webdriver))
        (git-overleaf--authenticate-sync "https://one.test"))
      (let ((git-overleaf-auth-backend 'firefox-cookies))
        (git-overleaf--authenticate-sync "https://two.test"))
      (should (equal (nreverse calls)
                     '((webdriver "https://one.test")
                       (firefox "https://two.test")))))
    (let ((git-overleaf-auth-backend 'unknown))
      (should-error (git-overleaf--authenticate-sync)
                    :type 'user-error))))

(ert-deftest git-overleaf-http-auth-test-webdriver-cookie-helpers ()
  (let* ((cookies (vector '((name . "connect.sid")
                            (value . "session")
                            (expiry . 100))
                          '((name . "GCLB")
                            (value . "lb")
                            (expiry . 50)))))
    (should (equal (git-overleaf--webdriver-cookie-string cookies)
                   "connect.sid=session; GCLB=lb"))
    (should (equal (git-overleaf--webdriver-cookie-expiry cookies)
                   100)))
  (should-error (git-overleaf--webdriver-cookie-string [])
                :type 'user-error)
  (let ((git-overleaf-url "https://example.overleaf.test"))
    (should (equal (git-overleaf--webdriver-project-url "/project/abc")
                   "https://example.overleaf.test/project/abc"))
    (should (equal (git-overleaf--webdriver-project-url
                    "https://other.test/project/abc")
                   "https://other.test/project/abc"))))

;;; git-overleaf-http-auth-test.el ends here
