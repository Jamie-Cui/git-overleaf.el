;;; git-overleaf-http.el --- HTTP and remote tree helpers for git-overleaf -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; URL: https://github.com/Jamie-Cui/git-overleaf
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overleaf HTTP requests, project discovery, downloads, remote tree
;; parsing, and real-time document updates.

;;; Code:

(require 'json)
(require 'mm-url)
(require 'websocket)
(require 'git-overleaf-core)

(cl-defstruct git-overleaf--socketio-client
  "Minimal Socket.IO 0.9 client state."
  websocket
  done
  failure
  next-ack-id
  acks
  events)

;;;; HTTP helpers

(defun git-overleaf--format-curl-headers (alist)
  "Format header ALIST into a list of \"Key: Value\" strings for curl."
  (mapcar (lambda (pair) (format "%s: %s" (car pair) (cdr pair))) alist))

(defun git-overleaf--base-headers (&optional referer extra-headers)
  "Return basic authenticated headers with REFERER and EXTRA-HEADERS."
  (append
   `(("Cookie" . ,(git-overleaf--get-cookies))
     ("Origin" . ,(git-overleaf--url))
     ("Referer" . ,(or referer (git-overleaf--url))))
   extra-headers))

(defun git-overleaf--project-headers (&optional project-id extra-headers)
  "Return mutation headers for PROJECT-ID with EXTRA-HEADERS appended."
  (append
   (git-overleaf--base-headers
    (and project-id (git-overleaf--project-page-url project-id)))
   (when project-id
     `(("Accept" . "application/json")
       ("Cache-Control" . "no-cache")
       ("x-csrf-token" . ,(git-overleaf--csrf-token project-id))))
   extra-headers))

(defun git-overleaf--csrf-cache-key (project-id &optional cookies)
  "Return the csrf cache key for PROJECT-ID and COOKIES."
  (format "%s|%s|%s"
          (git-overleaf--url)
          project-id
          (secure-hash 'sha1 (or cookies (git-overleaf--get-cookies)))))

(defun git-overleaf--clear-csrf-cache (&optional project-id)
  "Clear cached csrf token(s), optionally only for PROJECT-ID."
  (if project-id
      (maphash
       (lambda (key _value)
         (when (string-prefix-p (format "%s|%s|" (git-overleaf--url) project-id) key)
           (remhash key git-overleaf--csrf-cache)))
       git-overleaf--csrf-cache)
    (clrhash git-overleaf--csrf-cache)))

(defun git-overleaf--csrf-token (project-id &optional refresh)
  "Return the csrf token for PROJECT-ID.
If REFRESH is non-nil, bypass the cached token and fetch a fresh one."
  (let* ((cookies (git-overleaf--get-cookies))
         (cache-key (git-overleaf--csrf-cache-key project-id cookies))
         (cached (gethash cache-key git-overleaf--csrf-cache)))
    (or (and (not refresh) cached)
        (let* ((project-page
                (git-overleaf--curl-request
                 "GET"
                 (git-overleaf--project-page-url project-id)
                 (git-overleaf--format-curl-headers
                  `(("Cookie" . ,cookies)
                    ("Origin" . ,(git-overleaf--url))
                    ("Referer" . ,(git-overleaf--project-page-url project-id))))))
               (token
                (save-match-data
                  (when (string-match
                         "<meta name=\"ol-csrfToken\" content=\"\\([^\"]+\\)\""
                         project-page)
                    (match-string 1 project-page)))))
          (unless token
            (user-error "Could not extract csrf token for project %s" project-id))
          (puthash cache-key token git-overleaf--csrf-cache)
          token))))

(defun git-overleaf--curl-403-p (result)
  "Return non-nil if curl RESULT looks like an HTTP 403 failure."
  (and (integerp (git-overleaf--command-result-status result))
       (not (zerop (git-overleaf--command-result-status result)))
       (string-match-p
        "returned error: 403"
        (git-overleaf--command-result-output result))))

(defun git-overleaf--curl-timeout-args ()
  "Return timeout arguments shared by Overleaf curl commands."
  (append
   (when git-overleaf--curl-connect-timeout
     (list "--connect-timeout"
           (number-to-string git-overleaf--curl-connect-timeout)))
   (when git-overleaf--curl-max-time
     (list "--max-time"
           (number-to-string git-overleaf--curl-max-time)))))

(defun git-overleaf--curl-download-timeout-args ()
  "Return timeout arguments for project zip downloads."
  (append
   (when git-overleaf--curl-connect-timeout
     (list "--connect-timeout"
           (number-to-string git-overleaf--curl-connect-timeout)))
   (when git-overleaf--curl-download-max-time
     (list "--max-time"
           (number-to-string git-overleaf--curl-download-max-time)))
   (when (and git-overleaf--curl-download-speed-limit
              git-overleaf--curl-download-speed-time)
     (list "--speed-limit"
           (number-to-string git-overleaf--curl-download-speed-limit)
           "--speed-time"
           (number-to-string git-overleaf--curl-download-speed-time)))))

(defun git-overleaf--curl-base-args ()
  "Return common arguments shared by Overleaf curl commands."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (git-overleaf--curl-timeout-args)))

(defun git-overleaf--curl-download-base-args ()
  "Return common arguments shared by Overleaf project zip downloads."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (git-overleaf--curl-download-timeout-args)))

(defun git-overleaf--socket-cookies ()
  "Return cookies suitable for websocket access."
  (let* ((cookies (git-overleaf--get-cookies))
         (header-text
          (git-overleaf--curl-header-text
           "GET"
           (format "%s/socket.io/socket.io.js" (git-overleaf--url))
           (git-overleaf--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(git-overleaf--url))))))
         (gclb-cookie
          (and header-text
               (save-match-data
                 (when (string-match "\\(GCLB=.*?\\);" header-text)
                   (match-string 1 header-text))))))
    (if (and gclb-cookie (not (string-empty-p gclb-cookie)))
        (format "%s; %s" cookies gclb-cookie)
      cookies)))

(defun git-overleaf--curl-download-args (url output-file headers)
  "Return curl argument list to download URL into OUTPUT-FILE with HEADERS."
  (append
   (git-overleaf--curl-download-base-args)
   (apply #'append (mapcar (lambda (h) (list "-H" h)) headers))
   (list "--output" output-file url)))

(defun git-overleaf--curl-download (url output-file headers)
  "Download URL into OUTPUT-FILE with HEADERS using curl."
  (git-overleaf--run git-overleaf-curl-executable
                     (git-overleaf--curl-download-args
                      url output-file headers)))

(defun git-overleaf--curl-request (method url headers &optional body)
  "Run a curl request with METHOD to URL using HEADERS and optional BODY."
  (let ((args
         (append
          (git-overleaf--curl-base-args)
          (list "-X" method)
          (apply
           #'append
           (mapcar (lambda (header) (list "-H" header)) headers))
          (when body
            (list "--data-binary" body))
          (list url))))
    (git-overleaf--command-result-output
     (git-overleaf--run git-overleaf-curl-executable args))))

(defun git-overleaf--curl-header-text (method url headers &optional body)
  "Run a curl request and return raw response headers as text.
METHOD, URL, HEADERS, and optional BODY are passed through to curl.
The response body is discarded."
  (let ((args
         (append
          (git-overleaf--curl-base-args)
          (list "-X" method "--dump-header" "-" "--output" null-device)
          (apply
           #'append
           (mapcar (lambda (header) (list "-H" header)) headers))
          (when body
            (list "--data-binary" body))
          (list url))))
    (git-overleaf--command-result-output
     (git-overleaf--run git-overleaf-curl-executable args))))

(defun git-overleaf--curl-upload-file
    (project-id folder-id file-name file-path)
  "Upload FILE-PATH as FILE-NAME into FOLDER-ID on PROJECT-ID."
  (cl-labels
      ((build-args ()
         (let* ((url
                 (format "%s/project/%s/upload?folder_id=%s"
                         (git-overleaf--url)
                         project-id
                         folder-id))
                (headers
                 (git-overleaf--format-curl-headers
                  (git-overleaf--project-headers project-id))))
           (append
            (git-overleaf--curl-base-args)
            (list "-X" "POST")
            (apply #'append
                   (mapcar (lambda (header) (list "-H" header)) headers))
            (list
             "-F" "relativePath=null"
             "-F" (format "name=%s" file-name)
             "-F" "type=application/octet-stream"
             "-F"
             (format "qqfile=@%s;type=application/octet-stream" file-path)
             url)))))
    (let* ((args (build-args))
           (result
            (git-overleaf--run git-overleaf-curl-executable args nil nil t)))
      (when (git-overleaf--curl-403-p result)
        (git-overleaf--warn
         "Overleaf upload returned 403; refreshing csrf token and retrying once")
        (git-overleaf--clear-csrf-cache project-id)
        (setq args (build-args)
              result
              (git-overleaf--run git-overleaf-curl-executable args nil nil t)))
      (unless (and (integerp (git-overleaf--command-result-status result))
                   (zerop (git-overleaf--command-result-status result)))
        (error "%s"
               (git-overleaf--command-error-message
                (git-overleaf--ensure-executable git-overleaf-curl-executable)
                args
                (git-overleaf--command-result-output result))))
      (json-parse-string
       (git-overleaf--command-result-output result)
       :object-type 'plist
       :array-type 'list))))

(defun git-overleaf--download-snapshot (project-id)
  "Download PROJECT-ID as a temporary snapshot."
  (let* ((zipfile (make-temp-file "git-overleaf." nil ".zip"))
         (temp-dir (make-temp-file "git-overleaf." t))
         (headers
          (git-overleaf--format-curl-headers
           (git-overleaf--base-headers
            (git-overleaf--project-page-url project-id))))
         (url
          (format "%s/project/%s/download/zip"
                  (git-overleaf--url)
                  project-id)))
    (let ((success nil))
      (unwind-protect
          (prog1
              (progn
                (git-overleaf--message "Downloading project %s..." project-id)
                (git-overleaf--curl-download url zipfile headers)
                (git-overleaf--run
                 git-overleaf-unzip-executable
                 (list "-q" "-o" zipfile "-d" temp-dir))
                (make-git-overleaf--snapshot
                 :temp-dir temp-dir
                 :root (git-overleaf--normalize-extracted-root temp-dir)))
            (setq success t))
        (ignore-errors (delete-file zipfile))
        (unless success
          (ignore-errors (delete-directory temp-dir t)))))))

(defun git-overleaf--fetch-remote-table (project-id)
  "Return the remote entity table for PROJECT-ID."
  (git-overleaf--build-entity-table
   (git-overleaf--fetch-tree project-id)))

(defun git-overleaf--create-folder (project-id parent-id name)
  "Create folder NAME below PARENT-ID on PROJECT-ID."
  (json-parse-string
   (git-overleaf--curl-request
    "POST"
    (format "%s/project/%s/folder" (git-overleaf--url) project-id)
    (git-overleaf--format-curl-headers
     (append
      (git-overleaf--project-headers project-id)
      '(("Content-Type" . "application/json"))))
    (json-encode `(:parent_folder_id ,parent-id :name ,name)))
   :object-type 'plist
   :array-type 'list))

(defun git-overleaf--delete-entity (project-id entity)
  "Delete ENTITY from PROJECT-ID."
  (let ((entity-type
         (pcase (git-overleaf--entity-type entity)
           ('folder "folder")
           ('doc "doc")
           ('file "file")
           (_ (user-error "Unsupported entity type: %S"
                          (git-overleaf--entity-type entity))))))
    (git-overleaf--curl-request
     "DELETE"
     (format "%s/project/%s/%s/%s"
             (git-overleaf--url)
             project-id
             entity-type
             (git-overleaf--entity-id entity))
     (git-overleaf--format-curl-headers
      (append
       (git-overleaf--project-headers project-id)
       '(("Content-Type" . "application/json"))))
     "{}")))

;;;; Project discovery

(defun git-overleaf-list (&optional url)
  "Return the list of accessible Overleaf projects for URL."
  (setq git-overleaf-url (or url (git-overleaf--url)))
  (let* ((cookies (git-overleaf--get-cookies))
         (project-page
          (git-overleaf--curl-request
           "GET"
           (format "%s/project" (git-overleaf--url))
           (git-overleaf--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(git-overleaf--url))))))
         (projects-json
          (save-match-data
            (unless (string-match
                     "name=\"ol-prefetchedProjectsBlob\".*?content=\"\\(.*?\\)\""
                     project-page)
              (user-error "Could not find project list on %s" (git-overleaf--url)))
            (json-parse-string
             (url-unhex-string
              (mm-url-decode-entities-string (match-string 1 project-page)))
             :object-type 'plist
             :array-type 'list))))
    (plist-get projects-json :projects)))

(defun git-overleaf--select-project (projects)
  "Prompt for one project from PROJECTS and return its plist."
  (unless projects
    (user-error "No accessible Overleaf projects were found"))
  (let ((collection
         (mapcar
          (lambda (project)
            `(:fields
              (,(plist-get project :name)
               ,(or (plist-get (plist-get project :owner) :email) ""))
              :data ,project))
          projects)))
    (git-overleaf--completing-read "Project: " collection)))

(defun git-overleaf--read-project (&optional url)
  "Prompt for an Overleaf project on URL and return its plist."
  (git-overleaf--select-project (git-overleaf-list url)))

;;;; Remote project tree

(defun git-overleaf--socketio-connect (project-id)
  "Open a Socket.IO 0.9 websocket for PROJECT-ID."
  (let* ((cookies (git-overleaf--socket-cookies))
         (response-body
          (git-overleaf--curl-request
           "GET"
           (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1"
                   (git-overleaf--url)
                   project-id)
           (git-overleaf--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(git-overleaf--url))))))
         (ws-id (car (string-split response-body ":")))
         (ws-url
          (replace-regexp-in-string
           "^http" "ws"
           (replace-regexp-in-string
            "^https" "wss"
            (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                    (git-overleaf--url)
                    ws-id
                    project-id))))
         (client (make-git-overleaf--socketio-client
                  :next-ack-id 0
                  :acks nil
                  :events nil)))
    (setf
     (git-overleaf--socketio-client-websocket client)
     (websocket-open
      ws-url
      :custom-header-alist `(("Cookie" . ,cookies)
                             ("Origin" . ,(git-overleaf--url)))
      :on-message
      (lambda (socket frame)
        (git-overleaf--socketio-handle-frame
         client
         socket
         (websocket-frame-text frame)))
      :on-close
      (lambda (_socket)
        (setf (git-overleaf--socketio-client-done client) t))))
    (git-overleaf--async-register-process
     (websocket-conn (git-overleaf--socketio-client-websocket client)))
    client))

(defun git-overleaf--socketio-close (client)
  "Close Socket.IO CLIENT."
  (when-let* ((ws (git-overleaf--socketio-client-websocket client)))
    (git-overleaf--async-unregister-process (websocket-conn ws))
    (ignore-errors (websocket-close ws))))

(defun git-overleaf--socketio-handle-frame (client socket text)
  "Handle Socket.IO frame TEXT for CLIENT received from SOCKET."
  (git-overleaf--debug "Websocket frame: %s" text)
  (cond
   ((string-prefix-p "2::" text)
    (websocket-send-text socket "2::"))
   ((string-prefix-p "7:" text)
    (setf (git-overleaf--socketio-client-failure client)
          "Unauthorized websocket response")
    (setf (git-overleaf--socketio-client-done client) t)
    (ignore-errors (websocket-close socket)))
   ((string-prefix-p "5:" text)
    (condition-case err
        (git-overleaf--socketio-handle-event client text)
      (error
       (setf (git-overleaf--socketio-client-failure client)
             (error-message-string err))
       (setf (git-overleaf--socketio-client-done client) t)
       (ignore-errors (websocket-close socket)))))
   ((string-prefix-p "6:" text)
    (condition-case err
        (git-overleaf--socketio-handle-ack client text)
      (error
       (setf (git-overleaf--socketio-client-failure client)
             (error-message-string err))
       (setf (git-overleaf--socketio-client-done client) t)
       (ignore-errors (websocket-close socket)))))))

(defun git-overleaf--socketio-handle-event (client text)
  "Handle Socket.IO event frame TEXT for CLIENT."
  (let* ((payload (git-overleaf--socketio-event-data text))
         (message (json-parse-string
                   payload
                   :object-type 'plist
                   :array-type 'list
                   :null-object nil
                   :false-object nil))
         (name (plist-get message :name)))
    (push message (git-overleaf--socketio-client-events client))
    (when (string= name "connectionRejected")
      (setf (git-overleaf--socketio-client-failure client)
            (format "Overleaf rejected websocket connection: %S"
                    (plist-get message :args)))
      (setf (git-overleaf--socketio-client-done client) t))))

(defun git-overleaf--socketio-handle-ack (client text)
  "Handle Socket.IO ack frame TEXT for CLIENT."
  (save-match-data
    (unless (string-match "\\`6:[^:]*:[^:]*:\\([0-9]+\\)\\(?:+\\(.*\\)\\)?\\'" text)
      (error "Unsupported websocket ack frame: %s" text))
    (let* ((ack-id (match-string 1 text))
           (payload (match-string 2 text))
           (args (if (and payload (not (string-empty-p payload)))
                     (json-parse-string
                      payload
                      :object-type 'plist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)
                   nil)))
      (push (cons ack-id args)
            (git-overleaf--socketio-client-acks client)))))

(defun git-overleaf--socketio-event-data (text)
  "Return the JSON payload from a Socket.IO 0.9 event frame TEXT."
  (save-match-data
    (unless (string-match "\\`5:[^:]*:[^:]*:\\(.*\\)\\'" text)
      (error "Unsupported websocket event frame: %s" text))
    (match-string 1 text)))

(defun git-overleaf--socketio-take-event-if (client predicate)
  "Return and remove the oldest queued CLIENT event matching PREDICATE."
  (let ((events (nreverse (git-overleaf--socketio-client-events client)))
        (matched nil)
        (remaining nil))
    (dolist (event events)
      (if (and (not matched)
               (funcall predicate event))
          (setq matched event)
        (push event remaining)))
    (setf (git-overleaf--socketio-client-events client)
          remaining)
    matched))

(defun git-overleaf--socketio-take-event (client name)
  "Return and remove the oldest queued CLIENT event named NAME."
  (git-overleaf--socketio-take-event-if
   client
   (lambda (event)
     (string= (plist-get event :name) name))))

(defconst git-overleaf--socketio-wait-pending
  (make-symbol "git-overleaf-socketio-wait-pending")
  "Sentinel returned by Socket.IO wait predicates that are not ready.")

(defun git-overleaf--socketio-wait (client predicate description)
  "Wait until PREDICATE returns a non-sentinel value for CLIENT.
Signal an error labelled with DESCRIPTION on timeout or websocket
failure.  PREDICATE must return
`git-overleaf--socketio-wait-pending' while it is still waiting."
  (let ((deadline (+ (float-time) git-overleaf-socket-timeout))
        (result git-overleaf--socketio-wait-pending))
    (while (and (eq result git-overleaf--socketio-wait-pending)
                (not (git-overleaf--socketio-client-done client))
                (< (float-time) deadline))
      (setq result (funcall predicate))
      (when (eq result git-overleaf--socketio-wait-pending)
        (accept-process-output nil 0.1)))
    (when (eq result git-overleaf--socketio-wait-pending)
      (setq result (funcall predicate)))
    (when (eq result git-overleaf--socketio-wait-pending)
      (let ((failure (or (git-overleaf--socketio-client-failure client)
                         (and (>= (float-time) deadline)
                              (format "Timed out while waiting for %s"
                                      description))
                         (format "Websocket closed while waiting for %s"
                                 description))))
        (user-error "%s" failure)))
    result))

(defun git-overleaf--socketio-wait-event (client name)
  "Wait for a Socket.IO event named NAME from CLIENT."
  (git-overleaf--socketio-wait
   client
   (lambda ()
     (or (git-overleaf--socketio-take-event client name)
         git-overleaf--socketio-wait-pending))
   (format "websocket event `%s'" name)))

(defun git-overleaf--socketio-emit (client name &rest args)
  "Emit Socket.IO event NAME with ARGS on CLIENT and wait for its ack.
Return the ack arguments as a list.  Socket.IO acks with no arguments
are returned as nil."
  (let* ((ack-id
          (number-to-string
           (cl-incf
            (git-overleaf--socketio-client-next-ack-id client))))
         (payload (json-encode (list :name name :args args)))
         (frame (format "5:%s+::%s" ack-id payload))
         (ws (git-overleaf--socketio-client-websocket client)))
    (git-overleaf--debug "Websocket send: %s" frame)
    (websocket-send-text ws frame)
    (let ((ack
           (git-overleaf--socketio-wait
            client
            (lambda ()
              (if-let* ((entry
                         (assoc
                          ack-id
                          (git-overleaf--socketio-client-acks client))))
                  (cdr entry)
                git-overleaf--socketio-wait-pending))
            (format "websocket ack for `%s'" name))))
      (setf (git-overleaf--socketio-client-acks client)
            (assoc-delete-all
             ack-id
             (git-overleaf--socketio-client-acks client)))
      ack)))

(defun git-overleaf--socketio-call (project-id function)
  "Call FUNCTION with a connected Socket.IO client for PROJECT-ID.
FUNCTION receives the client and the initial `joinProjectResponse'
event."
  (let ((client nil)
        (join-event nil))
    (unwind-protect
        (progn
          (setq client (git-overleaf--socketio-connect project-id))
          (setq join-event
                (git-overleaf--socketio-wait-event
                 client
                 "joinProjectResponse"))
          (funcall function client join-event))
      (when client
        (git-overleaf--socketio-close client)))))

(defun git-overleaf--fetch-tree (project-id)
  "Return PROJECT-ID's root folder plist via websocket."
  (git-overleaf--socketio-call
   project-id
   (lambda (_client join-event)
     (or (git-overleaf--pget
          join-event
          :args 0 :project :rootFolder 0)
         (user-error "Could not fetch project tree for %s" project-id)))))

;;;; Remote document updates

(defun git-overleaf--file-string (file)
  "Return FILE contents as a raw Emacs string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file))
    (buffer-string)))

(defun git-overleaf--file-utf8-string (file)
  "Return FILE contents decoded as UTF-8 text."
  (let ((string (decode-coding-string
                 (git-overleaf--file-string file)
                 'utf-8-unix)))
    (when (cl-some
           (lambda (char)
             (eq (char-charset char) 'eight-bit))
           string)
      (user-error
       "Overleaf doc text update requires valid UTF-8 text: %s"
       file))
    string))

(defun git-overleaf--decode-socketio-doc-line (line doc-id)
  "Decode one LINE returned by Overleaf's Socket.IO `joinDoc' for DOC-ID."
  (when (cl-some (lambda (char) (> char #xff)) line)
    (user-error "Could not decode Overleaf doc %s text from websocket" doc-id))
  (let* ((bytes (make-string (length line) 0))
         (string nil))
    (dotimes (index (length line))
      (aset bytes index (aref line index)))
    (setq string (decode-coding-string bytes 'utf-8-unix))
    (when (cl-some
           (lambda (char)
             (eq (char-charset char) 'eight-bit))
           string)
      (user-error "Could not decode Overleaf doc %s text as UTF-8" doc-id))
    string))

(defun git-overleaf--utf-16-length (string)
  "Return STRING length in JavaScript UTF-16 code units."
  (/ (string-bytes (encode-coding-string string 'utf-16le)) 2))

(defun git-overleaf--sharejs-text-op (before after)
  "Return a ShareJS text operation changing BEFORE into AFTER."
  (unless (string= before after)
    (let* ((before-length (length before))
           (after-length (length after))
           (prefix 0)
           (suffix 0))
      (while (and (< prefix before-length)
                  (< prefix after-length)
                  (eq (aref before prefix) (aref after prefix)))
        (setq prefix (1+ prefix)))
      (while (and (< suffix (- before-length prefix))
                  (< suffix (- after-length prefix))
                  (eq (aref before (- before-length suffix 1))
                      (aref after (- after-length suffix 1))))
        (setq suffix (1+ suffix)))
      (let ((deleted (substring before prefix (- before-length suffix)))
            (inserted (substring after prefix (- after-length suffix)))
            (position (git-overleaf--utf-16-length
                       (substring before 0 prefix)))
            (ops nil))
        (unless (string-empty-p deleted)
          (push `(:d ,deleted :p ,position) ops))
        (unless (string-empty-p inserted)
          (push `(:i ,inserted :p ,position) ops))
        (vconcat (nreverse ops))))))

(defun git-overleaf--remote-doc-state (client doc-id)
  "Join DOC-ID via CLIENT and return its current text state."
  (let* ((ack (git-overleaf--socketio-emit
               client
               "joinDoc"
               doc-id
               -1
               '(:encodeRanges t :supportsHistoryOT t)))
         (error-object (car ack))
         (lines (nth 1 ack))
         (version (nth 2 ack))
         (type (nth 5 ack)))
    (when error-object
      (user-error "Could not join Overleaf doc %s: %s" doc-id error-object))
    (unless (equal type "sharejs-text-ot")
      (user-error
       "Overleaf doc %s uses unsupported OT type `%s'; only sharejs-text-ot is supported"
       doc-id
       type))
    (unless (integerp version)
      (user-error "Could not determine Overleaf doc version for %s" doc-id))
    (unless (and (listp lines)
                 (cl-every #'stringp lines))
      (user-error "Could not read Overleaf doc text for %s" doc-id))
    `(:version ,version
               :text ,(string-join
                       (mapcar
                        (lambda (line)
                          (git-overleaf--decode-socketio-doc-line line doc-id))
                        lines)
                       "\n"))))

(defun git-overleaf--socketio-error-message (object)
  "Return a concise message for Socket.IO error OBJECT."
  (cond
   ((null object) nil)
   ((stringp object) object)
   ((and (listp object)
         (plist-get object :message))
    (plist-get object :message))
   (t (format "%S" object))))

(defun git-overleaf--doc-update-event-doc-id (event)
  "Return the document id associated with Overleaf update EVENT."
  (let ((args (plist-get event :args)))
    (pcase (plist-get event :name)
      ("otUpdateApplied"
       (plist-get (car args) :doc))
      ("otUpdateError"
       (plist-get (cadr args) :doc_id)))))

(defun git-overleaf--source-doc-update-applied-p (event doc-id)
  "Return non-nil if EVENT is this client's applied update for DOC-ID."
  (and
   (string= (plist-get event :name) "otUpdateApplied")
   (let ((update (car (plist-get event :args))))
     (and (equal (plist-get update :doc) doc-id)
          (not (plist-member update :op))))))

(defun git-overleaf--wait-doc-update-applied (client doc-id)
  "Wait on CLIENT until DOC-ID's queued OT update is applied or rejected."
  (let ((event
         (git-overleaf--socketio-wait
          client
          (lambda ()
            (or
             (git-overleaf--socketio-take-event-if
              client
              (lambda (event)
                (or
                 (git-overleaf--source-doc-update-applied-p event doc-id)
                 (and
                  (string= (plist-get event :name) "otUpdateError")
                  (equal
                   (git-overleaf--doc-update-event-doc-id event)
                   doc-id)))))
             git-overleaf--socketio-wait-pending))
          (format "Overleaf doc `%s' update to be applied" doc-id))))
    (pcase (plist-get event :name)
      ("otUpdateError"
       (let* ((args (plist-get event :args))
              (error-object (car args))
              (message (cadr args))
              (detail
               (or (git-overleaf--socketio-error-message error-object)
                   (and (listp message)
                        (git-overleaf--socketio-error-message
                         (plist-get message :error)))
                   "document updater rejected the update")))
         (user-error
          "Could not update Overleaf doc %s through text OT: %s"
          doc-id
          detail)))
      ("otUpdateApplied" t)
      (_ t))))

(defun git-overleaf--update-doc-text-content
    (project-id doc-id before after)
  "Update existing DOC-ID on PROJECT-ID from text BEFORE to AFTER.
The update is sent through Overleaf's real-time ShareJS text OT path, so
the remote document id and Overleaf edit history are preserved."
  (let ((op (git-overleaf--sharejs-text-op before after)))
    (when op
      (git-overleaf--socketio-call
       project-id
       (lambda (client _join-event)
         (let* ((state (git-overleaf--remote-doc-state client doc-id))
                (version (plist-get state :version))
                (remote-text (plist-get state :text))
                (ack nil))
           (unless (string= before remote-text)
             (user-error
              "Remote Overleaf doc %s changed after the snapshot was downloaded; run `git-overleaf-pull' and retry"
              doc-id))
           (setq ack
                 (git-overleaf--socketio-emit
                  client
                  "applyOtUpdate"
                  doc-id
                  `(:doc ,doc-id
                         :op ,op
                         :v ,version
                         :meta (:source "git-overleaf"))))
           (when-let* ((error-object (car ack)))
             (user-error
              "Could not update Overleaf doc %s through text OT: %s"
              doc-id
              (or (git-overleaf--socketio-error-message error-object)
                  error-object)))
           (git-overleaf--wait-doc-update-applied client doc-id)
           (unless (string= after
                            (plist-get
                             (git-overleaf--remote-doc-state client doc-id)
                             :text))
             (user-error
              "Overleaf doc %s did not match the expected text after OT update; run `git-overleaf-pull' and retry"
              doc-id))
           (git-overleaf--message
            "Updated Overleaf doc %s through text OT"
            doc-id)
           t))))))

(defun git-overleaf--update-doc-text
    (project-id doc-id local-file remote-file)
  "Update existing DOC-ID on PROJECT-ID from REMOTE-FILE to LOCAL-FILE.
The update is sent through Overleaf's real-time ShareJS text OT path, so
the remote document id and Overleaf edit history are preserved."
  (git-overleaf--update-doc-text-content
   project-id
   doc-id
   (git-overleaf--file-utf8-string remote-file)
   (git-overleaf--file-utf8-string local-file)))

(defun git-overleaf--build-entity-table (root-folder)
  "Return a hash table containing all remote entities from ROOT-FOLDER."
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels
        ((walk-folder (folder parent-path parent-id)
           (let* ((name (plist-get folder :name))
                  (id (plist-get folder :_id))
                  (path (if (string= name "rootFolder")
                            ""
                          (if (string-empty-p parent-path)
                              name
                            (concat parent-path "/" name)))))
             (puthash
              path
              (make-git-overleaf--entity
               :path path
               :name name
               :id id
               :type 'folder
               :parent-id parent-id)
              table)
             (dolist (doc (plist-get folder :docs))
               (let ((doc-path
                      (if (string-empty-p path)
                          (plist-get doc :name)
                        (concat path "/" (plist-get doc :name)))))
                 (puthash
                  doc-path
                  (make-git-overleaf--entity
                   :path doc-path
                   :name (plist-get doc :name)
                   :id (plist-get doc :_id)
                   :type 'doc
                   :parent-id id)
                  table)))
             (dolist (file (plist-get folder :fileRefs))
               (let ((file-path
                      (if (string-empty-p path)
                          (plist-get file :name)
                        (concat path "/" (plist-get file :name)))))
                 (puthash
                  file-path
                  (make-git-overleaf--entity
                   :path file-path
                   :name (plist-get file :name)
                   :id (plist-get file :_id)
                   :type 'file
                   :parent-id id)
                  table)))
             (dolist (child (plist-get folder :folders))
               (walk-folder child path id)))))
      (walk-folder root-folder "" nil))
    table))

(defun git-overleaf--forget-entry (table path)
  "Delete PATH and all descendants from TABLE."
  (let ((prefix (if (string-empty-p path) path (concat path "/")))
        keys)
    (maphash
     (lambda (key _value)
       (when (or (string= key path)
                 (and (not (string-empty-p prefix))
                      (string-prefix-p prefix key)))
         (push key keys)))
     table)
    (dolist (key keys)
      (remhash key table))))


(provide 'git-overleaf-http)

;;; git-overleaf-http.el ends here
