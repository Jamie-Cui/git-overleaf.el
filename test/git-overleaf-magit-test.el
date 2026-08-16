;;; git-overleaf-magit-test.el --- Magit helper tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-magit)

(ert-deftest git-overleaf-magit-test-state-labels ()
  (let ((state (git-overleaf-magit--make-remote-state)))
    (cl-letf (((symbol-function 'git-overleaf--pending-state)
               (lambda (_repo) '(:action pull))))
      (should
       (equal
        (git-overleaf-magit--state-label
         "/repo" t 'in-sync nil state)
        '("pending pull" . error))))
    (cl-letf (((symbol-function 'git-overleaf--pending-state)
               (lambda (_repo) nil)))
      (should
       (equal
        (git-overleaf-magit--state-label "/repo" t nil nil state)
        '("remote unchecked" . magit-dimmed)))
      (should
       (equal
        (git-overleaf-magit--state-label "/repo" nil nil nil state)
        '("local changes; remote unchecked" . warning)))
      (dolist (case '((in-sync "in sync" magit-dimmed)
                      (head-matches-remote "content matches" magit-dimmed)
                      (remote-matches-base "local changes" warning)
                      (head-matches-base "remote changes" warning)
                      (diverged "local and remote changes" warning)))
        (should
         (equal
          (git-overleaf-magit--state-label
           "/repo" nil (nth 0 case) nil state)
          (cons (nth 1 case) (nth 2 case)))))
      (should
       (equal
        (git-overleaf-magit--state-label
         "/repo" t 'in-sync t state)
        '("in sync; working tree modified" . warning))))))

(ert-deftest git-overleaf-magit-test-refresh-state-labels ()
  (cl-letf (((symbol-function 'git-overleaf--pending-state)
             (lambda (_repo) nil)))
    (let ((checking
           (git-overleaf-magit--make-remote-state :refreshing t))
          (failed
           (git-overleaf-magit--make-remote-state :error "network"))
          (stale
           (git-overleaf-magit--make-remote-state
            :error "network"
            :last-success-time 1000)))
      (should
       (equal
        (git-overleaf-magit--state-label
         "/repo" t nil nil checking)
        '("checking remote..." . magit-dimmed)))
      (should
       (equal
        (git-overleaf-magit--state-label
         "/repo" t nil nil failed)
        '("remote refresh failed" . error)))
      (pcase-let ((`(,label . ,face)
                   (git-overleaf-magit--state-label
                    "/repo" t 'in-sync nil stale)))
        (should (eq face 'magit-dimmed))
        (should (string-match-p
                 "in sync; refresh failed; snapshot [0-9][0-9]:[0-9][0-9]"
                 label))))))

(ert-deftest git-overleaf-magit-test-heading-shows-logical-remote ()
  (should
   (equal
    (substring-no-properties
     (git-overleaf-magit--heading
      "Project" "paper" "in sync" 'magit-dimmed))
    "Overleaf: Project [paper] (in sync)"))
  (should
   (equal
    (substring-no-properties
     (git-overleaf-magit--heading
      "Project" nil "remote unchecked" 'magit-dimmed))
    "Overleaf: Project [unregistered] (remote unchecked)")))

(ert-deftest git-overleaf-magit-test-diff-kinds ()
  (should-not (git-overleaf-magit--diff-kinds t nil))
  (should (equal (git-overleaf-magit--diff-kinds nil nil) '(local)))
  (should
   (equal
    (git-overleaf-magit--diff-kinds nil 'head-matches-remote)
    '(matching)))
  (should
   (equal
    (git-overleaf-magit--diff-kinds nil 'remote-matches-base)
    '(local)))
  (should
   (equal
    (git-overleaf-magit--diff-kinds t 'head-matches-base)
    '(remote)))
  (should
   (equal
    (git-overleaf-magit--diff-kinds nil 'diverged)
    '(local remote))))

(ert-deftest git-overleaf-magit-test-auto-refresh-due-p ()
  (with-temp-buffer
    (let ((state (git-overleaf-magit--remote-state)))
      (should (git-overleaf-magit--auto-refresh-due-p))
      (setf (git-overleaf-magit--remote-state-last-attempt-time state)
            (float-time))
      (should-not (git-overleaf-magit--auto-refresh-due-p))
      (setf (git-overleaf-magit--remote-state-last-attempt-time state)
            (- (float-time)
               git-overleaf-magit--auto-refresh-remote-interval
               1))
      (should (git-overleaf-magit--auto-refresh-due-p)))))

(ert-deftest git-overleaf-magit-test-maybe-auto-refresh-remote ()
  (with-temp-buffer
    (let ((git-overleaf-magit-auto-refresh-remote t)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf--async-supported-p)
                 (lambda () t))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'magit-toplevel)
                 (lambda () "/repo"))
                ((symbol-function 'git-overleaf--managed-repo-p)
                 (lambda (_repo) t))
                ((symbol-function 'git-overleaf-magit--registered-remote-p)
                 (lambda (_repo) t))
                ((symbol-function 'git-overleaf--repo-async-key)
                 (lambda (_repo) "repo:/repo"))
                ((symbol-function 'git-overleaf--async-key-active-p)
                 (lambda (_key) nil))
                ((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda (&optional background)
                   (setq called background))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should called)))
    (let ((git-overleaf-magit-auto-refresh-remote t)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf--async-supported-p)
                 (lambda () t))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'magit-toplevel)
                 (lambda () "/repo"))
                ((symbol-function 'git-overleaf--managed-repo-p)
                 (lambda (_repo) t))
                ((symbol-function 'git-overleaf-magit--registered-remote-p)
                 (lambda (_repo) t))
                ((symbol-function 'git-overleaf--repo-async-key)
                 (lambda (_repo) "repo:/repo"))
                ((symbol-function 'git-overleaf--async-key-active-p)
                 (lambda (_key) t))
                ((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda (&optional _background)
                   (setq called t))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should-not called)))
    (let ((git-overleaf-magit-auto-refresh-remote nil)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda (&optional _background)
                   (setq called t))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should-not called)))))

(ert-deftest git-overleaf-magit-test-remote-refresh-success ()
  (let ((buffer (generate-new-buffer " *overleaf-magit-test*"))
        (refreshed nil)
        (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'git-overleaf--message)
                   (lambda (&rest args) (push args messages)))
                  ((symbol-function 'float-time)
                   (lambda (&optional _time) 123)))
          (with-current-buffer buffer
            (setq git-overleaf-magit--remote-state
                  (git-overleaf-magit--make-remote-state
                   :refreshing t
                   :error "old error")))
          (git-overleaf-magit--remote-refresh-succeeded
           buffer "remote-commit")
          (with-current-buffer buffer
            (let ((state (git-overleaf-magit--remote-state)))
              (should-not
               (git-overleaf-magit--remote-state-refreshing state))
              (should
               (= (git-overleaf-magit--remote-state-last-success-time state)
                  123))
              (should-not (git-overleaf-magit--remote-state-error state))))
          (should refreshed)
          (should (equal messages '(("Remote snapshot ready.")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest git-overleaf-magit-test-remote-refresh-failure-keeps-cache ()
  (let ((buffer (generate-new-buffer " *overleaf-magit-test*"))
        (refreshed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t))))
          (with-current-buffer buffer
            (setq git-overleaf-magit--remote-state
                  (git-overleaf-magit--make-remote-state
                   :refreshing t
                   :last-success-time 100)))
          (git-overleaf-magit--remote-refresh-failed buffer "network")
          (with-current-buffer buffer
            (let ((state (git-overleaf-magit--remote-state)))
              (should-not
               (git-overleaf-magit--remote-state-refreshing state))
              (should
               (equal
                (git-overleaf-magit--remote-state-error state)
                "network"))))
          (should refreshed))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest git-overleaf-magit-test-fetch-routing ()
  (let ((ordinary nil)
        (overleaf nil))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/repo"))
              ((symbol-function 'git-overleaf--managed-repo-p)
               (lambda (_repo) t))
              ((symbol-function 'git-overleaf--remote-name)
               (lambda (_repo) "overleaf"))
              ((symbol-function 'git-overleaf-magit-refresh-remote)
               (lambda (&optional background)
                 (setq overleaf background))))
      (git-overleaf-magit--around-git-fetch
       (lambda (remote args) (setq ordinary (list remote args)))
       "overleaf"
       nil)
      (should overleaf)
      (should-not ordinary)
      (setq overleaf nil)
      (git-overleaf-magit--around-git-fetch
       (lambda (remote args) (setq ordinary (list remote args)))
       "origin"
       '("--prune"))
      (should (equal ordinary '("origin" ("--prune"))))
      (should-not overleaf)
      (should-error
       (git-overleaf-magit--around-git-fetch
        (lambda (&rest _args) (ert-fail "ordinary fetch was called"))
        "overleaf"
        '("main"))
       :type 'user-error))))

(ert-deftest git-overleaf-magit-test-operation-succeeded-refreshes-status ()
  (let ((buffer (generate-new-buffer " *overleaf-status-test*"))
        (refreshed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-get-mode-buffer)
                   (lambda (&rest _args) buffer))
                  ((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'float-time)
                   (lambda (&optional _time) 123)))
          (git-overleaf-magit--operation-succeeded "/repo" 'pull)
          (with-current-buffer buffer
            (let ((state (git-overleaf-magit--remote-state)))
              (should-not
               (git-overleaf-magit--remote-state-refreshing state))
              (should (= (git-overleaf-magit--remote-state-last-attempt-time
                          state)
                         123))
              (should (= (git-overleaf-magit--remote-state-last-success-time
                          state)
                         123))))
          (should refreshed))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest git-overleaf-magit-test-section-keymap ()
  (should
   (eq (lookup-key git-overleaf-magit-section-map (kbd "G"))
       #'git-overleaf-magit-refresh-remote))
  (should
   (eq (lookup-key git-overleaf-magit-command-map (kbd "p"))
       #'git-overleaf-push))
  (should
   (keymapp
    (lookup-key git-overleaf-magit-section-map (kbd "C-c C-c")))))

(ert-deftest git-overleaf-magit-test-enable-status-buffer-hooks ()
  (with-temp-buffer
    (let ((magit-refresh-buffer-hook nil))
      (cl-letf (((symbol-function
                  'git-overleaf-magit--maybe-auto-refresh-remote)
                 (lambda ()
                   (ert-fail "Remote refresh ran during mode setup"))))
        (git-overleaf-magit--enable-status-buffer-hooks)
        (should
         (memq #'git-overleaf-magit--maybe-auto-refresh-remote
               magit-refresh-buffer-hook))))))

(ert-deftest git-overleaf-magit-test-remote-state-is-permanent-local ()
  (should (get 'git-overleaf-magit--remote-state 'permanent-local)))

;;; git-overleaf-magit-test.el ends here
