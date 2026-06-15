;;; git-overleaf-magit-test.el --- Magit helper tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-magit)

(ert-deftest git-overleaf-magit-test-state-labels ()
  (cl-letf (((symbol-function 'git-overleaf--pending-state)
             (lambda (_repo) '(:action pull))))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" t t t t)
                   '("pending pull" . error))))
  (cl-letf (((symbol-function 'git-overleaf--pending-state)
             (lambda (_repo) nil)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" t nil nil nil)
                   '("in sync" . magit-dimmed)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" t t nil nil)
                   '("remote changes" . warning)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" nil t nil t)
                   '("local matches remote" . warning)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" nil t t nil)
                   '("local changes" . warning)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" nil t nil nil)
                   '("local and remote changes" . warning)))
    (should (equal (git-overleaf-magit--state-label
                    "/repo" nil nil nil nil)
                   '("local changes" . warning)))))

(ert-deftest git-overleaf-magit-test-fresh-remote-commit-cache ()
  (with-temp-buffer
    (setq git-overleaf-magit--remote-commit "remote")
    (setq git-overleaf-magit--remote-base-rev "base")
    (setq git-overleaf-magit--last-remote-refresh-time 123)
    (should (equal (git-overleaf-magit--fresh-remote-commit "base")
                   "remote"))
    (should (equal git-overleaf-magit--last-remote-refresh-time 123))
    (should-not (git-overleaf-magit--fresh-remote-commit "other-base"))
    (should-not git-overleaf-magit--remote-commit)
    (should-not git-overleaf-magit--remote-base-rev)
    (should-not git-overleaf-magit--last-remote-refresh-time)))

(ert-deftest git-overleaf-magit-test-auto-refresh-due-p ()
  (with-temp-buffer
    (setq git-overleaf-magit--last-remote-refresh-time nil)
    (should (git-overleaf-magit--auto-refresh-due-p))
    (setq git-overleaf-magit--last-remote-refresh-time (float-time))
    (should-not (git-overleaf-magit--auto-refresh-due-p))
    (setq git-overleaf-magit--last-remote-refresh-time
          (- (float-time)
             git-overleaf-magit--auto-refresh-remote-interval
             1))
    (should (git-overleaf-magit--auto-refresh-due-p))))

(ert-deftest git-overleaf-magit-test-maybe-auto-refresh-remote ()
  (with-temp-buffer
    (let ((git-overleaf-magit--suppress-next-auto-refresh t)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda () (setq called t))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should-not called)
        (should-not git-overleaf-magit--suppress-next-auto-refresh)))
    (let ((git-overleaf-magit-auto-refresh-remote t)
          (git-overleaf-magit--refreshing nil)
          (git-overleaf-magit--last-remote-refresh-time nil)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf--async-enabled-p)
                 (lambda () t))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'magit-toplevel)
                 (lambda () "/repo"))
                ((symbol-function 'git-overleaf--managed-repo-p)
                 (lambda (_repo) t))
                ((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda () (setq called t))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should called)))
    (let ((git-overleaf-magit-auto-refresh-remote nil)
          (called nil))
      (cl-letf (((symbol-function 'git-overleaf-magit-refresh-remote)
                 (lambda () (setq called t))))
        (git-overleaf-magit--maybe-auto-refresh-remote)
        (should-not called)))))

(ert-deftest git-overleaf-magit-test-finish-remote-refresh ()
  (let ((buffer (generate-new-buffer " *overleaf-magit-test*"))
        (refreshed nil)
        (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'git-overleaf--message)
                   (lambda (&rest args) (push args messages))))
          (with-current-buffer buffer
            (setq git-overleaf-magit--refreshing t)
            (setq git-overleaf-magit--remote-commit nil)
            (setq git-overleaf-magit--remote-base-rev nil)
            (setq git-overleaf-magit--suppress-next-auto-refresh nil))
          (git-overleaf-magit--finish-remote-refresh
           buffer
           "remote-commit"
           "base-rev"
           "Ready")
          (with-current-buffer buffer
            (should (equal git-overleaf-magit--remote-commit
                           "remote-commit"))
            (should (equal git-overleaf-magit--remote-base-rev
                           "base-rev"))
            (should-not git-overleaf-magit--refreshing)
            (should git-overleaf-magit--suppress-next-auto-refresh))
          (should refreshed)
          (should (equal messages '(("%s" "Ready")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest git-overleaf-magit-test-enable-status-buffer-hooks ()
  (with-temp-buffer
    (let ((magit-refresh-buffer-hook nil))
      (git-overleaf-magit--enable-status-buffer-hooks)
      (should (memq #'git-overleaf-magit--maybe-auto-refresh-remote
                    magit-refresh-buffer-hook)))))

;;; git-overleaf-magit-test.el ends here
