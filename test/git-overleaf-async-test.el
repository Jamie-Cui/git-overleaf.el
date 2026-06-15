;;; git-overleaf-async-test.el --- Async state tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'git-overleaf-core)

(defmacro git-overleaf-async-test--with-clean-state (&rest body)
  "Run BODY with isolated async globals."
  (declare (indent 0) (debug t))
  `(let ((git-overleaf-enable-async nil)
         (git-overleaf--async-locks (make-hash-table :test #'equal))
         (git-overleaf--async-tasks (make-hash-table :test #'eql))
         (git-overleaf--async-canceled-task-ids
          (make-hash-table :test #'eql))
         (git-overleaf--async-next-task-id 0)
         (git-overleaf--async-completions nil)
         (git-overleaf--async-current-task-id nil)
         (git-overleaf--async-timer nil))
     ,@body))

(ert-deftest git-overleaf-async-test-enabled-p-respects-noninteractive ()
  (let ((git-overleaf-enable-async t)
        (noninteractive t))
    (should-not (git-overleaf--async-enabled-p)))
  (let ((git-overleaf-enable-async nil)
        (noninteractive nil))
    (should-not (git-overleaf--async-enabled-p))))

(ert-deftest git-overleaf-async-test-start-synchronous-success-and-error ()
  (git-overleaf-async-test--with-clean-state
    (let ((success nil)
          (error-message nil))
      (should (equal (git-overleaf--async-start
                      "success"
                      (lambda () 42)
                      :on-success (lambda (value) (setq success value)))
                     42))
      (should (equal success 42))
      (should-not error-message)
      (should (equal (git-overleaf--async-start
                      "error"
                      (lambda () (error "Boom"))
                      :on-error (lambda (message)
                                  (setq error-message message)))
                     "Boom"))
      (should (equal error-message "Boom"))
      (should-error
       (git-overleaf--async-start
        "error"
        (lambda () (error "Uncaught")))
       :type 'error))))

(ert-deftest git-overleaf-async-test-register-and-remove-tasks ()
  (git-overleaf-async-test--with-clean-state
    (let ((task (make-git-overleaf--async-task
                 :id 1
                 :name "task"
                 :key "key")))
      (should (git-overleaf--async-lock-empty-p))
      (should (git-overleaf--async-task-empty-p))
      (puthash "key" "task" git-overleaf--async-locks)
      (git-overleaf--async-register-task task)
      (should-not (git-overleaf--async-lock-empty-p))
      (should-not (git-overleaf--async-task-empty-p))
      (let ((git-overleaf--async-current-task-id 1))
        (should (eq (git-overleaf--async-current-task) task))
        (should-not (git-overleaf--async-current-task-canceled-p))
        (setf (git-overleaf--async-task-canceled task) t)
        (should (git-overleaf--async-current-task-canceled-p)))
      (git-overleaf--async-remove-task 1)
      (remhash "key" git-overleaf--async-locks)
      (should (git-overleaf--async-lock-empty-p))
      (should (git-overleaf--async-task-empty-p)))))

(ert-deftest git-overleaf-async-test-completion-queue-and-drain ()
  (git-overleaf-async-test--with-clean-state
    (let ((events nil)
          (warnings nil))
      (puthash "key-a" "operation-a" git-overleaf--async-locks)
      (puthash "key-b" "operation-b" git-overleaf--async-locks)
      (cl-letf (((symbol-function 'git-overleaf--warn)
                 (lambda (&rest args)
                   (push args warnings)))
                ((symbol-function 'git-overleaf--async-stop-timer-if-idle)
                 (lambda () nil)))
        (git-overleaf--async-push-completion
         (make-git-overleaf--async-completion
          :name "operation-a"
          :key "key-a"
          :status 'success
          :value "value"
          :on-success (lambda (value)
                        (push (list :success value) events))))
        (git-overleaf--async-push-completion
         (make-git-overleaf--async-completion
          :name "operation-b"
          :key "key-b"
          :status 'error
          :error "Failed"
          :on-error (lambda (message)
                      (push (list :error message) events))))
        (git-overleaf--async-drain-completions)
        (should (equal (nreverse events)
                       '((:success "value") (:error "Failed"))))
        (should-not warnings)
        (should-not (gethash "key-a" git-overleaf--async-locks))
        (should-not (gethash "key-b" git-overleaf--async-locks)))))

(ert-deftest git-overleaf-async-test-completion-default-handlers ()
  (git-overleaf-async-test--with-clean-state
    (let ((messages nil)
          (warnings nil))
      (cl-letf (((symbol-function 'git-overleaf--message)
                 (lambda (&rest args) (push args messages)))
                ((symbol-function 'git-overleaf--warn)
                 (lambda (&rest args) (push args warnings)))
                ((symbol-function 'git-overleaf--async-stop-timer-if-idle)
                 (lambda () nil)))
        (git-overleaf--async-push-completion
         (make-git-overleaf--async-completion
          :name "success-op"
          :status 'success
          :value "ignored"))
        (git-overleaf--async-push-completion
         (make-git-overleaf--async-completion
          :name "error-op"
          :status 'error
          :error "bad"))
        (git-overleaf--async-drain-completions)
        (should (member '("Finished %s" "success-op") messages))
        (should (member '("%s failed: %s" "error-op" "bad") warnings))))))

(ert-deftest git-overleaf-async-test-cancel-completion-removes-task-state ()
  (git-overleaf-async-test--with-clean-state
    (let ((task (make-git-overleaf--async-task
                 :id 7
                 :name "task")))
      (git-overleaf--async-register-task task)
      (puthash 7 t git-overleaf--async-canceled-task-ids)
      (should (git-overleaf--async-cancel-completion-p
               (make-git-overleaf--async-completion
                :task-id 7
                :name "task")))
      (should-not (gethash 7 git-overleaf--async-tasks))
      (should-not (gethash 7 git-overleaf--async-canceled-task-ids)))))

(ert-deftest git-overleaf-async-test-force-stop-clears-state ()
  (git-overleaf-async-test--with-clean-state
    (should-error (git-overleaf--force-stop) :type 'user-error)
    (let ((messages nil)
          (task (make-git-overleaf--async-task
                 :id 1
                 :name "task"
                 :key "key")))
      (puthash "key" "task" git-overleaf--async-locks)
      (git-overleaf--async-register-task task)
      (setq git-overleaf--async-completions
            (list (make-git-overleaf--async-completion
                   :name "task")))
      (cl-letf (((symbol-function 'git-overleaf--message)
                 (lambda (&rest args)
                   (push args messages))))
        (git-overleaf--force-stop))
      (should (git-overleaf--async-lock-empty-p))
      (should (git-overleaf--async-task-empty-p))
      (should-not git-overleaf--async-completions)
      (should (equal (car messages)
                     '("Stopped %d Overleaf background operation%s" 1 "")))))))

;;; git-overleaf-async-test.el ends here
