;;; auto-pause.el --- Run processes which will be paused when Emacs is idle  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-02-23
;; Version: 0.1
;; Package-Version: 20160426.516
;; Keywords: convenience, menu
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/lujun9972/auto-pause

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; auto-pause's code can be found here:
;;   http://github.com/lujun9972/auto-pause

;;; Commentary:
 
;; auto-pause is a llibrary for creating auto-pause process which will
;; be paused when Emacs idle for specific time and be resumed when emacs
;; becomes busy again 

;; Quick start:

;; see the doc-string of `with-auto-pause'

;;; Code:
(require 'cl-lib)

(defun auto-pause (pause-fn resume-fn delay-seconds)
    (let ((pause-function-name (cl-gensym "auto-pause-pause-"))
          (resume-function-name (cl-gensym "auto-pause-resume-"))
          (abort-function-name (cl-gensym "auto-pause-abort-"))
          (idle-timer (cl-gensym "auto-pause-idle-timer-")))
      (message "expand %s %s %s %s" pause-function-name resume-function-name abort-function-name idle-timer)
      (fset pause-function-name (lambda ()
                                  (funcall pause-fn )
                                  (add-hook 'post-command-hook resume-function-name)))
      (fset resume-function-name (lambda ()
                                   (funcall resume-fn )
                                   (remove-hook 'post-command-hook resume-function-name))) 
      (set idle-timer (run-with-idle-timer delay-seconds t pause-function-name))
      (fset abort-function-name (lambda ()
                                  (message "abort %s" abort-function-name)
                                  (cancel-timer (symbol-value idle-timer))
                                  ;; (unintern idle-timer nil)
                                  (fmakunbound pause-function-name)
                                  (fmakunbound resume-function-name )
                                  (fmakunbound abort-function-name )))))

(defun auto-pause-pause-process (proc)
  "Pause PROC by send SIGSTOP signal. PROC should be subprocess of emacs"
  (when (processp proc) 
    (signal-process proc 'SIGSTOP)))

(defun auto-pause-resume-process (proc)
  "Resume PROC by send SIGCONT signal. PROC should be subprocess of emacs"
  (when (processp proc)
    (signal-process proc 'SIGCONT)))

(defun auto-pause-process-p (proc)
  "Return t if PROC is a auto-pause process"
  (and (processp proc)
       (process-get proc 'auto-pause-abort-function)))

(defun auto-pause--make-auto-pause-sentinel (sentinel)
  (lambda (proc event)
    (unwind-protect 
        (funcall sentinel proc event)
      (when (eq 'exit (process-status proc))
        (funcall (process-get proc 'auto-pause-abort-function))))))

(defun auto-pause--set-process-sentinel (proc sentinel)
  "Give PROC the sentinel SENTINEL, PROC should be an auto-pause process"
  (when (auto-pause-process-p proc)
    (set-process-sentinel proc (auto-pause--make-auto-pause-sentinel sentinel))))

(defun auto-pause--reset-process-sentinel (proc)
  "Reset the sentinel function of PROC which should be an auto-pause function"
  (auto-pause--set-process-sentinel proc (process-sentinel proc)))

(defun auto-pause-mark-process (proc delay-seconds)
  "Pause the PROC the next time Emacs is idle for DELAY-SECONDS, and resume the PROC when emacs become busy again"
  (process-put proc 'auto-pause-abort-function
               (auto-pause (lambda ()
                             (auto-pause-pause-process proc))
                 (lambda ()
                   (auto-pause-resume-process proc))
                 delay-seconds))
  (auto-pause--reset-process-sentinel proc)
  proc)

(defmacro with-auto-pause (delay-seconds &rest body)
  "Evalute BODY, if BODY created an asynchronous subprocess, it will be an auto-pause-process"
  (declare (debug t) (indent 1))
  `(progn
     (advice-add 'start-process
                 :filter-return
                 (lambda (proc)
                   (auto-pause-mark-process proc ,delay-seconds))
                 '((name "auto-pause-advise-start-process")))
     (advice-add 'set-process-sentinel
                 :filter-args
                 (lambda (args)
                   (let ((proc (car args))
                         (sentinel (cadr args)))
                     (if (auto-pause-process-p proc)
                         (list proc (auto-pause--make-auto-pause-sentinel sentinel))
                       (list proc sentinel))))
                 '((name "auto-pause-advise-set-process-sentinel")))
     (unwind-protect (progn ,@body)
       (advice-remove 'start-process  "auto-pause-advise-start-process")
       (advice-remove 'set-process-sentinel  "auto-pause-advise-set-process-sentinel"))))

(provide 'auto-pause)
;;; auto-pause.el ends here
