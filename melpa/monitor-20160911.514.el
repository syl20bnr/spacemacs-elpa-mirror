;;; monitor.el --- Utilities for monitoring expressions.

;; Copyright (C) 2016 Ben Moon
;; Author: Ben Moon <guiltydolphin@gmail.com>
;; URL: https://github.com/guiltydolphin/monitor
;; Package-Version: 20160911.514
;; Git-Repository: git://github.com/guiltydolphin/monitor.git
;; Created: 2016-08-17
;; Version: 0.1.0
;; Keywords: lisp, monitor, utility
;; Package-Requires: ((dash "2.13.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Monitor provides utilities for monitoring expressions.
;; A predicate-based system is used to determine when to run
;; specific functions - not unlike Emacs' built-in hooks (see Info node `Hooks').
;;
;; For example, if we wanted to print "foo" every time the value
;; of (point) changed in the current buffer, we could write:
;;
;;    (monitor-expression-value (point) (lambda () (print "foo")))
;;
;; A (rather convoluted) way of mimicking the functionality of the
;; standard `after-change-major-mode-hook' could be to use the
;; following expression:
;;
;;    (monitor-expression-value major-mode (...))
;;
;; Which would run whenever the value of `major-mode' changed.

;;; Code:

(require 'dash)

;;; CORE

(defgroup monitor nil
  "Monitor expressions."
  :group 'lisp
  :prefix 'monitor-)

;;;###autoload
(define-minor-mode monitor-mode
  "Minor mode for monitoring expressions."
  :group 'monitor
  (if monitor-mode
      (add-hook 'post-command-hook 'monitor--check-monitored nil t)
    (remove-hook 'post-command-hook 'monitor--check-monitored t)))

;;;###autoload
(define-globalized-minor-mode monitor-global-mode monitor-mode monitor-mode
  :group 'monitor)

(defvar monitor--monitored nil
  "Monitored expressions.")

(defun monitor--monitor (pred &rest fns)
  "After `post-command-hook' check PRED for a non-NIL value.
If PRED evaluates to non-NIL, then run each function in FNS."
  (let ((exist-fns (cdr (assoc pred monitor--monitored))))
    (dolist (fn fns) (unless (member fn exist-fns) (push fn exist-fns)))
    (monitor--monitored-update-functions pred exist-fns)))

(defun monitor--monitored-update-functions (pred fns)
  "Update the functions of PRED to FNS.
If FNS is nil then this deletes the entry at PRED."
  (setq monitor--monitored (--reject (equal (car it) pred) monitor--monitored))
  (when fns (push (cons pred fns) monitor--monitored)))

(defun monitor--monitored-remove-function (pred &rest fns )
  "Remove from PRED, any functions `equal' to a member of FNS."
  (let ((exist-fns (cdr (assoc pred monitor--monitored))))
    (setq exist-fns (--reject (member it fns) exist-fns))
    (monitor--monitored-update-functions pred exist-fns)))

(defun monitor--monitored-remove-pred (pred)
  "Remove PRED from the monitored predicates."
  (monitor--monitored-update-functions pred nil))

(defun monitor--check-monitored ()
  "Check each monitored expression."
  (dolist (pexp (copy-alist monitor--monitored))
    (when (condition-case var (eval (car pexp))
            (error (progn (message "error when evaluating %s (got %s)" (car pexp) (error-message-string var))
                          (monitor--monitored-remove-pred (car pexp)) nil)))
      (dolist (f (cdr pexp))
        (condition-case var (funcall f)
          (error (progn (message "error when executing %s (got %s)" f (error-message-string var))
                        (monitor--monitored-remove-function (car pexp) f))))))))

;;; EXPRESSIONS

(defvar monitor--watched-expressions-global nil
  "Alist of expressions known to monitor.
Values are the last known values.")

(defvar monitor--watched-expressions-buffer nil
  "Alist of variables known to monitor in the current buffer.
Values are the last known values.")
(make-variable-buffer-local 'monitor--watched-expressions-buffer)

(defun monitor--update-watched-expression (var val &optional local)
  "Update the known value of VAR to VAL.
Update the buffer-local value if LOCAL is non-nil."
  (let ((monitor-var (monitor--watched-expressions-var local)))
    (eval `(setq ,monitor-var (assq-delete-all var ,monitor-var)))
    (eval `(push (cons var val) ,monitor-var))))

(defun monitor--watched-expressions-var (&optional local)
  "Return the correct watch variables based on whether LOCAL is non-nil."
  (intern (format "monitor--watched-expressions-%s" (if local 'buffer 'global))))

(defun monitor--watched-expressions-last-value (var &optional local)
  "Return the last known value of VAR.
Return the buffer-local version if LOCAL is non-nil."
  (eval `(cdr (assoc var ,(monitor--watched-expressions-var local)))))

(defun monitor--monitor-in-mode (pred mode &rest fns)
  "Monitor PRED in `major-mode' MODE.
If MODE is nil then no mode restriction is applied.
FNS are run as in `monitor--monitor'."
  (let ((mode-pred (if mode `(and (eq major-mode ',mode) ,pred) pred)))
    (apply 'monitor--monitor mode-pred fns)))

(defun monitor--expression-value-changed (expr &optional local)
  "Check for a change in value of EXPR.
Update the known value of EXPR if it has changed.
If LOCAL is non-nil, check the buffer-local value."
  (let ((old-val (monitor--watched-expressions-last-value expr local))
        (new-val (eval expr)))
    (unless (eq old-val new-val)
      (monitor--update-watched-expression expr new-val local) t)))

(defun monitor--monitor-expression-value (expr fn &optional mode local)
  "Monitor EXPR and run FN if it's value is changed.
Optional MODE should specify a `major-mode'.
If LOCAL is non-nil then monitor the buffer-local value."
  (monitor--monitor-in-mode `(monitor--expression-value-changed ',expr ,local) mode fn))

;;;###autoload
(defmacro monitor-expression-value (expr fn &optional mode local)
  "Monitor EXPR and run FN if it's value is changed.
Optional MODE should specify a `major-mode'.
If LOCAL is non-nil then monitor the buffer-local value."
  (monitor--monitor-expression-value expr fn mode local))

(provide 'monitor)
;;; monitor.el ends here
