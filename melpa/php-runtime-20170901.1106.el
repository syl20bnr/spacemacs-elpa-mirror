;;; php-runtime.el --- Language binding bridge to PHP -*- lexical-binding: t -*-

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 28 Aug 2017
;; Version: 0.0.1
;; Package-Version: 20170901.1106
;; Keywords: processes php
;; URL: https://github.com/emacs-php/php-runtime.el
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;;; Commentary:

;; Execute PHP code.  This package requires `php' command in runtime.
;;
;;     (string-to-number (php-runtime-eval "echo PHP_INT_MAX;"))
;;     ;; => 9.223372036854776e+18
;;
;;     (string-to-number (php-runtime-expr "PHP_INT_MAX")) ; short hand
;;


;;; Code:
(require 'cl-lib)
(require 'eieio)

(defgroup php-runtime nil
  "Language binding bridge to PHP"
  :tag "PHP Runtime"
  :group 'processes
  :group 'php)

(defcustom php-runtime-php-executable (and (executable-find "php") "php")
  "A command name or path to PHP executable."
  :group 'php-runtime
  :type 'string)

(defconst php-runtime-php-open-tag "<?php ")

(defconst php-runtime-error-buffer-name "*PHP Error Messages*")

(defvar php-runtime--kill-temp-output-buffer t)


;; Utility functions
(defun php-runtime--temp-buffer ()
  "Return new temp buffer."
  (generate-new-buffer "*PHP temp*"))

(defun php-runtime--stdin-satisfied-p (obj)
  "Return t if the object `OBJ' is satisfied to stdin format.

for example, (get-buffer \"foo-buffer\"), '(:file . \"/path/to/file\")."
  (cond
   ((null obj) t)
   ((and (bufferp obj) (buffer-live-p obj) t))
   ((and (consp obj)
         (eq :file (car obj)) (stringp (cdr obj))) t)))

(defun php-runtime--code-satisfied-p (obj)
  "Return t if the object `OBJ' is satisfied to code format."
  (and (consp obj)
       (member (car obj) (list :file :string))
       (stringp (cdr obj))))


;; PHP Execute class

;;;###autoload
(defclass php-runtime-execute nil
  ((executable :initarg :executable :type string)
   (code   :initarg :code   :type (satisfies php-runtime--code-satisfied-p))
   (stdin  :initarg :stdin  :type (satisfies php-runtime--stdin-satisfied-p) :initform nil)
   (stdout :initarg :stdout :type (or null buffer-live list) :initform nil)
   (stderr :initarg :stderr :type (or null buffer-live list) :initform nil)))

(cl-defmethod php-runtime-run ((php php-runtime-execute))
  "Execute PHP process using `php -r' with code.

This execution method is affected by the number of character limit of OS command arguments.
You can check the limitation by command, for example \(shell-command-to-string \"getconf ARG_MAX\") ."
  (let ((args (list (php-runtime--get-command-line-arg php))))
    (if (and (oref php stdin) (not (php-runtime--stdin-by-file-p php)))
        (php-runtime--call-php-process-with-input-buffer php args)
      (php-runtime--call-php-process php args))))

(cl-defmethod php-runtime--call-php-process ((php php-runtime-execute) args)
  "Execute PHP Process by php-execute `PHP' and `ARGS'."
  (apply #'call-process (oref php executable)
         (php-runtime--get-input php) ;input
         (cons (php-runtime-stdout-buffer php)
               (oref php stderr))
         nil ; suppress display
         args))

(cl-defmethod php-runtime--call-php-process-with-input-buffer ((php php-runtime-execute) args)
  "Execute PHP Process with STDIN by php-execute `PHP' and `ARGS'."
  (unless (buffer-live-p (oref php stdin))
    (error "STDIN buffer is not available"))
  (with-current-buffer (oref php stdin)
    (apply #'call-process-region (point-min) (point-max)
           (oref php executable)
           nil ;delete
           (cons (php-runtime-stdout-buffer php)
                 (oref php stderr))
           nil ; suppress display
           args)))

(cl-defmethod php-runtime--get-command-line-arg ((php php-runtime-execute))
  "Return command line string"
  (let ((code (oref php code)))
    (cl-case (car code)
      (:file (cdr code))
      (:string (concat "-r" (cdr code))))))

(cl-defmethod php-runtime--stdin-by-file-p ((php php-runtime-execute))
  "Return T if \(oref php stdin) is file."
  (let ((stdin (oref php stdin)))
    (and (consp stdin)
         (eq :file (car stdin)))))

(cl-defmethod php-runtime--get-input ((php php-runtime-execute))
  ""
  (if (php-runtime--stdin-by-file-p php)
      (cdr (oref php stdin))
    nil))

(cl-defmethod php-runtime-stdout-buffer ((php php-runtime-execute))
  "Return output buffer."
  (let ((buf (oref php stdout)))
    (if (and buf (buffer-live-p buf))
        buf
      (oset php stdout (generate-new-buffer "*PHP output*")))))


;; PHP Execute wrapper function

;;;###autoload
(defun php-runtime-expr (php-expr &optional input-buffer)
  "Evalute and echo PHP expression `PHP-EXPR'.

Pass `INPUT-BUFFER' to PHP executable as STDIN."
  (php-runtime-eval (format "echo %s;" php-expr) input-buffer))

;;;###autoload
(defun php-runtime-eval (code &optional input-buffer)
  "Evalute PHP code `CODE' without open tag, and return buffer.

Pass `INPUT-BUFFER' to PHP executable as STDIN."
  (let ((execute (php-runtime-execute :code (cons :string code)
                           :executable php-runtime-php-executable
                           :stderr (get-buffer-create php-runtime-error-buffer-name)))
        (temp-input-buffer (when (and input-buffer (not (bufferp input-buffer)))
                             (php-runtime--temp-buffer))))
    (when input-buffer
      (oset execute stdin
            (if (or (bufferp input-buffer)
                    (and (consp input-buffer) (eq :file (car input-buffer))))
                input-buffer
              (prog1 temp-input-buffer
                (with-current-buffer temp-input-buffer
                  (insert input-buffer))))))

    (unwind-protect
        (progn (php-runtime-run execute)
               (with-current-buffer (php-runtime-stdout-buffer execute)
                 (buffer-substring-no-properties (point-min) (point-max))))
      (when (and temp-input-buffer (buffer-live-p temp-input-buffer))
        (kill-buffer temp-input-buffer))
      (when php-runtime--kill-temp-output-buffer
        (kill-buffer (php-runtime-stdout-buffer execute))))))

(provide 'php-runtime)
;;; php-runtime.el ends here
