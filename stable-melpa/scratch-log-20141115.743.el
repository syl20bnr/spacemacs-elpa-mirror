;;; scratch-log.el --- Utility for *scratch* buffer.
;; Package-Version: 20141115.743

;; Copyright (C) 2010 by kmori

;; Author: kmori <morihenotegami@gmail.com>
;; Prefix: sl-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Installation:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'scratch-log)

;;; Change Log:

;; 0.0.1: scratch-log.el 0.0.1 released.

;;; Code:

(eval-when-compile
  (require 'cl))


(defvar sl-scratch-log-file "~/.emacs.d/.scratch-log")
(defvar sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
(defvar sl-restore-scratch-p t)
(defvar sl-prohibit-kill-scratch-buffer-p t)
(defvar sl-use-timer t)
(defvar sl-timer-interval 30 "*Seconds of timer interval.")


;; utility
(defmacro sl-aif (test-form then-form &rest else-forms)
  (declare (indent 2))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro* sl-awhen (test-form &body body)
  (declare (indent 1))
  `(sl-aif ,test-form
       (progn ,@body)))


;; main
(defun sl-dump-scratch-when-kill-buf ()
  (interactive)
  (when (string= "*scratch*" (buffer-name))
    (sl-make-prev-scratch-string-file)
    (sl-append-scratch-log-file)))

(defun sl-dump-scratch-when-kill-emacs ()
  (interactive)
  (sl-awhen (get-buffer "*scratch*")
    (with-current-buffer it
      (sl-make-prev-scratch-string-file)
      (sl-append-scratch-log-file))))

(defun sl-dump-scratch-for-timer ()
  (interactive)
  (if (sl-need-to-save)
      (sl-awhen (get-buffer "*scratch*")
        (with-current-buffer it
          (sl-make-prev-scratch-string-file)))))

(defun sl-need-to-save ()
  (sl-awhen (get-buffer "*scratch*")
    (let ((scratch-point-max (with-current-buffer it (point-max))))
      (with-temp-buffer
        (insert-file-contents sl-prev-scratch-string-file)
        (or (not (eq (point-max) scratch-point-max))
            (not (eq (compare-buffer-substrings
                      (current-buffer) 1 (point-max)
                      it 1 scratch-point-max)
                     0)))))))

(defun sl-make-prev-scratch-string-file ()
  (write-region (point-min) (point-max) sl-prev-scratch-string-file nil 'nomsg))

(defun sl-append-scratch-log-file ()
  (let* ((time (format-time-string "* %Y/%m/%d-%H:%m" (current-time)))
         (buf-str (buffer-substring-no-properties (point-min) (point-max)))
         (contents (concat "\n" time "\n" buf-str)))
    (with-temp-buffer
      (insert contents)
      (write-region (point-min) (point-max) sl-scratch-log-file t 'nomsg))))

(defun sl-restore-scratch ()
  (interactive)
  (when sl-restore-scratch-p
    (with-current-buffer "*scratch*"
      (erase-buffer)
      (when (file-exists-p sl-prev-scratch-string-file)
        (insert-file-contents sl-prev-scratch-string-file)))))

(defun sl-scratch-buffer-p ()
  (if (string= "*scratch*" (buffer-name)) nil t))

(add-hook 'kill-buffer-hook 'sl-dump-scratch-when-kill-buf)
(add-hook 'kill-emacs-hook 'sl-dump-scratch-when-kill-emacs)
(add-hook 'emacs-startup-hook 'sl-restore-scratch)
(when sl-prohibit-kill-scratch-buffer-p
  (add-hook 'kill-buffer-query-functions 'sl-scratch-buffer-p))
(when sl-use-timer
  (run-with-idle-timer sl-timer-interval t 'sl-dump-scratch-for-timer))

;;;; Bug report
(defvar scratch-log-maintainer-mail-address
  (concat "morihen" "otegami@gm" "ail.com"))
(defvar scratch-log-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of scratch-log.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"scratch-log.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun scratch-log-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   scratch-log-maintainer-mail-address
   "scratch-log.el"
   (apropos-internal "^eldoc-" 'boundp)
   nil nil
   scratch-log-bug-report-salutation))

(provide 'scratch-log)

;;; scratch-log.el ends here
