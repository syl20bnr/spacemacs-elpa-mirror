;;; sendto.el --- send the region content to a function -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 DarkSun <lujun9972@gmail.com>

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-4-22
;; Version: 0.1
;; Package-Version: 20160425.550
;; Keywords: convenience, region
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/lujun9972/sendto.el

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
;; sendto's code can be found here:
;;   http://github.com/lujun9972/sendto.el

;;; Commentary:

;; sendto is a little tool that make me convenience to call some functions with region content

;; Quick start:

;; 1. specify ~sendto-function-list~

;; The value should be a list of functions which accept a string.or a region(point and mark as 2 numeric args, smallest first)

;; 2. turn on sendto-mode: ~M-x sendto-mode~

;; 3. mark a region, and then click mouse-3, It will popup a menu

;; 4. select the function you want

;;; Code:

(defun sendto--generate-menu-fn (fn)
  "Generate a command used in sendto-menu keymap by FN."
  (lambda ()
    "Auto generated command by sendto"
    (interactive)
    (let* ((start (region-beginning))
           (end (region-end))
           (content (buffer-substring start end)))
      (condition-case nil
          (funcall fn content)
        ((debug wrong-number-of-arguments wrong-type-argument) (funcall fn start end))))))

(defun sendto--generate-menu-item (fn)
  "Generate menu item by FN."
  (vector (symbol-name fn) (sendto--generate-menu-fn fn)))

(defun sendto--generate-menu (&rest functions)
  "Generate menu configuration by FUNCTIONS."
  (cons "sendto" (mapcar #'sendto--generate-menu-item functions)))

(defun sendto-file (content)
  "Append CONTENT to a file."
  (let ((file (read-file-name "append to which file: ")))
    (append-to-file content nil file)))

(defun sendto-buffer (content)
  "Append CONTENT to a buffer."
  (with-current-buffer (read-buffer "append to which buffer: ")
    (save-excursion
      (goto-char (point-max))
      (insert content))))

(defun sendto-mail (content)
  "Mail the CONTENT."
  (mail)
  (goto-char (point-max))
  (insert content))

(defun sendto-appt (content)
  "Add an appointment for today at sometime with message CONTENT."
  (let ((time (read-string "Time (hh:mm[am/pm]): ")))
    (appt-add time content)
    (appt-activate 1)))

(defgroup sendto nil
  "Send content of region to functions")

(defcustom sendto-function-list '(sendto-file sendto-buffer sendto-mail sendto-appt)
  "Functions to be send to."
  :group 'sendto
  :type '(repeat function)
  :set (lambda (item val)
         (set-default item val)
         (when (and (boundp 'sendto-mode)
                    sendto-mode)
           (easy-menu-define sendto-menu nil "Menu for sendto" (apply #'sendto--generate-menu val)))))

(defun sendto-popup-functions (&rest functions)
  "Popup a menu with FUNCTIONS as menu items."
  (unless (and (boundp 'sendto-menu)
              (keymapp sendto-menu))
    (easy-menu-define sendto-menu nil "Menu for sendto" (apply #'sendto--generate-menu functions)))
  (popup-menu sendto-menu))

;;;###autoload
(defun sendto-popup ()
  "Pop up a sendto menu."
  (interactive)
  (apply #'sendto-popup-functions sendto-function-list))

(define-minor-mode sendto-mode "send region content to a function"
  ;; The initial value
  nil
  ;; The indicator for the mode line
  " Sendto"
  :group 'sendto
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<mouse-3>") 'sendto-popup)
            map))


(provide 'sendto)

;;; sendto.el ends here
