;;; emms-mode-line-cycle.el --- Display the emms mode line as a ticker -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 momomo5717

;; Keywords: emms, mode-line
;; Package-Version: 20160221.320
;; Version: 0.2.5
;; Package-Requires: ((emacs "24") (emms "4.0"))
;; Author: momomo5717
;; URL: https://github.com/momomo5717/emms-mode-line-cycle

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

;;  This is a minor mode for updating `emms-mode-line-string' cyclically within specified width
;;  with `emms-playing-time-display'.
;;
;;  It is useful for long track titles.
;;
;; Further information is available from:
;; https://github.com/momomo5717/emms-mode-line-cycle  (README.org)
;;

;; Setup:
;;
;; (add-to-list 'load-path "/path/to/emms-mode-line-cycle")
;; (require 'emms-mode-line-cycle)
;;
;; (emms-mode-line 1)
;; (emms-playing-time 1)
;;
;; ;; `emms-mode-line-cycle' can be used with emms-mode-line-icon.
;; (require 'emms-mode-line-icon)
;; (custom-set-variables '(emms-mode-line-cycle-use-icon-p t))
;;
;; (emms-mode-line-cycle 1)
;;
;; User Option:
;;
;;  + `emms-mode-line-cycle-max-width'
;;  + `emms-mode-line-cycle-any-width-p'
;;  + `emms-mode-line-cycle-additional-space-num'
;;  + `emms-mode-line-cycle-use-icon-p'
;;  + `emms-mode-line-cycle-current-title-function'
;;  + `emms-mode-line-cycle-velocity'
;;

;;; Code:
(require 'emms-mode-line)
(require 'emms-playing-time)

(defgroup emms-mode-line-cycle nil
  "Update `emms-mode-line-string' cyclically with `emms-playing-time-display'."
  :prefix "emms-mode-line-cycle-"
  :group 'emms-mode-line)

(defcustom emms-mode-line-cycle-max-width 16
  "Max width of display title."
  :type 'integer)

(defcustom emms-mode-line-cycle-additional-space-num 2
  "The number of space characters to add them to the current title."
  :type 'integer)

(defcustom emms-mode-line-cycle-any-width-p nil
  "Rotate title which is less than `emms-mode-line-cycle-max-width'."
  :type 'boolean)

(defcustom emms-mode-line-cycle-use-icon-p nil
  "Use icon like `emms-mode-line-icon-function'.
This feature depends on emms-mode-line-icon."
  :type 'boolean)

(defcustom emms-mode-line-cycle-current-title-function
  (lambda () (emms-track-description
          (emms-playlist-current-selected-track)))
  "Getter function for the current track title.
Its function returns a stirng."
  :type 'function)

(defcustom emms-mode-line-cycle-velocity 1
  "Number of characters per `emms-mode-line-cycle-update-mode-line-string'."
  :type 'integer)

(defvar emms-mode-line-cycle) ; Suppress a warning message
(defvar emms-mode-line-icon-before-format)
(defvar emms-mode-line-icon-image-cache)

(defvar emms-mode-line-cycle--title ""
  "The current track title.")

(defvar emms-mode-line-cycle--title-width 0
  "Width of the current track title via `string-width'.")

(defvar emms-mode-line-cycle--get-title-cache-function (lambda (&optional _n) "")
  "Getter function for the current title cache.")

(defun emms-mode-line-cycle--rotate-title-p ()
  "Return t if title can be rotated."
  (or emms-mode-line-cycle-any-width-p
      (> emms-mode-line-cycle--title-width emms-mode-line-cycle-max-width)))

(defun emms-mode-line-cycle--substring (str &optional width)
  "Substring STR with `emms-mode-line-cycle-max-width'.
WIDTH is string width."
  (truncate-string-to-width
   str (or width emms-mode-line-cycle-max-width) 0 ? ))

(defun emms-mode-line-cycle--make-title-queue (title)
  "Return a queue of TITLE."
  (let ((char-ls (nconc (string-to-list title)
                        (make-list emms-mode-line-cycle-additional-space-num
                                   ? )))
        (queue (cons nil nil)))
    (setcar queue (last (setcdr queue char-ls)))
    queue))

(defun emms-mode-line-cycle--rotate-queue (queue)
  "Rotate QUEUE."
  (unless (null (cdr queue))
    (let ((head (cadr queue)))
      (setcdr queue (cddr queue))
      (when (null (cdr queue)) (setcar queue queue))
      (setcar queue (setcdr (car queue) (cons head nil)))))
  queue)

(defun emms-mode-line-cycle--make-title-cache (queue &optional width)
  "Make title cache of QUEUE.
WIDTH is string width."
  (let* ((len (length (cdr queue)))
         (v (make-vector len nil)))
    (dotimes (i len)
      (aset v i (emms-mode-line-cycle--substring (apply #'string (cdr queue)) width))
      (setq queue (emms-mode-line-cycle--rotate-queue queue)))
    v))

(defun emms-mode-line-cycle--make-title-cache-getter (queue &optional width)
  "Make getter function for title cache of QUEUE.
WIDTH is string width."
  (let* ((len (length (cdr queue)))
         (count 0)
         (title-cache (emms-mode-line-cycle--make-title-cache queue width)))
    (if (zerop len) (lambda (&optional _n) "")
      (lambda (&optional n)
        (aref title-cache (setq count (mod (+ count (or n 1)) len)))))))

(defun emms-mode-line-cycle--get-title-cache (&optional n)
  "Return title cache rotated N times.
If N is nil, title cache is not rotated."
  (if (functionp emms-mode-line-cycle--get-title-cache-function)
      (funcall emms-mode-line-cycle--get-title-cache-function (or n 0))
    ""))

;;;###autoload
(defun emms-mode-line-cycle-get-title (&optional n)
  "Retrun title or title cache rotated N times.
If N is nil, title cache is not rotated."
  (if (emms-mode-line-cycle--rotate-title-p)
      (emms-mode-line-cycle--get-title-cache n)
    emms-mode-line-cycle--title))

(defun emms-mode-line-cycle--initialize (title)
  "Initialize emms-mode-line-cycle's global variables to the TITLE."
  (setq emms-mode-line-cycle--title title
        emms-mode-line-cycle--title-width (string-width title)
        emms-mode-line-cycle--get-title-cache-function
        (emms-mode-line-cycle--make-title-cache-getter
         (emms-mode-line-cycle--make-title-queue title)
         (min emms-mode-line-cycle-max-width emms-mode-line-cycle--title-width))))

(defun emms-mode-line-cycle-clear ()
  "Clear `emms-mode-line-cycle--title'."
  (setq emms-mode-line-cycle--title ""
        emms-mode-line-cycle--title-width 0
        emms-mode-line-cycle--get-title-cache-function (lambda (&optional _n) "")))

(defun emms-mode-line-cycle--playlist-current (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-playlist-current'.
If INITIALP is no-nil, initialized."
  (when initialp
    (emms-mode-line-cycle--initialize
     (or title (funcall emms-mode-line-cycle-current-title-function))))
  (format emms-mode-line-format
          (emms-mode-line-cycle-get-title (unless initialp emms-mode-line-cycle-velocity))))

(defun emms-mode-line-cycle--icon-function (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-icon-function'.
If INITIALP is no-nil, initialized."
  (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-cycle--playlist-current title initialp)))

;;;###autoload
(defun emms-mode-line-cycle-mode-line-function (&optional title)
  "This is used as `emms-mode-line-mode-line-function'.
If TITLE is no-nil, it is set to emms-mode-line-cycle's global variables."
  (if emms-mode-line-cycle-use-icon-p
      (emms-mode-line-cycle--icon-function title t)
    (emms-mode-line-cycle--playlist-current title t)))

;;;###autoload
(defun emms-mode-line-cycle-update-mode-line-string (&rest _)
  "Update `emms-mode-line-string', if `emms-mode-line-cycle' is non-nil.
This can be used as a before/after advice."
  (when (and emms-mode-line-cycle
             (emms-mode-line-cycle--rotate-title-p))
    (setq emms-mode-line-string
          (if emms-mode-line-cycle-use-icon-p
              (emms-mode-line-cycle--icon-function)
            (emms-mode-line-cycle--playlist-current)))))

(unless (require 'nadvice nil t)
  (defadvice emms-playing-time-display
      (before emms-mode-line-cycle-advice-before activate)
    (emms-mode-line-cycle-update-mode-line-string)))

;;;###autoload
(define-minor-mode emms-mode-line-cycle
  "Update `emms-mode-line-string' cyclically with `emms-playing-time-display'."
  :global t
  (if emms-mode-line-cycle
      (progn
        (unless (eq emms-mode-line-mode-line-function
                    'emms-mode-line-cycle-mode-line-function)
          (put 'emms-mode-line-cycle-mode-line-function :default-mode-line-function
               emms-mode-line-mode-line-function)
          (setq emms-mode-line-mode-line-function
                'emms-mode-line-cycle-mode-line-function))
        (when (fboundp 'advice-add)
          (advice-add 'emms-playing-time-display :before
                      #'emms-mode-line-cycle-update-mode-line-string))
        (add-hook 'emms-player-finished-hook 'emms-mode-line-cycle-clear)
        (add-hook 'emms-player-stopped-hook 'emms-mode-line-cycle-clear))
    (when (eq emms-mode-line-mode-line-function
              'emms-mode-line-cycle-mode-line-function)
      (setq emms-mode-line-mode-line-function
            (or (get 'emms-mode-line-cycle-mode-line-function :default-mode-line-function)
                emms-mode-line-mode-line-function))
      (put 'emms-mode-line-cycle-mode-line-function :default-mode-line-function
           nil))
    (when (fboundp 'advice-remove)
      (advice-remove 'emms-playing-time-display
                     #'emms-mode-line-cycle-update-mode-line-string))
    (remove-hook 'emms-player-finished-hook 'emms-mode-line-cycle-clear)
    (remove-hook 'emms-player-stopped-hook 'emms-mode-line-cycle-clear)))

(provide 'emms-mode-line-cycle)
;;; emms-mode-line-cycle.el ends here
