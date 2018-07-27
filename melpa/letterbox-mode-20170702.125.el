;;; letterbox-mode.el --- hide sensitive text on a buffer -*- lexical-binding: t; -*-

;; Author: Fernando Leboran <f.leboran@gmail.com>
;; URL: http://github.com/pacha64/letterbox-mode
;; Package-Version: 20170702.125
;; Version: 0.3
;; Keywords: password, convenience
;; URL: http://www.github.com/pacha64/letterbox-mode
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 2017 Fernando Leboran
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Letterbox-mode is a simple minor mode to add letterboxing to sensitive text.
;;
;; Select a region of text you want to censor and call M-x
;; letterbox-add, the selected region will be letterboxed/censored.
;; Call M-x letterbox-remove to remove all letterboxes, or M-x to
;; toggle the letterboxes (without removing them).  Some use cases:
;;
;;	 This is a buffer with some sensitive information. 123456 is
;;	 the password for my bank account, and qwerty is my account
;;	 name. please don't read it.
;;
;; Select "123456" and call letterbox-add, you will see that part
;; letterboxed:
;;
;;	 This is a buffer with some sensitive information. ██████ is
;;	 the password for my bank account, and qwerty is my account
;;	 name. please don't read it.
;;
;; Select "qwerty" and call letterbox-add again, you will see that
;; part letterboxed as well:
;;
;;	 This is a buffer with some sensitive information. ██████ is
;;	 the password for my bank account, and ██████ is my account
;;	 name. please don't read it.
;;
;; Call letterbox-toggle to hide/show the sensitive text, or
;; letterbox-remove to remove them.

;;; Code:

;;;###autoload
(define-minor-mode letterbox-mode
  "Mode for hiding sensitive text on a buffer.

After activating this mode in current buffer, select region and
press <C-x l> to letterbox, press <C-x t> to toggle letterbox
visibility, press <C-x d> to remove all active letterboxes."
  :init-value nil
  :lighter " Letterbox"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a") 'letterbox-add)
            (define-key map (kbd "C-c t") 'letterbox-toggle)
            (define-key map (kbd "C-c r") 'letterbox-remove)
            map)
  (setq letterbox-current-text nil)
  (setq letterbox-is-visible t)
  (remove-overlays (point-min) (point-max) 'category 'letterbox))

(defvar letterbox-current-text nil)
(defvar letterbox-is-visible t)

(defgroup letterbox nil
  "Letterbox text in current buffer"
  :group 'convenience)

(defface letterbox-face
  '(t (:background (face-attribute 'default :foreground)
       :foreground (face-attribute 'default :foreground)))
  "Letterbox mode face."
  :group 'letterbox)

;;;###autoload
(defun letterbox-add()
  (interactive)
  (if (and mark-active (not (= 0 (- (region-beginning) (region-end)))))
      (progn (add-to-list 'letterbox-current-text
                          (list (region-beginning) (region-end))
                          t)
             (deactivate-mark)
             (unless letterbox-is-visible
               (letterbox-toggle))
             (letterbox-refresh-overlays))
    (message "No region active to add text to a letterbox.")))

;;;###autoload
(defun letterbox-remove()
  (interactive)
  (setq letterbox-current-text nil)
  (letterbox-refresh-overlays)
  (message "All letterboxes removed"))

;;;###autoload
(defun letterbox-toggle()
  (interactive)
  (setq letterbox-is-visible (not letterbox-is-visible))
  (if letterbox-is-visible
      (message "Letterbox enabled")
    (message "Letterbox disabled"))
  (letterbox-refresh-overlays))

(defun letterbox-refresh-overlays()
  (if (and letterbox-is-visible letterbox-current-text)
      (dolist (letterbox-list letterbox-current-text)
        (let ((ov (make-overlay (car letterbox-list)
                                (cadr letterbox-list))))
          (overlay-put ov 'category 'letterbox)
          (overlay-put ov 'face 'letterbox-face)))
    (remove-overlays (point-min) (point-max) 'category 'letterbox)))

(provide 'letterbox-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; letterbox-mode.el ends here
