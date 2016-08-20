;;; smart-shift.el --- Smart shift text left/right.

;; Copyright © 2014 Bin Huang <huangbin88@foxmail.com>

;; Author: Bin Huang <huangbin88@foxmail.com>
;; Maintainer: Bin Huang <huangbin88@foxmail.com>
;; URL: https://github.com/hbin/smart-shift
;; Package-Version: 20150202.2325
;; Created: 5th Jun 2014
;; Version: 0.3
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; `smart-shift' make it easy to shift current line or region to
;; left/right according to current major mode indentation.

;;; Code:

(defgroup smart-shift nil
  "Shift line/region by inferred indentation level."
  :prefix "smart-shift-"
  :group 'convenience)

(defcustom smart-shift-mode-alist
  '((lisp-mode . lisp-body-indent)
    (emacs-lisp-mode . lisp-body-indent)

    ;; Modes directly supported by CC Mode
    (c-mode . c-basic-offset)
    (c++-mode . c-basic-offset)
    (objc-mode . c-basic-offset)
    (java-mode . c-basic-offset)
    (idl-mode . c-basic-offset)
    (pike-mode . c-basic-offset)
    (awk-mode . c-basic-offset)

    (ruby-mode . ruby-indent-level)
    (python-mode . python-indent-offset)
    (swift-mode . swift-indent-offset)

    (js-mode . js-indent-level)
    (js2-mode . js2-basic-offset)
    (coffee-mode . coffee-tab-width)

    (css-mode . css-indent-offset)
    (slim-mode . slim-indent-offset)
    (html-mode . sgml-basic-offset)
    (web-mode . (lambda ()
                  (cond ((string= web-mode-content-type "css")
                         web-mode-css-indent-offset)
                        ((member web-mode-content-type '("javascript" "json" "jsx" "php"))
                         web-mode-code-indent-offset)
                        (t web-mode-markup-indent-offset)))) ; xml, html, etc...

    (sh-mode . sh-basic-offset)
    (yaml-mode . yaml-indent-offset)
    (text-mode . tab-width)
    (fundamental-mode . tab-width))
  "Alist which maps major modes to its indentation-level."
  :type '(repeat (cons (symbol :tag "Major mode name")
                       (choice (function :tag "Method evaluting to a number")
                               (integer :tag "Indentation level"
                                        :value tab-width))))
  :group 'smart-shift)

(defvar smart-shift-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-C <left>") 'smart-shift-left)
    (define-key map (kbd "C-C <right>") 'smart-shift-right)
    (define-key map (kbd "C-C <up>") 'smart-shift-up)
    (define-key map (kbd "C-C <down>") 'smart-shift-down)
    map))

(defun smart-shift-override-local-map ()
  "Override local key map for continuous indentation."
  (setq overriding-local-map
        (let ((map (copy-keymap smart-shift-mode-map)))
          (define-key map (kbd "<left>") 'smart-shift-left)
          (define-key map (kbd "<right>") 'smart-shift-right)
          (define-key map (kbd "<up>") 'smart-shift-up)
          (define-key map (kbd "<down>") 'smart-shift-down)
          (define-key map [t] 'smart-shift-pass-through) ;done with shifting
          map))
  (message (propertize "Still in smart-shift key chord..."
                       'face 'error)))

(defvar smart-shift-indentation-level nil
  "Variable used to specify the indentation-level for the current buffer.")
(make-variable-buffer-local 'smart-shift-indentation-level)

(defun smart-shift-infer-indentation-level ()
  "Infer indentation-level of current major mode."
  (let ((offset (assoc-default major-mode smart-shift-mode-alist
                               (lambda (k v)
                                 (derived-mode-p k)))))
    (cond ((numberp offset) offset)
          ((functionp offset) (funcall offset))
          ((symbolp offset) (symbol-value offset))
          (t tab-width))))

(defun smart-shift-mode-on ()
  "Turn on smart-shift mode."
  (smart-shift-mode 1))

(defun smart-shift-mode-off ()
  "Turn off smart-shift mode."
  (smart-shift-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line or Region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smart-indent-lines (step)
  (and (not (integerp step))
       (error "smart-indent-lines's argument STEP should be an integer! step = %s"
              step))
  (let ((beg (if (use-region-p)
                 (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position))
               (line-beginning-position)))
        (end (if (use-region-p)
                 (save-excursion
                   (goto-char (region-end))
                   (line-end-position))
               (line-end-position))))
    (indent-rigidly beg end step)))

(defun smart-shift-lines (step)
  "Move the current line or region to STEP lines forwardly. Negative value of
STEP means move backwardly. Notice: It won't modify `kill-ring'."
  (and (not (integerp step))
       (error "smart-shift-lines's argument STEP should be an integer! step = %s"
              step))
  ;; There're two situation:
  ;;
  ;; (point) ---------------
  ;; ---------------- (mark)
  ;;
  ;; or
  ;;
  ;; (mark) ----------------
  ;; --------------- (point)
  ;;
  ;; So here are the point-excursion and mark-excursion.
  (let* ((beg (if (use-region-p)
                  (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position 1))
                (line-beginning-position 1)))
         (end (if (use-region-p)
                  (save-excursion
                    (goto-char (region-end))
                    (line-beginning-position 2))
                (line-beginning-position 2)))
         (point-excursion (- (point) end))
         (mark-excursion (- (mark) (point)))
         (text (delete-and-extract-region beg end)))
    ;; Shift text.
    (forward-line step)
    (insert text)
    ;; Set new point.
    (goto-char (+ (point) point-excursion))
    ;; Set new mark.
    (when (use-region-p)
      (push-mark (+ (point) mark-excursion) t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun smart-shift-right (&optional arg)
  "Shift the line or region to the ARG times to the right."
  ;; TODO: Think about what things can shift right for convenient.
  (interactive "P")
  (let ((deactivate-mark nil)
        (times (cond ((equal arg nil) 1)  ; universal-argument not called
                     ((equal arg '(4)) 4) ; C-u
                     (t arg)))            ; all other cases
        (shift (or smart-shift-indentation-level
                   (smart-shift-infer-indentation-level)
                   tab-width)))
    (smart-indent-lines (* times shift))
    (smart-shift-override-local-map)))

;;;###autoload
(defun smart-shift-left (&optional arg)
  "Shift the line or region to the ARG times to the left."
  ;; TODO: Think about what things can shift left for convenient.
  (interactive "P")
  (let ((deactivate-mark nil)
        (times (cond ((equal arg nil) 1)  ; universal-argument not called
                     ((equal arg '(4)) 4) ; C-u
                     (t arg)))            ; all other cases
        (shift (or smart-shift-indentation-level
                   (smart-shift-infer-indentation-level)
                   tab-width)))
    (smart-indent-lines (* -1 (* times shift)))
    (smart-shift-override-local-map)))

;;;###autoload
(defun smart-shift-up (&optional arg)
  "Shift current line or region to the ARG lines backwardly."
  ;; TODO: Think about what things can shift up for convenient.
  (interactive "P")
  (let ((deactivate-mark nil))
    (smart-shift-lines (* -1 (cond ((equal arg nil) 1)
                                   ((equal arg '(4)) 4)
                                   (t arg))))
    (smart-shift-override-local-map)))

;;;###autoload
(defun smart-shift-down (&optional arg)
  "Shift current line or region to the ARG lines forwardly."
  ;; TODO: Think about what things can shift down for convenient.
  (interactive "P")
  (let ((deactivate-mark nil))
    (smart-shift-lines (cond ((equal arg nil) 1)
                             ((equal arg '(4)) 4)
                             (t arg)))
    (smart-shift-override-local-map)))

;;;###autoload
(defun smart-shift-pass-through ()
  "Finish shifting and invoke the corresponding command."
  (interactive)
  (setq overriding-local-map nil)
  (let* ((keys (progn
                 (setq unread-command-events
                       (append (this-single-command-raw-keys)
                               unread-command-events))
                 (read-key-sequence-vector "")))
         (command (and keys (key-binding keys))))
    (when (commandp command)
      (call-interactively command)))
  (message "Exit smart-shift key chord!"))

;;;###autoload
(define-minor-mode smart-shift-mode
  "Shift line/region to left/right."
  :lighter ""
  :keymap smart-shift-mode-map)

;;;###autoload
(define-globalized-minor-mode global-smart-shift-mode
  smart-shift-mode smart-shift-mode-on)

(provide 'smart-shift)

;;; smart-shift.el ends here
