;;; sackspace.el --- A better backspace

;; Copyright (C) 2010--2013 Michael Markert
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Created: 2010/08/01
;; Version: 0.8.2
;; Package-Version: 20130719.956

;; Keywords: delete convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Install:
;; Put file into load-path and (require 'sackspace).
;;
;; Usage:
;; (global-sackspace-mode 1)
;; See (describe-function 'sackspace-mode)
;;
;; Support for other packages:
;; * Supports `subword-mode`, and `paredit-mode`.
;;   To disable this support change the "honor" customs in
;;   `M-x customize-group RET sackspace RET`.
;;
;; * Supports `evil` directly by prohibiting edits in non-editing states if evil is enabled.
;;
;; Limitations:
;; * Within `term-mode` `sacks/whitespace` won't work (it will delete the chars
;;   from the emacs buffer, but not from the terminal).
;; * C-<backspace> and S-<backspace> are not available in a terminal emacs

;; Homepage: http://github.com/cofi/sackspace.el
;; Git-Repository: git://github.com/cofi/sackspace.el.git

;; Known Bugs:
;; See http://github.com/cofi/sackspace.el/issues

;;; Code:

(defgroup sackspace nil
  "A better backspace."
  :tag "Sackspace"
  :link '(url-link :tag "Homepage" "http://github.com/cofi/sackspace.el")
  :prefix "sack/"
  :group 'convenience)

(defvar sack/map (let ((map (make-sparse-keymap "Sackspace")))
                   (define-key map (kbd "<backspace>")   'sack/tabstop)
                   (define-key map (kbd "DEL")           'sack/tabstop)
                   (define-key map (kbd "C-<backspace>") 'sack/word)
                   (define-key map (kbd "M-<backspace>") 'sack/plain)
                   (define-key map (kbd "M-DEL")         'sack/plain)
                   (define-key map (kbd "S-<backspace>") 'sack/whitespace)
                   map)
  "Sackspace mode keymap.")

(defcustom sack/backward-word #'backward-kill-word
  "Function to use for removing a word backward."
  :type 'function
  :group 'sackspace)

(defcustom sack/honor-subword t
  "If sackspace should follow `subword-mode'."
  :type 'boolean
  :group 'sackspace)

(defcustom sack/honor-paredit t
  "If sackspace should follow function `paredit-mode'."
  :type 'boolean
  :group 'sackspace)

(defcustom sack/ignored-modes '(
                                minibuffer-inactive-mode
                                calc-mode
                                )
  "Modes where `sackspace-global-mode' should not turn on."
  :type '(repeat symbol)
  :group 'sackspace)

;;;###autoload
(define-minor-mode sackspace-mode
  "Reasonable bindings around the backspace key.

  \\{sack/map}"
  :lighter " sack"
  :group 'sackspace
  :keymap sack/map)

;;;###autoload
(defun turn-on-sackspace ()
  "Turn on sackspace."
  (interactive)
  (unless (member major-mode sack/ignored-modes)
    (sackspace-mode t)))

;;;###autoload
(define-globalized-minor-mode sackspace-global-mode
  sackspace-mode
  turn-on-sackspace)

(defun sack/word (&optional count)
  "Kill `COUNT' words.
Honors `subword-mode' (if enabled).
Works for `term-mode'"
  (interactive "p")
  (sack--protect-evil count
    (if (eq major-mode 'term-mode)
        (dotimes (_ (or count 1))
          (term-send-backward-kill-word))
      (if (and sack/honor-subword (bound-and-true-p subword-mode))
          (subword-backward-kill count)
        (funcall sack/backward-word count)))))

(defun sack/plain (&optional count)
  "Delete `COUNT' chars.
Works for `term-mode'."
  (interactive "p")
  (sack--protect-evil count
    (if (eq major-mode 'term-mode)
        (dotimes (_ (or count 1))
          (term-send-backspace))
      (backward-delete-char (or count 1)))))

(defun sack/plain-space (&optional count)
  "Delete `COUNT' chars (untabify tabs before)."
  (interactive "p")
  (if (eq major-mode 'term-mode)
      (dotimes (_ (or count 1))
        (term-send-backspace))
    (backward-delete-char-untabify (or count 1))))

(defun sack/tabstop (&optional count)
  "Delete preceding space or chars.
Delete up to `COUNT' times `tab-width' preceding spaces.
On preceding non-space delete up to `count' chars.
Honors paredit (if enabled).
In `term-mode' will only delete one char."
  (interactive "p")
  (sack--protect-evil count
    (if (eq major-mode 'term-mode)
        (dotimes (_ (or count 1))
          (term-send-backspace))
      (unless (sack--paredit-backspace count)
        (let* ((start (point))
               (tab-off (mod (current-column)
                             (* count tab-width)))
               (max-back (if (= tab-off 0)
                             (* count tab-width)
                           tab-off)))
          (skip-chars-backward " " (- start max-back))
          (if (/= (point) start)
              (delete-region (point) start)
            (backward-delete-char count)))))))

(defun sack/whitespace (&optional cross-line)
  "Kill all whitespace -- except end of lines -- before point.
Also kills end of lines if `CROSS-LINE' is non-nil."
  (interactive "P")
  (let* ((start (point))
         (whitespace (if cross-line
                         " \t\r\n"
                       " \t"))
         (move (skip-chars-backward whitespace)))
    (when (/= move 0)
        (sack--protect-evil (+ move start)
          (delete-region (+ move start) start)))))

(defun sack--paredit-backspace (&optional count)
  "Call `paredit-backward-delete' `COUNT' times if we honor paredit."
  (when (and sack/honor-paredit (bound-and-true-p paredit-mode))
    (paredit-backward-delete count)
    t))                                 ; need to signal fun was successful

(defmacro sack--protect-evil (count &rest body)
  "Execute `BODY' only in evil's insert and emacs state if `evil-mode' is
non-nil.
If `evil-mode' is non-nil but the state is not an editing state
call `evil-backward-char' with `count'."
  (declare (indent defun))
  `(if (bound-and-true-p evil-mode)
       (if (or (minibufferp) (evil-insert-state-p) (evil-emacs-state-p))
           (progn
             ,@body)
         (evil-backward-char ,count evil-cross-lines (evil-kbd-macro-suppress-motion-error)))
     ,@body))

(provide 'sackspace)
;;; sackspace.el ends here
