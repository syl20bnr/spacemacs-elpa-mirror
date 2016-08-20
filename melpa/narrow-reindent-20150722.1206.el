;;; narrow-reindent.el --- Defines a minor mode to left-align narrowed regions.

;; Copyright: © 2015 J David Smith
;;
;; Author: J David Smith <emallson@atlanis.net>
;; Maintainer: J David Smith <emallson@atlanis.net>
;; Homepage: https://github.com/emallson/narrow-reindent.el
;; Version: 0.2.0
;; Package-Version: 20150722.1206
;; Package-Requires: ((emacs "24.4"))

;; This program is free software: you can redistribute it and/or modify
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

;; Defines a minor mode `narrow-reindent-mode' that reindents the region
;; narrowed to. The region is then indented again after widening the buffer.

;;; Code:

(defvar-local narrow-reindent--point-min 0)
(defvar-local narrow-reindent--point-max 0)
(defvar-local narrow-reindent--indent-amount 0)

;;;###autoload
(define-minor-mode narrow-reindent-mode
  "Toggle Narrow-Reindent mode.

When Narrow-Reindent mode is active, after narrowing the buffer
is re-indented. Before widening, this narrowed region is
re-indented again. This mode uses the `indent-region' to perform
indentation."
  :lighter " NaRe"
  :group 'narrow-reindent
  :init-value nil
  :keymap '(([remap narrow-to-defun]  . narrow-reindent-to-defun)
            ([remap narrow-to-page]   . narrow-reindent-to-page)
            ([remap narrow-to-region] . narrow-reindent-to-region)
            ([remap widen]            . narrow-reindent-widen)))

(defmacro narrow-reindent--without-undo (&rest forms)
  "Execute FORMS with a temporary `buffer-undo-list'.

Taken from http://www.emacswiki.org/emacs/UndoCommands with some
modifications.

This function uses a two-hyphen prefix because it is not intended
for other packages to use.  If another package author wishes to
use this function, contact the maintainer of `narrow-reindent'
and figure out a way to do it without adding an oddball
dependency."
  `(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t))
     (unwind-protect
         (progn ,@forms)
       (set-buffer-modified-p modified)) ()))

(defun narrow-reindent--after-narrow (&rest _r)
  "Indent narrowed buffer."
  (when narrow-reindent-mode
    (let ((beg (point-min))
          (end (point-max)))
      (setq narrow-reindent--point-min beg)
      (setq narrow-reindent--point-max end)
      (setq narrow-reindent--indent-amount (indent-rigidly--current-indentation beg end))
      (narrow-reindent--without-undo
       (indent-rigidly beg end (- narrow-reindent--indent-amount))))))

(defun narrow-reindent--before-widen (&rest _r)
  "Indent the region that the buffer was narrowed to."
  (when narrow-reindent-mode
    (narrow-reindent--without-undo
     (indent-rigidly narrow-reindent--point-min narrow-reindent--point-max narrow-reindent--indent-amount))))

(defun narrow-reindent-to-defun (&optional arg)
  "Make text outside the current defun invisible.
Align the visible region to the margin.
Optional `ARG' is ignored."
  (interactive)
  (narrow-to-defun)
  (narrow-reindent--after-narrow))

(defun narrow-reindent-to-region (start end)
  "Restrict editing in this buffer to the region from `START' to `END'.
Align the visible region to the margin."
  (interactive "r")
  (narrow-to-region start end)
  (narrow-reindent--after-narrow))

(defun narrow-reindent-to-page (&optional arg)
  "Restrict editing in this buffer to the current page.
Align the visible region to the margin.
Prefix argument specifies how many pages to jump forward or backward."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (narrow-to-page arg)
  (narrow-reindent--after-narrow))

(defun narrow-reindent-widen ()
  "Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited.
Remove margin-alignment done by other narrow-reindent functions."
  (interactive)
  (narrow-reindent--before-widen)
  (widen))

(provide 'narrow-reindent)
;;; narrow-reindent.el ends here
