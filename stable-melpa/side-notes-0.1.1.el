;;; side-notes.el --- Easy access to a directory notes file  -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Paul W. Rankin

;; Author: Paul W. Rankin <hello@paulwrankin.com>
;; Keywords: convenience
;; Package-Version: 0.1.1
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.5"))
;; URL: https://github.com/rnkn/side-notes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Side Notes
;; ==========

;; Quickly display your quick side notes in quick side window.

;; Side notes live in a file in the current directory defined by custom option
;; `side-notes-file', which defaults to "notes.txt".

;; Installation
;; ------------

;; Add the following to you init file:

;; (define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes)

;;; Code:

(defgroup side-notes ()
  "Display a notes file."
  :group 'convenience)

(defcustom side-notes-hook
  nil
  "Hook run after showing notes buffer."
  :type 'hook
  :group 'side-notes)

(defcustom side-notes-file
  "notes.txt"
  "Name of the notes file to find.

This is always relative to `default-directory'. The idea is that
your project has its own directory and notes file, but if you
would like to use a file-specific notes file, specify a string
with `add-file-local-variable'."
  :type 'string
  :safe 'stringp
  :group 'side-notes)
(make-variable-buffer-local 'side-notes-file)

(defcustom side-notes-select-window
  t
  "If non-nil, switch to notes window upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'side-notes)

(defcustom side-notes-display-alist
  '((side . right)
    (window-width . 35)
    (slot . 0))
  "Alist used to display notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :group 'side-notes)

(defface side-notes
  '((t nil))
  "Default face for notes buffer."
  :group 'side-notes)

(defvar-local side-notes-buffer-identify
  nil
  "Buffer local variable to identify a notes buffer.")

(defun side-notes-toggle-notes ()
  "Pop up a window containing notes of current directory."
  (interactive)
  (if side-notes-buffer-identify
      (quit-window)
    (let ((display-buffer-mark-dedicated t)
          (buffer (find-file-noselect
                   (expand-file-name side-notes-file default-directory))))
      (if (get-buffer-window buffer (selected-frame))
          (delete-windows-on buffer (selected-frame))
        (display-buffer-in-side-window buffer side-notes-display-alist)
        (with-current-buffer buffer
          (setq side-notes-buffer-identify t)
          (face-remap-add-relative 'default 'side-notes)
          (run-hooks 'side-notes-hook))
        (if side-notes-select-window
            (select-window (get-buffer-window buffer (selected-frame))))
        (message "Showing `%s'; %s to hide" buffer
                 (key-description (where-is-internal this-command
                                                     overriding-local-map t)))))))

(provide 'side-notes)
;;; side-notes.el ends here
