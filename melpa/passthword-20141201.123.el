;;; passthword.el --- Simple password manager

;; Copyright (C) 2014  Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 1.4
;; Package-Version: 20141201.123
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords:

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

;; M-x passthword retrieves an entry by showing you the username and
;; making the password available for yanking.
;;
;; C-u M-x passthword creates a new entry, to generate a random
;; password while creating a new entry press C-SPC when prompted for
;; password.
;;
;; M-x customize-group [passthword] to specify an alternative password
;; file.

;;; Code:

(require 'cl-lib)
(require 'ido)

(defcustom passthword-password-file
 (expand-file-name "~/.emacs.d/passthword.gpg")
 "Where to store saved password. This should be a file ending with .gpg to make sure epg encrypt it."
 :group 'passthword)

(defun passthword--read-store ()
 "Read password store into memory."
 (with-temp-buffer
  (ignore-errors
   (insert-file-contents passthword-password-file nil nil nil t)
   (read (buffer-string)))))

(defun passthword--persist (contents)
 "Persist CONTENTS as password store."
 (with-current-buffer (find-file passthword-password-file)
  (erase-buffer)
  (insert (with-output-to-string (prin1 contents)))
  (save-buffer)
  (kill-buffer)))

(defun passthword--write-to-store (entry)
 "Update or create ENTRY in password store."
 (let ((entries (passthword--read-store)))
  (let (updated-entries updated-p)
   (dolist (e entries)
    (if (equal (car e) (car entry))
     (progn
      (push entry updated-entries)
      (setq updated-p t))
     (push e updated-entries)))
   (unless updated-p
    (push entry updated-entries))
   (passthword--persist updated-entries))))

(defun passthword-random-password ()
 "Insert a random password."
 (interactive)
 (insert
  (sha1
   (mapconcat 'number-to-string
    (cl-loop for n below 10 collect (random t)) ""))))

;;;###autoload
(defun passthword-delete ()
 "Delete an entry from the password store."
 (interactive)
 (let* ((entries (passthword--read-store))
        (description (ido-completing-read "Select credential: " (mapcar 'car entries)))
        (entry (cl-find description entries :key 'car :test 'equal)))
  (when entry
   (passthword--persist (cl-remove-if (lambda (e) (equal (car entry) (car e))) entries))
   (message "Deleted: %s" (car entry)))))

;;;###autoload
(defun passthword-get ()
 "Read an entry from the password store."
 (interactive)
 (let* ((entries (passthword--read-store))
        (description (ido-completing-read "Select credential: " (mapcar 'car entries)))
        (entry (cl-find description entries :key 'car :test 'equal)))
  (with-temp-buffer
   (insert (caddr entry))
   (kill-region (point-min) (point-max)))
  (message "Copied password for username: %s" (cadr entry))))

;;;###autoload
(defun passthword-put ()
 "Create a new entry in the password store."
 (interactive)
 (let* ((entries (passthword--read-store))
        (description (ido-completing-read "New [Description]: " (mapcar 'car entries) nil 'confirm))
        (username (ido-completing-read "New [Username]: " (mapcar 'cadr entries) nil 'confirm))
        (password (minibuffer-with-setup-hook
                   (lambda () (local-set-key (kbd "C-SPC") 'passthword-random-password))
                   (read-passwd "New [Password]: "))))
  (passthword--write-to-store (list description username password))))

;;;###autoload
(defun passthword (prefix)
 "Manage passwords. With PREFIX interactively create a new entry.
Without PREFIX pick an entry and copy it's password."
 (interactive "P")
 (if prefix
  (passthword-put)
  (passthword-get)))

(provide 'passthword)
;;; passthword.el ends here
