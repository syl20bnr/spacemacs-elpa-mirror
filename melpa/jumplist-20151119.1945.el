;;; jumplist.el --- Jump like vim jumplist or ex jumplist

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/jumplist
;; Package-Version: 20151119.1945
;; Version: 0.0.2
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: jumplist vim

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


;;; Code:

(require 'cl-lib)

(defgroup jumplist nil
  "jumplist configuration options."
  :prefix "jumplist"
  :group 'convenience)

(defcustom jumplist-max-length 100
  "Max length of jumplist."
  :type 'integer
  :group 'jumplist)

(defcustom jumplist-ex-mode 'nil
  "Original vim like jumplist or not."
  :type 'boolean
  :group 'jumplist)

(defcustom jumplist-hook-commands '(end-of-buffer beginning-of-buffer find-file)
  "Commands to hook."
  :type 'list
  :group 'jumplist)

(defvar jumplist--list '()
  "Jumplist that save file info.")

(defvar jumplist--idx 0
  "Index of jumplist.")

(defvar jumplist--jumping nil
  "Jumplist state.")

(defun jumplist--do-jump (buff)
  "Do jump to target file and point from BUFF."
  (find-file (car buff))
  (goto-char (cdr buff)))

(defun jumplist--reset-idx ()
  "Reset `jumplist--idx'."
  (setq jumplist--idx 0))

(defun jumplist--last? ()
  "Check `jumplist--idx' is last of list."
  (= jumplist--idx (- (length jumplist--list) 1)))

(defun jumplist--first? ()
  "Check `jumplist--idx' is first of list."
  (= jumplist--idx 0))

(defun jumplist--dec-idx ()
  "Descrement `jumplist--idx'."
  (setq jumplist--idx (- jumplist--idx 1)))

(defun jumplist--inc-idx ()
  "Increment `jumplist--idx'."
  (setq jumplist--idx (+ jumplist--idx 1)))

(defun jumplist--drop! (idx)
  "Drop item form list of IDX."
  (setq jumplist--list (nthcdr jumplist--idx jumplist--list)))

(defun jumplist--push (pointer)
  "Push POINTER to `jumplist'."
  (while (> (length jumplist--list) jumplist-max-length)
    (nbutlast jumplist--list 1))
  (push pointer jumplist--list))

(defun jumplist--same-position? (pointer)
  (let ((new-point (cdr pointer))
        (top-point (cdar jumplist--list)))
    (cond ((not new-point) nil)
          ((not top-point) nil)
          ((eq (marker-position new-point) (marker-position top-point)) 't))))

(defun jumplist--set ()
  "The record data structure is (file-name . pointer)."
  (interactive)
  (if (buffer-file-name)
      (let ((pointer (cons (buffer-file-name) (point-marker))))
        (unless (jumplist--same-position? pointer)
          (when (and jumplist-ex-mode jumplist--jumping)
            (jumplist--drop! jumplist--idx)
            (setq jumplist--jumping nil)
            (jumplist--reset-idx))
          (unless (jumplist--same-position? pointer)
            (jumplist--push pointer))))))

(defun jumplist--do-command? (command do-hook-command-list)
  (if do-hook-command-list
      (or
       (eq command (car do-hook-command-list))
       (jumplist--do-command? command (cdr do-hook-command-list)))))

(defun jumplist--command-hook ()
  "Pre command hook that call `jumplist--set' when registerd command hook called."
  (cond
   ((jumplist--do-command? this-command jumplist-hook-commands) (jumplist--set))
   ((and jumplist--jumping               ; when jump and move
         (not (memq this-command '(jumplist-previous jumplist-next))))
    (jumplist--set))))
(add-hook 'pre-command-hook 'jumplist--command-hook)

;;;###autoload
(defun jumplist-previous ()
  "Jump back."
  (interactive)
  (if (or (not jumplist--list)
          (and (not (jumplist--first?))
               (jumplist--last?)))
      (message "No further undo point.")
    (if jumplist-ex-mode
        (unless jumplist--jumping
          (jumplist--set)
          (setq jumplist--jumping 't)))
    (jumplist--inc-idx)
    (let ((buff (nth jumplist--idx jumplist--list)))
      (jumplist--do-jump buff))))

;;;###autoload
(defun jumplist-next ()
  "Jump forward."
  (interactive)
  (if (or (not jumplist--list)
          (jumplist--first?))
      (message "No further redo point.")
    (if jumplist-ex-mode
        (unless jumplist--jumping
          (jumplist--set)
          (setq jumplist--jumping 't)))
    (jumplist--dec-idx)
    (let ((buff (nth jumplist--idx jumplist--list)))
      (jumplist--do-jump buff))))

(provide 'jumplist)
;;; jumplist.el ends here
