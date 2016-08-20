;;; phi-search-dired.el --- interactive filtering for dired powered by phi-search

;; Copyright (C) 2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Version: 20150405.14
;; Version: 1.1.1
;; Package-Requires: ((phi-search "2.2.0"))

;;; Commentary:

;; Put phi-search and this script into load-path and (require
;; 'phi-search-dired). Then you can invoke the command with "M-x
;; phi-search-dired" in a dired buffer.

;;; Change Log:

;; 1.0.0 first release
;; 1.1.0 compatibility with phi-search-core 2.0.0
;; 1.1.1 add `phi-search-dired-backspace-or-undo' command

;;; Code:

(require 'dired)
(require 'dired-aux)
(require 'phi-search-core)

(defconst phi-search-dired-version "1.1.1")

(defmacro phi-search-dired--with-silent-message (&rest body)
  `(let ((original-message-fn (symbol-function 'message)))
     (unwind-protect
         (progn
           (fset 'message (lambda (&rest _) nil))
           ,@body)
       (fset 'message original-message-fn))))

(defun phi-search-dired--complete-function ()
  (phi-search--with-target-buffer
   (if phi-search--overlays
       (dolist (ov phi-search--overlays)
         (save-excursion
           (goto-char (overlay-start ov))
           (dired-mark 1)))
     (dired-unmark-all-marks)
     (dired-toggle-marks))))

(defun phi-search-dired--filter-function ()
  (text-property-not-all (match-beginning 0) (match-end 0) 'dired-filename nil))

;;;###autoload
(defun phi-search-dired ()
  "Filter files in dired buffer with phi-search interface."
  (interactive)
  (dired-unmark-all-marks)
  (phi-search--initialize
   '(" *phi-search-dired*"
     (:eval (format " [ %d ]" (length phi-search--overlays))))
   '(((kbd "SPC") . 'phi-search-dired-restrict-to-matches)
     ((kbd "DEL") . 'phi-search-dired-backspace-or-undo))
   'phi-search-dired--filter-function
   nil
   'phi-search-dired--complete-function))

(defun phi-search-dired-restrict-to-matches ()
  "Hide unmached lines in phi-search-dired."
  (interactive)
  (if (string= (minibuffer-contents) "")
      (phi-search-complete)
    (phi-search--with-target-buffer
     (dolist (ov phi-search--overlays)
       (save-excursion
         (goto-char (overlay-start ov))
         (dired-mark 1)))
     (dired-toggle-marks)
     (dired-do-kill-lines))
    (delete-region (minibuffer-prompt-end) (point-max))))

(defun phi-search-dired-backspace-or-undo ()
  (interactive)
  (if (= (minibuffer-prompt-end) (point))
      (phi-search-dired--with-silent-message
       (phi-search--with-target-buffer (dired-undo)))
    (backward-delete-char 1)))

(provide 'phi-search-dired)

;;; phi-search-dired.el ends here
