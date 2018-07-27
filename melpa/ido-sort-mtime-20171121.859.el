;;; ido-sort-mtime.el --- Sort Ido's file list by modification time

;; Author: Paweł Kraśnicki
;; Created: 24 Apr 2013
;; Version: 0.5
;; Package-Version: 20171121.859
;; Keywords: convenience, files

;; Copyright 2013, 2017 Paweł Kraśnicki

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Display recently modified files at the beginning of Ido's file list.
;;
;; See documentation at <https://github.com/pkkm/ido-sort-mtime>.

;;; Code:

(require 'ido)

(defcustom ido-sort-mtime-tramp-files-at-end t
  "Non-nil causes files handled by TRAMP to appear at the end of the file list.
Nil causes them to appear at the beginning.
(Checking modification time for TRAMP files is not supported.)"
  :type 'boolean
  :group 'ido-sort-mtime)

(defcustom ido-sort-mtime-dot-at-beginning nil
  "Non-nil causes . to always be at the beginning of the list.
If you want Ido to show . even in file mode, see `ido-show-dot-for-dired'."
  :type 'boolean
  :group 'ido-sort-mtime)

(defcustom ido-sort-mtime-limit nil
  "If the list of files/directories has more entries than this number, don't
sort it by modification time. nil means no limit."
  :type '(choice (integer :tag "Number of files")
                 (const :tag "No limit" nil))
  :group 'ido-sort-mtime)

;;;###autoload
(define-minor-mode ido-sort-mtime-mode
  "Sort files in Ido's file list by modification time."
  nil nil nil :global t
  (if ido-sort-mtime-mode
      (progn
        (add-hook 'ido-make-file-list-hook 'ido-sort-mtime--sort)
        (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime--sort))
    (remove-hook 'ido-make-file-list-hook 'ido-sort-mtime--sort)
    (remove-hook 'ido-make-dir-list-hook 'ido-sort-mtime--sort)))

(defun ido-sort-mtime--sort ()
  "Sort Ido's file list by modification time (most recent first)."
  (unless (and ido-sort-mtime-limit
               (> (length ido-temp-list) ido-sort-mtime-limit))
    (setq ido-temp-list (sort ido-temp-list #'ido-sort-mtime--compare))))

(defun ido-sort-mtime--compare (a b)
  (cond
   ;; Ensure . is at the beginning if `ido-sort-mtime-dot-at-beginning' is
   ;; non-nil.
   ((and ido-sort-mtime-dot-at-beginning (string= a "."))
    t)
   ((and ido-sort-mtime-dot-at-beginning (string= b "."))
    nil)

   ;; TRAMP files: don't check mtime, instead use
   ;; `ido-sort-mtime-tramp-files-at-end'. If it's nil, the files will be sorted
   ;; alphabetically (because `ido-temp-list' is sorted to start with). Use
   ;; `concat' instead of `expand-file-name' because the latter will try to
   ;; access the file.
   ((string-match tramp-file-name-regexp (concat ido-current-directory a))
    (not ido-sort-mtime-tramp-files-at-end))
   ((string-match tramp-file-name-regexp (concat ido-current-directory b))
    ido-sort-mtime-tramp-files-at-end)

   ;; Local files: display the most recently modified first.
   (t (file-newer-than-file-p (expand-file-name a ido-current-directory)
                              (expand-file-name b ido-current-directory)))))

(provide 'ido-sort-mtime)
;;; ido-sort-mtime.el ends here.
