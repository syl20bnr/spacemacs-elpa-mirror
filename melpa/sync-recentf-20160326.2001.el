;;; sync-recentf.el --- Synchronize the recent files list between Emacs instances

;; Copyright (C) 2014 François Févotte

;; Author: François Févotte <fevotte@gmail.com>
;; Created: 24 Mar 2014
;; Version: 1.0
;; Package-Version: 20160326.2001
;; Keywords: recentf
;; X-URL: https://github.com/ffevotte/sync-recentf

;; This file is NOT part of Emacs.

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

;; This package helps synchronizing the recent files list between Emacs
;; instances.  Without it, each Emacs instance manages its own recent files
;; list.  The last one to close persistently saves its list into
;; `recentf-save-file'; all files recently opened by other instances are
;; overwritten.
;;
;; With sync-recentf, all running Emacs instances periodically synchronize their
;; local recent files list with `recentf-save-file'.  This ensures that all
;; instances share the same list, which is persistently saved across sessions.

;; `sync-recentf-marker' is always pushed on top of `recentf-list' when it is
;; synchronized, after a load from `recentf-save-file' or an explicit merge.
;;
;; All files appearing before `sync-recentf-marker' in `recentf-list' were
;; visited after the last synchronization, meaning that they should be pushed up
;; in the next synchronization phase.  Synchronization actually happens at file
;; save (see `recentf-save-list') or during periodical cleanups (see
;; `recentf-cleanup')

;; If you make improvements to this code or have suggestions, please do not
;; hesitate to fork the repository or submit bug reports on github.  The
;; repository is at:
;;
;;     https://github.com/ffevotte/sync-recentf


;;; Code:

(require 'recentf)
(eval-when-compile
  (require 'cl))

;; * Recent list synchronization

(defvar sync-recentf-marker
  (concat temporary-file-directory "sync-recentf-marker")
  "File used as a marker for recentf list merges.")

;; The recentf marker file must exist (otherwise it will be cleaned up by
;; `recentf-cleanup')
(unless (file-exists-p sync-recentf-marker)
  (with-temp-buffer (write-file sync-recentf-marker)))

(defun sync-recentf--push-marker ()
  "Push `sync-recentf-marker' on top of `recentf-list'."
  (recentf-push sync-recentf-marker))

(defun sync-recentf--marker-top-p ()
  "Non-nil if `sync-recentf-marker' is on top of `recentf-list'."
  (string= (car recentf-list) sync-recentf-marker))

(defun sync-recentf--to-sync ()
  "List of files which have been visited since the last synchronization."
  (let ((to-sync nil)
        (recent-files recentf-list))
    (while (and recent-files
                (not (string= sync-recentf-marker (car recent-files))))
      (setq to-sync (cons (car recent-files) to-sync))
      (setq recent-files (cdr recent-files)))
    to-sync))

(defun sync-recentf--sync ()
  "Synchronize the local list with that of `recentf-file'.

Every file in `recentf-list' more recent (i.e. before)
`sync-recentf-marker' will be pushed in front of the list coming
from `recentf-save-file'.  The result of the merge is put into
`recentf-list'.

Return nil if no merge was needed."
  (if (sync-recentf--marker-top-p)
      ;; No merge needed; just reload `recentf-save-list'
      (progn
        (recentf-load-list)
        nil)
    ;; Actually merge local and saved lists
    (let ((to-sync (sync-recentf--to-sync)))
      (recentf-load-list)
      (mapc 'recentf-push to-sync)
      (sync-recentf--push-marker)
      t)))



;; * Entry points

(defun sync-recentf-save-list ()
  "Copy of the original `recentf-save-list' function."
  t)
(fset 'sync-recentf-save-list (symbol-function 'recentf-save-list))

(defun recentf-save-list ()
  "Save the recent files list.

The original version of this function is accessible through
`sync-recentf-save-list'.  This version has the same effect,
except it synchronizes the recent files list before saving it to
`recentf-save-file'."
  (when (sync-recentf--sync)
    (sync-recentf-save-list)))

(defadvice recentf-cleanup (around sync-recentf activate)
  "Synchronize the recent files list."
  (sync-recentf--sync)
  ad-do-it
  (cl-letf (((symbol-function 'ask-user-about-lock)
             (lambda (file opponent)
               (message "sync-recentf: file `%s' locked by `%s'. Aborting."
                        file opponent)
               (throw :sync-recentf (list file opponent)))))
    (catch :sync-recentf
      (sync-recentf-save-list))))

(defadvice recentf-load-list (after sync-recentf activate)
  "Mark the recentf files list as synchronized."
  (sync-recentf--push-marker))


;; * Postamble
(provide 'sync-recentf)
;;; sync-recentf.el ends here
