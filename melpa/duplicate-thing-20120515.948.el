;;; duplicate-thing.el --- Duplicate current line & selection

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: command duplicate line selection
;; Package-Version: 20120515.948
;; Version: 0.2

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
;; 1. Duplicate current line.
;; 2. Duplicate a selection when selection is active.
;; 3. Only C-u, replicate, comment out the range.
;; 4. Numerical prefix is specified as 'C-u 5': do multiple times repeatedly.
;; 
;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)
;; 

;;; Code:

;;;###autoload
(defun duplicate-thing (n)
  (interactive "P")
  (save-excursion
    (let (start
          end
          (with-comment-out (consp n)))
      (cond (mark-active
             (setq start (region-beginning) end (region-end)))
            (t
             (beginning-of-line)
             (setq start (point))
             (forward-line)
             (setq end (point))))
      (kill-ring-save start end)
      (if with-comment-out
          (progn
            (comment-region start end)
            (yank))
        (dotimes (i (or n 1))
          (yank))))))

(provide 'duplicate-thing)
;;; duplicate-thing.el ends here
