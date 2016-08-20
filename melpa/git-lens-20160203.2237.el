;;; git-lens.el --- Show new, deleted or modified files in branch

;; Copyright (C) 2015  Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Keywords: vc, convenience
;; Package-Version: 20160203.2237
;; Version: 0.4.1
;; Package-Requires: ((emacs "24.4"))

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

;; git-lens will give you a sidebar listing added, modified or deleted
;; files in your current git branch when compared to another branch.

;;; Code:

(require 'subr-x)
(require 'vc)

(defface git-lens-header
 '((default :weight bold :height 1.1 :foreground "orange"))
 "Face for branch files header."
 :group 'git-lens)

(defcustom git-lens-default-branch
 "master"
 "Default branch to compare the current branch to."
 :group 'git-lens)

(defvar-local git-lens-branch nil
 "The branch to compare current branch to.")

(defvar-local git-lens-root ""
 "Git root directory of repository.")

(defun git-lens--branches ()
 "Get available branches."
 (let (branches)
  (with-temp-buffer
   (when (zerop (process-file vc-git-program nil t nil "branch"))
    (goto-char (point-min))
    (while (not (eobp))
     (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (unless (string-prefix-p "*" line)
       (push (string-trim line) branches)))
     (forward-line))))
  branches))

(defun git-lens--select-branch ()
 "Select branch to compare to."
 (completing-read (format "Branch (%s): " git-lens-default-branch)
  (git-lens--branches) nil t nil nil git-lens-default-branch))

(defun git-lens--root-directory ()
 "Name of the current branch."
 (with-temp-buffer
  (when (zerop (process-file vc-git-program nil t nil "rev-parse" "--show-toplevel"))
   (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun git-lens--current-branch ()
 "Name of the current branch."
 (with-temp-buffer
  (when (zerop (process-file vc-git-program nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
   (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun git-lens--files (status)
 "Files with STATUS for diff between `git-lens-branch` and the current branch."
 (let (files)
  (let ((delta (concat git-lens-branch ".." (git-lens--current-branch))))
   (with-temp-buffer
    (when (zerop (process-file vc-git-program nil t nil "diff" "--name-status" delta))
     (goto-char (point-min))
     (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
       (when (string-prefix-p status line)
        (push (string-trim (string-remove-prefix status line)) files)))
      (forward-line))))
   files)))

(defun git-lens--buffer-name (branch)
 "Construct buffer name using BRANCH for the git lens buffer."
 (format "*Git Lens: %s..%s*" branch (git-lens--current-branch)))

(defun git-lens--file-at-point ()
 "Full path to file at point in lens buffer."
 (concat git-lens-root "/"
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position))))

(defun git-lens--insert (status header)
 "Insert files matching STATUS and prepend buffer with HEADER."
 (setq buffer-read-only nil)
 (erase-buffer)
 (insert (propertize (format "%s (compared to %s)" header git-lens-branch) 'face 'git-lens-header))
 (newline)
 (dolist (file (git-lens--files status))
  (insert file)
  (newline))
 (goto-char (point-min))
 (forward-line)
 (setq buffer-read-only t)
 (git-lens-fit-window-horizontally))

(defun git-lens-added ()
 "Show added files in branch."
 (interactive)
 (git-lens--insert "A" "Added files"))

(defun git-lens-deleted ()
 "Show delete files in branch."
 (interactive)
 (git-lens--insert "D" "Deleted files"))

(defun git-lens-modified ()
 "Show modified files in branch."
 (interactive)
 (git-lens--insert "M" "Modified files"))

(defun git-lens-fit-window-horizontally ()
 "Fit window to buffer contents horizontally."
 (interactive)
 (let* ((lines (split-string (buffer-string) "\n" t))
        (line-lengths (mapcar 'length lines))
        (desired-width (+ 1 (apply 'max line-lengths)))
        (max-width (/ (frame-width) 2))
        (new-width (max (min desired-width max-width) window-min-width)))
  (if (> (window-width) new-width)
   (shrink-window-horizontally
    (- (window-width) (max window-min-width (- (window-width) (- (window-width) new-width)))))
   (enlarge-window-horizontally (- new-width (window-width))))))

(defun git-lens-find-file ()
 "Find file at point."
 (interactive)
 (let ((file (git-lens--file-at-point)))
  (if (file-exists-p file)
   (progn
    (condition-case err
     (windmove-right)
     (error
      (split-window-horizontally)
      (windmove-right)))
    (find-file file))
   (message "Can't visit deleted file"))))

(defun git-lens-quit ()
 "Quit the git lens buffer."
 (interactive)
 (kill-buffer)
 (delete-window))

(defvar git-lens-mode-map
 (let ((keymap (make-sparse-keymap)))
  (define-key keymap (kbd "<mouse-1>") 'git-lens-find-file)
  (define-key keymap (kbd "<return>") 'git-lens-find-file)
  (define-key keymap (kbd "A") 'git-lens-added)
  (define-key keymap (kbd "D") 'git-lens-deleted)
  (define-key keymap (kbd "M") 'git-lens-modified)
  (define-key keymap (kbd "q") 'git-lens-quit)
  keymap))

(define-derived-mode git-lens-mode fundamental-mode "Git Lens Mode"
 (setq mode-name "GitLens")
 (setq buffer-read-only t)
 (setq truncate-lines t))

 ;;;###autoload
(defun git-lens ()
 "Start git lens."
 (interactive)
 (let* ((branch (git-lens--select-branch))
        (root (git-lens--root-directory))
        (buffer (get-buffer-create (git-lens--buffer-name branch))))
  (split-window-horizontally)
  (with-current-buffer buffer
   (git-lens-mode)
   (setq git-lens-branch branch)
   (setq git-lens-root root)
   (git-lens-added)
   (switch-to-buffer buffer))))

(provide 'git-lens)
;;; git-lens.el ends here
