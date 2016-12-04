;;; helm-rdefs.el --- rdefs with helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2016 by Hiroshi Saito

;; Version: 1.0.1
;; Package-Version: 20161129.2136
;; Author: Hiroshi Saito <monodie@gmail.com>
;; URL: https://github.com/saidie/helm-rdefs
;; Package-Requires: ((emacs "24") (helm "1.6.4"))
;; Keywords: matching, tools

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

;; This package provides helm interface for rdefs.  Run `helm-rdefs' on ruby
;; file.

;;; Code:

(require 'helm)

(defvar helm-rdefs-command (executable-find "rdefs"))
(defvar helm-rdefs--current-file)

(defun helm-rdefs--exec-rdefs (path)
  "Execute rdefs for file specified by `PATH'."
  (with-temp-buffer
    (unless (process-file helm-rdefs-command nil t nil "-n" path)
      (error "Failed: '%s -n %s'" helm-rdefs-command path))
    (buffer-string)))

(defun helm-rdefs--init ()
  "Initialize a candidate helm buffer with rdefs output for current file."
  (let ((buf (helm-init-candidates-in-buffer 'global
               (helm-rdefs--exec-rdefs helm-rdefs--current-file))))
    (with-current-buffer buf
      (goto-char (point-min))
      (flush-lines "^[:space:]*$")
      (while (re-search-forward "^ *\\([^ 0-9].*\\)?$" nil t 1)
        (beginning-of-line)
        (delete-char -1)
        (just-one-space)))))

(defun helm-rdefs--goto (candidate)
  "Go to the line of `CANDIDATE'."
  (when (string-match "^ *\\([0-9]+\\):" candidate)
    (let ((line (string-to-number (match-string 1 candidate))))
      (goto-char (point-min))
      (forward-line (1- line))))
  (recenter (/ (window-height) 2)))

(defvar helm-rdefs--source
  (helm-build-in-buffer-source "helm-rdefs"
    :init #'helm-rdefs--init
    :action '(("Go to" . helm-rdefs--goto))))

;;;###autoload
(defun helm-rdefs ()
  "Open a helm buffer with rdefs output."
  (interactive)
  (setq helm-rdefs--current-file (buffer-file-name))
  (helm :sources '(helm-rdefs--source)
        :buffer "*helm-rdefs*"))

(provide 'helm-rdefs)

;;; helm-rdefs.el ends here
