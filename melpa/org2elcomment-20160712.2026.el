;;; org2elcomment.el --- Convert Org file to Elisp comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((org "8.3.4"))
;; Package-Version: 20160712.2026
;; Keywords: extensions

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

;;                             _______________

;;                              ORG2ELCOMMENT

;;                               Junpeng Qiu
;;                             _______________


;; Table of Contents
;; _________________

;; 1 Overview
;; 2 Usage
;; 3 Customization


;; Convert `org-mode' file to Elisp comments.


;; 1 Overview
;; ==========

;;   This simple package is mainly used for Elisp package writers. After
;;   you've written the `README.org' for your package, you can use
;;   `org2elcomment' to convert the org file to Elisp comments in the
;;   corresponding source code file.


;; 2 Usage
;; =======

;;   Make sure your source code file has `;;; Commentary:' and `;;; Code:'
;;   lines. The generated comments will be put between these two lines. If
;;   you use `auto-insert', it will take care of generating a standard file
;;   header that contains these two lines in your source code.

;;   In your Org file, invoke `org2elcomment', select the source code file,
;;   and done! Now take a look at your source code file, you can see your
;;   Org file has been converted to the comments in your source code file.


;; 3 Customization
;; ===============

;;   Behind the scenes, this package uses `org-export-as' function and the
;;   default backend is `ascii'. You can change to whatever backend that
;;   your org-mode export engine supports, such as `md' (for markdown):
;;   ,----
;;   | (setq org2elcomment-backend 'md)
;;   `----

;;; Code:

(require 'org)
(require 'pulse nil t)

(defvar org2elcomment-backend 'ascii)

(defvar org2elcomment-last-source nil)

(make-variable-buffer-local 'org2elcomment-last-source)

(defun org2elcomment--find-bounds (buffer)
  (let (beg end)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^;;; Commentary:$" nil t)
          (setq beg (line-beginning-position 2))
          (when (re-search-forward "^;;; Code:$")
            (setq end (line-beginning-position))
            (cons beg end)))))))

;;;###autoload
(defun org2elcomment (file-name)
  (interactive
   (list
    (let ((prompt
           (if org2elcomment-last-source
               (format "Source file (default \"%s\"): "
                       org2elcomment-last-source)
             "Source file: ")))
      (setq org2elcomment-last-source (read-file-name prompt
                                                      nil
                                                      org2elcomment-last-source
                                                      t)))))
  (let* ((src-buf (find-file-noselect file-name))
         (bounds (org2elcomment--find-bounds src-buf))
         (output (org-export-as org2elcomment-backend))
         beg end)
    (if bounds
        (progn
          (with-current-buffer src-buf
            (kill-region (car bounds) (cdr bounds))
            (save-excursion
              (goto-char (car bounds))
              (insert "\n")
              (setq beg (point))
              (insert output)
              (comment-region beg (point))
              (insert "\n")
              (setq end (point))))
          (switch-to-buffer src-buf)
          (push-mark)
          (goto-char (car bounds))
          (recenter 0)
          (when (featurep 'pulse)
            (pulse-momentary-highlight-region (car bounds) end)))
      (error "No \";;; Commentary:\" or \";;; Code:\" found"))))

(provide 'org2elcomment)
;;; org2elcomment.el ends here
