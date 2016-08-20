;;; copyit-pandoc.el --- Copy it, yank anything! -*- mode: lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 6 Jun 2016
;; Version: 0.0.1
;; Package-Version: 20160624.1328
;; Keywords: convinience yank clipboard
;; Homepage: https://github.com/zonuexe/emacs-copyit
;; Package-Requires: ((emacs "24") (copyit "0.0.1") (pandoc "0.0.1"))

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

;; “Copy” is known as “Yank” in Emacs.
;;
;; ** Functions
;; - copyit-pandoc-export-to-html
;; - copyit-pandoc-export-to-markdown

;;; Code:
(require 'copyit)
(require 'pandoc)

;;;###autoload
(defun copyit-pandoc-export-to-html (file-path)
  "Convert and Copy `FILE-PATH' file as HTML."
  (interactive "p")
  (cond
   ((null file-path)
    (setq file-path (read-file-name "")))
   ((and (equal 4 file-path) buffer-file-name)
    (setq file-path buffer-file-name)))
  (kill-new (pandoc-convert-file file-path nil "html")))

;;;###autoload
(defun copyit-pandoc-export-to-markdown (file-path)
  "Convert and Copy `FILE-PATH' file as Markdown."
  (interactive "p")
  (cond
   ((null file-path)
    (setq file-path (read-file-name "")))
   ((and (equal 4 file-path) buffer-file-name)
    (setq file-path buffer-file-name)))
  (kill-new (pandoc-convert-file file-path nil (pandoc-markdown-dialect))))

(provide 'copyit-pandoc)
;;; copyit-pandoc.el ends here
