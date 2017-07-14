;;; yara-mode.el --- Major mode for editing yara rule file

;; Copyright 2012 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: yara-mode.el,v 0.0 2012/10/16 14:11:51 binjo Exp $
;; Package-Version: 20170713.2206
;; Keywords: yara
;; X-URL: not distributed yet
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;;; History:

;; 2012/10/16, init
;; 2016/08/19, 1st pull request from @syohex

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'yara-mode)

;;; Code:

(eval-when-compile
  (require 'cl))


(defvar yara-mode-hook nil)
(defvar yara-mode-map
  (make-keymap)
  "Keymap for YARA major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ya?r" . yara-mode))

(defun yara-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "//")
        (comment-start-skip "//")
        (comment-end ""))
    (comment-dwim arg)))

(defvar yara-font-lock-keywords
  `((,(regexp-opt
       '("all" "and" "any" "ascii" "at" "condition" "contains"
         "entrypoint" "false" "filesize" "fullword" "for" "global" "in"
         "import" "include"
         "matches" "meta" "nocase" "not" "or" "of"
         "private" "rule" "strings" "them" "true"
         "wide")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("int8" "int16" "int32" "int8be" "int16be" "int32be"
         "uint8" "uint16" "uint32" "uint8be" "uint16be" "uint32be")
       'symbols)
     . font-lock-function-name-face))
  "Keywords to highlight in yara-mode.")

(defvar yara-mode-syntax-table
  (let ((yara-mode-syntax-table (make-syntax-table)))

    ;; Comment style /* ... */
    (modify-syntax-entry ?/ ". 14" yara-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" yara-mode-syntax-table)
    (modify-syntax-entry ?\n ">" yara-mode-syntax-table)

    yara-mode-syntax-table)
  "Syntax table for yara-mode.")

;;;###autoload
(define-derived-mode yara-mode prog-mode "Yara"
  "Major Mode for editing yara rule files."
  (define-key yara-mode-map [remap comment-dwim] 'yara-comment-dwim)
  (setq font-lock-defaults '(yara-font-lock-keywords nil t))
  (setq tab-width 4))

(provide 'yara-mode)

;;; yara-mode.el ends here
