;;; scrooge.el --- Major mode for Twitter Scrooge files
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Daniel McClanahan <danieldmcclanahan@gmail.com>
;; Version: 0.1
;; Package-Version: 20170728.1106
;; Package-Requires: ((emacs "24") (thrift "0.9.3"))
;; Keywords: scrooge, thrift

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/) by Danny McClanahan
;; <danieldmcclanahan@gmail.com>.

;;; Code:

(require 'thrift)
(require 'font-lock)

;; compat for older emacs
(defvar jit-lock-start)
(defvar jit-lock-end)

(defvar scrooge-mode-hook nil)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))

(defconst scrooge-special-namespace-regexp
  "^\\(\\(?:#@\\)?\\)\\(namespace\\)[ \t\v\f]+\\([^ \t\v\f]+\\)[ \t\v\f]+\\([^ \t\v\f].*\\).*?$")

;; syntax coloring
(defconst scrooge-font-lock-keywords
  (append
   thrift-font-lock-keywords
   `((,scrooge-special-namespace-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-builtin-face)
      (3 font-lock-type-face)
      (4 font-lock-string-face)) ;; namespace decls
     ("#.*\\(\n\\|\\'\\)" (0 font-lock-comment-face))))
  "Scrooge Keywords.")

;; C/C++- and sh-style comments; also allowing underscore in words
(defvar scrooge-mode-syntax-table
  (let ((scrooge-mode-syntax-table
         (make-syntax-table thrift-mode-syntax-table)))
    ;; #-comments removed
    (modify-syntax-entry ?# "." scrooge-mode-syntax-table)
    scrooge-mode-syntax-table)
  "Syntax table for scrooge-mode.")

(defun scrooge-syntax-propertize-extend-region (start end)
  "Extend region to propertize between START and END upon change."
  (let* ((b (line-beginning-position))
         (e (if (and (bolp) (> (point) b)) (point)
              (min (1+ (line-end-position)) (point-max)))))
    (unless (and (= start b) (= end e)) (cons b e))))

(defun scrooge-font-lock-extend (start end _)
  "Extend font locking beyond START and END if necessary."
  (let ((res (scrooge-syntax-propertize-extend-region start end)))
    (when res
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

;;;###autoload
(define-derived-mode scrooge-mode thrift-mode "Scrooge"
  "Mode for editing Scrooge files."
  :syntax-table scrooge-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(scrooge-font-lock-keywords))
  (set (make-local-variable 'comment-start) "# ")
  (add-hook 'jit-lock-after-change-extend-region-functions
            'scrooge-font-lock-extend t t))

(provide 'scrooge)
;;; scrooge.el ends here
