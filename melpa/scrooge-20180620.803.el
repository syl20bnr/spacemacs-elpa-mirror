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
;; Package-Version: 20180620.803
;; Package-Requires: ((emacs "24") (thrift "0.9.3"))
;; Keywords: scrooge, thrift

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/) by Danny McClanahan
;; <danieldmcclanahan@gmail.com>.

;;; Code:

(require 'thrift)
(require 'font-lock)
(require 'rx)


;; Compatibility for older emacs (ripped from thrift.el, may be out of date).
(defvar jit-lock-start)
(defvar jit-lock-end)

(defvar scrooge-mode-hook nil)

;;; TODO: named regexp groups macro (w more pcase extensions)!!
(defconst scrooge--special-namespace-regexp
  (rx (: bol
         (group-n 1 (? "#@"))
         (group-n 2 "namespace")
         (+ whitespace)
         (group-n 3 (+ (not whitespace)))
         (+ whitespace)
         (group-n 4 (: (not whitespace)
                       (* not-newline)))
         eol))
  ;; FIXME: document this!!! this is the only important part!!!
  "\\(?:^\\(?1:\\(?:#@\\)?\\)\\(?2:namespace\\)[ \t\v\f]+\\(?3:[^ \t\v\f]+\\)[ \t\v\f]+\\(?4:[^ \t\v\f].*\\)?$\\)")

(defconst scrooge--extra-keywords-regexp
  (rx (: symbol-start
         ;; Insert more strings here as needed (note that they get regex-escaped)!
         (| "union")
         symbol-end)))

;; syntax coloring
(defconst scrooge-font-lock-keywords
  (append
   ;; FIXME: `thrift-font-lock-keywords' uses beginning/end-of-word syntax in their patterns, and it
   ;; should definitely be matching symbol boundaries instead, so either parse it out of the
   ;; existing patterns, or just recreate it (that would be sad though).
   thrift-font-lock-keywords
   `((,scrooge--extra-keywords-regexp . font-lock-keyword-face))
   `((,scrooge-special-namespace-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-builtin-face)
      (3 font-lock-type-face)
      (4 font-lock-string-face)) ;; namespace decls
     ;; FIXME: fix this regexp and add case-based syntax highlighting!
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))

(provide 'scrooge)
;;; scrooge.el ends here
