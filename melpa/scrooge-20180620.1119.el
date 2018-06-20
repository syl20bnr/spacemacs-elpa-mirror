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
;; Version: 0.2
;; Package-Version: 20180620.1119
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.13.0") (rainbow-delimiters "1.3.7") (thrift "0.9.3"))
;; Keywords: scrooge, thrift

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/).

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'thrift)
(require 'font-lock)
(require 'rainbow-delimiters)
(require 'rx)


;; Compatibility for older emacs (ripped from thrift.el, may be out of date).
(defvar jit-lock-start)
(defvar jit-lock-end)


;; Public mutable variables
(defvar scrooge-mode-hook nil)


;; Private helper methods
(defun scrooge--rx-join (rx-exprs) `(| ,@rx-exprs))

(defun scrooge--rx-symbol-wrap (rx-expr) `(: symbol-start ,rx-expr symbol-end))


;; Defcustoms
(defgroup scrooge nil
  "Group for `scrooge-mode' customizations."
  :group 'prog-mode)

(defcustom scrooge-use-rainbow-delimiters t
  "Whether to enable `rainbow-delimiters-mode' when entering `scrooge-mode'."
  :type 'boolean
  :group 'scrooge)

(defcustom scrooge-indent-level 2
  "The number of spaces to indent thrift and scrooge files."
  :type 'integer
  :group 'scrooge)


;; "Private" Constants
(defconst scrooge--keyword-literals
  `("include"
    "struct"
    "exception"
    "typedef"
    "const"
    "enum"
    "service"
    "extends"
    "void"
    "oneway"
    "throws"
    "optional"
    "required"
    ;; Non-thrift-mode keywords
    "union"))

(defconst scrooge--primitive-types-literals
  `("bool"
    "byte"
    "i16" "i32" "i64"
    "double"
    "binary"
    "map"
    "list"
    "set"))

(defconst scrooge--ordinals-rx-expr `(+ digit)
  "???")

;;; TODO: named regexp groups macro (w more pcase extensions)!!
(defconst scrooge--special-namespace-line-regexp
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

(defconst scrooge--function-decl-rx-expr
  `(: symbol-start
      (group-n 1 (+ word))
      (* whitespace)
      "("))

(defconst scrooge--comment-start "# "
  "Value to set `comment-start' when entering `scrooge-mode'.

TODO: make this a `defcustom'!")

(defconst scrooge--line-comment-rx-expr `(: "#" (* not-newline) line-end)
  "???")

(defconst scrooge--title-case-symbol-rx-expr `(: upper-case (* (| alpha "_"))))

(defconst scrooge--lower-case-symbol-rx-expr `(: lower-case (* (| alpha "_"))))


;; Public "immutable" variables
(defconst scrooge-font-lock-keywords
  `((,scrooge--special-namespace-line-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-type-face)
     (4 font-lock-string-face))
    (,(rx-to-string scrooge--line-comment-rx-expr)
     . font-lock-comment-face)
    (,(-> scrooge--keyword-literals (scrooge--rx-join) (scrooge--rx-symbol-wrap)
          (rx-to-string))
     . font-lock-keyword-face)
    (,(-> scrooge--primitive-types-literals (scrooge--rx-join) (scrooge--rx-symbol-wrap)
          (rx-to-string))
     . font-lock-builtin-face)
    (,(-> scrooge--ordinals-rx-expr (scrooge--rx-symbol-wrap)
          (rx-to-string))
     . font-lock-variable-name-face)
    (,(-> scrooge--title-case-symbol-rx-expr (scrooge--rx-symbol-wrap)
          (rx-to-string))
     . font-lock-type-face)
    (,(-> scrooge--lower-case-symbol-rx-expr (scrooge--rx-symbol-wrap)
          (rx-to-string))
     . font-lock-variable-name-face)
    (,(rx-to-string scrooge--function-decl-rx-expr)
     (1 font-lock-function-name-face)))
  "Scrooge Keywords.

FIXME: add case-based syntax highlighting!")

(defconst scrooge-mode-syntax-table
  (let ((tbl (make-syntax-table thrift-mode-syntax-table)))
    ;; Remove the syntax entries for sh-style comments -- we need to parse them ourselves.
    (modify-syntax-entry ?# "." tbl)
    (modify-syntax-entry ?\n " " tbl)
    ;; We want to use symbols here, in my opinion, since
    (modify-syntax-entry ?_ "_")
    tbl)
  "Syntax table for scrooge-mode.

also allowing underscore in words

this does NOT mark sh-style comments! that is done with font locking due to the ambiguity with the
scrooge #@namespace hack")


;; Implementation methods
(defun scrooge--syntax-propertize-extend-region (start end)
  "Extend region to propertize between START and END upon change."
  (let* ((b (line-beginning-position))
         (e (if (and (bolp) (> (point) b)) (point)
              (min (1+ (line-end-position)) (point-max)))))
    (unless (and (= start b) (= end e)) (cons b e))))

(defun scrooge--font-lock-extend (start end _)
  "Extend font locking beyond START and END if necessary."
  (let ((res (scrooge--syntax-propertize-extend-region start end)))
    (when res
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))


;; Interactive methods
(defun scrooge-indent-line ()
  "Indent current line using `thrift-indent-line'."
  (interactive)
  (let ((thrift-indent-level scrooge-indent-level))
    (call-interactively #'thrift-indent-line)))

;;;###autoload
(define-derived-mode scrooge-mode prog-mode "Scrooge"
  "Mode for editing Scrooge files."
  ;; We can't use :syntax-table because `thrift-mode' clobbers it ):
  (set-syntax-table scrooge-mode-syntax-table)
  (setq-local font-lock-defaults '(scrooge-font-lock-keywords))
  (setq-local comment-start scrooge--comment-start)
  (setq-local indent-line-function #'scrooge-indent-line)
  (add-hook 'jit-lock-after-change-extend-region-functions
            'scrooge--font-lock-extend t t)
  (rainbow-delimiters-mode-disable)
  ;; (when scrooge-use-rainbow-delimiters
  ;;   (rainbow-delimiters-mode-enable))
  )


;; Modifications of global state (advice goes here too)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))

;; (remove-hook 'scrooge-mode-hook #'rainbow-delimiters-mode-enable)


(provide 'scrooge)
;;; scrooge.el ends here
