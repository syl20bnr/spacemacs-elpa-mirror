;;; scrooge.el --- Major mode for Twitter Scrooge files -*- lexical-binding: t -*-
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
;; Package-Version: 20180630.322
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.13.0") (thrift "0.9.3"))
;; Keywords: scrooge, thrift

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/).

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'thrift)
(require 'font-lock)
(require 'rx)


;; Compatibility for older emacs (ripped from thrift.el, may be out of date).
(defvar jit-lock-start)
(defvar jit-lock-end)


;; Public mutable variables
(defvar scrooge-mode-hook nil)


;; Private helper methods
(defun scrooge--rx-join (rx-exprs) `(| ,@rx-exprs))

(defun scrooge--rx-symbol-wrap (rx-expr) `(: symbol-start ,rx-expr symbol-end))

(defun scrooge--invoke-regexp (pattern)
  (lambda (last)
    (let ((case-fold-search nil))
      (re-search-forward pattern last t))))


;; Defcustoms
(defgroup scrooge nil
  "Group for `scrooge-mode' customizations."
  :group 'prog-mode)

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
    "binary"))

(defconst scrooge--generic-type-symbols
  `("map"
    "list"
    "set"))

(defconst scrooge--ordinals-rx-expr `(+ digit))

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
  "This regexp matches a line beginning with #@namespace.

These lines are how scrooge ensures compatibility with Apache Thrift, because they are normally
parsed as comments.")

(defconst scrooge--function-decl-rx-expr
  `(: symbol-start
      (group-n 1 (+ word))
      (* whitespace)
      "("))

(defconst scrooge--comment-start "# "
  "Value to set `comment-start' when entering `scrooge-mode'.

TODO: make this a `defcustom'! Should probably support `comment-end' etc too.")

(defconst scrooge--line-comment-rx-expr `(: "#" (* not-newline) "\n")
  "We contain the newline because that's what you get if you use a syntax table to mark comments.

That is to say, they mark the newline with a comment face. If you use a color theme which changes
the comment's background background color this can be seen clearly.")

(defconst scrooge--title-case-symbol-rx-expr `(: upper-case (* (| alpha "_"))))

(defconst scrooge--scoped-symbol-rx-expr `(: (* (* (+ (| alpha "_")) ".")
                                                (+ (| alpha "_"))
                                                ","
                                                (* whitespace))
                                             (* (+ (| alpha "_")) ".") (+ (| alpha "_"))))

(defconst scrooge--lower-case-symbol-rx-expr `(: lower-case (* (| alpha "_"))))


;; Helper methods
(defun scrooge--match-generic-types-rx-expr ()
  "Creates an `rx' sexp to match a use of a generic collection type, e.g. list<MyType>."
  `(: (group-n 1 (| ,@scrooge--generic-type-symbols))
      (group-n 2 "<")
      (group-n 3 ,scrooge--scoped-symbol-rx-expr)
      (group-n 4 ">")))


;; Public "immutable" variables
(defconst scrooge-font-lock-keywords
  (--map
   ;; Deconstruct each pair into a regexp and the rest, and convert the regexp into a closure. The
   ;; closure is invoked by `font-lock' to set match data, and allows us to control
   ;; e.g. `case-fold-search' for our font lock keyword matching.
   (cl-destructuring-bind (regexp . rest) it
     (cons (scrooge--invoke-regexp regexp) rest))
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
     (,(-> (scrooge--match-generic-types-rx-expr) (scrooge--rx-symbol-wrap)
           (rx-to-string))
      (1 font-lock-builtin-face)
      (2 font-lock-builtin-face)
      (3 font-lock-type-face)
      (4 font-lock-builtin-face))
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
      (1 font-lock-function-name-face))))
  "Scrooge Keywords.")


;; Macros
(defmacro scrooge--at-point-in-buffer (pt &rest body)
  "Go to the point PT and execute BODY, wrapped in a `save-excursion'."
  (declare (indent 1))
  `(save-excursion
     ;; ,pt is only evaluated once!
     (goto-char ,pt)
     ,@body))


;; Implementation methods
(defun scrooge--syntax-propertize-extend-region (start end)
  "Extend region to propertize between START and END upon change."
  (let* ((b (scrooge--at-point-in-buffer start
              (line-beginning-position)))
         (e (scrooge--at-point-in-buffer end
              (-> (line-end-position) (1+) (min (point-max))))))
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
  (setq-local font-lock-defaults '(scrooge-font-lock-keywords))
  (setq-local comment-start scrooge--comment-start)
  (setq-local indent-line-function #'scrooge-indent-line)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'scrooge--font-lock-extend
            t t))


;; Modifications of global state (advice goes here too)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))


(provide 'scrooge)
;;; scrooge.el ends here
