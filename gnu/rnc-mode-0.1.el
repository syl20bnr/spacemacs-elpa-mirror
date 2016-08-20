;;; rnc-mode.el --- Emacs mode to edit Relax-NG Compact files  -*- lexical-binding:t -*-

;; Copyright (C) 1994-1998, 2001-2016 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: xml relaxng
;; Version: 0.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

(require 'smie)
(require 'nxml-mode)

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

(defconst rnc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?_ "_" st)
    st))

(defconst rnc--keywords
  ;; Taken from the grammar in http://relaxng.org/compact-20021121.html,
  ;; by order of appearance.
  '("namespace" "default" "datatypes" "element" "attribute"
    "list" "mixed" "parent" "empty" "text" "notAllowed" "external"
    "grammar" "div" "include" ;; "start"
    "string" "token" "inherit"))

(defconst rnc--def-regexp "^[ \t]*\\([\\[:alpha:]][[:alnum:]-._]*\\)[ \t]*=")

(defconst rnc-font-lock-keywords
  `((,rnc--def-regexp (1 font-lock-function-name-face))
    (,(concat "\\_<" (regexp-opt rnc--keywords) "\\_>")
     (0 font-lock-keyword-face))
    ("attribute[ \t\n]+\\([^ ]+\\)" (1 'nxml-attribute-local-name))
    ;; FIXME: We'd like to use nxml-element-local-name for element names,
    ;; but by default this looks exactly like font-lock-function-name-face,
    ;; which we want to use for local pattern definitions.
    ;; ("element[ \t\n]+\\([^ ]+\\)" (1 'nxml-element-local-name))
    ))

(defconst rnc-imenu-generic-expression `((nil ,rnc--def-regexp 1)))

(defconst rnc-smie-grammar
  ;; The body of an RNC file is a sequence of definitions.
  ;; Problem is: these definitions are not separated by any special keyword.
  ;; It's basically a repetition of (id "=" pattern), where
  ;; patterns can end with:
  ;;     "}", ")" "*", "+", "?", id, stringliteral
  ;; Since this is way beyond the power of SMIE, we resort to using a pseudo
  ;; " ; " separator which is introduced by the tokenizer.
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id) (atom) (args)
      (header (header "include" atom))
      (decls (id "=" pattern) (id "|=" pattern) (id "&=" pattern)
             (decls " ; " decls))
      (pattern ("element" args) ("attribute" args)
               ("list" args) ("mixed" args)
               ("parent" id) ("external" id)
               ("grammar" atom)
	       ("{" pattern "}")
	       (pattern "," pattern)
	       (pattern "&" pattern)
	       (pattern "|" pattern)
	       (pattern "?")
	       (pattern "*")
	       (pattern "+")))
    ;; The spec says "There is no notion of operator precedence".
    '((assoc " ; "))
    '((assoc "," "&" "|") (nonassoc "?" "*" "+"))
    )))

(defun rnc-smie-forward-token ()
  (let ((start (point)))
    (forward-comment (point-max))
    (if (and (> (point) start)
             (looking-at "\\(?:\\s_\\|\\sw\\)+[ \t\n]*[|&]?=")
             (save-excursion
               (goto-char start)
               (forward-comment -1)
               (= (point) start)))
        " ; "
      (smie-default-forward-token))))

(defun rnc-smie-backward-token ()
  (let ((start (point)))
    (forward-comment (- (point)))
    (if (and (< (point) start)
             (let ((pos (point)))
               (goto-char start)
               (prog1
                   (looking-at "\\(?:\\s_\\|\\sw\\)+[ \t\n]*[|&]?=")
                 (goto-char pos))))
        " ; "
      (smie-default-backward-token))))

(defun rnc-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:list-intro . "element") t)
    (`(:before . ,(or "include" "default" "namespace" "datatypes")) 0)
    (`(:before . "{")
     (save-excursion
       (let ((offset (if (smie-rule-bolp) smie-indent-basic 0))
             x)
         (while (or (null (car-safe x))
                    (integerp (car-safe x)))
           (setq x (smie-backward-sexp 'halfsexp)))
         (goto-char (nth 1 x))
         `(column . ,(+ (smie-indent-virtual) offset)))))
    (`(:after . ,(or "=" "|=" "&=")) smie-indent-basic)
    (`(:before . ,(or "|" "&" ","))
     (and (smie-rule-bolp) (smie-rule-parent-p "(" "{") (smie-rule-parent)))
    (`(,_ . " ; ") (smie-rule-separator kind))
    ))

;;;###autoload
(define-derived-mode rnc-mode prog-mode "RNC"
  "Major mode to edit Relax-NG Compact files."
  (setq-local comment-start "#")
  (setq-local font-lock-defaults '(rnc-font-lock-keywords))
  (setq-local imenu-generic-expression rnc-imenu-generic-expression)
  (smie-setup rnc-smie-grammar #'rnc-smie-rules
              :forward-token #'rnc-smie-forward-token
              :backward-token #'rnc-smie-backward-token))

;;;; ChangeLog:

;; 2016-01-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* rnc-mode: New package
;; 


(provide 'rnc-mode)
;;; rnc-mode.el ends here
