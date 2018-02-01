;;; minizinc-mode.el --- Major mode for MiniZinc code -*- lexical-binding: t; -*-

;; Copyright © 2015-2017 Yushi Wang
;; Authors : Yushi Wang <dot_wangyushi@yeah.net>
;; URL : http://github.com/m00nlight/minizinc-mode
;; Package-Version: 20180201.650
;; Keywords : languages minizinc
;; Version : 0.0.2
;; Package-Requires : ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide font-lock for minizinc(http://www.minizinc.org/) code

;; Note: pretty-rendering is shamelessly stolen from haskell-mode.

;; Here are some example of configuration:

;;; Installation:
;;
;; You can either manually install minizinc-mode or automatically
;; install from melpa. Add the following line into your ~/.emacs
;; file or any your emacs start file to install it from the melpa.
;;
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (unless package-archive-contents (package-refresh-contents))
;; (package-initialize)
;;
;; install minizinc-mode
;;
;; M-x package-install
;; minizinc-mode
;;
;; Add the following lines to your emacs after installation.
;; (require 'minizinc-mode)
;; (add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))


;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cc-mode)

(defgroup minizinc nil
  "Major mode for MiniZinc code"
  :group 'languages)

(defcustom minizinc-font-lock-symbols nil
  "Display /\\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises with regards to layout."
  :group 'minizinc
  :type 'boolean)


(defcustom minizinc-font-lock-symbols-alist
  '(("/\\" . "∧")
    ("->"  . "⇒")
    ("<-"  . "⇐")
    ("<->" . "⇔")
    ("\\/" . "∨")
    ("not" . "¬")
    ("xor" . "⊻")
    ("in"  . "∈")
    (">="  . "≥")
    ("<="  . "≤")
    (".." . "…")
    ("++" . "⧺")
    ("exists" . "∃")
    ("sum" . "Σ")
    ("forall" . "∀"))
  "Alist mapping MiniZinc symbols to chars.
Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).
STRING is the MiniZinc symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'minizinc)


(defun minizinc-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)


(defun minizinc-font-lock-symbols-keywords ()
  (when (and minizinc-font-lock-symbols
             minizinc-font-lock-symbols-alist)
    `((,(regexp-opt (mapcar 'car minizinc-font-lock-symbols-alist) t)
       (0 (haskell-font-lock-compose-symbol ',minizinc-font-lock-symbols-alist)
          ;; In Emacs-21, if the `override' field is nil, the face
          ;; expressions is only evaluated if the text has currently
          ;; no face.  So force evaluation by using `keep'.
          keep)))))

;; (defface font-lock-operator-face
;;   '((t :inherit font-lock-builtin-face))
;;   "Used for operators."
;;   :group 'font-lock-faces)

(defface minizinc-operator-face
  '((t (:foreground "dark orange")))
  "Used for operators."
  :group 'minizinc)

(defvar minizinc-operator-face 'minizinc-operator-face)


(defvar minizinc-keywords
  '("var" "constraint" "solve" "satisfy" "maximize"
    "minimize" "output" "par" "of" "where" "ann"
    "annotation" "any" "array" "function" "include"
    "op" "predicate" "record" "test" "tuple" "type"
    "if" "else" "endif" "false" "true" "in" "then"
    "opt"))

(defvar minizinc-types
  '("float" "int" "bool" "string" "list" "tuple" "set"))

;; Ref: http://www.minizinc.org/downloads/doc-latest/minizinc-tute.pdf
(defvar minizinc-builtins
  '(;; A
    "abs" "acos" "acosh" "aisnh" "alldifferent" "ann" "annotation" "array1d"
    "array2d" "arraynd" "asin" "assert" "assignment" "atan" "atanh"
    ;; B
    "bool2int" "bool_search"
    ;; C
    "card" "cos" "cosh" "cumulative" "ceil"
    ;; D
    "diff" "disjunctive" "dom" "domain" "dom_array"
    ;; E
    "exists" "exp"
    ;; F
    "first_fail" "fix" "fixed" "forall"
    ;; I
    "ifall" "index_set" "index_set_1of2" "index_set_2of2" "indomain_median"
    "indomain_min" "indomain_random" "indomain_split" "input_order" "int2float"
    "inter" "int_search" "inverse"
    ;; L
    "lb" "lb_array" "let" "ln" "log" "log2" "log10"
    ;; M
    "max" "min" "mzn-g12fd"
    ;; P
    "par" "pow" "predict" "product"
    ;; R
    "regular"
    ;; S
    "sex_search" "set_search" "show" "show_float" "show_int" "sin" "sinh"
    "smallest" "sqrt" "sum"
    ;; T
    "table" "tan" "tanh" "test" "trace"
    ;; U
    "ub" "ub_array" "unfixed"
    ;; X
    "xorall"
    ))


(defvar minizinc-operators
  (concat "<\\->\\|\\->\\|<-\\|\\\\/\\|/\\\\\\|<\\|>=\\|<="
          "\\|==\\|!=\\|>\\|++\\|+\\|-\\|*\\|/\\|\\.\\.\\|"
          "=\\|\\<\\(superset\\|diff\\|symdiff\\|intersect"
          "\\|div\\|mod\\|xor\\|in\\|subset\\|union\\|not\\)\\>"))

(defvar minizinc-keywords-regex
  (regexp-opt minizinc-keywords 'words))

(defvar minizinc-types-regex
  (regexp-opt minizinc-types 'words))

(defvar minizinc-builtins-regex
  (regexp-opt minizinc-builtins 'words))

(defvar minizinc-operators-regex
  minizinc-operators)

(defvar minizinc-font-lock-keywords
  `(
    ("\\(%[^\n]*\\)$" . font-lock-comment-face)
    ,@(minizinc-font-lock-symbols-keywords)
    (,minizinc-builtins-regex . font-lock-builtin-face)
    (,minizinc-types-regex . font-lock-type-face)
    (,minizinc-keywords-regex . font-lock-keyword-face)
    (,minizinc-operators-regex . minizinc-operator-face)
    ))

;;;###autoload
(define-derived-mode minizinc-mode java-mode "MiniZinc mode"
  "Major mode for edigint minizinc source file."
  (setq mode-name "minizinc-mode")
  (setq font-lock-defaults '((minizinc-font-lock-keywords)))
  (set (make-local-variable 'c-basic-offset) 0)
  (setq comment-start "%")
  (setq comment-end "")
  (modify-syntax-entry ?% "< b" minizinc-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" minizinc-mode-syntax-table)
  )


(setq minizinc-keywords nil)
(setq minizinc-keywords-regex nil)
(setq minizinc-builtins nil)
(setq minizinc-builtins-regex nil)
(setq minizinc-types nil)
(setq minizinc-types-regex nil)
(setq minizinc-operators nil)
(setq minizinc-operators-regex nil)


;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(when (require 'flycheck nil :noerror)
  (flycheck-define-checker mzn2fzn
  "A MiniZinc syntax checker using the MiniZinc compiler.

  See URL `http://www.minizinc.org/'."
  :command ("mzn2fzn" "--model-check-only" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":\n"
          (* any) "\n"
          (* any) "\n"
          "Error: " (message) line-end)
   (error line-start (file-name) ":" line ":\n"
          "MiniZinc:" (message) line-end))
    :modes minizinc-mode)
  (add-to-list 'flycheck-checkers 'mzn2fzn)
  (add-hook 'minizinc-mode-hook
            (lambda ()
              (flycheck-mode))))

(provide 'minizinc-mode)

;; Local Variable:
;; coding: utf-8
;; End

;;; minizinc-mode.el ends here
