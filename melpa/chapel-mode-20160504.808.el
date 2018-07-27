;;; chapel-mode.el --- a CC Mode for Chapel derived from derived-mode-ex.el

;; Author:     Steven T Balensiefer
;; Contributor: Russel Winder
;; Maintainer: Russel Winder <russel@winder.org.uk>
;; Created:    December 2002
;; Version:    201601260655
;; Package-Version: 20160504.808
;; Keywords:   Chapel languages oop

;;;; NB Version number is date and time yyyymmddhhMM UTC.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; If you not used packaging to install this mode then put these lines in
;; your init file.
;;   (autoload 'chapel-mode "chapel-mode" "Major mode for editing Chapel code." t)
;;   (add-to-list 'auto-mode-alist '("\\.chpl\\'" . chapel-mode))

;;; Commentary:

;; This is a mode for the Chapel programming language under
;; development by Cray Inc.  The mode itself was originally developed
;; by Steven T Balensiefer at UW as part of his course project for
;; CSE590o, based on the derived-mode-ex.el code supported with the
;; standard cc-mode.
;;
;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
(eval-and-compile
  (when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

;; Need to exclude xemacs from some behavior
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'chapel-mode 'c-mode))

;; Define chapel primitive types

(c-lang-defconst c-primitive-type-kwds
  chapel '("bool"
           "complex"
           "domain"
           "imag" "index" "int"
           "locale"
           "opaque"
           "range" "real"
           "string" "subdomain"
           "uint"))

;; Define chapel type modifiers
(c-lang-defconst c-type-modifier-kwds
  "Type modifier keywords.  These can occur almost anywhere in types
but they don't build a type of themselves.  Unlike the keywords on
`c-primitive-type-kwds', they are fontified with the keyword face and
not the type face."
  chapel '("const" "config"
           "export" "extern"
           "inline" "iter"
           "module"
           "param" "private" "proc" "public"
           "require"
           "type"
           "use"
           "var"))

;; Class-style declarations
(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  chapel '("class" "record" "union"))

(c-lang-defconst c-type-start-kwds
  "Keywords that can start a type."
  chapel '("class" "enum" "record" "type" "union"))

;; Type aliases
(c-lang-defconst c-typedef-decl-kwds
  "Keywords introducing declarations where the identifier(s) being
declared are types.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is a type that's being defined in "class Foo
  ;; {...}").
  chapel '("type"))


(c-lang-defconst c-typeless-decl-kwds
  "Keywords introducing declarations where the \(first) identifier
\(declarator) follows directly after the keyword, without any type.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is the identifier being defined in "class Foo
  ;; {...}").
  chapel '("const" "iter" "module" "param" "proc" "type" "var"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  chapel '("use"))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  chapel '("do" "else" "then"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  chapel '("select"))
  ; putting "if" "for" "forall" here would cause
  ; weird indentation since parens not required

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  chapel '("break" "continue" "return" "yield"))

(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  chapel '("otherwise" "when"))

(c-lang-defconst c-constant-kwds
  "Keywords for constants."
  chapel '("false" "nil" "true"))

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  chapel '("new" "delete")) ;; Not really a keyword, but practically works as one.


(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  chapel '("align" "atomic" "begin" "by" "cobegin" "coforall" "dmapped" "for" "forall" "if" "in" "inout" "local" "noinit" "on" "out" "reduce" "ref" "scan" "serial" "single" "sparse" "sync" "where" "while" "with" "zip"))

;;; Chapel.

(defun c-font-lock-chapel-new (limit)
  ;; Assuming point is after a "new" word, check that it isn't inside
  ;; a string or comment, and if so try to fontify the type in the
  ;; allocation expression.  Nil is always returned.
  ;;
  ;; As usual, Chapel takes the prize in coming up with a hard to parse
  ;; syntax. :P

  (unless (c-skip-comments-and-strings limit)
    (save-excursion
      (catch 'false-alarm
	;; A "new" keyword is followed by one to three expressions, where
	;; the type is the middle one, and the only required part.
	(let (expr1-pos expr2-pos
	      ;; Enable recording of identifier ranges in `c-forward-type'
	      ;; etc for later fontification.  Not using
	      ;; `c-fontify-types-and-refs' here since the ranges should
	      ;; be fontified selectively only when an allocation
	      ;; expression is successfully recognized.
	      (c-record-type-identifiers t)
	      c-record-ref-identifiers
	      ;; The font-lock package in Emacs is known to clobber
	      ;; `parse-sexp-lookup-properties' (when it exists).
	      (parse-sexp-lookup-properties
	       (cc-eval-when-compile
		 (boundp 'parse-sexp-lookup-properties))))
	  (c-forward-syntactic-ws)

	  ;; The first placement arglist is always parenthesized, if it
	  ;; exists.
	  (when (eq (char-after) ?\()
	    (setq expr1-pos (1+ (point)))
	    (condition-case nil
		(c-forward-sexp)
	      (scan-error (throw 'false-alarm t)))
	    (c-forward-syntactic-ws))

	  ;; The second expression is either a type followed by some "*" or
	  ;; "[...]" or similar, or a parenthesized type followed by a full
	  ;; identifierless declarator.
	  (setq expr2-pos (1+ (point)))
	  (cond ((eq (char-after) ?\())
		((let ((c-promote-possible-types t))
		   (c-forward-type)))
		(t (setq expr2-pos nil)))

	  (when expr1-pos
	    (cond
	     ((not expr2-pos)
	      ;; No second expression, so the first has to be a
	      ;; parenthesized type.
	      (goto-char expr1-pos)
	      (let ((c-promote-possible-types t))
		(c-forward-type)))

	     ((eq (char-before expr2-pos) ?\()
	      ;; Got two parenthesized expressions, so we have to look
	      ;; closer at them to decide which is the type.  No need to
	      ;; handle `c-record-ref-identifiers' since all references
	      ;; has already been handled by other fontification rules.
	      (let (expr1-res expr2-res)

		(goto-char expr1-pos)
		(when (setq expr1-res (c-forward-type))
		  (unless (looking-at
			   (cc-eval-when-compile
			     (concat (c-lang-const c-symbol-start chapel)
				     "\\|[*:\)\[]")))
		    ;; There's something after the would-be type that
		    ;; can't be there, so this is a placement arglist.
		    (setq expr1-res nil)))

		(goto-char expr2-pos)
		(when (setq expr2-res (c-forward-type))
		  (unless (looking-at
			   (cc-eval-when-compile
			     (concat (c-lang-const c-symbol-start chapel)
				     "\\|[*:\)\[]")))
		    ;; There's something after the would-be type that can't
		    ;; be there, so this is an initialization expression.
		    (setq expr2-res nil))
		  (when (and (c-go-up-list-forward)
			     (progn (c-forward-syntactic-ws)
				    (eq (char-after) ?\()))
		    ;; If there's a third initialization expression
		    ;; then the second one is the type, so demote the
		    ;; first match.
		    (setq expr1-res nil)))

		;; We fontify the most likely type, with a preference for
		;; the first argument since a placement arglist is more
		;; unusual than an initializer.
		(cond ((memq expr1-res '(t known prefix)))
		      ((memq expr2-res '(t known prefix)))
		      ((eq expr1-res 'found)
		       (let ((c-promote-possible-types t))
			 (goto-char expr1-pos)
			 (c-forward-type)))
		      ((eq expr2-res 'found)
		       (let ((c-promote-possible-types t))
			 (goto-char expr2-pos)
			 (c-forward-type)))
		      ((and (eq expr1-res 'maybe) (not expr2-res))
		       (let ((c-promote-possible-types t))
			 (goto-char expr1-pos)
			 (c-forward-type)))
		      ((and (not expr1-res) (eq expr2-res 'maybe))
		       (let ((c-promote-possible-types t))
			 (goto-char expr2-pos)
			 (c-forward-type)))
		      ;; If both type matches are 'maybe then we're
		      ;; too uncertain to promote either of them.
		      )))))

	  ;; Fontify the type that now is recorded in
	  ;; `c-record-type-identifiers', if any.
	  (c-fontify-recorded-types-and-refs)))))
  nil)

(c-override-default-keywords 'chapel-font-lock-keywords)

(defconst chapel-font-lock-keywords-1 (c-lang-const c-matchers-1 chapel)
  "Minimal font locking for Chapel mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst chapel-font-lock-keywords-2 (c-lang-const c-matchers-2 chapel)
  "Fast normal font locking for Chapel mode.
In addition to `chapel-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `chapel-font-lock-extra-types', and the doc comment
styles specified by `c-doc-comment-style'.")

(defconst chapel-font-lock-keywords-3 (c-lang-const c-matchers-3 chapel)
  "Accurate normal font locking for Chapel mode.
Like `chapel-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `chapel-font-lock-extra-types'.")

(defvar chapel-font-lock-keywords chapel-font-lock-keywords-3
  "Default expressions to highlight in Chapel mode.")

(defun chapel-font-lock-keywords-2 ()
  (c-compose-keywords-list chapel-font-lock-keywords-2))
(defun chapel-font-lock-keywords-3 ()
  (c-compose-keywords-list chapel-font-lock-keywords-3))
(defun chapel-font-lock-keywords ()
  (c-compose-keywords-list chapel-font-lock-keywords))

(defvar cc-imenu-chapel-generic-expression
  '((nil "^[ \t]*\\def\\|(function\\)[ \t\n]+\\([a-zA-Z0-9_.:]+\\)" 2))
  "Imenu expression for Chapel-mode.  See `imenu-generic-expression'.")


;; Support for Chapel mode

;;;###autoload
(defvar chapel-mode-syntax-table nil
  "Syntax table used in chapel-mode buffers.")
(or chapel-mode-syntax-table
    (setq chapel-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table chapel))))

;; Nested block comments -- add "n" to the syntax table entry for "*"
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html#Syntax-Flags
(if (not running-xemacs) ; xemacs doesn't support the "n" modifier.
    (modify-syntax-entry ?* ". 23n" chapel-mode-syntax-table))


(defvar chapel-mode-abbrev-table nil
  "Abbreviation table used in chapel-mode buffers.")
(c-define-abbrev-table 'chapel-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar chapel-mode-map ()
  "Keymap used in chapel-mode buffers.")
(if chapel-mode-map
    nil
  (setq chapel-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for chapel
  (define-key chapel-mode-map "\C-c\C-e" 'c-macro-expand)
  (define-key chapel-mode-map "\C-c:"    'c-scope-operator)
  (define-key chapel-mode-map "<"        'c-electric-lt-gt)
  (define-key chapel-mode-map ">"        'c-electric-lt-gt))

(easy-menu-define c-chapel-menu chapel-mode-map "Chapel Mode Commands"
		  (cons "chapel" (c-lang-const c-mode-menu chapel)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.chpl\\'" . chapel-mode))

;;;###autoload
(defcustom chapel-mode-hook nil
  "*Hook called by `chapel-mode'."
  :type 'hook
  :group 'c)

;; For compatibility with Emacs < 24
(defalias 'chapel-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode chapel-mode chapel-parent-mode "Chapel"
  "Major mode for editing Chapel code.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `chapel-mode-hook'.

Key bindings:
\\{chapel-mode-map}"
  (c-initialize-cc-mode t)
  (setq local-abbrev-table chapel-mode-abbrev-table
	abbrev-mode t)
  (use-local-map chapel-mode-map)
  (c-init-language-vars chapel-mode)
  (c-common-init 'chapel-mode)
  (easy-menu-add c-chapel-menu)
  (cc-imenu-init cc-imenu-chapel-generic-expression)
  (run-hooks 'c-mode-common-hook 'chapel-mode-hook)
  (c-update-modeline))

(provide 'chapel-mode)

;;; chapel-mode.el ends here
