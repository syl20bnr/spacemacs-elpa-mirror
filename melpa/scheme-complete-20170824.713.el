;;; scheme-complete.el --- Smart auto completion for Scheme in Emacs

;;; Author: Alex Shinn
;;; Version: 0.9.7
;; Package-Version: 20170824.713

;;; This code is written by Alex Shinn and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; Commentary:
;;; This file provides a single function, `scheme-smart-complete',
;;; which you can use for intelligent, context-sensitive completion
;;; for any Scheme implementation.  To use it just load this file and
;;; bind that function to a key in your preferred mode:
;;;
;;; (autoload 'scheme-smart-complete "scheme-complete" nil t)
;;; (eval-after-load 'scheme
;;;   '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))
;;;
;;; Alternately, you may want to just bind TAB to the
;;; `scheme-complete-or-indent' function, which indents at the start
;;; of a line and otherwise performs the smart completion:
;;;
;;; (eval-after-load 'scheme
;;;   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))
;;;
;;;   Note: the completion uses a somewhat less common style than
;;;   typically found in other modes.  The first tab will complete the
;;;   longest prefix common to all possible completions.  The second
;;;   tab will show a list of those completions.  Subsequent tabs will
;;;   scroll that list.  You can't use the mouse to select from the
;;;   list - when you see what you want, just type the next one or
;;;   more characters in the symbol you want and hit tab again to
;;;   continue completing it.  Any key typed will bury the completion
;;;   list.  This ensures you can achieve a completion with the
;;;   minimal number of keystrokes without the completions window
;;;   lingering and taking up space.
;;;
;;; If you use eldoc-mode (included in Emacs), you can also get live
;;; scheme documentation with:
;;;
;;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;;; (add-hook 'scheme-mode-hook
;;;   (lambda ()
;;;     (make-local-variable 'eldoc-documentation-function)
;;;     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;;     (eldoc-mode)))
;;;
;;; You can enable slightly smarter indentation with
;;;
;;; (setq lisp-indent-function 'scheme-smart-indent-function)
;;;
;;; which basically ignores the scheme-indent-function property for
;;; locally overridden symbols (e.g. if you use the (let loop () ...)
;;; idiom it won't use the special loop indentation inside).
;;;
;;; There's a single custom variable, `scheme-default-implementation',
;;; which you can use to specify your preferred implementation when we
;;; can't infer it from the source code.
;;;
;;; That's all there is to it.

;;; History:
;;;  0.9.7: 2017/08/24 - improving caching, adding some missing (scheme char)
;;;                       bindings
;;;  0.9.6: 2017/04/10 - fix possible inf loop in enclosing-2-sexp-prefixes
;;;  0.9.5: 2017/04/02 - completiong for only/except/export, better caching
;;;  0.9.4: 2017/04/01 - don't open non-existant files
;;;  0.9.3: 2016/06/04 - string-cursors, bugfixes, speedups, introducing
;;:                      unit tests with ert
;;;  0.9.2: 2016/05/03 - several bugfixes
;;;  0.9.1: 2016/04/08 - fixing bug in cond-expand parsing
;;;  0.9.0: 2015/12/23 - R7RS support
;;; 0.8.11: 2013/02/20 - formatting for melpa packaging
;;; 0.8.10: 2010/01/31 - factoring out a `scheme-get-completions' utility
;;;                       (thanks to Scott Dolim), and not jumping to end
;;;                      of current symbol if there are no completions for it
;;;  0.8.9: 2009/10/28 - allowing indented module/library definitions,
;;;                      added various customizations for tab/indent behavior,
;;;                      complete jumps to end of current symbol
;;;  0.8.8: 2009/08/18 - fixing bug in scheme-directory-tree-files
;;                       with funny file names
;;;  0.8.7: 2009/07/18 - foof-loop support, don't complete current var,
;;                       updating chicken 4 module information
;;;  0.8.6: 2009/05/03 - fixing support for chicken 4 w/ unbalanced parens
;;;  0.8.5: 2009/04/30 - full support for chicken 4, fixed bug in caching
;;;  0.8.4: 2008/12/26 - numerous small bugfixes (Merry Christmas!)
;;;  0.8.3: 2008/10/06 - smart indent, inferring types from imported modules,
;;;                      optionally caching exports, chicken 4 support
;;;  0.8.2: 2008/07/04 - both TAB and M-TAB scroll results (thanks Peter Bex),
;;;                      better MATCH handling, fixed SRFI-55, other bugfixes
;;;  0.8.1: 2008/04/17 - great renaming, everthing starts with `scheme-'
;;;                      also, don't scan imported modules multiple times
;;;    0.8: 2008/02/08 - several parsing bugfixes on unclosed parenthesis
;;;                        (thanks to Kazushi NODA)
;;;                      filename completion works properly on absolute paths
;;;                      eldoc works properly on dotted lambdas
;;;    0.7: 2008/01/18 - handles higher-order types (for apply, map, etc.)
;;;                      smarter string completion (hostname, username, etc.)
;;;                      smarter type inference, various bugfixes
;;;    0.6: 2008/01/06 - more bugfixes (merry christmas)
;;;    0.5: 2008/01/03 - handling internal defines, records, smarter
;;;                      parsing
;;;    0.4: 2007/11/14 - silly bugfix plus better repo env support
;;;                      for searching chicken and gauche modules
;;;    0.3: 2007/11/13 - bugfixes, better inference, smart strings
;;;    0.2: 2007/10/15 - basic type inference
;;;    0.1: 2007/09/11 - initial release
;;;
;;;   What is this talk of 'release'? Klingons do not make software
;;;   'releases'. Our software 'escapes' leaving a bloody trail of
;;;   designers and quality assurance people in its wake.

(require 'cl)

;; this is just to eliminate some warnings when compiling - this file
;; should be loaded after 'scheme
(eval-when (compile)
  (require 'scheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can set the `scheme-default-implementation' to your preferred
;; implementation, for when we can't figure out the file from
;; heuristics.  Alternately, in any given buffer, just
;;
;; (setq *scheme-current-implementation* whatever)

(defvar *scheme-current-implementation* nil)
(make-variable-buffer-local '*scheme-current-implementation*)

(defvar *scheme-current-context* nil)
(make-variable-buffer-local '*scheme-current-context*)

;; most implementations use their name as the script name
(defvar *scheme-interpreter-alist*
  '(("chibi-scheme" . chibi)
    ("csi"  . chicken)
    ("gosh" . gauche)
    ("gsi"  . gambit)
    ))

(defvar in-mod-p nil)

(defgroup scheme-complete nil
  "Smart tab completion"
  :group 'scheme)

(defcustom scheme-default-implementation nil
  "Default scheme implementation to provide completion for
when scheme-complete can't infer the current implementation."
  :type 'symbol
  :group 'scheme-complete)

(defcustom scheme-always-use-default-implementation-p nil
  "Always use `scheme-default-implementation' instead of heuristics."
  :type 'symbol
  :group 'scheme-complete)

(defcustom scheme-complete-smart-indent-p t
  "Toggles using `scheme-smart-indent' for `scheme-complete-or-indent'."
  :type 'boolean
  :group 'scheme-complete)

(defcustom scheme-indent-before-complete-p nil
  "Toggles indenting the current line before completing."
  :type 'boolean
  :group 'scheme-complete)

(defcustom scheme-complete-empty-tab-behavior 'complete
  "Behavior for `scheme-complete-or-indent' when completing an empty symbol.
A value of `complete' (the default) will complete all symbols
elligible according to the normal type inference rules.  Since
they are not being filtered by any prefix, the list may be long -
you can scroll through it or switch to the *Completions* buffer
to view it.  A value of `indent' will assume you meant to indent
at that location, and `beep' will just beep and do nothing."
  :type '(choice (const complete) (const indent) (const beep))
  :group 'scheme-complete)

(defcustom scheme-complete-from-end-of-symbol-p t
  "If true jump to the end when completing from the middle of a symbol."
  :type 'boolean
  :group 'scheme-complete)

(defcustom scheme-complete-cache-p t
  "Toggles caching of module/load export information."
  :type 'boolean
  :group 'scheme-complete)

(defcustom scheme-interleave-definitions-p nil
  "Allow internal defines to be mixed with expressions."
  :type 'boolean
  :group 'scheme-complete)

(defcustom scheme-complete-recursive-inference-p t
  "Recursively infer types from imported modules rather than shallow parses."
  :type 'boolean
  :group 'scheme-complete)

(defcustom *scheme-r7rs-extension* ".sld"
  "File extension for R7RS library declarations."
  :type 'string
  :group 'scheme-complete)

(defcustom *scheme-max-decl-file-search-depth* 5
  "Maximum number of directories to backtrack looking for including libraries."
  :type 'integer
  :group 'scheme-complete)

(defcustom *scheme-use-r7rs* t
  "Set to nil to restore legacy behavior."
  :type 'boolean
  :group 'scheme-complete)

(defcustom *scheme-default-library-path* nil
  "Library path to search for modules. Inferred (possibly slowly) if not set."
  :type '(repeat file)
  :group 'scheme-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; info
;;
;; identifier type [doc-string no-type-display?]
;;
;; types:
;;
;;   pair, number, symbol, etc.
;;   (lambda (param-types) [return-type])
;;   (syntax (param-types) [return-type])
;;   (set name values ...)
;;   (flags name values ...)
;;   (list type)
;;   (string expander)
;;   (special type function [outer-function])

(defvar *scheme-r7rs-lib-decl-info*
  '((begin (syntax (body \.\.\.)))
    (cond-expand (syntax (clause \.\.\.)))
    (export (syntax identifier \.\.\.))
    (import (special list scheme-available-modules))
    (include (syntax filename \.\.\.))
    (include-ci (syntax filename \.\.\.))
    (include-library-declarations (syntax filename \.\.\.))))

(defvar *scheme-r7rs-info*
  `(((scheme base)
     (_ (syntax) "auxiliary syntax")
     (... (syntax) "auxiliary syntax")
     (* (lambda (z1 \.\.\.) z))
     (+ (lambda (z1 \.\.\.) z))
     (- (lambda (z1 \.\.\.) z))
     (/ (lambda (z1 \.\.\.) z))
     (<= (lambda (x1 x2 \.\.\.) bool) "returns #t iff the arguments are monotonically nondecreasing")
     (< (lambda (x1 x2 \.\.\.) bool) "returns #t iff the arguments are monotonically increasing")
     (=> (syntax) "auxiliary syntax")
     (= (lambda (z1 z2 \.\.\.) bool) "returns #t iff the arguments are all equal")
     (>= (lambda (x1 x2 \.\.\.) bool) "returns #t iff the arguments are monotonically nonincreasing")
     (> (lambda (x1 x2 \.\.\.) bool) "returns #t iff the arguments are monotonically decreasing")
     (abs (lambda (x1) x2) "returns the absolute value of X")
     (and (syntax (expr \.\.\.)) "evaluate EXPRs while true, return last")
     (append (lambda (list \.\.\.) list) "concatenates the list arguments")
     (apply (lambda ((lambda obj a) obj \.\.\.) a) "procedure application")
     (assoc (lambda (obj list)) "the element of LIST whose car is equal? to OBJ")
     (assq (lambda (obj list)) "the element of LIST whose car is eq? to OBJ")
     (assv (lambda (obj list)) "the element of LIST whose car is eqv? to OBJ")
     (begin (syntax (expr \.\.\.)) "evaluate each EXPR in turn and return the last")
     (boolean? (lambda (obj) bool) "returns #t iff OBJ is #t or #f")
     (boolean=? (lambda (bool1 bool2) bool))
     (bytevector (lambda (i \.\.\.) bytevector))
     (bytevector-copy (lambda (bytevector :optional start end) bytevector))
     (bytevector-append (lambda (bytevector \.\.\.) bytevector))
     (bytevector-copy! (lambda (bytevector i \.\.\. bytevector :optional start end) undefined))
     (bytevector-length (lambda (bytevector) i))
     (bytevector-u8-ref (lambda (bytevector i) i))
     (bytevector-u8-set! (lambda (bytevector i i) undefined))
     (bytevector? (lambda (obj) bool))
     (caar (lambda (pair) obj))
     (cadr (lambda (pair) obj))
     (call-with-current-continuation (lambda (proc) obj) "goto on steroids")
     (call-with-values (lambda (producer consumer) obj))
     (call-with-port (lambda (port proc) obj))
     (call/cc (lambda (proc) obj) "short for call-with-current-continuation")
     (case (syntax (expr clause \.\.\.)) "look for EXPR among literal lists")
     (car (lambda (pair) obj))
     (cdr (lambda (pair) obj))
     (cdar (lambda (pair) obj))
     (cddr (lambda (pair) obj))
     (ceiling (lambda (x1) n) "smallest integer not smaller than X")
     (char->integer (lambda (ch) int))
     (char<=? (lambda (ch1 ch2) bool))
     (char<? (lambda (ch1 ch2) bool))
     (char=? (lambda (ch1 ch2) bool))
     (char>=? (lambda (ch1 ch2) bool))
     (char>? (lambda (ch1 ch2) bool))
     (char? (lambda (obj) bool) "returns #t iff OBJ is a character")
     (complex? (lambda (obj) bool) "returns #t iff OBJ is a complex number")
     (cond (syntax (clause \.\.\.)) "try each clause until one succeeds")
     (cond-expand (syntax (clause \.\.\.)))
     (cons (lambda (obj1 obj2) pair) "create a newly allocated pair")
     (define-syntax (syntax (identifier body \.\.\.) undefined) "create a macro")
     (define (syntax (identifier value) undefined) "define a new variable")
     (define-values (syntax (identifier \.\.\.) expr))
     (define-record-type (syntax name (make field \.\.\.) pred-name (field get set) \.\.\.))
     (denominator (lambda (rational) n))
     (do (syntax (vars finish body \.\.\.)) "simple iterator")
     (dynamic-wind (lambda (before-thunk thunk after-thunk) obj))
     (else (syntax) "auxiliary syntax")
     (eq? (lambda (obj1 obj2) bool) "finer grained version of EQV?")
     (equal? (lambda (obj1 obj2) bool) "recursive equivalence")
     (eqv? (lambda (obj1 obj2) bool) "returns #t if OBJ1 and OBJ2 are the same object")
     (error (lambda (msg args \.\.\.) error))
     (error-object? (lambda (obj) bool))
     (error-object-message (lambda (error) string))
     (error-object-irritants (lambda (error) list))
     (even? (lambda (n) bool))
     (exact (lambda (z) z))
     (exact-integer-sqrt (lambda (n) n))
     (exact-integer? (lambda (z) bool))
     (exact? (lambda (z) bool) "returns #t iff Z is exact")
     (expt (lambda (z1 z2) z) "returns Z1 raised to the Z2 power")
     (floor (lambda (x1) n) "largest integer not larger than X")
     (for-each (lambda ((lambda obj a) obj \.\.\.) undefined) "apply PROC to each element of LIST in order")
     (gcd (lambda (n1 \.\.\.) n) "greatest common divisor")
     (floor/ (lambda (x1) (values n n)))
     (floor-quotient (lambda (x1) n))
     (floor-remainder (lambda (x1) n))
     (truncate/ (lambda (x1) (values n n)))
     (truncate-quotient (lambda (x1) n))
     (truncate-remainder (lambda (x1) n))
     (features (lambda () (list symbol)))
     (guard (syntax ((var clause \.\.\.) body \.\.\.)))
     (if (syntax (cond then else)) "conditional evaluation")
     (include (syntax filename \.\.\.))
     (include-ci (syntax filename \.\.\.))
     (inexact (lambda (z) z))
     (inexact? (lambda (z) bool) "returns #t iff Z is inexact")
     (integer->char (lambda (int) ch))
     (integer? (lambda (obj) bool) "returns #t iff OBJ is an integer")
     (lambda (syntax (params body \.\.\.)) "procedure syntax")
     (lcm (lambda (n2 \.\.\.) n) "least common multiple")
     (length (lambda (list) n))
     (let* (syntax (vars body \.\.\.)) "bind new local variables sequentially")
     (let-syntax (syntax (syntaxes body \.\.\.)) "a local macro")
     (letrec* (syntax (vars body \.\.\.)) "bind new local variables recursively in order")
     (letrec-syntax (syntax (syntaxes body \.\.\.)) "a local macro")
     (let-values (syntax (vars body \.\.\.)))
     (let*-values (syntax (vars body \.\.\.)))
     (letrec (syntax (vars body \.\.\.)) "bind new local variables recursively")
     (let (syntax (vars body \.\.\.)) "bind new local variables in parallel")
     (list-copy (lambda (list) list))
     (list->string (lambda (list) str))
     (list->vector (lambda (list) vec))
     (list-ref (lambda (list k) obj) "returns the Kth element of LIST")
     (list-set! (lambda (list k val) undefined))
     (list-tail (lambda (list k) list) "returns the Kth cdr of LIST")
     (list? (lambda (obj) bool) "returns #t iff OBJ is a proper list")
     (list (lambda (obj \.\.\.) list) "returns a newly allocated list")
     (make-bytevector (lambda (k :optional u8) bytevector))
     (make-list (lambda (k :optional obj) list))
     (make-parameter (lambda (init :optional converter) parameter))
     (make-string (lambda (k :optional ch) str) "a new string of length k")
     (make-vector (lambda (len :optional fill) vec) "a new vector of K elements")
     (map (lambda ((lambda (obj1 . obj2) a) list \.\.\.) (list a)) "a new list of PROC applied to every element of LIST")
     (max (lambda (x1 x2 \.\.\.) x3) "returns the maximum of the arguments")
     (member (lambda (obj list)) "the sublist of LIST whose car is equal? to OBJ")
     (memq (lambda (obj list)) "the sublist of LIST whose car is eq? to OBJ")
     (memv (lambda (obj list)) "the sublist of LIST whose car is eqv? to OBJ")
     (min (lambda (x1 x2 \.\.\.) x3) "returns the minimum of the arguments")
     (modulo (lambda (n1 n2) n) "same sign as N2")
     (negative? (lambda (x1) bool))
     (not (lambda (obj) bool) "returns #t iff OBJ is false")
     (null? (lambda (obj) bool) "returns #t iff OBJ is the empty list")
     (number->string (lambda (z :optional radix) str))
     (number? (lambda (obj) bool) "returns #t iff OBJ is a number")
     (numerator (lambda (rational) n))
     (odd? (lambda (n) bool))
     (or (syntax (expr \.\.\.)) "return the first true EXPR")
     (pair? (lambda (obj) bool) "returns #t iff OBJ is a pair")
     (parameterize (syntax (((id value) \.\.\.) body \.\.\.)))
     (positive? (lambda (x1) bool))
     (procedure? (lambda (obj) bool) "returns #t iff OBJ is a procedure")
     (quasiquote (syntax (expr)) "quote literals allowing escapes")
     (quote (syntax (expr)) "represent EXPR literally without evaluating it")
     (quotient (lambda (n1 n2) n) "integer division")
     (raise-continuable (lambda (obj) error))
     (raise (lambda (obj) error))
     (rational? (lambda (obj) bool) "returns #t iff OBJ is a rational number")
     (rationalize (lambda (x1 y) n) "rational number differing from X by at most Y")
     (real? (lambda (obj) bool) "returns #t iff OBJ is a real number")
     (remainder (lambda (n1 n2) n) "same sign as N1")
     (reverse (lambda (list) list))
     (round (lambda (x1) n) "round to even (banker's rounding)")
     (set! (syntax (identifier value) undefined) "set the value of a variable")
     (set-car! (lambda (pair obj) undefined))
     (set-cdr! (lambda (pair obj) undefined))
     (square (lambda (z) z))
     (string->list (lambda (str) list))
     (string->number (lambda (str :optional radix) z))
     (string->symbol (lambda (str) symbol))
     (string->vector (lambda (str) vector))
     (string-append (lambda (str \.\.\.) str) "concatenate the string arguments")
     (string-copy (lambda (str) str))
     (string-copy! (lambda (str k str :optional start end) undefined))
     (string-fill! (lambda (str ch) undefined) "set every char in STR to CH")
     (string-for-each (lambda (proc str \.\.\.) undefined))
     (string-length (lambda (str) n) "the number of characters in STR")
     (string-map (lambda (proc str \.\.\.) str))
     (string-ref (lambda (str i) ch) "the Ith character of STR")
     (string-set! (lambda (str i ch) undefined) "set the Ith character of STR to CH")
     (string<=? (lambda (str1 str2) bool))
     (string<? (lambda (str1 str2) bool))
     (string=? (lambda (str1 str2) bool))
     (string>=? (lambda (str1 str2) bool))
     (string>? (lambda (str1 str2) bool))
     (string? (lambda (obj) bool) "returns #t iff OBJ is a string")
     (string (lambda (ch \.\.\.) str) "a new string made of the char arguments")
     (substring (lambda (str start end) str))
     (symbol->string (lambda (symbol) str))
     (symbol=? (lambda (symbol1 symbol2) bool))
     (symbol? (lambda (obj) bool) "returns #t iff OBJ is a symbol")
     (syntax-error (syntax (msg obj \.\.\.) error))
     (syntax-rules (syntax (literals clauses \.\.\.) undefined) "simple macro language")
     (truncate (lambda (x1) n) "drop fractional part")
     (values (lambda (obj \.\.\.) (values obj \.\.\.)) "send multiple values to the calling continuation")
     (unquote (syntax (expr)) "escape an expression inside quasiquote")
     (unquote-splicing (syntax (expr)) "escape and splice a list expression inside quasiquote")
     (vector-append (lambda (vec \.\.\.) vec))
     (vector-copy (lambda (vec :optional start end) vec))
     (vector-copy! (lambda (vec k vec :optional start end) undefined))
     (vector->list (lambda (vec) list))
     (vector->string (lambda (vec) str))
     (vector-fill! (lambda (vec obj) undefined) "set every element in VEC to OBJ")
     (vector-for-each (lambda (proc vec \.\.\.) undefined))
     (vector-length (lambda (vec) n) "the number of elements in VEC")
     (vector-map (lambda (proc vec \.\.\.) map))
     (vector-ref (lambda (vec i) obj) "the Ith element of VEC")
     (vector-set! (lambda (vec i obj) undefined) "set the Ith element of VEC to OBJ")
     (vector? (lambda (obj) bool) "returns #t iff OBJ is a vector")
     (vector (lambda (obj \.\.\.) vec))
     (zero? (lambda (z) bool))
     (when (syntax (expr body \.\.\.)))
     (with-exception-handler (lambda (proc thunk)))
     (unless (syntax (expr body \.\.\.)))
     (binary-port? (lambda (obj) bool))
     (char-ready? (lambda (:optional input-port) bool))
     (textual-port? (lambda (obj) bool))
     (close-port (lambda (port) undefined))
     (close-input-port (lambda (input-port)))
     (close-output-port (lambda (output-port)))
     (current-error-port (lambda () output-port) "the default output for error messages")
     (current-input-port (lambda () input-port) "the default input for read procedures")
     (current-output-port (lambda () output-port) "the default output for write procedures")
     (eof-object (lambda () eof-object))
     (eof-object? (lambda (obj) bool) "returns #t iff OBJ is the end-of-file object")
     (file-error? (lambda (obj) bool))
     (flush-output-port (lambda (:optional output-port) undefined))
     (get-output-string (lambda (output-port) str))
     (get-output-bytevector (lambda (output-port) bytevector))
     (input-port? (lambda (obj) bool) "returns #t iff OBJ is an input port")
     (input-port-open? (lambda (input-port) bool))
     (newline (lambda (:optional output-port) undefined) "send a linefeed")
     (open-input-string (lambda (str) input-port))
     (open-input-bytevector (lambda (bytevector) input-port))
     (open-output-string (lambda () output-port))
     (open-output-bytevector (lambda () output-port))
     (output-port? (lambda (obj) bool) "returns #t iff OBJ is an output port")
     (output-port-open? (lambda (output-port) bool))
     (peek-char (lambda (:optional input-port) ch))
     (peek-u8 (lambda (:optional input-port) u8))
     (port? (lambda (obj) port))
     (read-bytevector (lambda (k :optional input-port) bytevector))
     (read-bytevector! (lambda (bytevector :optional input-port start end) undefined))
     (read-char (lambda (:optional input-port) ch) "read a single character")
     (read-error? (lambda (obj) error))
     (read-line (lambda (:optional input-port) str))
     (read-string (lambda (k :optional input-port)))
     (read-u8 (lambda (:optional input-port) u8))
     (string->utf8 (lambda (str :optional start end) bytevector))
     (utf8->string (lambda (bytevector :optional start end) str))
     (u8-ready? (lambda (:optional input-port) bool))
     (write-bytevector (lambda (bytevector :optional output-port start end)))
     (write-char (lambda (char :optional output-port) undefined) "write a single character")
     (write-string (lambda (str :optional output-port start end)))
     (write-u8 (lambda (u8 :optional output-port))))
    ((scheme case-lambda)
     (case-lambda (syntax (clauses \.\.\.) procedure)))
    ((scheme char)
     (digit-value (lambda (ch) n))
     (char-downcase (lambda (ch) ch))
     (char-foldcase (lambda (ch) ch))
     (char-upcase (lambda (ch) ch))
     (char-alphabetic? (lambda (ch) bool))
     (char-lower-case? (lambda (ch) bool))
     (char-upper-case? (lambda (ch) bool))
     (char-numeric? (lambda (ch) bool))
     (char-whitespace? (lambda (ch) bool))
     (char-ci<=? (lambda (ch1 ch2 \.\.\.) bool))
     (char-ci<? (lambda (ch1 ch2 \.\.\.) bool))
     (char-ci=? (lambda (ch1 ch2 \.\.\.) bool))
     (char-ci>=? (lambda (ch1 ch2 \.\.\.) bool))
     (char-ci>? (lambda (ch1 ch2 \.\.\.) bool))
     (string-downcase (lambda (str) str))
     (string-foldcase (lambda (str) str))
     (string-upcase (lambda (str) str))
     (string-ci<=? (lambda (str1 str2 \.\.\.) bool))
     (string-ci<? (lambda (str1 str2 \.\.\.) bool))
     (string-ci=? (lambda (str1 str2 \.\.\.) bool))
     (string-ci>=? (lambda (str1 str2 \.\.\.) bool))
     (string-ci>? (lambda (str1 str2 \.\.\.) bool)))
    ((scheme complex)
     (angle (lambda (z) x1))
     (imag-part (lambda (z) x1))
     (magnitude (lambda (z) x1))
     (make-polar (lambda (x1 x2) z) "create a complex number")
     (make-rectangular (lambda (x1 x2) z) "create a complex number")
     (real-part (lambda (z) x1)))
    ((scheme cxr)
     (caaaar (lambda (pair))) (caaadr (lambda (pair)))
     (caadar (lambda (pair))) (caaddr (lambda (pair)))
     (cadaar (lambda (pair))) (cadadr (lambda (pair)))
     (caddar (lambda (pair))) (cadddr (lambda (pair)))
     (cdaaar (lambda (pair))) (cdaadr (lambda (pair)))
     (cdadar (lambda (pair))) (cdaddr (lambda (pair)))
     (cddaar (lambda (pair))) (cddadr (lambda (pair)))
     (cdddar (lambda (pair))) (cddddr (lambda (pair)))
     (caaar (lambda (pair))) (caadr (lambda (pair)))
     (cadar (lambda (pair))) (caddr (lambda (pair)))
     (cdaar (lambda (pair))) (cdadr (lambda (pair)))
     (cddar (lambda (pair))) (cdddr (lambda (pair))))
    ((scheme eval)
     (eval (lambda (expr env)))
     (environment (lambda (list \.\.\.) env)))
    ((scheme file)
     (call-with-input-file (lambda (path proc) input-port))
     (call-with-output-file (lambda (path proc) output-port))
     (delete-file (lambda (path) undefined))
     (file-exists? (lambda (path) bool))
     (open-input-file (lambda (path) input-port))
     (open-output-file (lambda (path) output-port))
     (open-binary-input-file (lambda (path) input-port))
     (open-binary-output-file (lambda (path) output-port))
     (with-input-from-file (lambda (path thunk) obj))
     (with-output-to-file (lambda (path thunk) obj)))
    ((scheme inexact)
     (acos (lambda (z) z) "arccosine function")
     (asin (lambda (z) z) "arcsine function")
     (atan (lambda (z) z) "arctangent function")
     (cos (lambda (z) z) "cosine function")
     (exp (lambda (z) z) "e^Z")
     (finite? (lambda (z) bool))
     (infinite? (lambda (z) bool))
     (log (lambda (z) z) "natural logarithm of Z")
     (nan? (lambda (z) bool))
     (sin (lambda (z) z) "sine function")
     (sqrt (lambda (z) z) "principal square root of Z")
     (tan (lambda (z) z)))
    ((scheme lazy)
     (delay (syntax (expr)) "create a promise to evaluate EXPR")
     (delay-force (syntax (expr)))
     (force (lambda (promise) obj) "force the delayed value of PROMISE")
     (make-promise (lambda (obj) promise))
     (promise? (lambda (obj) bool)))
    ((scheme load)
     (load (lambda (path) undefined)))
    ((scheme process-context)
     (get-environment-variable (lambda (str) (or str bool)))
     (get-environment-variables (lambda () (list (pair str str))))
     (command-line (lambda () (list str)))
     (emergency-exit (lambda (:optional obj) undefined))
     (exit (lambda (:optional obj) undefined)))
    ((scheme read)
     (read (lambda (:optional input-port) obj)))
    ((scheme repl)
     (interaction-environment (lambda () env)))
    ((scheme time)
     (current-second (lambda () x1))
     (current-jiffy (lambda () n))
     (jiffies-per-second (lambda () n)))
    ((scheme write)
     (write (lambda (obj :optional output-port) undefined) "write an object, handling cycles")
     (write-shared (lambda (obj :optional output-port) undefined) "write an object showing all shared structure")
     (write-simple (lambda (obj :optional output-port) undefined) "write a non-cyclic object")
     (display (lambda (obj :optional output-port) undefined) "display an object"))
    ))

(defvar *scheme-r5rs-info*
  nil)

(defvar *scheme-r5rs-bindings*
  '(define set! let let* letrec lambda if cond
    case delay and or begin do quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax syntax-rules eqv? eq? equal? not
    boolean? number? complex? real? rational? integer? exact? inexact? = < >
    <= >= zero? positive? negative? odd? even? max min + * - / abs quotient
    remainder modulo gcd lcm numerator denominator floor ceiling truncate
    round rationalize exp log sin cos tan asin acos atan sqrt expt
    make-rectangular make-polar real-part imag-part magnitude angle
    number->string string->number pair? cons car cdr set-car! set-cdr! caar
    cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
    caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
    cdaddr cddaar cddadr cdddar cddddr null? list? list length append reverse
    list-tail list-ref memq memv member assq assv assoc symbol? symbol->string
    string->symbol char? char=? char<? char>? char<=? char>=? char-ci=?
    char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic? char-numeric?
    char-whitespace? char-upper-case? char-lower-case? char->integer
    integer->char char-upcase char-downcase string? make-string string
    string-length string-ref string-set! string=? string-ci=? string<?
    string>? string<=? string>=? string-ci<? string-ci>? string-ci<=?
    string-ci>=? substring string-append string->list list->string string-copy
    string-fill! vector? make-vector vector vector-length vector-ref
    vector-set! vector->list list->vector vector-fill! procedure? apply map
    for-each force call-with-current-continuation values call-with-values
    dynamic-wind scheme-report-environment null-environment
    call-with-input-file call-with-output-file input-port? output-port?
    current-input-port current-output-port with-input-from-file
    with-output-to-file open-input-file open-output-file close-input-port
    close-output-port read read-char peek-char eof-object? char-ready? write
    display newline write-char load eval))

(defun scheme-r5rs-info ()
  (unless *scheme-r5rs-info*
    (setq *scheme-r5rs-info*
          (append
           '((exact->inexact (lambda (z) z))
             (inexact->exact (lambda (z) z)))
           (mapcar #'(lambda (x)
                       (scheme-env-lookup *scheme-r7rs-info* x))
                   *scheme-r5rs-bindings*))))
  *scheme-r5rs-info*)

(defvar *scheme-srfi-info*
  [
   ;; SRFI 0
   ("Feature-based conditional expansion construct"
    (cond-expand (syntax (clause \.\.\.))))
   
   ;; SRFI 1
   ("List Library"
    (xcons (lambda (object object) pair))
    (cons* (lambda (object \.\.\.) pair))
    (make-list (lambda (integer :optional object) list))
    (list-tabulate (lambda (integer procedure) list))
    (list-copy (lambda (list) list))
    (circular-list (lambda (object \.\.\.) list))
    (iota (lambda (integer :optional integer integer) list))
    (proper-list? (lambda (object) bool))
    (circular-list? (lambda (object) bool))
    (dotted-list? (lambda (object) bool))
    (not-pair? (lambda (object) bool))
    (null-list? (lambda (object) bool))
    (list= (lambda (procedure list \.\.\.) bool))
    (first (lambda (pair)))
    (second (lambda (pair)))
    (third (lambda (pair)))
    (fourth (lambda (pair)))
    (fifth (lambda (pair)))
    (sixth (lambda (pair)))
    (seventh (lambda (pair)))
    (eighth (lambda (pair)))
    (ninth (lambda (pair)))
    (tenth (lambda (pair)))
    (car+cdr (lambda (pair)))
    (take (lambda (pair integer) list))
    (drop (lambda (pair integer) list))
    (take-right (lambda (pair integer) list))
    (drop-right (lambda (pair integer) list))
    (take! (lambda (pair integer) list))
    (drop-right! (lambda (pair integer) list))
    (split-at (lambda (pair integer) list))
    (split-at! (lambda (pair integer) list))
    (last (lambda (pair) obj))
    (last-pair (lambda (pair) pair))
    (length+ (lambda (object) n))
    (concatenate (lambda (list) list))
    (append! (lambda (list \.\.\.) list))
    (concatenate! (lambda (list) list))
    (reverse! (lambda (list) list))
    (append-reverse (lambda (list list) list))
    (append-reverse! (lambda (list list) list))
    (zip (lambda (list \.\.\.) list))
    (unzip1 (lambda (list) list))
    (unzip2 (lambda (list) list))
    (unzip3 (lambda (list) list))
    (unzip4 (lambda (list) list))
    (unzip5 (lambda (list) list))
    (count (lambda ((lambda (obj1 . obj2)) list \.\.\.) n))
    (fold (lambda ((lambda (obj1 obj2 . obj3) a) object list \.\.\.) a))
    (unfold (lambda (procedure procedure procedure object :optional procedure) obj))
    (pair-fold (lambda ((lambda obj a) object list \.\.\.) a))
    (reduce (lambda ((lambda (obj1 obj2 . obj3) a) object list \.\.\.) a))
    (fold-right (lambda ((lambda (obj1 obj2 . obj3) a) object list \.\.\.) a))
    (unfold-right (lambda (procedure procedure procedure object :optional object) obj))
    (pair-fold-right (lambda ((lambda (obj1 obj2 . obj3) a) object list \.\.\.) a))
    (reduce-right (lambda ((lambda (obj1 obj2 . obj3) a) object list \.\.\.) a))
    (append-map (lambda ((lambda (obj1 . obj2)) list \.\.\.) list))
    (append-map! (lambda ((lambda (obj1 . obj2)) list \.\.\.) list))
    (map! (lambda ((lambda (obj1 . obj2)) list \.\.\.) list))
    (pair-for-each (lambda ((lambda (obj1 . obj2)) list \.\.\.) undefined))
    (filter-map (lambda ((lambda (obj1 . obj2)) list \.\.\.) list))
    (map-in-order (lambda ((lambda (obj1 . obj2)) list \.\.\.) list))
    (filter (lambda ((lambda (obj1 . obj2)) list) list))
    (partition (lambda ((lambda (obj) bool) list) list))
    (remove (lambda ((lambda (obj1) bool) list) list))
    (filter! (lambda ((lambda (obj1) bool) list) list))
    (partition! (lambda ((lambda (obj1) bool) list) list))
    (remove! (lambda ((lambda (obj1) bool) list) list))
    (find (lambda ((lambda (obj1) bool) list) obj))
    (find-tail (lambda ((lambda (obj1) bool) list) obj))
    (any (lambda ((lambda (obj1 . obj2) a) list \.\.\.) a))
    (every (lambda ((lambda (obj1 . obj2) a) list \.\.\.) a))
    (list-index (lambda ((lambda (obj1 . obj2)) list \.\.\.) (or bool integer)))
    (take-while (lambda ((lambda (obj)) list) list))
    (drop-while (lambda ((lambda (obj)) list) list))
    (take-while! (lambda ((lambda (obj)) list) list))
    (span (lambda ((lambda (obj)) list) list))
    (break (lambda ((lambda (obj)) list) list))
    (span! (lambda ((lambda (obj)) list) list))
    (break! (lambda ((lambda (obj)) list) list))
    (delete (lambda (object list :optional procedure) list))
    (delete-duplicates (lambda (list :optional procedure) list))
    (delete! (lambda (obj list :optional procedure) list))
    (delete-duplicates! (lambda (list :optional procedure) list))
    (alist-cons (lambda (obj1 obj2 alist) alist))
    (alist-copy (lambda (alist) alist))
    (alist-delete (lambda (obj alist) alist))
    (alist-delete! (lambda (obj alist) alist))
    (lset<= (lambda (procedure list \.\.\.) bool))
    (lset= (lambda (procedure list \.\.\.) bool))
    (lset-adjoin (lambda (procedure list object \.\.\.) list))
    (lset-union (lambda (procedure list \.\.\.) list))
    (lset-union! (lambda (procedure list \.\.\.) list))
    (lset-intersection (lambda (procedure list \.\.\.) list))
    (lset-intersection! (lambda (procedure list \.\.\.) list))
    (lset-difference (lambda (procedure list \.\.\.) list))
    (lset-difference! (lambda (procedure list \.\.\.) list))
    (lset-xor (lambda (procedure list \.\.\.) list))
    (lset-xor! (lambda (procedure list \.\.\.) list))
    (lset-diff+intersection (lambda (procedure list \.\.\.) list))
    (lset-diff+intersection! (lambda (procedure list \.\.\.) list))

    )

   ;; SRFI 2
   ("AND-LET*: an AND with local bindings, a guarded LET* special form"
    (and-let* (syntax (bindings body \.\.\.))))

   ()

   ;; SRFI 4
   ("Homogeneous numeric vector datatypes"

    (u8vector? (lambda (obj) bool))
    (make-u8vector (lambda (size integer) u8vector))
    (u8vector (lambda (integer \.\.\.) u8vector))
    (u8vector-length (lambda (u8vector) n))
    (u8vector-ref (lambda (u8vector i) int))
    (u8vector-set! (lambda (u8vector i u8value) undefined))
    (u8vector->list (lambda (u8vector) list))
    (list->u8vector (lambda (list) u8vector))

    (s8vector? (lambda (obj) bool))
    (make-s8vector (lambda (size integer) s8vector))
    (s8vector (lambda (integer \.\.\.) s8vector))
    (s8vector-length (lambda (s8vector) n))
    (s8vector-ref (lambda (s8vector i) int))
    (s8vector-set! (lambda (s8vector i s8value) undefined))
    (s8vector->list (lambda (s8vector) list))
    (list->s8vector (lambda (list) s8vector))

    (u16vector? (lambda (obj) bool))
    (make-u16vector (lambda (size integer) u16vector))
    (u16vector (lambda (integer \.\.\.)))
    (u16vector-length (lambda (u16vector) n))
    (u16vector-ref (lambda (u16vector i) int))
    (u16vector-set! (lambda (u16vector i u16value) undefined))
    (u16vector->list (lambda (u16vector) list))
    (list->u16vector (lambda (list) u16vector))

    (s16vector? (lambda (obj) bool))
    (make-s16vector (lambda (size integer) s16vector))
    (s16vector (lambda (integer \.\.\.) s16vector))
    (s16vector-length (lambda (s16vector) n))
    (s16vector-ref (lambda (s16vector i) int))
    (s16vector-set! (lambda (s16vector i s16value) undefined))
    (s16vector->list (lambda (s16vector) list))
    (list->s16vector (lambda (list) s16vector))

    (u32vector? (lambda (obj) bool))
    (make-u32vector (lambda (size integer) u32vector))
    (u32vector (lambda (integer \.\.\.) u32vector))
    (u32vector-length (lambda (u32vector) n))
    (u32vector-ref (lambda (u32vector i) int))
    (u32vector-set! (lambda (u32vector i u32value) undefined))
    (u32vector->list (lambda (u32vector) list))
    (list->u32vector (lambda (list) u32vector))

    (s32vector? (lambda (obj) bool))
    (make-s32vector (lambda (size integer) s32vector))
    (s32vector (lambda (integer \.\.\.) s32vector))
    (s32vector-length (lambda (s32vector) n))
    (s32vector-ref (lambda (s32vector i) int))
    (s32vector-set! (lambda (s32vector i s32value) undefined))
    (s32vector->list (lambda (s32vector) list))
    (list->s32vector (lambda (list) s32vector))

    (u64vector? (lambda (obj) bool))
    (make-u64vector (lambda (size integer) u64vector))
    (u64vector (lambda (integer \.\.\.) u64vector))
    (u64vector-length (lambda (u64vector) n))
    (u64vector-ref (lambda (u64vector i) int))
    (u64vector-set! (lambda (u64vector i u64value) undefined))
    (u64vector->list (lambda (u64vector) list))
    (list->u64vector (lambda (list) u64vector))

    (s64vector? (lambda (obj) bool))
    (make-s64vector (lambda (size integer) s64vector))
    (s64vector (lambda (integer \.\.\.) s64vector))
    (s64vector-length (lambda (s64vector) n))
    (s64vector-ref (lambda (s64vector i) int))
    (s64vector-set! (lambda (s64vector i s64value) undefined))
    (s64vector->list (lambda (s64vector) list))
    (list->s64vector (lambda (list) s64vector))

    (f32vector? (lambda (obj) bool))
    (make-f32vector (lambda (size integer) f32vector))
    (f32vector (lambda (number \.\.\.) f32vector))
    (f32vector-length (lambda (f32vector) n))
    (f32vector-ref (lambda (f32vector i) int))
    (f32vector-set! (lambda (f32vector i f32value) undefined))
    (f32vector->list (lambda (f32vector) list))
    (list->f32vector (lambda (list) f32vector))

    (f64vector? (lambda (obj) bool))
    (make-f64vector (lambda (size integer) f64vector))
    (f64vector (lambda (number \.\.\.) f64vector))
    (f64vector-length (lambda (f64vector) n))
    (f64vector-ref (lambda (f64vector i) int))
    (f64vector-set! (lambda (f64vector i f64value) undefined))
    (f64vector->list (lambda (f64vector) list))
    (list->f64vector (lambda (list) f64vector))
    )

   ;; SRFI 5
   ("A compatible let form with signatures and rest arguments"
    (let (syntax (bindings body \.\.\.))))

   ;; SRFI 6
   ("Basic String Ports"
    (open-input-string (lambda (str) input-port))
    (open-output-string (lambda () output-port))
    (get-output-string (lambda (output-port) str)))

   ;; SRFI 7
   ("Feature-based program configuration language"
    (program (syntax (clause \.\.\.)))
    (feature-cond (syntax (clause))))

   ;; SRFI 8
   ("receive: Binding to multiple values"
    (receive (syntax (identifiers producer body \.\.\.))))

   ;; SRFI 9
   ("Defining Record Types"
    (define-record-type (syntax (name constructor-name pred-name fields \.\.\.))))

   ;; SRFI 10
   ("Sharp-Comma External Form"
    (define-reader-ctor (syntax (name proc) undefined)))

   ;; SRFI 11
   ("Syntax for receiving multiple values"
    (let-values (syntax (bindings body \.\.\.)))
    (let-values* (syntax (bindings body \.\.\.))))

   ()

   ;; SRFI 13
   ("String Library"
    (string-map (lambda (proc str :optional start end) str))
    (string-map! (lambda (proc str :optional start end) undefined))
    (string-fold (lambda (kons knil str :optional start end) obj))
    (string-fold-right (lambda (kons knil str :optional start end) obj))
    (string-unfold (lambda (p f g seed :optional base make-final) str))
    (string-unfold-right (lambda (p f g seed :optional base make-final) str))
    (string-tabulate (lambda (proc len) str))
    (string-for-each (lambda (proc str :optional start end) undefined))
    (string-for-each-index (lambda (proc str :optional start end) undefined))
    (string-every (lambda (pred str :optional start end) obj))
    (string-any (lambda (pred str :optional start end) obj))
    (string-hash (lambda (str :optional bound start end) int))
    (string-hash-ci (lambda (str :optional bound start end) int))
    (string-compare (lambda (string1 string2 lt-proc eq-proc gt-proc :optional start end) obj))
    (string-compare-ci (lambda (string1 string2 lt-proc eq-proc gt-proc :optional start end) obj))
    (string= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string<> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string< (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string<= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string>= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci<> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci< (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci<= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci>= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-titlecase (lambda (string :optional start end) str))
    (string-upcase (lambda (string :optional start end) str))
    (string-downcase (lambda (string :optional start end) str))
    (string-titlecase! (lambda (string :optional start end) undefined))
    (string-upcase! (lambda (string :optional start end) undefined))
    (string-downcase! (lambda (string :optional start end) undefined))
    (string-take (lambda (string nchars) str))
    (string-drop (lambda (string nchars) str))
    (string-take-right (lambda (string nchars) str))
    (string-drop-right (lambda (string nchars) str))
    (string-pad (lambda (string k :optional char start end) str))
    (string-pad-right (lambda (string k :optional char start end) str))
    (string-trim (lambda (string :optional char/char-set/pred start end) str))
    (string-trim-right (lambda (string :optional char/char-set/pred start end) str))
    (string-trim-both (lambda (string :optional char/char-set/pred start end) str))
    (string-filter (lambda (char/char-set/pred string :optional start end) str))
    (string-delete (lambda (char/char-set/pred string :optional start end) str))
    (string-index (lambda (string char/char-set/pred :optional start end) (or integer bool)))
    (string-index-right (lambda (string char/char-set/pred :optional end start) (or integer bool)))
    (string-skip (lambda (string char/char-set/pred :optional start end) (or integer bool)))
    (string-skip-right (lambda (string char/char-set/pred :optional end start) (or integer bool)))
    (string-count (lambda (string char/char-set/pred :optional start end) n))
    (string-prefix-length (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-suffix-length (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-prefix-length-ci (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-suffix-length-ci (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-prefix? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-suffix? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-prefix-ci? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-suffix-ci? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-contains (lambda (string pattern :optional s-start s-end p-start p-end) obj))
    (string-contains-ci (lambda (string pattern :optional s-start s-end p-start p-end) obj))
    (string-fill! (lambda (string char :optional start end) undefined))
    (string-copy! (lambda (to tstart from :optional fstart fend) undefined))
    (string-copy (lambda (str :optional start end) str))
    (substring/shared (lambda (str start :optional end) str))
    (string-reverse (lambda (str :optional start end) str))
    (string-reverse! (lambda (str :optional start end) undefined))
    (reverse-list->string (lambda (char-list) str))
    (string->list (lambda (str :optional start end) list))
    (string-concatenate (lambda (string-list) str))
    (string-concatenate/shared (lambda (string-list) str))
    (string-append/shared (lambda (str \.\.\.) str))
    (string-concatenate-reverse (lambda (string-list :optional final-string end) str))
    (string-concatenate-reverse/shared (lambda (string-list :optional final-string end) str))
    (xsubstring (lambda (str from :optional to start end) str))
    (string-xcopy! (lambda (target tstart str from :optional to start end) undefined))
    (string-null? (lambda (str) bool))
    (string-join (lambda (string-list :optional delim grammar) str))
    (string-tokenize (lambda (string :optional token-chars start end) str))
    (string-replace (lambda (str1 str2 start1 end1 :optional start2 end2) str))
    (string-kmp-partial-search (lambda (pat rv str i :optional c= p-start s-start s-end) n))
    (make-kmp-restart-vector (lambda (str :optional c= start end) vec))
    (kmp-step (lambda (pat rv c i c= p-start) n))
    )

   ;; SRFI 14
   ("Character-Set Library"
    (char-set? (lambda (cset) bool))
    (char-set= (lambda (cset \.\.\.) bool))
    (char-set<= (lambda (cset \.\.\.) bool))
    (char-set-hash (lambda (cset :optional int) int))
    (char-set-cursor (lambda (cset) cursor))
    (char-set-ref (lambda (cset cursor) ch))
    (char-set-cursor-next (lambda (cset cursor) int))
    (end-of-char-set? (lambda (cursor) bool))
    (char-set-fold (lambda (proc obj cset) obj))
    (char-set-unfold (lambda (proc proc proc obj :optional obj) cset))
    (char-set-unfold! (lambda (proc proc proc obj obj) cset))
    (char-set-for-each (lambda (proc cset) undefined))
    (char-set-map (lambda (proc cset) cset))
    (char-set-copy (lambda (cset) cset))
    (char-set (lambda (ch \.\.\.) cset))
    (list->char-set (lambda (list :optional obj) cset))
    (list->char-set! (lambda (list cset) cset))
    (string->char-set (lambda (str :optional cset) cset))
    (string->char-set! (lambda (str cset) cset))
    (ucs-range->char-set (lambda (int int :optional bool cset) cset))
    (ucs-range->char-set! (lambda (int int bool cset) cset))
    (char-set-filter (lambda (proc cset :optional base-cset) cset))
    (char-set-filter! (lambda (proc cset base-cset) cset))
    (->char-set (lambda (obj) cset))
    (char-set-size (lambda (cset) n))
    (char-set-count (lambda (proc cset) n))
    (char-set-contains? (lambda (cset ch) bool))
    (char-set-every (lambda (proc cset) obj))
    (char-set-any (lambda (proc cset) obj))
    (char-set-adjoin (lambda (cset ch \.\.\.) cset))
    (char-set-delete (lambda (cset ch \.\.\.) cset))
    (char-set-adjoin! (lambda (cset ch \.\.\.) cset))
    (char-set-delete! (lambda (cset ch \.\.\.) cset))
    (char-set->list (lambda (cset) list))
    (char-set->string (lambda (cset) str))
    (char-set-complement (lambda (cset) cset))
    (char-set-union (lambda (cset \.\.\.) cset))
    (char-set-intersection (lambda (cset \.\.\.) cset))
    (char-set-xor (lambda (cset \.\.\.) cset))
    (char-set-difference (lambda (cset \.\.\.) cset))
    (char-set-diff+intersection (lambda (cset \.\.\.) cset))
    (char-set-complement! (lambda (cset) cset))
    (char-set-union! (lambda (cset \.\.\.) cset))
    (char-set-intersection! (lambda (cset \.\.\.) cset))
    (char-set-xor! (lambda (cset \.\.\.) cset))
    (char-set-difference! (lambda (cset \.\.\.) cset))
    (char-set-diff+intersection! (lambda (cset \.\.\.) cset))
    (char-set:lower-case char-set)
    (char-set:upper-case char-set)
    (char-set:letter char-set)
    (char-set:digit char-set)
    (char-set:letter+digit char-set)
    (char-set:graphic char-set)
    (char-set:printing char-set)
    (char-set:whitespace char-set)
    (char-set:blank char-set)
    (char-set:iso-control char-set)
    (char-set:punctuation char-set)
    (char-set:symbol char-set)
    (char-set:hex-digit char-set)
    (char-set:ascii char-set)
    (char-set:empty char-set)
    (char-set:full char-set)
    )

   ()

   ;; SRFI 16
   ("Syntax for procedures of variable arity"
    (case-lambda (syntax (clauses \.\.\.) procedure)))

   ;; SRFI 17
   ("Generalized set!"
    (set! (syntax (what value) undefined)))

   ;; SRFI 18
   ("Multithreading support"
    (current-thread (lambda () thread))
    (thread? (lambda (obj) bool))
    (make-thread (lambda (thunk :optional name) thread))
    (thread-name (lambda (thread) name))
    (thread-specific (lambda (thread)))
    (thread-specific-set! (lambda (thread obj)))
    (thread-base-priority (lambda (thread)))
    (thread-base-priority-set! (lambda (thread number)))
    (thread-priority-boost (lambda (thread)))
    (thread-priority-boost-set! (lambda (thread number)))
    (thread-quantum (lambda (thread)))
    (thread-quantum-set! (lambda (thread number)))
    (thread-start! (lambda (thread)))
    (thread-yield! (lambda ()))
    (thread-sleep! (lambda (number)))
    (thread-terminate! (lambda (thread)))
    (thread-join! (lambda (thread :optional timeout timeout-val)))
    (mutex? (lambda (obj) bool))
    (make-mutex (lambda (:optional name) mutex))
    (mutex-name (lambda (mutex) name))
    (mutex-specific (lambda (mutex)))
    (mutex-specific-set! (lambda (mutex obj)))
    (mutex-state (lambda (mutex)))
    (mutex-lock! (lambda (mutex :optional timeout thread)))
    (mutex-unlock! (lambda (mutex :optional condition-variable timeout)))
    (condition-variable? (lambda (obj) bool))
    (make-condition-variable (lambda (:optional name) condition-variable))
    (condition-variable-name (lambda (condition-variable) name))
    (condition-variable-specific (lambda (condition-variable)))
    (condition-variable-specific-set! (lambda (condition-variable obj)))
    (condition-variable-signal! (lambda (condition-variable)))
    (condition-variable-broadcast! (lambda (condition-variable)))
    (current-time (lambda () time))
    (time? (lambda (obj) bool))
    (time->seconds (lambda (time) x1))
    (seconds->time (lambda (x1) time))
    (current-exception-handler (lambda () handler))
    (with-exception-handler (lambda (handler thunk)))
    (raise (lambda (obj)))
    (join-timeout-exception? (lambda (obj) bool))
    (abandoned-mutex-exception? (lambda (obj) bool))
    (terminated-thread-exception? (lambda (obj) bool))
    (uncaught-exception? (lambda (obj) bool))
    (uncaught-exception-reason (lambda (exc) obj))
    )

   ;; SRFI 19
   ("Time Data Types and Procedures"
    (current-date (lambda (:optional tz-offset)) date)
    (current-julian-day (lambda ()) jdn)
    (current-modified-julian-day (lambda ()) mjdn)
    (current-time (lambda (:optional time-type)) time)
    (time-resolution (lambda (:optional time-type)) nanoseconds)
    (make-time (lambda (type nanosecond second)))
    (time? (lambda (obj)))
    (time-type (lambda (time)))
    (time-nanosecond (lambda (time)))
    (time-second (lambda (time)))
    (set-time-type! (lambda (time)))
    (set-time-nanosecond! (lambda (time)))
    (set-time-second! (lambda (time)))
    (copy-time (lambda (time)))
    (time<=? (lambda (time1 time2)))
    (time<? (lambda (time1 time2)))
    (time=? (lambda (time1 time2)))
    (time>=? (lambda (time1 time2)))
    (time>? (lambda (time1 time2)))
    (time-difference (lambda (time1 time2)))
    (time-difference! (lambda (time1 time2)))
    (add-duration (lambda (time duration)))
    (add-duration! (lambda (time duration)))
    (subtract-duration (lambda (time duration)))
    (subtract-duration! (lambda (time duration)))
    (make-date (lambda (nanosecond second minute hour day month year zone-offset)))
    (date? (lambda (obj)))
    (date-nanosecond (lambda (date)))
    (date-second (lambda (date)))
    (date-minute (lambda (date)))
    (date-hour (lambda (date)))
    (date-day (lambda (date)))
    (date-month (lambda (date)))
    (date-year (lambda (date)))
    (date-zone-offset (lambda (date)))
    (date-year-day (lambda (date)))
    (date-week-day (lambda (date)))
    (date-week-number (lambda (date)))
    (date->julian-day (lambda (date)))
    (date->modified-julian-day (lambda (date)))
    (date->time-monotonic (lambda (date)))
    (date->time-tai (lambda (date)))
    (date->time-utc (lambda (date)))
    (julian-day->date (lambda (date)))
    (julian-day->time-monotonic (lambda (date)))
    (julian-day->time-tai (lambda (date)))
    (julian-day->time-utc (lambda (date)))
    (modified-julian-day->date (lambda (date)))
    (modified-julian-day->time-monotonic (lambda (date)))
    (modified-julian-day->time-tai (lambda (date)))
    (modified-julian-day->time-utc (lambda (date)))
    (time-monotonic->date (lambda (date)))
    (time-monotonic->julian-day (lambda (date)))
    (time-monotonic->modified-julian-day (lambda (date)))
    (time-monotonic->time-monotonic (lambda (date)))
    (time-monotonic->time-tai (lambda (date)))
    (time-monotonic->time-tai! (lambda (date)))
    (time-monotonic->time-utc (lambda (date)))
    (time-monotonic->time-utc! (lambda (date)))
    (time-tai->date (lambda (date)))
    (time-tai->julian-day (lambda (date)))
    (time-tai->modified-julian-day (lambda (date)))
    (time-tai->time-monotonic (lambda (date)))
    (time-tai->time-monotonic! (lambda (date)))
    (time-tai->time-utc (lambda (date)))
    (time-tai->time-utc! (lambda (date)))
    (time-utc->date (lambda (date)))
    (time-utc->julian-day (lambda (date)))
    (time-utc->modified-julian-day (lambda (date)))
    (time-utc->time-monotonic (lambda (date)))
    (time-utc->time-monotonic! (lambda (date)))
    (time-utc->time-tai (lambda (date)))
    (time-utc->time-tai! (lambda (date)))
    (date->string (lambda (date :optional format-string)))
    (string->date (lambda (input-string template-string)))
    )

   ()

   ;; SRFI 21
   ("Real-time multithreading support"
    srfi-18)                            ; same as srfi-18

   ;; SRFI 22
   ("Running Scheme Scripts on Unix"
    )

   ;; SRFI 23
   ("Error reporting mechanism"
    (error (lambda (reason-string arg \.\.\.) error)))

   ()

   ;; SRFI 25
   ("Multi-dimensional Array Primitives"
    (array? (lambda (obj)))
    (make-array (lambda (shape :optional init)))
    (shape (lambda (bound \.\.\.)))
    (array (lambda (shape obj \.\.\.)))
    (array-rank (lambda (array)))
    (array-start (lambda (array)))
    (array-end (lambda (array)))
    (array-shape (lambda (array)))
    (array-ref (lambda (array i \.\.\.)))
    (array-set! (lambda (array obj \.\.\.) undefined))
    (share-array (lambda (array shape proc)))
    )

   ;; SRFI 26
   ("Notation for Specializing Parameters without Currying"
    (cut (syntax (obj \.\.\.)))
    (cute (lambda (obj \.\.\.))))

   ;; SRFI 27
   ("Sources of Random Bits"
    (random-integer (lambda (n)))
    (random-real (lambda ()))
    (default-random-source (lambda ()))
    (make-random-source (lambda ()))
    (random-source? (lambda (obj)))
    (random-source-state-ref (lambda (random-source)))
    (random-source-state-set! (lambda (random-source state)))
    (random-source-randomize! (lambda (random-source)))
    (random-source-pseudo-randomize! (lambda (random-source i j)))
    (random-source-make-integers (lambda (random-source)))
    (random-source-make-reals (lambda (random-source)))
    )

   ;; SRFI 28
   ("Basic Format Strings"
    (format (lambda (port-or-boolean format-string arg \.\.\.))))

   ;; SRFI 29
   ("Localization"
    (current-language (lambda (:optional symbol)))
    (current-country (lambda (:optional symbol)))
    (current-locale-details (lambda (:optional list)))
    (declare-bundle! (lambda (bundle-name association-list)))
    (store-bundle (lambda (bundle-name)))
    (load-bundle! (lambda (bundle-name)))
    (localized-template (lambda (package-name message-template-name)))
    )

   ;; SRFI 30
   ("Nested Multi-line Comments"
    )

   ;; SRFI 31
   ("A special form for recursive evaluation"
    (rec (syntax (name body \.\.\.) procedure)))

   ()

   ()

   ;; SRFI 34
   ("Exception Handling for Programs"
    (guard (syntax (clauses \.\.\.)))
    (raise (lambda (obj)))
    )

   ;; SRFI 35
   ("Conditions"
    (make-condition-type (lambda (id parent field-name-list)))
    (condition-type? (lambda (obj)))
    (make-condition (lambda (condition-type)))
    (condition? (lambda (obj)))
    (condition-has-type? (lambda (condition condition-type)))
    (condition-ref (lambda (condition field-name)))
    (make-compound-condition (lambda (condition \.\.\.)))
    (extract-condition (lambda (condition condition-type)))
    (define-condition-type (syntax (name parent pred-name fields \.\.\.)))
    (condition (syntax (type-field-binding \.\.\.)))
    )

   ;; SRFI 36
   ("I/O Conditions"
    (&error condition)
    (&i/o-error condition)
    (&i/o-port-error condition)
    (&i/o-read-error condition)
    (&i/o-write-error condition)
    (&i/o-closed-error condition)
    (&i/o-filename-error condition)
    (&i/o-malformed-filename-error condition)
    (&i/o-file-protection-error condition)
    (&i/o-file-is-read-only-error condition)
    (&i/o-file-already-exists-error condition)
    (&i/o-no-such-file-error condition)
    )

   ;; SRFI 37
   ("args-fold: a program argument processor"
    (args-fold
     (arg-list option-list unrecognized-option-proc operand-proc seed \.\.\.))
    (option-processor (lambda (option name arg seeds \.\.\.)))
    (operand-processor (lambda (operand seeds \.\.\.)))
    (option (lambda (name-list required-arg? optional-arg? option-proc)))
    (option-names (lambda (option)))
    (option-required-arg? (lambda (option)))
    (option-optional-arg? (lambda (option)))
    (option-processor (lambda (option)))
    )

   ;; SRFI 38
   ("External Representation for Data With Shared Structure"
    (write-with-shared-structure (lambda (obj :optional port optarg)))
    (read-with-shared-structure (lambda (:optional port)))
    )

   ;; SRFI 39
   ("Parameter objects"
    (make-parameter (lambda (init-value :optional converter)))
    (parameterize (syntax (bindings body \.\.\.))))

   ;; SRFI 40
   ("A Library of Streams"
    (stream-null stream)
    (stream-cons (syntax (obj stream)))
    (stream? (lambda (obj)))
    (stream-null? (lambda (obj)))
    (stream-pair? (lambda (obj)))
    (stream-car (lambda (stream)))
    (stream-cdr (lambda (stream)))
    (stream-delay (syntax (expr)))
    (stream (lambda (obj \.\.\.)))
    (stream-unfoldn (lambda (generator-proc seed n)))
    (stream-map (lambda (proc stream \.\.\.)))
    (stream-for-each (lambda (proc stream \.\.\.) undefined))
    (stream-filter (lambda (pred stream)))
    )

   ()

   ;; SRFI 42
   ("Eager Comprehensions"
    (list-ec (syntax))
    (append-ec (syntax))
    (sum-ec (syntax))
    (min-ec (syntax))
    (max-ec (syntax))
    (any?-ec (syntax))
    (every?-ec (syntax))
    (first-ec (syntax))
    (do-ec (syntax))
    (fold-ec (syntax))
    (fold3-ec (syntax))
    (:list (syntax () undefined))
    (:string (syntax () undefined))
    (:vector (syntax () undefined))
    (:integers (syntax () undefined))
    (:range (syntax () undefined))
    (:real-range (syntax () undefined))
    (:char-range (syntax () undefined))
    (:port (syntax () undefined))
    (:do (syntax () undefined))
    (:let (syntax () undefined))
    (:parallel (syntax () undefined))
    (:while (syntax () undefined))
    (:until (syntax () undefined))
    )

   ;; SRFI 43
   ("Vector Library"
    (vector-unfold (f length initial-seed \.\.\.))
    (vector-unfold-right (lambda (f length initial-seed \.\.\.)))
    (vector-tabulate (lambda (f size)))
    (vector-copy (lambda (vec :optional start end fill)))
    (vector-reverse-copy (lambda (vec :optional start end)))
    (vector-append (lambda (vec \.\.\.)))
    (vector-concatenate (lambda (vector-list)))
    (vector-empty? (lambda (obj)))
    (vector= (lambda (eq-proc vec \.\.\.)))
    (vector-fold (lambda (kons knil vec \.\.\.)))
    (vector-fold-right (lambda (kons knil vec \.\.\.)))
    (vector-map (lambda (f vec \.\.\.)))
    (vector-map! (lambda (f vec \.\.\.)))
    (vector-for-each (lambda (f vec \.\.\.) undefined))
    (vector-count (lambda (pred vec \.\.\.)))
    (vector-index (lambda (pred vec \.\.\.)))
    (vector-index-right (lambda (pred vec \.\.\.)))
    (vector-skip (lambda (pred vec \.\.\.)))
    (vector-skip-right (lambda (pred vec \.\.\.)))
    (vector-binary-search (lambda (vec value cmp-proc)))
    (vector-any (lambda (pred vec \.\.\.)))
    (vector-every (lambda (pred vec \.\.\.)))
    (vector-swap! (lambda (vec i j) undefined))
    (vector-reverse! (lambda (vec :optional start end) undefined))
    (vector-copy! (lambda (target-vec t-start source-vec :optional start end) undefined))
    (vector-reverse-copy! (lambda (target-vec t-start source-vec :optional start end) undefined))
    (reverse-vector-to-list (lambda (vec :optional start end)))
    (reverse-list-to-vector (lambda (list)))
    )

   ;; SRFI 44
   ("Collections"
    )

   ;; SRFI 45
   ("Primitives for expressing iterative lazy algorithms"
    (delay (syntax (expr)))
    (lazy (syntax (expr)))
    (force (lambda (promise)))
    (eager (lambda (promise)))
    )

   ;; SRFI 46
   ("Basic Syntax-rules Extensions"
    (syntax-rules (syntax () undefined)))

   ;; SRFI 47
   ("Array"
    (make-array (lambda (prototype k \.\.\.)))
    (ac64 (lambda (:optional z)))
    (ac32 (lambda (:optional z)))
    (ar64 (lambda (:optional x1)))
    (ar32 (lambda (:optional x1)))
    (as64 (lambda (:optional n)))
    (as32 (lambda (:optional n)))
    (as16 (lambda (:optional n)))
    (as8 (lambda (:optional n)))
    (au64 (lambda (:optional n)))
    (au32 (lambda (:optional n)))
    (au16 (lambda (:optional n)))
    (au8 (lambda (:optional n)))
    (at1 (lambda (:optional bool)))
    (make-shared-array (lambda (array mapper k \.\.\.)))
    (array-rank (lambda (obj)))
    (array-dimensions (lambda (array)))
    (array-in-bounds? (lambda (array k \.\.\.)))
    (array-ref (lambda (array k \.\.\.)))
    (array-set! (lambda (array obj k \.\.\.)))
    )

   ;; SRFI 48
   ("Intermediate Format Strings"
    (format (lambda (port-or-boolean format-string arg \.\.\.))))

   ;; SRFI 49
   ("Indentation-sensitive syntax"
    )

   ()

   ;; SRFI 51
   ("Handling rest list"
    (rest-values (lambda (caller rest-list :optional args-number-limit default)))
    (arg-and (syntax))
    (arg-ands (syntax))
    (err-and (syntax))
    (err-ands (syntax))
    (arg-or (syntax))
    (arg-ors (syntax))
    (err-or (syntax))
    (err-ors (syntax))
    )

   ()

   ()

   ;; SRFI 54
   ("Formatting"
    (cat (lambda (obj \.\.\.))))

   ;; SRFI 55
   ("require-extension"
    (require-extension (syntax)))

   ()

   ;; SRFI 57
   ("Records"
    (define-record-type (syntax))
    (define-record-scheme (syntax))
    (record-update (syntax))
    (record-update! (syntax))
    (record-compose (syntax)))

   ;; SRFI 58
   ("Array Notation"
    )

   ;; SRFI 59
   ("Vicinity"
    (program-vicinity (lambda ()))
    (library-vicinity (lambda ()))
    (implementation-vicinity (lambda ()))
    (user-vicinity (lambda ()))
    (home-vicinity (lambda ()))
    (in-vicinity (lambda (vicinity filename)))
    (sub-vicinity (lambda (vicinity name)))
    (make-vicinity (lambda (dirname)))
    (path-vicinity (lambda (path)))
    (vicinity:suffix? (lambda (ch)))
    )

   ;; SRFI 60
   ("Integers as Bits"
    (bitwise-and (lambda (n \.\.\.) int))
    (bitwise-ior (lambda (n \.\.\.) int))
    (bitwise-xor (lambda (n \.\.\.) int))
    (bitwise-not (lambda (n) int))
    (bitwise-if (lambda (mask n m) int))
    (any-bits-set? (lambda (n m) bool))
    (bit-count (lambda (n) int))
    (integer-length (lambda (n) int))
    (first-bit-set (lambda (n) int))
    (bit-set? (lambda (index n) bool))
    (copy-bit (lambda (index n bool) int))
    (bit-field (lambda (n start end) int))
    (copy-bit-field (lambda (to-int from-int start end) int))
    (arithmetic-shift (lambda (n count) int))
    (rotate-bit-field (lambda (n count start end) int))
    (reverse-bit-field (lambda (n start end) int))
    (integer->list (lambda (k :optional len) list))
    (list->integer (lambda (list) int))
    )

   ;; SRFI 61
   ("A more general cond clause"
    (cond (syntax)))

   ;; SRFI 62
   ("S-expression comments"
    )

   ;; SRFI 63
   ("Homogeneous and Heterogeneous Arrays"
    )

   ;; SRFI 64
   ("A Scheme API for test suites"
    (test-assert (syntax))
    (test-eqv (syntax))
    (test-equal (syntax))
    (test-eq (syntax))
    (test-approximate (syntax))
    (test-error (syntax))
    (test-read-eval-string (lambda (string)))
    (test-begin (syntax (suite-name :optional count)))
    (test-end (syntax (suite-name)))
    (test-group (syntax (suite-name decl-or-expr \.\.\.)))
    (test-group-with-cleanup (syntax (suite-name decl-or-expr \.\.\.)))
    (test-match-name (lambda (name)))
    (test-match-nth (lambda (n :optional count)))
    (test-match-any (lambda (specifier \.\.\.)))
    (test-match-all (lambda (specifier \.\.\.)))
    (test-skip (syntax (specifier)))
    (test-expect-fail (syntax (specifier)))
    (test-runner? (lambda (obj)))
    (test-runner-current (lambda (:optional runner)))
    (test-runner-get (lambda ()))
    (test-runner-simple (lambda ()))
    (test-runner-null (lambda ()))
    (test-runner-create (lambda ()))
    (test-runner-factory (lambda (:optional factory)))
    (test-apply (syntax (runner specifier \.\.\.)))
    (test-with-runner (syntax (runner decl-or-expr \.\.\.)))
    (test-result-kind (lambda (:optional runner)))
    (test-passed? (lambda (:optional runner)))
    (test-result-ref (lambda (runner prop-name (:optional default))))
    (test-result-set! (lambda (runner prop-name value)))
    (test-result-remove (lambda (runner prop-name)))
    (test-result-clear (lambda (runner)))
    (test-result-alist (lambda (runner)))
    (test-runner-on-test-begin (lambda (runner :optional proc)))
    (test-runner-on-test-begin! (lambda (runner :optional proc)))
    (test-runner-on-test-end (lambda (runner :optional proc)))
    (test-runner-on-test-end! (lambda (runner :optional proc)))
    (test-runner-on-group-begin (lambda (runner :optional proc)))
    (test-runner-on-group-begin! (lambda (runner :optional proc)))
    (test-runner-on-group-end (lambda (runner :optional proc)))
    (test-runner-on-group-end! (lambda (runner :optional proc)))
    (test-runner-on-bad-count (lambda (runner :optional proc)))
    (test-runner-on-bad-count! (lambda (runner :optional proc)))
    (test-runner-on-bad-end-name (lambda (runner :optional proc)))
    (test-runner-on-bad-end-name! (lambda (runner :optional proc)))
    (test-runner-on-final (lambda (runner :optional proc)))
    (test-runner-on-final! (lambda (runner :optional proc)))
    (test-runner-pass-count (lambda (runner)))
    (test-runner-fail-count (lambda (runner)))
    (test-runner-xpass-count (lambda (runner)))
    (test-runner-skip-count (lambda (runner)))
    (test-runner-test-name (lambda (runner)))
    (test-runner-group-path (lambda (runner)))
    (test-runner-group-stack (lambda (runner)))
    (test-runner-aux-value (lambda (runner)))
    (test-runner-aux-value! (lambda (runner)))
    (test-runner-reset (lambda (runner)))
    )

   ()

   ;; SRFI 66
   ("Octet Vectors"
    (make-u8vector (lambda (len n)))
    (u8vector (lambda (n \.\.\.)))
    (u8vector->list (lambda (u8vector)))
    (list->u8vector (lambda (octet-list)))
    (u8vector-length u8vector)
    (u8vector-ref (lambda (u8vector k)))
    (u8vector-set! (lambda (u8vector k n)))
    (u8vector=? (lambda (u8vector-1 u8vector-2)))
    (u8vector-compare (lambda (u8vector-1 u8vector-2)))
    (u8vector-copy! (lambda (source source-start target target-start n)))
    (u8vector-copy (lambda (u8vector)))
    )

   ;; SRFI 67
   ("Compare Procedures"
    )

   ()

   ;; SRFI 69
   ("Basic hash tables"
    (alist->hash-table (lambda (alist) hash-table))
    (hash (lambda (obj :optional n) int))
    (hash-by-identity (lambda (obj :optional n) int))
    (hash-table->alist (lambda (hash-table) alist))
    (hash-table-copy (lambda (hash-table) hash-table))
    (hash-table-delete! (lambda (hash-table key) undefined))
    (hash-table-equivalence-function (lambda (hash-table) pred))
    (hash-table-exists? (lambda (hash-table key) bool))
    (hash-table-fold (lambda (hash-table f init-value)))
    (hash-table-hash-function (lambda (hash-table) f))
    (hash-table-keys (lambda (hash-table) list))
    (hash-table-merge! (lambda (hash-table1 hash-table2) undefined))
    (hash-table-ref (lambda (hash-table key :optional thunk)))
    (hash-table-ref/default (lambda (hash-table key default)))
    (hash-table-remove! (lambda (hash-table proc) undefined))
    (hash-table-set! (lambda (hash-table key value) undefined))
    (hash-table-size (lambda (hash-table) n))
    (hash-table-update! (lambda (hash-table key proc :optional thunk) undefined))
    (hash-table-update!/default (lambda (hash-table key proc default) undefined))
    (hash-table-values (lambda (hash-table) list))
    (hash-table-walk (lambda (hash-table proc) undefined))
    (hash-table? (lambda (obj) bool))
    (make-hash-table (lambda (:optional eq-fn hash-fn) hash-table))
    (string-ci-hash (lambda (str :optional n) n))
    (string-hash (lambda (str1 :optional n) n))
    )

   ;; SRFI 70
   ("Numbers"
    )

   ;; SRFI 71
   ("LET-syntax for multiple values"
    )

   ;; SRFI 72
   ("Simple hygienic macros"
    )

   ()

   ;; SRFI 74
   ("Octet-Addressed Binary Blocks"
    )

   () () () () ()                 ; 75-79
   () () () () () () () () () ()  ; 80-89
   () () () () () () () () () ()  ; 90-99
   () () () () () () () () () ()  ; 100-109
   () () () () () () () () () ()  ; 110-119
   () () () () () () () () () ()  ; 120-129

   ;; SRFI 130
   ("Cursor-based string library"
    (string-cursor-ref (lambda (string string-cursor) char))
    (string-cursor-set! (lambda (string string-cursor char)))
    (string-cursor-start (lambda (string) string-cursor))
    (string-cursor-end (lambda (string) string-cursor))
    (string-cursor->index (lambda (string string-cursor) i))
    (string-index->cursor (lambda (string i) string-cursor))
    (string-cursor-next (lambda (string string-cursor) string-cursor))
    (string-cursor-prev (lambda (string string-cursor) string-cursor))
    (string-cursor-forward (lambda (string string-cursor i) string-cursor))
    (string-cursor-back (lambda (string string-cursor i) string-cursor))
    (string-cursor<? (lambda (string-cursor1 string-cursor2) boolean))
    (string-cursor>? (lambda (string-cursor1 string-cursor2) boolean))
    (string-cursor<=? (lambda (string-cursor1 string-cursor2) boolean))
    (string-cursor>=? (lambda (string-cursor1 string-cursor2) boolean))
    (string-cursor=? (lambda (string-cursor1 string-cursor2) boolean))
    (string-cursor-diff (lambda (string string-cursor1 string-cursor2) i))
    (string-fold (lambda (kons knil str :optional start end) obj))
    (string-fold-right (lambda (kons knil str :optional start end) obj))
    (string-tabulate (lambda (proc len) str))
    (string-for-each-cursor (lambda (proc str :optional start-indexer end-indexer) undefined))
    (string-every (lambda (pred str :optional start end) obj))
    (string-any (lambda (pred str :optional start end) obj))
    (string-ref/cursor  (lambda (str string-indexer) char))
    (string->list/cursors (lambda (str :optional start-indexer end-indexer) ls))
    (string->vector/cursors (lambda (str :optional start-indexer end-indexer) vector))
    (substring/cursors (lambda (str start-indexer :optional end-indexer) str))
    (string-copy/cursors (lambda (str :optional start-indexer end-indexer) str))
    (reverse-list->string (lambda (ls) str))
    (string-take (lambda (string nchars) str))
    (string-drop (lambda (string nchars) str))
    (string-take-right (lambda (string nchars) str))
    (string-drop-right (lambda (string nchars) str))
    (string-pad (lambda (string k :optional char start-indexer end-indexer) str))
    (string-pad-right (lambda (string k :optional char start-indexer end-indexer) str))
    (string-trim (lambda (string :optional pred start-indexer end-indexer) str))
    (string-trim-right (lambda (string :optional pred start-indexer end-indexer) str))
    (string-trim-both (lambda (string :optional pred start-indexer end-indexer) str))
    (string-filter (lambda (pred string :optional start-indexer end-indexer) str))
    (string-remove (lambda (pred string :optional start-indexer end-indexer) str))
    (string-index (lambda (string pred :optional start-indexer end-indexer) (or string-cursor bool)))
    (string-index-right (lambda (string pred :optional end-indexer start-indexer) (or string-cursor bool)))
    (string-skip (lambda (string pred :optional start-indexer end-indexer) (or string-cursor bool)))
    (string-skip-right (lambda (string pred :optional end-indexer start-indexer) (or string-cursor bool)))
    (string-count (lambda (string pred :optional start-indexer end-indexer) n))
    (string-prefix-length (lambda (string1 string2 :optional start-indexer1 end-indexer1 start-indexer2 end-indexer2) n))
    (string-suffix-length (lambda (string1 string2 :optional start-indexer1 end-indexer1 start-indexer2 end-indexer2) n))
    (string-prefix? (lambda (string1 string2 :optional start-indexer1 end-indexer1 start-indexer2 end-indexer2) bool))
    (string-suffix? (lambda (string1 string2 :optional start-indexer1 end-indexer1 start-indexer2 end-indexer2) bool))
    (string-contains (lambda (string pattern :optional s-start s-end p-start p-end) obj))
    (string-reverse (lambda (str :optional start end) str))
    (string-concatenate (lambda (string-list) str))
    (string-concatenate-reverse (lambda (string-list :optional final-string end) str))
    (string-replicate (lambda (str from :optional to start end) str))
    (string-null? (lambda (str) bool))
    (string-join (lambda (string-list :optional delim grammar) str))
    (string-split (lambda (string :optional token-chars start end) str))
    (string-replace (lambda (str1 str2 start1 end1 :optional start2 end2) str))
    )

   ])

;; another big table - consider moving to a separate file
(defvar *scheme-implementation-exports*
  `((chibi
     (current-exception-handler (lambda (:optional thunk) thunk))
     (command-line (lambda () list))
     (pair-source (lambda (pair)))
     (pair-source-set! (lambda (pair obj)))
     (cons-source (lambda (kar kdr source) pair))
     (bytevector-u8-ref (lambda (bytevector i) n))
     (bytevector-u8-set! (lambda (bytevector i n)))
     (bytevector-length (lambda (bytevector) n))
     (string-cursor-ref (lambda (string string-cursor) char))
     (string-cursor-set! (lambda (string string-cursor char)))
     (string-cursor-start (lambda (string) string-cursor))
     (string-cursor-end (lambda (string) string-cursor))
     (string-cursor->index (lambda (string string-cursor) i))
     (string-index->cursor (lambda (string i) string-cursor))
     (string-cursor-offset (lambda (string string-cursor) i))
     (string-cursor-next (lambda (string string-cursor) string-cursor))
     (string-cursor-prev (lambda (string string-cursor) string-cursor))
     (string-cursor<? (lambda (string-cursor1 string-cursor2) boolean))
     (string-cursor>? (lambda (string-cursor1 string-cursor2) boolean))
     (string-cursor<=? (lambda (string-cursor1 string-cursor2) boolean))
     (string-cursor>=? (lambda (string-cursor1 string-cursor2) boolean))
     (string-cursor=? (lambda (string-cursor1 string-cursor2) boolean))
     (string-size (lambda (string) i))
     (make-exception (lambda (symbol string list proc source) exception))
     (exception-kind (lambda (exception) symbol))
     (exception-irritants (lambda (exception) list))
     (is-a? (lambda (obj type) boolean))
     (fixnum? (lambda (obj) boolean))
     (flonum? (lambda (obj) boolean))
     (bignum? (lambda (obj) boolean))
     (ratio? (lambda (obj) boolean))
     (ratio-numerator (lambda (n) n))
     (ratio-denominator (lambda (n) n))
     (complex? (lambda (obj) boolean))
     (complex-real (lambda (z) x))
     (complex-imag (lambda (z) x))
     (bytevector? (lambda (obj) boolean))
     (fileno? (lambda (obj) boolean))
     (exception? (lambda (obj) boolean))
     (closure? (lambda (obj) boolean))
     (opcode? (lambda (obj) boolean))
     (binary-port? (lambda (obj) boolean))
     (textual-port? (lambda (obj) boolean))
     (port-open? (lambda (obj) boolean))
     (apply1 (lambda (proc list)))
     (raise (lambda (obj)))
     (flush-output (lambda (:optional port)))
     (equal?/bounded (lambda (a b bound) boolean))
     (identifier? (lambda (obj) boolean))
     (identifier=? (lambda (obj env obj env) boolean))
     (identifier->symbol (lambda (obj) symbol))
     (length* (lambda (obj) n))
     (append2 (lambda (ls ls) ls))
     (open-binary-input-file (lambda (filename) port))
     (open-binary-output-file (lambda (filename) port))
     (make-environment (lambda () env))
     (primitive-environment (lambda (i) env))
     (env-parent (lambda (env) env))
     (compile (lambda (obj :optional env)))
     (generate (lambda (obj :optional env)))
     (%load (lambda (filename :optional env)))
     (%import (lambda (env env list boolean)))
     (print-exception (lambda (exception :optional port)))
     (print-stack-trace (lambda (:optional port)))
     (warn-undefs (lambda (from to res)))
     (make-bytevector (lambda (i) bytevector))
     (string-concatenate (lambda (list :optional string) string))
     (make-syntactic-closure (lambda (env list obj) syntactic-closure))
     (strip-syntactic-closures (lambda (obj)))
     (open-output-string (lambda () output-port))
     (open-input-string (lambda (string) input-port))
     (get-output-string (lambda (output-port) string))
     (open-input-file-descriptor (lambda (i :optional boolean) input-port))
     (open-output-file-descriptor (lambda (i :optional boolean) output-port))
     (set-port-line! (lambda (port i)))
     (register-optimization! (lambda (proc i)))
     (exact-sqrt (lambda (i) i))
     (string-index->offset (lambda (string i) i))
     (string-offset->index (lambda (string i) i))
     (substring-cursor (lambda (string start-indexer :optional end-indexer) string))
     (subbytes (lambda (bytevector start :optional end) bytevector))
     (port-fold-case? (lambda (port) boolean))
     (set-port-fold-case! (lambda (port boolean)))
     (lookup-type (lambda (string obj) type))
     (register-simple-type (lambda (string type list) type))
     (make-type-predicate (lambda (string i) proc))
     (make-constructor (lambda (string i i) proc))
     (make-getter (lambda (string i i) proc))
     (make-setter (lambda (string i i) proc))
     (type-slot-offset (lambda (type symbol) proc))
     (slot-ref (lambda (obj obj obj i)))
     (slot-set! (lambda (obj obj obj i obj)))
     (port-fileno (lambda (port) fileno))
     (current-environment (lambda () env))
     (set-current-environment! (lambda (env)))
     (%meta-env (lambda () env))
     (env-exports (lambda (env) list))
     (current-module-path (lambda (:optional list) list))
     (find-module-file (lambda (filename) filename))
     (load-module-file (lambda (filename env)))
     (add-module-directory (lambda (filename boolean)))
     (%dk (lambda (:optional obj)))
     (yield! (lambda ()))
     (thread-parameters (lambda ()))
     (thread-parameters-set! (lambda (obj)))
     (any (lambda ((lambda (obj1 . obj2) a) list \.\.\.) a))
     (every (lambda ((lambda (obj1 . obj2) a) list \.\.\.) a))
     (error (lambda (reason-string arg \.\.\.) error))
     (sc-macro-transformer (lambda (proc) proc))
     (rsc-macro-transformer (lambda (proc) proc))
     (er-macro-transformer (lambda (proc) proc))
     (delay-force (syntax expr))
     (define-auxiliary-syntax (syntax symbol))
     (call-with-input-string (lambda (str proc)))
     (call-with-output-string (lambda (proc) str))
     (syntax-error (syntax string))
     (letrec* (syntax (vars body \.\.\.)) "bind new local variables recursively in order")
     (let-optionals* (syntax (list vars body \.\.\.)) "bind variables optionally")
     (raise-continuable (lambda (exception)))
     (with-exception-handler (lambda (handler thunk)))
     (protect (syntax (clauses \.\.\.)))
     (with-exception-protect (lambda (thunk thunk)))
     (exception-protect (syntax expr final)))
    (gauche
     (E2BIG integer)
     (EACCES integer)
     (EADDRINUSE integer)
     (EADDRNOTAVAIL integer)
     (EADV integer)
     (EAFNOSUPPORT integer)
     (EAGAIN integer)
     (EALREADY integer)
     (EBADE integer)
     (EBADF integer)
     (EBADFD integer)
     (EBADMSG integer)
     (EBADR integer)
     (EBADRQC integer)
     (EBADSLT integer)
     (EBFONT integer)
     (EBUSY integer)
     (ECANCELED integer)
     (ECHILD integer)
     (ECHRNG integer)
     (ECOMM integer)
     (ECONNABORTED integer)
     (ECONNREFUSED integer)
     (ECONNRESET integer)
     (EDEADLK integer)
     (EDEADLOCK integer)
     (EDESTADDRREQ integer)
     (EDOM integer)
     (EDOTDOT integer)
     (EDQUOT integer)
     (EEXIST integer)
     (EFAULT integer)
     (EFBIG integer)
     (EHOSTDOWN integer)
     (EHOSTUNREACH integer)
     (EIDRM integer)
     (EILSEQ integer)
     (EINPROGRESS integer)
     (EINTR integer)
     (EINVAL integer)
     (EIO integer)
     (EISCONN integer)
     (EISDIR integer)
     (EISNAM integer)
     (EKEYEXPIRED integer)
     (EKEYREJECTED integer)
     (EKEYREVOKED integer)
     (EL2HLT integer)
     (EL2NSYNC integer)
     (EL3HLT integer)
     (EL3RST integer)
     (ELIBACC integer)
     (ELIBBAD integer)
     (ELIBEXEC integer)
     (ELIBMAX integer)
     (ELIBSCN integer)
     (ELNRNG integer)
     (ELOOP integer)
     (EMEDIUMTYPE integer)
     (EMFILE integer)
     (EMLINK integer)
     (EMSGSIZE integer)
     (EMULTIHOP integer)
     (ENAMETOOLONG integer)
     (ENAVAIL integer)
     (ENETDOWN integer)
     (ENETRESET integer)
     (ENETUNREACH integer)
     (ENFILE integer)
     (ENOANO integer)
     (ENOBUFS integer)
     (ENOCSI integer)
     (ENODATA integer)
     (ENODEV integer)
     (ENOENT integer)
     (ENOEXEC integer)
     (ENOKEY integer)
     (ENOLCK integer)
     (ENOLINK integer)
     (ENOMEDIUM integer)
     (ENOMEM integer)
     (ENOMSG integer)
     (ENONET integer)
     (ENOPKG integer)
     (ENOPROTOOPT integer)
     (ENOSPC integer)
     (ENOSR integer)
     (ENOSTR integer)
     (ENOSYS integer)
     (ENOTBLK integer)
     (ENOTCONN integer)
     (ENOTDIR integer)
     (ENOTEMPTY integer)
     (ENOTNAM integer)
     (ENOTSOCK integer)
     (ENOTTY integer)
     (ENOTUNIQ integer)
     (ENXIO integer)
     (EOPNOTSUPP integer)
     (EOVERFLOW integer)
     (EPERM integer)
     (EPFNOSUPPORT integer)
     (EPIPE integer)
     (EPROTO integer)
     (EPROTONOSUPPORT integer)
     (EPROTOTYPE integer)
     (ERANGE integer)
     (EREMCHG integer)
     (EREMOTE integer)
     (EREMOTEIO integer)
     (ERESTART integer)
     (EROFS integer)
     (ESHUTDOWN integer)
     (ESOCKTNOSUPPORT integer)
     (ESPIPE integer)
     (ESRCH integer)
     (ESRMNT integer)
     (ESTALE integer)
     (ESTRPIPE integer)
     (ETIME integer)
     (ETIMEDOUT integer)
     (ETOOMANYREFS integer)
     (ETXTBSY integer)
     (EUCLEAN integer)
     (EUNATCH integer)
     (EUSERS integer)
     (EWOULDBLOCK integer)
     (EXDEV integer)
     (EXFULL integer)
     (F_OK integer)
     (LC_ALL integer)
     (LC_COLLATE integer)
     (LC_CTYPE integer)
     (LC_MONETARY integer)
     (LC_NUMERIC integer)
     (LC_TIME integer)
     (RAND_MAX integer)
     (R_OK integer)
     (SEEK_CUR integer)
     (SEEK_END integer)
     (SEEK_SET integer)
     (SIGABRT integer)
     (SIGALRM integer)
     (SIGBUS integer)
     (SIGCHLD integer)
     (SIGCONT integer)
     (SIGFPE integer)
     (SIGHUP integer)
     (SIGILL integer)
     (SIGINT integer)
     (SIGIO integer)
     (SIGIOT integer)
     (SIGKILL integer)
     (SIGPIPE integer)
     (SIGPOLL integer)
     (SIGPROF integer)
     (SIGPWR integer)
     (SIGQUIT integer)
     (SIGSEGV integer)
     (SIGSTKFLT integer)
     (SIGSTOP integer)
     (SIGTERM integer)
     (SIGTRAP integer)
     (SIGTSTP integer)
     (SIGTTIN integer)
     (SIGTTOU integer)
     (SIGURG integer)
     (SIGUSR1 integer)
     (SIGUSR2 integer)
     (SIGVTALRM integer)
     (SIGWINCH integer)
     (SIGXCPU integer)
     (SIGXFSZ integer)
     (SIG_BLOCK integer)
     (SIG_SETMASK integer)
     (SIG_UNBLOCK integer)
     (W_OK integer)
     (X_OK integer)
     (acons (lambda (key value alist) alist))
     (acosh (lambda (z) z))
     (add-load-path (lambda (path) undefined))
     (add-method! (lambda (generic method) undefined))
     (all-modules (lambda () list))
     (allocate-instance (lambda (class list)))
     (and-let* (syntax))
     (any (lambda (pred list)))
     (any$ (lambda (pred) proc))
     (any-pred (lambda (pred \.\.\.) pred))
     (append! (lambda (list \.\.\.) list))
     (apply$ (lambda (proc) proc))
     (apply-generic (lambda (generic list)))
     (apply-method (lambda (method list)))
     (apply-methods (lambda (generic list list)))
     (arity (lambda (proc) n))
     (arity-at-least-value (lambda (n)))
     (arity-at-least? (lambda (proc) bool))
     (ash (lambda (n i) n))
     (asinh (lambda (z) z))
     (assoc$ (lambda (obj) proc))
     (atanh (lambda (z) z))
     (autoload (syntax))
     (begin0 (syntax))
     (bignum? (lambda (obj) bool))
     (bit-field (lambda (n start end) n))
     (byte-ready? (lambda (:optional input-port) bool))
     (call-with-input-string (lambda (str proc)))
     (call-with-output-string (lambda (proc) str))
     (call-with-string-io (lambda (str proc) str))
     (case-lambda (syntax (clauses \.\.\.) procedure))
     (change-class (lambda (obj new-class)))
     (change-object-class (lambda (obj orig-class new-class)))
     (char->ucs (lambda (ch) int))
     (char-set (lambda (ch \.\.\.) char-set))
     (char-set-contains? (lambda (char-set ch) bool))
     (char-set-copy (lambda (char-set) char-set))
     (char-set? (lambda (obj) bool))
     (check-arg (syntax))
     (circular-list? (lambda (obj) bool))
     (clamp (lambda (x1 :optional min-x max-x) x2))
     (class-direct-methods (lambda (class) list))
     (class-direct-slots (lambda (class) list))
     (class-direct-subclasses (lambda (class) list))
     (class-direct-supers (lambda (class) list))
     (class-name (lambda (class) sym))
     (class-of (lambda (obj) class))
     (class-precedence-list (lambda (class) list))
     (class-slot-accessor (lambda (class id) proc))
     (class-slot-bound? (lambda (class id) bool))
     (class-slot-definition (lambda (class id)))
     (class-slot-ref (lambda (class slot)))
     (class-slot-set! (lambda (class slot val) undefined))
     (class-slots (lambda (class) list))
     (closure-code (lambda (proc)))
     (closure? (lambda (obj) bool))
     (compare (lambda (obj1 obj2) n))
     (complement (lambda (proc) proc))
     (compose (lambda (proc \.\.\.) proc))
     (compute-applicable-methods (lambda (generic list)))
     (compute-cpl (lambda (generic list)))
     (compute-get-n-set (lambda (class slot)))
     (compute-slot-accessor (lambda (class slot)))
     (compute-slots (lambda (class)))
     (cond-expand (syntax))
     (condition (syntax))
     (condition-has-type? (lambda (condition obj)))
     (condition-ref (lambda (condition id)))
     (condition-type? (lambda (obj) bool))
     (condition? (lambda (obj) bool))
     (copy-bit (lambda (index n i) n))
     (copy-bit-field (lambda (n start end from) n))
     (copy-port (lambda (from-port to-port :optional unit-sym) undefined))
     (cosh (lambda (z) z))
     (count$ (lambda (pred) proc))
     (current-class-of (lambda (obj) class))
     (current-error-port (lambda () output-port))
     (current-exception-handler (lambda () handler))
     (current-load-history (lambda () list))
     (current-load-next (lambda () list))
     (current-load-port (lambda () port))
     (current-module (lambda () env))
     (current-thread (lambda () thread))
     (current-time (lambda () time))
     (cut (syntax))
     (cute (lambda (args \.\.\.) proc))
     (debug-print (lambda (obj)))
     (debug-print-width (lambda () int))
     (debug-source-info (lambda (obj)))
     (dec! (syntax))
     (decode-float (lambda (x1) vector))
     (define-class (syntax))
     (define-condition-type (syntax))
     (define-constant (syntax))
     (define-generic (syntax))
     (define-in-module (syntax))
     (define-inline (syntax))
     (define-macro (syntax))
     (define-method (syntax))
     (define-module (syntax))
     (define-reader-ctor (lambda (sym proc) undefined))
     (define-values (syntax))
     (delete$ (lambda (obj) proc))
     (delete-keyword (lambda (id list) list))
     (delete-keyword! (lambda (id list) list))
     (delete-method! (lambda (generic method) undefined))
     (digit->integer (lambda (ch) n))
     (disasm (lambda (proc) undefined))
     (dolist (syntax))
     (dotimes (syntax))
     (dotted-list? (lambda (obj) bool))
     (dynamic-load (lambda (file)))
     (eager (lambda (obj)))
     (eq-hash (lambda (obj)))
     (eqv-hash (lambda (obj)))
     (error (lambda (msg-string args \.\.\.)))
     (errorf (lambda (fmt-string args \.\.\.)))
     (eval-when (syntax))
     (every$ (lambda (pred) pred))
     (every-pred (lambda (pred \.\.\.) pred))
     (exit (lambda (:optional n) undefined))
     (export (syntax))
     (export-all (syntax))
     (export-if-defined (syntax))
     (extend (syntax))
     (extract-condition (lambda (condition type)))
     (file-exists? (lambda (filename) bool))
     (file-is-directory? (lambda (filename) bool))
     (file-is-regular? (lambda (filename) bool))
     (filter$ (lambda (pred) proc))
     (find (lambda (pred list)))
     (find$ (lambda (pred) proc))
     (find-module (lambda (id) env))
     (find-tail$ (lambda (pred) proc))
     (fixnum? (lambda (obj) bool))
     (flonum? (lambda (obj) bool))
     (fluid-let (syntax))
     (flush (lambda (:optional output-port) undefined))
     (flush-all-ports (lambda () undefined))
     (fmod (lambda (x1 x2) x3))
     (fold (lambda (proc init list)))
     (fold$ (lambda (proc :optional init) proc))
     (fold-right (lambda (proc init list)))
     (fold-right$ (lambda (proc :optional init)))
     (for-each$ (lambda (proc) (lambda (ls) undefined)))
     (foreign-pointer-attribute-get (lambda (ptr attr)))
     (foreign-pointer-attribute-set (lambda (ptr attr val)))
     (foreign-pointer-attributes (lambda (ptr) list))
     (format (lambda (fmt-string arg \.\.\.)))
     (format/ss (lambda (fmt-string arg \.\.\.)))
     (frexp (lambda (x1) x2))
     (gauche-architecture (lambda () string))
     (gauche-architecture-directory (lambda () string))
     (gauche-character-encoding (lambda () symbol))
     (gauche-dso-suffix (lambda () string))
     (gauche-library-directory (lambda () string))
     (gauche-site-architecture-directory (lambda () string))
     (gauche-site-library-directory (lambda () string))
     (gauche-version (lambda () string))
     (gc (lambda () undefined))
     (gc-stat (lambda () list))
     (gensym (lambda (:optional name) symbol))
     (get-keyword (lambda (id list :optional default)))
     (get-keyword* (syntax))
     (get-optional (syntax))
     (get-output-string (lambda (string-output-port) string))
     (get-remaining-input-string (lambda (port) string))
     (get-signal-handler (lambda (n) proc))
     (get-signal-handler-mask (lambda (n) n))
     (get-signal-handlers (lambda () list))
     (get-signal-pending-limit (lambda () n))
     (getter-with-setter (lambda (get-proc set-proc) proc))
     (global-variable-bound? (lambda (sym) bool))
     (global-variable-ref (lambda (sym)))
     (guard (syntax))
     (has-setter? (lambda (proc) bool))
     (hash (lambda (obj)))
     (hash-table (lambda (id pair \.\.\.) hash-table))
     (hash-table-delete! (lambda (hash-table key) undefined))
     (hash-table-exists? (lambda (hash-table key) bool))
     (hash-table-fold (lambda (hash-table proc init)))
     (hash-table-for-each (lambda (hash-table proc) undefined))
     (hash-table-get (lambda (hash-table key :optional default)))
     (hash-table-keys (lambda (hash-table) list))
     (hash-table-map (lambda (hash-table proc) list))
     (hash-table-num-entries (lambda (hash-table) n))
     (hash-table-pop! (lambda (hash-table key :optional default)))
     (hash-table-push! (lambda (hash-table key value) undefined))
     (hash-table-put! (lambda (hash-table key value) undefined))
     (hash-table-stat (lambda (hash-table) list))
     (hash-table-type (lambda (hash-table) id))
     (hash-table-update! (lambda (hash-table key proc :optional default) undefined))
     (hash-table-values (lambda (hash-table) list))
     (hash-table? (lambda (obj) bool))
     (identifier->symbol (lambda (obj) sym))
     (identifier? (lambda (obj) bool))
     (identity (lambda (obj)))
     (import (syntax))
     (inc! (syntax))
     (inexact-/ (lambda (x1 x2) x3))
     (initialize (lambda (obj)))
     (instance-slot-ref (lambda (obj id)))
     (instance-slot-set (lambda (obj id value)))
     (integer->digit (lambda (n) ch))
     (integer-length (lambda (n) n))
     (is-a? (lambda (obj class) bool))
     (keyword->string (lambda (id) string))
     (keyword? (lambda (obj) bool))
     (last-pair (lambda (pair) pair))
     (lazy (syntax))
     (ldexp (lambda (x1 n) x2))
     (let-keywords* (syntax))
     (let-optionals* (syntax))
     (let/cc (syntax))
     (let1 (syntax))
     (library-exists? (lambda (filename) bool))
     (library-fold (lambda (string proc init)))
     (library-for-each (lambda (string proc) undefined))
     (library-has-module? (lambda (filename id) bool))
     (library-map (lambda (string proc) list))
     (list* (lambda (obj \.\.\.) list))
     (list-copy (lambda (list) list))
     (logand (lambda (n \.\.\.) n))
     (logbit? (lambda (index n) bool))
     (logcount (lambda (n) n))
     (logior (lambda (n \.\.\.) n))
     (lognot (lambda (n) n))
     (logtest (lambda (n \.\.\.) bool))
     (logxor (lambda (n \.\.\.) n))
     (macroexpand (lambda (obj)))
     (macroexpand-1 (lambda (obj)))
     (make (lambda (class args \.\.\.)))
     (make-byte-string (lambda (n :optional int) byte-string))
     (make-compound-condition (lambda (condition \.\.\.) condition))
     (make-condition (lambda (condition-type field+value \.\.\.) condition))
     (make-condition-type (lambda (id condition-type list) condition-type))
     (make-hash-table (lambda (:optional id) hash-table))
     (make-keyword (lambda (string) sym))
     (make-list (lambda (n :optional init) list))
     (make-module (lambda (id :optional if-exists-proc) env))
     (make-weak-vector (lambda (n) vector))
     (map$ (lambda (proc) proc))
     (member$ (lambda (obj) proc))
     (merge (lambda (list1 list2 proc) list))
     (merge! (lambda (list1 list2 proc) list))
     (method-more-specific? (lambda (method1 method2 list) bool))
     (min&max (lambda (x1 \.\.\.) (values x2 x3)))
     (modf (lambda (x1) x2))
     (module-exports (lambda (env) list))
     (module-imports (lambda (env) list))
     (module-name (lambda (env) sym))
     (module-name->path (lambda (sym) string))
     (module-parents (lambda (env) list))
     (module-precedence-list (lambda (env) list))
     (module-table (lambda (env) hash-table))
     (module? (lambda (obj) bool))
     (null-list? (lambda (obj) bool))
     (object-* (lambda (z \.\.\.) z))
     (object-+ (lambda (z \.\.\.) z))
     (object-- (lambda (z \.\.\.) z))
     (object-/ (lambda (z \.\.\.) z))
     (object-apply (lambda (proc arg \.\.\.)))
     (object-compare (lambda (obj1 obj2) n))
     (object-equal? (lambda (obj1 obj2) bool))
     (object-hash (lambda (obj) n))
     (open-coding-aware-port (lambda (input-port) input-port))
     (open-input-buffered-port (lambda ()))
     (open-input-fd-port (lambda (fileno) input-port))
     (open-input-string (lambda (str) input-port))
     (open-output-buffered-port (lambda ()))
     (open-output-fd-port (lambda (fileno) output-port))
     (open-output-string (lambda () string-output-port))
     (pa$ (lambda (proc arg \.\.\.) proc))
     (partition$ (lambda (pred) proc))
     (path->module-name (lambda (str) sym))
     (peek-byte (lambda (:optional input-port) n))
     (pop! (syntax (list)))
     (port->byte-string (lambda (input-port) byte-string))
     (port->list (lambda (proc input-port) list))
     (port->sexp-list (lambda (port) list))
     (port->string (lambda (port) string))
     (port->string-list (lambda (port) list))
     (port-buffering (lambda (port) sym))
     (port-closed? (lambda (port) bool))
     (port-current-line (lambda (port) n))
     (port-file-number (lambda (port) n))
     (port-fold (lambda (proc init port)))
     (port-fold-right (lambda (proc init port)))
     (port-for-each (lambda (proc read-proc) undefined))
     (port-map (lambda (proc read-proc)))
     (port-name (lambda (port) name))
     (port-position-prefix (lambda ()))
     (port-seek (lambda (port offset (set int SEEK_SET SEEK_CUR SEEK_END))))
     (port-tell (lambda (port) n))
     (port-type (lambda (port) sym))
     (print (lambda (obj \.\.\.)))
     (procedure-arity-includes? (lambda (proc n) bool))
     (procedure-info (lambda (proc)))
     (profiler-reset (lambda () undefined))
     (profiler-show (lambda () undefined))
     (profiler-show-load-stats (lambda () undefined))
     (profiler-start (lambda () undefined))
     (profiler-stop (lambda () undefined))
     (program (syntax))
     (promise-kind (lambda ()))
     (promise? (lambda (obj) bool))
     (proper-list? (lambda (obj) bool))
     (provide (lambda (str) undefined))
     (provided? (lambda (str) bool))
     (push! (syntax))
     (quotient&remainder (lambda (n1 n2) (values n1 n2)))
     (raise (lambda (exn) undefined))
     (read-block (lambda (n :optional input-port) string))
     (read-byte (lambda (:optional input-port) n))
     (read-eval-print-loop (lambda () undefined))
     (read-from-string (lambda (str)))
     (read-line (lambda (:optional input-port) str))
     (read-list (lambda (ch :optional input-port)))
     (read-reference-has-value? (lambda ()))
     (read-reference-value (lambda ()))
     (read-reference? (lambda ()))
     (read-with-shared-structure (lambda (:optional input-port)))
     (read/ss (lambda (:optional input-port)))
     (rec (syntax))
     (receive (syntax))
     (redefine-class! (lambda ()))
     (reduce$ (lambda (proc :optional default) proc))
     (reduce-right$ (lambda (proc :optional default) proc))
     (ref (lambda (obj key \.\.\.)))
     (ref* (lambda (obj key \.\.\.)))
     (regexp->string (lambda (regexp) string))
     (regexp-case-fold? (lambda (regexp) bool))
     (regexp-compile (lambda (str) regexp))
     (regexp-optimize (lambda (str) str))
     (regexp-parse (lambda (str) list))
     (regexp-quote (lambda (str) str))
     (regexp-replace (lambda (regexp string subst) string))
     (regexp-replace* (lambda (string regexp subst \.\.\.) string))
     (regexp-replace-all (lambda (regexp string subst) string))
     (regexp-replace-all* (lambda (string regexp subst \.\.\.)))
     (regexp? (lambda (obj) bool))
     (regmatch? (lambda (obj) bool))
     (remove$ (lambda (pred) proc))
     (report-error (lambda ()))
     (require (syntax))
     (require-extension (syntax))
     (reverse! (lambda (list) list))
     (rxmatch (lambda (regexp string) regmatch))
     (rxmatch-after (lambda (regmatch :optional i) str))
     (rxmatch-before (lambda (regmatch :optional i) str))
     (rxmatch-case (syntax))
     (rxmatch-cond (syntax))
     (rxmatch-end (lambda (regmatch :optional i) n))
     (rxmatch-if (syntax))
     (rxmatch-let (syntax))
     (rxmatch-num-matches (lambda (regmatch) i))
     (rxmatch-start (lambda (regmatch :optional i) n))
     (rxmatch-substring (lambda (regmatch :optional i) str))
     (seconds->time (lambda (x1) time))
     (select-module (syntax))
     (set!-values (syntax))
     (set-signal-handler! (lambda (signals handler) undefined))
     (set-signal-pending-limit (lambda (n) undefined))
     (setter (lambda (proc) proc))
     (sinh (lambda (z) z))
     (slot-bound-using-accessor? (lambda (proc obj id) bool))
     (slot-bound-using-class? (lambda (class obj id) bool))
     (slot-bound? (lambda (obj id) bool))
     (slot-definition-accessor (lambda ()))
     (slot-definition-allocation (lambda ()))
     (slot-definition-getter (lambda ()))
     (slot-definition-name (lambda ()))
     (slot-definition-option (lambda ()))
     (slot-definition-options (lambda ()))
     (slot-definition-setter (lambda ()))
     (slot-exists-using-class? (lambda (class obj id) bool))
     (slot-exists? (lambda (obj id) bool))
     (slot-initialize-using-accessor! (lambda ()))
     (slot-missing (lambda (class obj id)))
     (slot-push! (lambda (obj id value) undefined))
     (slot-ref (lambda (obj id)))
     (slot-ref-using-accessor (lambda (proc obj id)))
     (slot-ref-using-class (lambda (class obj id)))
     (slot-set! (lambda (obj id value) undefined))
     (slot-set-using-accessor! (lambda (proc obj id value) undefined))
     (slot-set-using-class! (lambda (class obj id value) undefined))
     (slot-unbound (lambda (class obj id)))
     (sort (lambda (seq :optional proc)))
     (sort! (lambda (seq :optional proc)))
     (sort-applicable-methods (lambda ()))
     (sorted? (lambda (seq :optional proc)))
     (split-at (lambda (list i) (values list list)))
     (stable-sort (lambda (seq :optional proc)))
     (stable-sort! (lambda (seq :optional proc)))
     (standard-error-port (lambda () output-port))
     (standard-input-port (lambda () input-port))
     (standard-output-port (lambda () output-port))
     (string->regexp (lambda (str) regexp))
     (string-byte-ref (lambda (str i) n))
     (string-byte-set! (lambda (str i n) undefined))
     (string-complete->incomplete (lambda (str) str))
     (string-immutable? (lambda (str) bool))
     (string-incomplete->complete (lambda (str) str))
     (string-incomplete->complete! (lambda (str) str))
     (string-incomplete? (lambda (str) bool))
     (string-interpolate (lambda (str) list))
     (string-join (lambda (list :optional delim-str (set grammar infix strict-infix prefix suffix))))
     ;; deprecated
     ;;      (string-pointer-byte-index (lambda ()))
     ;;      (string-pointer-copy (lambda ()))
     ;;      (string-pointer-index (lambda ()))
     ;;      (string-pointer-next! (lambda ()))
     ;;      (string-pointer-prev! (lambda ()))
     ;;      (string-pointer-ref (lambda ()))
     ;;      (string-pointer-set! (lambda ()))
     ;;      (string-pointer-substring (lambda ()))
     ;;      (string-pointer? (lambda ()))
     (string-scan (lambda (string item :optional (set return index before after before* after* both))))
     (string-size (lambda (str) n))
     (string-split (lambda (str splitter) list))
     (string-substitute! (lambda ()))
     (subr? (lambda (obj) bool))
     (supported-character-encoding? (lambda (id) bool))
     (supported-character-encodings (lambda () list))
     (symbol-bound? (lambda (id) bool))
     (syntax-error (syntax))
     (syntax-errorf (syntax))
     (sys-abort (lambda () undefined))
     (sys-access (lambda (filename (flags amode R_OK W_OK X_OK F_OK))))
     (sys-alarm (lambda (x1) x2))
     (sys-asctime (lambda (time) str))
     (sys-basename (lambda (filename) str))
     (sys-chdir (lambda (dirname)))
     (sys-chmod (lambda (filename n)))
     (sys-chown (lambda (filename uid gid)))
     (sys-close (lambda (fileno)))
     (sys-crypt (lambda (key-str salt-str) str))
     (sys-ctermid (lambda () string))
     (sys-ctime (lambda (time) string))
     (sys-difftime (lambda (time1 time2) x1))
     (sys-dirname (lambda (filename) string))
     (sys-exec (lambda (command-string list) n))
     (sys-exit (lambda (n) undefined))
     (sys-fchmod (lambda (port-or-fileno n)))
     (sys-fdset-max-fd (lambda (fdset)))
     (sys-fdset-ref (lambda (fdset port-or-fileno)))
     (sys-fdset-set! (lambda (fdset port-or-fileno)))
     (sys-fork (lambda () n))
     (sys-fork-and-exec (lambda (command-string list) n))
     (sys-fstat (lambda (port-or-fileno) sys-stat))
     (sys-ftruncate (lambda (port-or-fileno n)))
     (sys-getcwd (lambda () string))
     (sys-getdomainname (lambda () string))
     (sys-getegid (lambda () gid))
     (sys-getenv (lambda (name) string))
     (sys-geteuid (lambda () uid))
     (sys-getgid (lambda () gid))
     (sys-getgrgid (lambda () gid))
     (sys-getgrnam (lambda (name)))
     (sys-getgroups (lambda () list))
     (sys-gethostname (lambda () string))
     (sys-getloadavg (lambda () list))
     (sys-getlogin (lambda () string))
     (sys-getpgid (lambda () gid))
     (sys-getpgrp (lambda () gid))
     (sys-getpid (lambda () pid))
     (sys-getppid (lambda () pid))
     (sys-getpwnam (lambda (name)))
     (sys-getpwuid (lambda () uid))
     (sys-gettimeofday (lambda () (values x1 x2)))
     (sys-getuid (lambda () uid))
     (sys-gid->group-name (lambda (gid) name))
     (sys-glob (lambda (string) list))
     (sys-gmtime (lambda (time) string))
     (sys-group-name->gid (lambda (name) gid))
     (sys-isatty (lambda (port-or-fileno) bool))
     (sys-kill (lambda (pid)))
     (sys-lchown (lambda (filename uid gid)))
     (sys-link (lambda (old-filename new-filename)))
     (sys-localeconv (lambda () alist))
     (sys-localtime (lambda (time) string))
     (sys-lstat (lambda (filename) sys-stat))
     (sys-mkdir (lambda (dirname)))
     (sys-mkfifo (lambda (filename)))
     (sys-mkstemp (lambda (filename)))
     (sys-mktime (lambda (time) x1))
     (sys-nanosleep (lambda (x1)))
     (sys-normalize-pathname (lambda (filename) string))
     (sys-pause (lambda (x1)))
     (sys-pipe (lambda (:optional buffering) (values input-port output-port)))
     (sys-putenv (lambda (name string)))
     (sys-random (lambda () n))
     (sys-readdir (lambda (dirname) list))
     (sys-readlink (lambda (filename) string))
     (sys-realpath (lambda (filename) string))
     (sys-remove (lambda (filename)))
     (sys-rename (lambda (old-filename new-filename)))
     (sys-rmdir (lambda (dirname)))
     (sys-select (lambda (read-filenos write-filenos execpt-filenos :optional timeout-x)))
     (sys-select! (lambda (read-filenos write-filenos execpt-filenos :optional timeout-x)))
     (sys-setenv (lambda (name string)))
     (sys-setgid (lambda (gid)))
     (sys-setlocale (lambda (locale-string)))
     (sys-setpgid (lambda (gid)))
     (sys-setsid (lambda ()))
     (sys-setuid (lambda (uid)))
     (sys-sigmask (lambda ((set how SIG_SETMASK SIG_BLOCK SIG_UNBLOCK) sigset)))
     (sys-signal-name (lambda (n)))
     (sys-sigset (lambda (n \.\.\.) sigset))
     (sys-sigset-add! (lambda (sigset n)))
     (sys-sigset-delete! (lambda (sigset n)))
     (sys-sigset-empty! (lambda (sigset)))
     (sys-sigset-fill! (lambda (sigset)))
     (sys-sigsuspend (lambda (sigset)))
     (sys-sigwait (lambda (sigset)))
     (sys-sleep (lambda (x1)))
     (sys-srandom (lambda (n)))
     (sys-stat (lambda (filename)))
     ;; deprecated
     ;;      (sys-stat->atime (lambda ()))
     ;;      (sys-stat->ctime (lambda ()))
     ;;      (sys-stat->dev (lambda ()))
     ;;      (sys-stat->file-type (lambda ()))
     ;;      (sys-stat->gid (lambda ()))
     ;;      (sys-stat->ino (lambda ()))
     ;;      (sys-stat->mode (lambda ()))
     ;;      (sys-stat->mtime (lambda ()))
     ;;      (sys-stat->nlink (lambda ()))
     ;;      (sys-stat->rdev (lambda ()))
     ;;      (sys-stat->size (lambda ()))
     ;;      (sys-stat->type (lambda ()))
     ;;      (sys-stat->uid (lambda ()))
     (sys-strerror (lambda (errno) string))
     (sys-strftime (lambda (format-string time)))
     (sys-symlink (lambda (old-filename new-filename)))
     (sys-system (lambda (command) n))
     (sys-time (lambda () n))
     (sys-times (lambda () list))
     ;;      (sys-tm->alist (lambda ()))
     (sys-tmpnam (lambda () string))
     (sys-truncate (lambda (filename n)))
     (sys-ttyname (lambda (port-or-fileno) string))
     (sys-uid->user-name (lambda (uid) name))
     (sys-umask (lambda () n))
     (sys-uname (lambda () string))
     (sys-unlink (lambda (filename)))
     (sys-unsetenv (lambda (name)))
     (sys-user-name->uid (lambda (name) uid))
     (sys-utime (lambda (filename)))
     (sys-wait (lambda ()))
     (sys-wait-exit-status (lambda (n) n))
     (sys-wait-exited? (lambda (n) bool))
     (sys-wait-signaled? (lambda (n) bool))
     (sys-wait-stopped? (lambda (n) bool))
     (sys-wait-stopsig (lambda (n) n))
     (sys-wait-termsig (lambda (n) n))
     (sys-waitpid (lambda (pid)))
     (tanh (lambda (z) z))
     (time (syntax))
     (time->seconds (lambda (time) x1))
     (time? (lambda (obj) bool))
     (toplevel-closure? (lambda (obj) bool))
     (touch-instance! (lambda ()))
     (ucs->char (lambda (n) ch))
     (undefined (lambda () undefined))
     (undefined? (lambda (obj) bool))
     (unless (syntax))
     (until (syntax))
     (unwrap-syntax (lambda (obj)))
     (update! (syntax))
     (update-direct-method! (lambda ()))
     (update-direct-subclass! (lambda ()))
     (use (special symbol scheme-gauche-available-modules))
     (use-version (syntax))
     (values-ref (syntax))
     (vector-copy (lambda (vector :optional start end fill) vector))
     (vm-dump (lambda () undefined))
     (vm-get-stack-trace (lambda () undefined))
     (vm-get-stack-trace-lite (lambda () undefined))
     (vm-set-default-exception-handler (lambda (handler) undefined))
     (warn (lambda (message-str args) undefined))
     (weak-vector-length (lambda (vector) n))
     (weak-vector-ref (lambda (vector i)))
     (weak-vector-set! (lambda (vector i value) undefined))
     (when (syntax))
     (while (syntax))
     (with-error-handler (lambda (handler thunk)))
     (with-error-to-port (lambda (port thunk)))
     (with-exception-handler (lambda (handler thunk)))
     (with-input-from-port (lambda (port thunk)))
     (with-input-from-string (lambda (string thunk)))
     (with-module (syntax))
     (with-output-to-port (lambda (port thunk)))
     (with-output-to-string (lambda (thunk) string))
     (with-port-locking (lambda (port thunk)))
     (with-ports (lambda (input-port output-port error-port thunk)))
     (with-signal-handlers (syntax))
     (with-string-io (lambda (string thunk) string))
     (write* (lambda (obj :optional output-port) undefined))
     (write-byte (lambda (n :optional output-port) undefined))
     (write-limited (lambda (obj :optional output-port)))
     (write-object (lambda (obj output-port)))
     (write-to-string (lambda (obj) string))
     (write-with-shared-structure (lambda (obj :optional output-port)))
     (write/ss (lambda (obj :optional output-port)))
     (x->integer (lambda (obj) integer))
     (x->number (lambda (obj) number))
     (x->string (lambda (obj) string))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special lookups (TODO add more impls, try to abstract better)

;; visit a file and kill the buffer only if it wasn't already open
(defmacro scheme-with-find-file (path-expr &rest body)
  (let ((path (gensym "path"))
        (buf0 (gensym "buf"))
        (buf (gensym "buf")))
    `(save-window-excursion
       (let* ((,path (file-truename ,path-expr))
              (,buf0 (find-if
                      #'(lambda (x)
                          (let ((buf-file (buffer-file-name x)))
                            (and buf-file
                                 (equal ,path (file-truename buf-file)))))
                      (buffer-list)))
              (,buf (or ,buf0 (and (file-exists-p ,path)
                                   (find-file-noselect ,path t)))))
         (unless ,buf
           (error "no such file" ,path))
         (set-buffer ,buf)
         (unwind-protect
             (save-excursion
               (goto-char (point-min))
               ,@body)
           (unless ,buf0 (kill-buffer (current-buffer))))))))

(defun scheme-srfi-exports (i)
  (and (integerp i)
       (>= i 0)
       (< i (length *scheme-srfi-info*))
       (let ((info (cdr (aref *scheme-srfi-info* i))))
         (if (and (consp info) (null (cdr info)) (symbolp (car info)))
             (scheme-module-exports/compute (car info))
           info))))

(defvar scheme-module-exports-function nil
  "Set to override the exports inferred from the implementation")

(defvar *scheme-module-exports-functions*
  '((chicken  . scheme-module-exports/chicken)
    (gauche   . scheme-module-exports/gauche)))

(defun scheme-fixup-path (path)
  (and (consp path)
       (delete-duplicates
        (remove-if-not #'(lambda (dir)
                           (and (stringp dir)
                                (not (equal dir ""))
                                (or (string-prefix-p "./" dir)
                                    (file-directory-p dir))))
                       path))))

(defvar *scheme-chibi-repo-path* 'compute-me-later)
(defvar *scheme-chicken-repo-path* 'compute-me-later)
(defvar *scheme-gauche-repo-path* 'compute-me-later)
(defvar *scheme-gauche-site-repo-path* 'compute-me-later)

(defun scheme-library-path (impl)
  (case impl
    ((chibi)
     (when (not (listp *scheme-chibi-repo-path*))
       (setq *scheme-chibi-repo-path*
             (or (scheme-fixup-path
                  (read (scheme-shell-command-to-line
                         "chibi-scheme -q -p '(current-module-path)'")))
                 (list "." "./lib" "/usr/local/share/chibi"))))
     *scheme-chibi-repo-path*)
    ((chicken)
     (when (not (listp *scheme-chicken-repo-path*))
       (setq *scheme-chicken-repo-path*
             (scheme-fixup-path
              (let ((base
                     (scheme-fixup-path
                      (list (getenv "CHICKEN_HOME")
                            (scheme-shell-command-to-line
                             "csi -p '(repository-path)'")))))
                (or base (list "/usr/local/lib/chicken/7"))))))
     *scheme-chicken-repo-path*)
    ((gauche)
     (when (not (listp *scheme-gauche-repo-path*))
       (setq *scheme-gauche-repo-path*
             (scheme-fixup-path
              (cons (scheme-shell-command-to-line "gauche-config --syslibdir")
                    (list "/usr/share/gauche"
                          "/usr/local/share/gauche"
                          "/opt/share/gauche"
                          "/opt/local/share/gauche"))))
       (setq *scheme-gauche-site-repo-path*
             (scheme-fixup-path
              (cons (scheme-shell-command-to-line "gauche-config --sitelibdir")
                    (mapcar #'(lambda (dir) (concat dir "/site/lib"))
                            *scheme-gauche-repo-path*)))))
     (append *scheme-gauche-site-repo-path*
             *scheme-gauche-repo-path*))
    (t '())))

(defun scheme-library-root (base)
  "The root library dir for `base'."
  (scheme-with-find-file base
    (re-search-forward "^(define-library\\s-" nil t)
    (let ((name (reverse (scheme-nth-sexp-at-point 0)))
          (dir base))
      (while (and (consp name)
                  (string-match (concat (scheme-sexp-to-string (car name))
                                        "\\(/+\\|\\.sld\\)?")
                                dir))
        (setq name (cdr name))
        (setq dir
              (replace-regexp-in-string "/+$" "" (file-name-directory dir))))
      (and (null name) dir))))

(defun scheme-guess-containing-library-path (impl)
  (or *scheme-default-library-path*
      (let* ((lang+base
              (scheme-code-context (buffer-file-name (current-buffer))))
             (dir (and (memq (car lang+base) '(r7rs r7rs-library-declaration))
                       (cadr lang+base)
                       (scheme-library-root (cadr lang+base)))))
        (if dir (list dir) (list "." "./lib")))))

(defun scheme-r7rs-library-path (impl)
  (scheme-fixup-path
   (append (scheme-guess-containing-library-path impl)
           (and (symbolp impl)
                (list (concat "/usr/local/share/snow/" (symbol-name impl))))
           ;; default to chibi since it installs source
           (scheme-library-path
            (if (or (not impl) (eq 'r7rs impl))
                (or scheme-default-implementation 'chibi)
              impl)))))

(defun scheme-mtime>? (a b)
  (or (> (car a) (car b))
      (and (= (car a) (car b))
           (> (cadr a) (cadr b)))))

(defvar *scheme-complete-module-cache* (make-hash-table :test #'equal)
  "Cache of module exports.")

(defun scheme-complete-clear-cache ()
  "Clear all scheme-complete caches."
  (interactive)
  (setq *scheme-imported-modules* '())
  (setq *scheme-complete-module-cache* (make-hash-table :test #'equal))
  (setq *scheme-library-includes-cache* (make-hash-table :test #'equal))
  (setq *scheme-include-globals-cache* (make-hash-table :test #'equal)))

(defun scheme-module-exports (mod)
  (unless (member mod *scheme-imported-modules*)
    (push mod *scheme-imported-modules*)
    (scheme-module-exports/compute mod)))

(defun scheme-module-exports/compute (mod)
  (cond
   ((and (consp mod) (eq 'srfi (car mod)) (integerp (cadr mod))
         (scheme-srfi-exports (cadr mod)))
    )
   ((and (symbolp mod) (string-match "^srfi-" (symbol-name mod)))
    (scheme-srfi-exports
     (string-to-number (substring (symbol-name mod) 5))))
   ((equal '(scheme r5rs) mod)
    (scheme-r5rs-info))
   ((equal '(chibi) mod)
    (append (scheme-r5rs-info)
            (cdr (assq (car mod) *scheme-implementation-exports*))))
   ((and (consp mod) (eq 'scheme (car mod))
         (cdr (assoc mod *scheme-r7rs-info*))))
   ((and (consp mod) (null (cdr mod))
         (cdr (assq (car mod) *scheme-implementation-exports*))))
   ((and (symbolp mod)
         (cdr (assq mod *scheme-implementation-exports*))))
   (t
    (let ((cached (gethash mod *scheme-complete-module-cache*)))
      ;; check cache
      (if (and cached
               (stringp (car cached))
               (not
                (ignore-errors
                  (let ((mtime (nth 5 (file-attributes (car cached))))
                        (ptime (cadr cached)))
                    (scheme-mtime>? mtime ptime)))))
          (caddr cached)
        ;; (re)compute module exports
        (let ((export-fun
               (if (consp mod)
                   #'scheme-module-exports/r7rs
                 (or scheme-module-exports-function
                     (cdr (assq (scheme-current-implementation)
                                *scheme-module-exports-functions*))
                     #'scheme-module-exports/r7rs))))
          (when export-fun
            (let ((res (funcall export-fun mod)))
              (when res
                (when (and scheme-complete-cache-p (car res))
                  (puthash mod
                           (list
                            (car res)
                            (nth 5 (file-attributes (car res)))
                            (cadr res))
                           *scheme-complete-module-cache*))
                (cadr res))))))))))

(defun scheme-module-exports/r7rs (mod)
  (let* ((file (concat (if (symbolp mod)
                           (subst-char-in-string ?. ?/ (symbol-name mod))
                         (mapconcat #'(lambda (x) (if (numberp x)
                                                 (number-to-string x)
                                                 (symbol-name x)))
                                    mod
                                    "/"))
                       *scheme-r7rs-extension*))
         (path (scheme-r7rs-library-path (scheme-current-implementation)))
         (dir (scheme-find-file-in-path file path)))
    ;;(message "mod: %s file: %s path: %s dir: %s" mod file path dir)
    (when dir
      (list (concat dir "/" file)
            (scheme-with-find-file (concat dir "/" file)
              (scheme-current-exports/typed))))))

(defun scheme-module-exports/chicken (mod)
  ;; TODO: use types.db
  (let* ((mod-str (symbol-name mod))
         ;; look for the source in the current directory
         (source-file (concat mod-str ".scm"))
         ;; try the chicken modules db
         (modules-db (concat (scheme-library-path 'chicken) "/modules.db")))
    (cond
     ((eq mod 'scheme)
      (list nil (scheme-r5rs-info)))
     ((file-exists-p source-file)
      (list source-file
            (scheme-with-find-file source-file
              (scheme-current-exports/typed))))
     ((file-exists-p modules-db)
      (list modules-db
            (mapcar
             #'(lambda (x)
                 (cons (intern (car (split-string (substring x 1))))
                       '((lambda ()))))
             (remove-if-not
              #'(lambda (x) (string-match (concat " " mod-str ")") x))
              (scheme-file->lines modules-db))))))))

(defun scheme-module-exports/gauche (mod)
  (let* ((file (concat (subst-char-in-string ?. ?/ (symbol-name mod)) ".scm"))
         (path (scheme-library-path 'gauche))
         (dir (scheme-find-file-in-path file path)))
    (when dir
      (list
       (concat dir "/" file)
       (scheme-with-find-file (concat dir "/" file)
         (or (scheme-current-exports)
             (scheme-current-globals)))))))

(defun scheme-chicken-available-modules (&optional sym)
  (mapcar
   #'(lambda (mod) (split-string mod "\\."))
   (append
    (mapcar
     #'(lambda (f)
         (let ((f (file-name-sans-extension f)))
           (if (equalp "import" (file-name-extension f))
               (file-name-sans-extension f)
             f)))
     (directory-files "." nil "^[^.].*\\.scm$" t))
    (scheme-append-map
     #'(lambda (dir)
         (mapcar
          #'(lambda (f)
              (let ((f (file-name-sans-extension f)))
                (if (equalp "import" (file-name-extension f))
                    (file-name-sans-extension f)
                  f)))
          (if (string-match "/7" dir)
              (directory-files dir nil "^[^.].*\\.import\\.\\(so\\|scm\\)$" t)
            (directory-files dir nil "^[^.].*\\.\\(so\\|scm\\)$" t))))
     (scheme-library-path 'chicken)))))

(defun scheme-gauche-available-modules (&optional sym)
  (let ((path (scheme-library-path 'gauche)))
    (mapcar
     #'(lambda (f) (split-string f "/"))
     (mapcar
      #'file-name-sans-extension
      (scheme-append-map
       #'(lambda (dir)
           (let ((len (length dir)))
             (mapcar #'(lambda (f) (substring f (+ 1 len)))
                     (scheme-directory-tree-files dir t "\\.scm\\'" 2))))
       path)))))

(defun scheme-r7rs-available-modules (&optional impl)
  (let ((path (scheme-r7rs-library-path
               (or impl (scheme-current-implementation)))))
    (append
     '(("scheme" "base") ("scheme" "case-lambda") ("scheme char")
       ("scheme" "complex") ("scheme" "cxr") ("scheme" "eval")
       ("scheme" "file") ("scheme" "inexact") ("scheme" "lazy")
       ("scheme" "load") ("scheme" "process-context") ("scheme" "r5rs")
       ("scheme" "read") ("scheme" "repl") ("scheme" "time") ("scheme" "write"))
     (mapcar
      #'(lambda (f) (split-string f "/+" t))
      (mapcar
       #'file-name-sans-extension
       (scheme-append-map
        #'(lambda (dir)
            (let ((len (length dir)))
              (mapcar #'(lambda (f) (substring f (+ 1 len)))
                      (scheme-directory-tree-files dir t "\\.sld\\'" 2))))
        path))))))

(defun scheme-available-modules (&optional impl)
  (case impl
    ((chicken) (scheme-chicken-available-modules impl))
    ((gauche) (scheme-gauche-available-modules impl))
    (t (scheme-r7rs-available-modules impl))))

(defun scheme-library-completions (prefix)
  (let ((libs (scheme-available-modules (scheme-current-implementation))))
    (while (consp prefix)
      (let* ((s (pop prefix))
             (str (cond ((symbolp s) (symbol-name s))
                        ((numberp s) (number-to-string s))
                        (t s))))
        (setq libs (mapcar #'cdr (remove-if-not
                                  #'(lambda (x) (equal str (car x)))
                                  libs)))))
    (remove-duplicates (remove nil (mapcar #'car libs)) :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun scheme-assoc-delete-all (key alist)
  (while (and (consp (car alist))
              (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun scheme-append-map (proc init-ls)
  (if (null init-ls)
      '()
    (let* ((ls (reverse init-ls))
           (res (funcall proc (pop ls))))
      (while (consp ls)
        (setq res (append (funcall proc (pop ls)) res)))
      res)))

(defun scheme-flatten (ls)
  (cond
   ((consp ls) (cons (car ls) (scheme-flatten (cdr ls))))
   ((null ls) '())
   (t (list ls))))

(defun scheme-in-string-p ()
  (let ((orig (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((parses (parse-partial-sexp (point) orig)))
        (nth 3 parses)))))

(defun scheme-beginning-of-sexp ()
  (ignore-errors
    (let ((syn (char-syntax (char-before (point)))))
      (if (or (eq syn ?\()
              (and (eq syn ?\") (scheme-in-string-p)))
          (forward-char -1)
        (forward-sexp -1))
      t)))

(defun scheme-shell-command-to-line (cmd)
  (and (fboundp 'shell-command-to-string)
       (let* ((res (shell-command-to-string cmd))
              (len (length res)))
         (and (> len 0) (substring res 0 (- len 1))))))

(defun scheme-find-file-in-path (file path)
  (car (remove-if-not
        #'(lambda (dir) (file-exists-p (concat dir "/" file)))
        path)))

(defun scheme-directory-tree-files (init-dir &optional full match maxdepth)
  (let ((res '())
        (stack
         (list (cons 0 (if (file-directory-p init-dir) (list init-dir) '())))))
    (while (consp stack)
      (let* ((x (pop stack))
             (depth (car x))
             (dirs (cdr x)))
        (setq depth (car x))
        (while (consp dirs)
          (let* ((dir (pop dirs))
                 (files (remove-if #'(lambda (f) (string-prefix-p "." f))
                                   (directory-files dir)))
                 (full-files (mapcar #'(lambda (f) (concat dir "/" f)) files))
                 (matches (if match
                              (remove-if-not
                               #'(lambda (f) (string-match match f))
                               (if full full-files files))
                            (if full full-files files))))
            (setq res (append matches res))
            (when (or (not maxdepth) (consp matches) (< depth maxdepth))
              (push (cons (+ depth 1)
                          (remove-if-not #'file-directory-p full-files))
                    stack))))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sexp manipulation

(defun scheme-in-comment-p ()
  (memq 'font-lock-comment-face (text-properties-at (point))))

;; returns current argument position within sexp
(defun scheme-beginning-of-current-sexp-operator ()
  (ignore-errors
    (let ((pos 0))
      (if (scheme-in-comment-p)
          (comment-search-backward))
      (skip-syntax-backward "w_")
      (while (and (not (bobp))
                  (not (eq ?\( (char-before)))
                  (scheme-beginning-of-sexp))
        (incf pos))
      pos)))

(defun scheme-beginning-of-next-sexp ()
  (forward-sexp 2)
  (backward-sexp 1))

(defun scheme-beginning-of-string ()
  (interactive)
  (search-backward "\"" nil t)
  (while (and (> (point) (point-min)) (eq ?\\ (char-before)))
    (search-backward "\"" nil t)))

;; for the enclosing sexp, returns a cons of the leading symbol (if
;; any) and the current position within the sexp (starting at 0)
;; (defun scheme-enclosing-sexp-prefix ()
;;   (save-excursion
;;     (let ((pos (scheme-beginning-of-current-sexp-operator)))
;;       (cons (scheme-symbol-at-point) pos))))

(defun scheme-enclosing-2-sexp-prefixes ()
  (ignore-errors
    (save-excursion
     (let* ((pos1 (scheme-beginning-of-current-sexp-operator))
            (sym1 (scheme-symbol-at-point)))
       (backward-char)
       (or
        (ignore-errors
          (let ((pos2 (scheme-beginning-of-current-sexp-operator)))
            (list sym1 pos1 (scheme-symbol-at-point) pos2)))
        (list sym1 pos1 nil 0))))))

(defun scheme-preceding-sexps ()
  (save-excursion
    (let ((end (point))
          (res '()))
      (scheme-beginning-of-current-sexp-operator)
      (ignore-errors
        (while (< (point) end)
          (let ((x (scheme-nth-sexp-at-point 0)))
            (forward-sexp 1)
            (when (< (point) end)
              (push x res)))))
      (reverse res))))

(defun scheme-sexp->elisp (str)
  "Hack to handle differences between scheme and elisp sexp syntax."
  (replace-regexp-in-string
    "#\\\\" "?"
   (replace-regexp-in-string
    "#t\\(rue\\)?\\>" "t"
    (replace-regexp-in-string "#f\\(alse\\)?\\>" "nil" str))))

;; sexp-at-point is always fragile, both because the user can input
;; incomplete sexps and because some scheme sexps are not valid elisp
;; sexps.  this is one of the few places we use it, so we're careful
;; to wrap it in ignore-errors.
(defun scheme-nth-sexp-at-point (n)
  (ignore-errors
    (save-excursion
      (forward-sexp (+ n 1))
      (let ((end (point)))
        (forward-sexp -1)
        (let ((str (buffer-substring-no-properties (point) end)))
          (car (or (ignore-errors (read-from-string str))
                   (read-from-string (scheme-sexp->elisp str)))))))))

(defun scheme-symbol-at-point ()
  (let ((str (scheme-symbol-string-at-point)))
    (and str (intern str))))

(defun scheme-symbol-string-at-point ()
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (and (< start (point))
           (buffer-substring-no-properties start (point))))))

;; should be called from start of current top-level
(defun scheme-goto-next-top-level (&optional in-mod-p)
  (let ((here (point)))
    (or (ignore-errors (forward-sexp 2)
                       (forward-sexp -1)
                       (< here (point)))
        (if in-mod-p
            (progn (goto-char here)
                   ;; heuristic, if the parens are unbalanced look
                   ;; for a define starting after a blank line.
                   ;; better might be to assume they're using a
                   ;; consistent indentation for the module body.
                   (and (re-search-forward
                         "\n\\s-*\n\\(;[^\n]*\n\\)?\\s-*(" nil t)
                        (progn (backward-char 1) t)))
          (or (ignore-errors (end-of-defun) (end-of-defun)
                             (beginning-of-defun)
                             (< here (point)))
              (progn (goto-char here)
                     (forward-char)
                     ;; allow for prompts in inferior-scheme mode
                     (and (re-search-forward
                           "^\\(\\(#;\\|[0-9]*\\)?\\sw*=?\]?> *\\)?(" nil t)
                          (progn (backward-char 1) t)))))
        (goto-char (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable extraction

(defun scheme-sexp-type-at-point (&optional env)
  (case (and (not (eobp)) (char-syntax (char-after)))
    ((?\()
     (forward-char 1)
     (if (memq (char-syntax (char-after)) '(?w ?_))
         (let ((op (scheme-symbol-at-point)))
           (cond
            ((eq op 'lambda)
             (let ((params (scheme-nth-sexp-at-point 1)))
               `(lambda ,params)))
            (t
             (let ((spec (scheme-env-lookup env op)))
               (and spec
                    (consp (cadr spec))
                    (eq 'lambda (caadr spec))
                    (cddadr spec)
                    (car (cddadr spec)))))))
       nil))
    ((?\")
     'string)
    ((?\w)
     (if (string-match "[0-9]" (string (char-after)))
         'number
       (let ((sym (scheme-symbol-at-point)))
         (cadr (scheme-env-lookup env sym)))))
    (t
     nil)))

(defun scheme-let-vars-at-point (&optional env limit loopp)
  (let ((end (min (or limit (point-max))
                  (or (ignore-errors
                        (save-excursion (forward-sexp) (point)))
                      (point-min))))
        (vars '()))
    (forward-char 1)
    (while (< (point) end)
      (when (eq ?\( (char-after))
        (save-excursion
          (forward-char 1)
          (if (and loopp (looking-at "\\(for\\|let\\|with\\)\\>"))
              (scheme-beginning-of-next-sexp))
          (if (eq ?w (char-syntax (char-after)))
              (let* ((sym (scheme-symbol-at-point))
                     (type (and (not loopp)
                                (ignore-errors
                                  (scheme-beginning-of-next-sexp)
                                  (scheme-sexp-type-at-point env)))))
                (push (if type (list sym type) (list sym)) vars)))
          (when loopp
            (while (and (< (point) end)
                        (ignore-errors
                          (scheme-beginning-of-next-sexp)
                          (eq ?w (char-syntax (char-after)))))
              (push (list (scheme-symbol-at-point)) vars)))))
      (unless (ignore-errors (let ((here (point)))
                               (scheme-beginning-of-next-sexp)
                               (> (point) here)))
        (goto-char end)))
    (reverse vars)))

(defun scheme-extract-match-clause-vars (x)
  (cond
   ((null x) '())
   ((symbolp x)
    (if (memq x '(_ ___ \.\.\.))
        '()
      (list (list x))))
   ((consp x)
    (case (car x)
      ((or not)
       (scheme-extract-match-clause-vars (cdr x)))
      ((and)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (consp (caddr x))
                (not (memq (caaddr x)
                           '(= $ @ ? and or not quote quasiquote get! set!))))
           (cons (list (cadr x) (if (listp (caddr x)) 'list 'pair))
                 (scheme-extract-match-clause-vars (cddr x)))
         (scheme-extract-match-clause-vars (cddr x))))
      ((= $ @)
       (if (consp (cdr x)) (scheme-extract-match-clause-vars (cddr x)) '()))
      ((\? ? ) ; XXXX this is a hack, the lone ? gets read as a char (space)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (symbolp (caddr x)))
           (cons (list (caddr x) (scheme-predicate->type (cadr x)))
                 (scheme-extract-match-clause-vars (cdddr x)))
         (scheme-extract-match-clause-vars (cddr x))))
      ((get! set!)
       (if (consp (cdr x)) (scheme-extract-match-clause-vars (cadr x)) '()))
      ((quote) '())
      ((quasiquote) '())                ; XXXX
      (t
       (union (scheme-extract-match-clause-vars (car x))
              (scheme-extract-match-clause-vars (cdr x))))))
   ((vectorp x)
    (scheme-extract-match-clause-vars (concatenate 'list x)))
   (t
    '())))

;; call this from the first opening paren of the match clauses
(defun scheme-extract-match-vars (&optional pos limit)
  (let ((match-vars '())
        (limit (or limit
                   (save-excursion
                     (or
                      (ignore-errors (end-of-defun) (point))
                      (point-max))))))
    (save-excursion
      (while (< (point) limit)
        (let* ((end (ignore-errors (forward-sexp) (point)))
               (start (and end (progn (backward-sexp) (point)))))
          (cond
           ((and pos start end (or (< pos start) (> pos end)))
            (goto-char (if end (+ end 1) limit)))
           (t
            (forward-char 1)
            (let* ((pat (scheme-nth-sexp-at-point 0))
                   (new-vars (ignore-errors
                               (scheme-extract-match-clause-vars pat))))
              (setq match-vars (append new-vars match-vars)))
            (goto-char (if (or pos (not end)) limit (+ end 1)))))))
      match-vars)))

(defvar *scheme-imported-modules* '()
  "Dynamically shadowed in scheme-current-imports.")

(defun scheme-guess-implementation ()
  (save-excursion
    (goto-char (point-min))
    (or
     (and (looking-at "#! *\\([^ \t\n]+\\)")
          (let ((script (file-name-nondirectory (match-string 1))))
            (cdr (assoc script *scheme-interpreter-alist*))))
     (cond
      ((re-search-forward "(define-library +\\(.\\)" nil t)
       'r7rs)
      ((re-search-forward "(define-module +\\(.\\)" nil t)
       (if (equal "(" (match-string 1))
           'guile
         'gauche))
      ((re-search-forward "(\\(?:use\\|require-library\\) " nil t)
       'chicken)
      ((re-search-forward "(module\\s-" nil t)
       'chicken)))))

(defun scheme-current-implementation ()
  (when (and (not *scheme-current-implementation*)
             (not scheme-always-use-default-implementation-p))
    (setq *scheme-current-implementation* (scheme-guess-implementation)))
  (or *scheme-current-implementation*
      scheme-default-implementation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scheme-current-local-vars (&optional env)
  (let ((vars '())
        (start (point))
        (limit (save-excursion (beginning-of-defun) (+ (point) 1)))
        (let-limit (save-excursion (scheme-beginning-of-sexp)
                                   (scheme-beginning-of-sexp)
                                   (point)))
        (scan-internal))
    (save-excursion
      (while (> (point) limit)
        (or (ignore-errors
              (progn
                (skip-chars-backward " \t\n" limit)
                (scheme-beginning-of-sexp)))
            (goto-char limit))
        (when (and (> (point) (point-min))
                   (eq ?\( (char-syntax (char-before (point))))
                   (eq ?w (char-syntax (char-after (point)))))
          (setq scan-internal t)
          (let ((sym (scheme-symbol-at-point)))
            (case sym
              ((lambda)
               (setq vars
                     (append
                      (mapcar #'list
                              (scheme-flatten (scheme-nth-sexp-at-point 1)))
                      vars)))
              ((match match-let match-let*)
               (setq vars
                     (append
                      (ignore-errors
                        (save-excursion
                          (let ((limit (save-excursion
                                         (cond
                                          ((eq sym 'match)
                                           (backward-char 1)
                                           (forward-sexp 1))
                                          (t
                                           (forward-sexp 2)))
                                         (point))))
                            (forward-sexp 2)
                            (if (eq sym 'match)
                                (forward-sexp 1))
                            (backward-sexp 1)
                            (if (not (eq sym 'match))
                                (forward-char 1))
                            (scheme-extract-match-vars
                             (and (or (eq sym 'match) (< start limit)) start)
                             limit))))
                      vars)))
              ((let let* letrec letrec* let-syntax letrec-syntax
                    and-let* do loop)
               (or
                (ignore-errors
                  (save-excursion
                    (scheme-beginning-of-next-sexp)
                    (let* ((loop-name
                            (and (memq sym '(let loop))
                                 (eq ?w (char-syntax (char-after (point))))
                                 (prog1 (scheme-symbol-at-point)
                                   (scheme-beginning-of-next-sexp))))
                           (let-vars
                            (scheme-let-vars-at-point
                             env let-limit (eq sym 'loop))))
                      (if loop-name
                          ;; named let
                          (setq vars
                                (cons `(,loop-name (lambda ,(mapcar #'car let-vars)))
                                      (append let-vars vars)))
                        (setq vars (append let-vars vars))))
                    t))
                (goto-char limit)))
              ((let-values let*-values)
               (setq vars
                     (append (mapcar
                              #'list
                              (scheme-append-map
                               #'scheme-flatten
                               (remove-if-not #'consp
                                              (scheme-nth-sexp-at-point 1))))
                             vars)))
              ((receive defun defmacro)
               (setq vars
                     (append (mapcar #'list
                                     (scheme-flatten
                                      (scheme-nth-sexp-at-point 1)))
                             vars)))
              (t
               (if (string-match "^\\(jazz\\.\\)?define\\(-.*\\)?"
                                 (symbol-name sym))
                   (let ((defs (save-excursion
                                 (backward-char)
                                 (scheme-extract-definitions))))
                     (setq vars
                           (append (scheme-append-map
                                    #'(lambda (x)
                                        (and (consp (cdr x))
                                             (consp (cadr x))
                                             (eq 'lambda (caadr x))
                                             (mapcar #'list
                                                     (scheme-flatten
                                                      (cadadr x)))))
                                    defs)
                                   (and (not (= 1 (current-column))) defs)
                                   vars)))
                 (setq scan-internal nil))))
            ;; check for internal defines
            (when scan-internal
              (ignore-errors
                (save-excursion
                  (forward-sexp
                   (+ 1 (if (numberp scan-internal) scan-internal 2)))
                  (backward-sexp)
                  (if (< (point) start)
                      (setq vars (append (scheme-current-definitions) vars))
                    ))))))))
    (reverse vars)))

(defun scheme-extract-import-module-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((prefix prefix-in)
     (let* ((ids (scheme-extract-import-module-imports (cadr sexp)))
            (prefix0 (caddr sexp))
            (prefix (if (symbolp prefix0) (symbol-name prefix0) prefix0)))
       (mapcar #'(lambda (x)
                   (cons (intern (concat prefix (symbol-name (car x))))
                         (cdr x)))
               ids)))
    ((prefix-all-except)
     (let ((prefix
            (if (symbolp (cadr sexp)) (symbol-name (cadr sexp)) (cadr sexp)))
           (exceptions (cddr sexp)))
       (mapcar #'(lambda (x)
                   (if (memq (car x) exceptions)
                       x
                     (cons (intern (concat prefix (symbol-name (car x))))
                           (cdr x))))
               (scheme-extract-import-module-imports (caddr sexp)))))
    ((for for-syntax for-template for-label for-meta)
     (scheme-extract-import-module-imports (cadr sexp)))
    ((rename rename-in)
     (let ((renames (cddr sexp)))
       (mapcar #'(lambda (x)
                   (cons (or (cadr (assq (car x) renames)) (car x)) (cdr x)))
               (scheme-extract-import-module-imports (cadr sexp)))))
    ((except except-in)
     (remove-if #'(lambda (x) (memq (car x) (cddr sexp)))
                (scheme-extract-import-module-imports (cadr sexp))))
    ((only only-in)
     (remove-if-not
      #'(lambda (x) (memq (car x) (cddr sexp)))
      (scheme-extract-import-module-imports (cadr sexp))))
    ((import import-for-syntax require)
     (scheme-append-map #'scheme-extract-import-module-imports (cdr sexp)))
    ((library)
     (if (and (stringp (cadr sexp)) (file-exists-p (cadr sexp)))
         (scheme-module-exports (intern (cadr sexp)))))
    ((lib)
     (if (and (equal "srfi" (caddr sexp))
              (stringp (cadr sexp))
              (string-match "^[0-9]+\\." (cadr sexp)))
         (scheme-module-exports
          (intern (file-name-sans-extension (concat "srfi-" (cadr sexp)))))
       (scheme-module-exports
        (intern (apply 'concat (append (cddr sexp) (list (cadr sexp))))))))
    (t
     (scheme-module-exports sexp))))

(defun scheme-extract-sexp-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((begin define-module)
     (scheme-append-map #'scheme-extract-sexp-imports (cdr sexp)))
    ((cond-expand)
     (scheme-append-map #'scheme-extract-sexp-imports
                        (scheme-append-map #'cdr (cdr sexp))))
    ((use require-extension)
     (scheme-append-map #'scheme-module-exports (cdr sexp)))
    ((import)
     (scheme-append-map #'scheme-extract-import-module-imports (cdr sexp)))
    ((autoload)
     (unless (member (cadr sexp) *scheme-imported-modules*)
       (push (cadr sexp) *scheme-imported-modules*)
       (mapcar #'(lambda (x) (cons (if (consp x) (car x) x) '((lambda obj))))
               (cddr sexp))))
    ((load)
     (unless (member (cadr sexp) *scheme-imported-modules*)
       (push (cadr sexp) *scheme-imported-modules*)
       (and (stringp (cadr sexp))
            (not (string-match ".*\\.\\(o[0-9]*\\|so\\|dll\\|dylib\\)"
                               (cadr sexp)))
            (file-exists-p (cadr sexp))
            (scheme-with-find-file (cadr sexp)
              (scheme-current-globals)))))
    ((library module)
     (scheme-append-map #'scheme-extract-import-module-imports
                        (remove-if #'(lambda (x)
                                       (memq (car x) '(import require)))
                                   (cdr sexp))))
    ))

(defun scheme-module-symbol-p (sym)
  (memq sym '(use require require-extension begin cond-expand
              module library define-module autoload load import)))

(defun scheme-skip-shebang ()
  ;; skip shebang if present
  (if (looking-at "#!")
      ;; guile skips until a closing !#
      (if (eq 'guile (scheme-current-implementation))
          (re-search-forward "!#" nil t)
        (forward-line))))

(defun scheme-current-imports ()
  (let ((imports '())
        (*scheme-imported-modules* '())
        (in-mod-p nil))
    (save-excursion
      (goto-char (point-min))
      (scheme-skip-shebang)
      (if (re-search-forward "^(" nil t)
          (forward-char -1))
      ;; scan for module forms
      (while (not (eobp))
        (when (eq ?\( (char-after))
          (forward-char 1)
          (let ((sym (and (not (eq ?\( (char-after)))
                          (scheme-symbol-at-point))))
            (cond
             ((memq sym '(module library))
              (forward-sexp 3)
              (forward-sexp -1)
              (setq in-mod-p t))
             ((eq sym 'define-library)
              (forward-sexp 1)
              (setq in-mod-p t))
             ((scheme-module-symbol-p sym)
              (forward-char -1)
              (ignore-errors
                (let* ((decl (scheme-nth-sexp-at-point 0))
                       (res (scheme-extract-sexp-imports decl)))
                  (setq imports (append res imports)))))
             (t
              (forward-char -1)))))
        (scheme-goto-next-top-level in-mod-p)))
    imports))

(defun scheme-file-imports (file)
  (scheme-with-find-file file
    (scheme-current-imports)))

;; we should be just inside the opening paren of an expression
(defun scheme-name-of-define ()
  (save-excursion
    (scheme-beginning-of-next-sexp)
    (if (eq ?\( (char-syntax (char-after)))
        (forward-char))
    (and (memq (char-syntax (char-after)) '(?\w ?\_))
         (scheme-symbol-at-point))))

(defun scheme-type-of-define (&optional env)
  (save-excursion
    (scheme-beginning-of-next-sexp)
    (cond
     ((eq ?\( (char-syntax (char-after)))
      `(lambda ,(cdr (scheme-nth-sexp-at-point 0))))
     (t
      (ignore-errors (scheme-beginning-of-next-sexp)
                     (scheme-sexp-type-at-point env))))))

;; we should be at the opening paren of an expression
(defun scheme-extract-definitions (&optional env)
  (save-excursion
    (let ((sym (ignore-errors (and (eq ?\( (char-syntax (char-after)))
                                   (progn (forward-char)
                                          (scheme-symbol-at-point))))))
      (case sym
        ((define-syntax define-compiled-syntax defmacro define-macro)
         (list (list (scheme-name-of-define) '(syntax))))
        ((define define-inline define-constant define-primitive defun)
         (let ((name (scheme-name-of-define))
               (type (scheme-type-of-define env)))
           (list (if type (list name type) (list name)))))
        ((defvar define-class)
         (list (list (scheme-name-of-define) 'non-procedure)))
        ((define-record)
         (backward-char)
         (ignore-errors
           (let* ((sexp (scheme-nth-sexp-at-point 0))
                  (name (symbol-name (cadr sexp))))
             `((,(intern (concat name "?")) (lambda (obj) boolean))
               (,(intern (concat "make-" name)) (lambda ,(cddr sexp) ))
               ,@(scheme-append-map
                  #'(lambda (x)
                      `((,(intern (concat name "-" (symbol-name x)))
                         (lambda (non-procedure)))
                        (,(intern (concat name "-" (symbol-name x) "-set!"))
                         (lambda (non-procedure val) undefined))))
                  (cddr sexp))))))
        ((define-record-type)
         (backward-char)
         (ignore-errors
           (let ((sexp (scheme-nth-sexp-at-point 0)))
             `((,(caaddr sexp) (lambda ,(cdaddr sexp)))
               (,(cadddr sexp) (lambda (obj)))
               ,@(scheme-append-map 
                  #'(lambda (x)
                      (if (consp x)
                          (if (consp (cddr x))
                              `((,(cadr x) (lambda (non-procedure)))
                                (,(caddr x)
                                 (lambda (non-procedure val) undefined)))
                            `((,(cadr x) (lambda (non-procedure)))))))
                  (cddddr sexp))))))
        ((begin progn)
         (forward-sexp)
         (scheme-current-definitions))
        ((include include-ci)
         (scheme-append-map
          #'(lambda (file)
              (scheme-with-find-file file
                (scheme-current-globals)))
          (ignore-errors (cdr (scheme-nth-sexp-at-point 0)))))
        (t
         '())))))

(defun scheme-in-define-name ()
  (ignore-errors
    (save-excursion
     (dotimes (i 2)
       (scheme-beginning-of-sexp)
       (backward-char))
     (and (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*(define"))
          (point)))))

(defvar *scheme-include-globals-cache* (make-hash-table :test #'equal)
  "Cache for included unmodified files.")

(defun scheme-include-globals (file from-file env)
  (let* ((key (cons file from-file))
         (mtime (nth 5 (file-attributes file)))
         (cached (gethash key *scheme-include-globals-cache*)))
    (if (and cached
             (not (or (and mtime (scheme-mtime>? mtime (caadr cached)))
                      (let ((buf (get-buffer file)))
                        (and buf
                             (equal (buffer-file-name buf)
                                    (file-truename file))
                             (buffer-modified-p buf))))))
        (caddr cached)
      (let ((res (ignore-errors
                   (scheme-with-find-file file
                     (scheme-current-globals env)))))
        (puthash key (list mtime res) *scheme-include-globals-cache*)
        res))))

;; a little more liberal than -extract-definitions, we try to scan to
;; a new top-level form (i.e. a line beginning with an open paren) if
;; there's an error during normal sexp movement
(defun scheme-current-globals (&optional env)
  (let ((res '())
        (in-mod-p nil)
        (skip (scheme-in-define-name))
        (from-file (buffer-file-name (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (or (ignore-errors (end-of-defun) (beginning-of-defun) t)
          (re-search-forward "^(" nil t)
          (goto-char (point-max)))
      (while (not (eobp))
        (when (and (eq ?\( (char-syntax (char-after)))
                   (eq ?w (char-syntax (char-after (1+ (point))))))
          (let ((sym (save-excursion (forward-char) (scheme-symbol-at-point))))
            (case sym
              ((module define-module define-library)
               (setq in-mod-p t)
               (forward-char))
              ((include include-ci)
               (let* ((files (cdr (scheme-nth-sexp-at-point 0)))
                      (defs (scheme-append-map
                             #'(lambda (file)
                                 (scheme-include-globals file from-file env))
                             files)))
                 (setq res (append defs res))))
              ((begin)
               (save-excursion
                 (forward-char)
                 (setq res
                       (append (ignore-errors (scheme-extract-definitions env))
                               res))))
              (t
               (unless (eq (point) skip)
                 (setq res
                       (append (ignore-errors (scheme-extract-definitions env))
                               res)))))))
        (scheme-goto-next-top-level in-mod-p)))
    res))

;; for internal defines, etc.
(defun scheme-current-definitions (&optional enclosing-end)
  (let ((defs '())
        (end (or enclosing-end (point-max))))
    (save-excursion
      (while (< (point) end)
        (let ((here (point))
              (new-defs (scheme-extract-definitions)))
          (cond
           (new-defs
             (setq defs (append new-defs defs))
             (or (ignore-errors (scheme-beginning-of-next-sexp)
                                (> (point) here))
                 (goto-char end)))
           ;; non-definition form, maybe stop scanning
           ((not scheme-interleave-definitions-p)
            (goto-char end))))))
    defs))

(defun scheme-current-exports ()
  "Returns a list of all symbols exported in the current file"
  (let ((res '())
        (in-mod-p nil))
    (save-excursion
      (goto-char (point-min))
      (or (ignore-errors (end-of-defun) (beginning-of-defun) t)
          (re-search-forward "^(" nil t)
          (goto-char (point-max)))
      (while (not (eobp))
        (when (and (eq ?\( (char-syntax (char-after)))
                   (eq ?w (char-syntax (char-after (1+ (point))))))
          (let ((sym (save-excursion (forward-char) (scheme-symbol-at-point))))
            (case sym
              ((define-module define-library)
               (setq in-mod-p t)
               (forward-char))
              ((export provide)
               (unless (and (eq 'provide sym)
                            (eq 'chicken (scheme-current-implementation)))
                 (setq res (nconc (cdr (scheme-nth-sexp-at-point 0)) res))))
              ((export-all)
               (goto-char (point-max)))
              ((extend)
               (let ((parents (cdr (scheme-nth-sexp-at-point 0))))
                 (setq res (nconc (mapcar #'car
                                          (scheme-append-map
                                           #'scheme-module-exports/gauche
                                           parents))
                                  res))))
              ((module)
               (forward-char)
               (forward-sexp)
               (let ((x (scheme-nth-sexp-at-point 0)))
                 (cond
                  ((eq '* x)
                   (goto-char (point-max)))
                  ((listp x)
                   (setq res
                         (nconc (remove-if-not #'symbolp (cdr x)) res)))))))))
        (scheme-goto-next-top-level in-mod-p)))
    res))

(defun scheme-current-exports/typed ()
  "Returns an alist of (symbols . type) for all current exports"
  (let* ((exports (scheme-current-exports))
         (imports (and scheme-complete-recursive-inference-p
                       (ignore-errors (scheme-current-imports))))
         (globals (ignore-errors (scheme-current-globals (list imports))))
         (env (append imports globals))
         (typed-exports
          (if exports
              (remove-if-not #'(lambda (x) (memq (car x) exports)) env)
            env))
         (undefined-exports
          (remove-if #'(lambda (x) (assq x typed-exports)) exports)))
    ;;(message "exports: %s" exports)
    (append typed-exports
            (mapcar #'(lambda (x) (list x 'object)) undefined-exports))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is rather complicated because we want to auto-generate
;; docstring summaries from the type information, which means
;; inferring various types from common names.  The benefit is that you
;; don't have to input the same information twice, and can often
;; cut&paste&munge procedure descriptions from the original
;; documentation.

(defun scheme-translate-type (type)
  (if (not (symbolp type))
      type
    (case type
      ((pred proc thunk handler dispatch producer consumer f fn g kons)
       'procedure)
      ((num) 'number)
      ((z) 'complex)
      ((x1 x2 x3 y timeout seconds nanoseconds) 'real)
      ((i j k n m int index size count len length bound nchars start end
        pid uid gid fd fileno errno u8 byte)
       'integer)
      ((scur string-cursor)
       'string-cursor)
      ((start-indexer end-indexer)
       '(or integer string-cursor))
      ((ch) 'char)
      ((str name pattern) 'string)
      ((file path pathname) 'filename)
      ((dir dirname) 'directory)
      ((sym id identifier) 'symbol)
      ((ls lis lst list alist lists) 'list)
      ((vec vector) 'vector)
      ((bv bvec bytevector byte-vector) 'bytevector)
      ((ht hashtable hash-table) 'hash-table)
      ((exc excn exn err error) 'exception)
      ((ptr) 'pointer)
      ((bool) 'boolean)
      ((env) 'environment)
      ((char string boolean number complex real integer procedure char-set
        port input-port output-port pair vector array stream
        thread mutex condition-variable time exception date duration locative
        random-source state condition condition-type queue pointer
        u8vector s8vector u16vector s16vector u32vector s32vector
        u64vector s64vector f32vector f64vector undefined symbol
        block filename directory mmap listener environment non-procedure
        read-table continuation blob generic method class regexp regmatch
        sys-stat fdset)
       type)
      ((parent seed option mode) 'non-procedure)
      (t
       (let* ((str (symbol-name type))
              (i (string-match "-?[0-9]+$" str)))
         (if i
             (scheme-translate-type (intern (substring str 0 i)))
           (let ((i (string-match "-\\([^-]+\\)$" str)))
             (if i
                 (scheme-translate-type (intern (substring str (+ i 1))))
               (if (string-match "\\?$" str)
                   'boolean
                 'object)))))))))

(defun scheme-lookup-type (spec pos)
  "Resolve the type for `pos' the given param list `spec', handling :optional"
  (let ((i 1)
        (type nil))
    (while (and (consp spec) (<= i pos))
      (cond
       ((eq :optional (car spec))
        (decf i))
       ((= i pos)
        (setq type (car spec))
        (setq spec nil))
       ((and (consp (cdr spec)) (eq '\.\.\. (cadr spec)))
        (setq type (car spec))
        (setq spec nil)))
      (setq spec (cdr spec))
      (incf i))
    (if type
        (setq type (scheme-translate-type type)))
    type))

(defun scheme-predicate->type (pred)
  (case pred
    ((even? odd?) 'integer)
    ((char-upper-case? char-lower-case?
      char-alphabetic? char-numeric? char-whitespace?)
     'char)
    (t
     ;; catch all the `type?' predicates with pattern matching
     ;; ... we could be smarter if the env was passed
     (let ((str (symbol-name pred)))
       (if (string-match "\\?$" str)
           (scheme-translate-type
            (intern (substring str 0 (- (length str) 1))))
         'object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion

(eval-when (compile load eval)
  (unless (fboundp 'event-matches-key-specifier-p)
    (defalias 'event-matches-key-specifier-p 'eq)))

(unless (fboundp 'read-event)
  (defun read-event ()
    (aref (read-key-sequence nil) 0)))

(unless (fboundp 'event-basic-type)
  (defalias 'event-basic-type 'event-key))

(defun scheme-string-prefix-p (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))

(defun scheme-do-completion (str coll &optional strs pred)
  (let* ((completion1 (try-completion str coll pred))
         (completion2 (and strs (try-completion str strs pred)))
         (completion (if (and completion2
                              (or (not completion1)
                                  (< (length completion2)
                                     (length completion1))))
                         completion2
                       completion1)))
    (cond
     ((eq completion t))
     ((not completion)
      (message "Can't find completion for \"%s\"" str)
      (ding))
     ((not (string= str completion))
      (let ((prefix-p (scheme-string-prefix-p completion completion1)))
        ;; prefix-p is false when we need to auto-quote a string
        (unless prefix-p
          (save-excursion
            (backward-char (length str))
            (insert "\"")))
        (insert (substring completion (length str)))
        (unless prefix-p
          (insert "\"")
          (when (not (or (memq (char-syntax (char-after)) '(32 41))))
            (insert " ")
            (backward-char))
          (backward-char))))
     (t
      (let ((win-config (current-window-configuration))
            (done nil))
        (message "Hit space to flush")
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (sort
            (all-completions str (append strs coll) pred)
            'string-lessp)))
        (while (not done)
          (let* ((orig-event
                  (with-current-buffer (get-buffer "*Completions*")
                    (read-event)))
                 (event (event-basic-type orig-event)))
            (cond
             ((or (event-matches-key-specifier-p event 'tab)
                  (event-matches-key-specifier-p event 9))
              (save-selected-window
                (select-window (get-buffer-window "*Completions*"))
                (if (pos-visible-in-window-p (point-max))
                    (goto-char (point-min))
                  (scroll-up))))
             (t
              (set-window-configuration win-config)
              (if (or (event-matches-key-specifier-p event 'space)
                      (event-matches-key-specifier-p event 32))
                  (bury-buffer (get-buffer "*Completions*"))
                (setq unread-command-events (list orig-event)))
              (setq done t))))))))))

(defun scheme-env-lookup (env sym)
  (let ((spec nil)
        (ls env))
    (while (and ls (not spec))
      (setq spec (assq sym (pop ls))))
    spec))

(defun scheme-inside-top-level-p (pat)
  (save-excursion
    (beginning-of-defun)
    (looking-at pat)))

(defun scheme-inside-module-p ()
  (scheme-inside-top-level-p "^(\\(?:module\\|library\\)\\s-"))

(defun scheme-inside-define-library-p ()
  (scheme-inside-top-level-p "^(define-library\\s-"))

(defun scheme-inside-define*-p ()
  (scheme-inside-top-level-p
   "^(\\(?:define-library\\|define-syntax\\|define\\)\\s-"))

(defun scheme-inside-library-begin-p ()
  (let ((res nil))
    (save-excursion
      (while (and (not (bobp))
                  (ignore-errors
                    (scheme-beginning-of-sexp)
                    (backward-char)
                    t))
        (when (looking-at "(begin\\(\\s-\\|\n\\)")
          (setq res t)
          (goto-char (point-min)))))
    res))

(defun scheme-library-decl-includes (decl base)
  (case (and (consp decl) (car decl))
    ((include include-ci)
     (list '()
           (mapcar #'(lambda (f) (concat (file-name-directory base) f))
                   (cdr decl))))
    ((include-library-declarations)
     (let* ((file (concat (file-name-directory base) (cadr decl)))
            (res (scheme-library-includes file t)))
       `((,file ,@(car res)) ,(cadr res))))
    ((cond-expand)
     (let ((x
            (mapcar
             #'(lambda (clause) (scheme-library-decl-includes (cadr clause) base))
             (cdr decl))))
       (list (scheme-append-map #'car x)
             (scheme-append-map #'cadr x))))
    (t
     (list '() '()))))

(defvar *scheme-library-includes-cache* (make-hash-table :test #'equal))

(defun scheme-library-includes/uncached (base &optional flatp)
  (let ((decls '())
        (files '()))
    (when (file-exists-p base)
      (scheme-with-find-file base
        (let ((limit (point-max)))
          (when (not flatp)
            (re-search-forward "^(define-library\\s-" nil t)
            (forward-sexp 1))
          (while (< (point) limit)
            (let* ((decl (scheme-nth-sexp-at-point 0))
                   (includes (and decl (scheme-library-decl-includes decl base))))
              (when includes
                (setq decls (append decls (car includes)))
                (setq files (append files (cadr includes))))
              (scheme-goto-next-top-level (not flatp)))))))
    (list decls files)))

(defun scheme-library-includes (base &optional flatp)
  (let ((cached (and (not flatp)
                     (gethash base *scheme-library-includes-cache*))))
    (if (and cached
               (not
                (ignore-errors
                  (let ((mtime (nth 5 (file-attributes base)))
                        (ptime (car cached)))
                    (scheme-mtime>? mtime ptime)))))
        (cdr cached)
      (let ((res (scheme-library-includes/uncached base flatp)))
        (puthash base
                 (cons (nth 5 (file-attributes base)) res)
                 *scheme-library-includes-cache*)
        res))))

(defun scheme-library-include-type (base file)
  (let ((decls+files (scheme-library-includes base)))
    (cond ((member file (car decls+files)) 'r7rs-library-declaration)
          ((member file (cadr decls+files)) 'r7rs))))

;; Returns (<lang> <define-library-file>)
;; where <lang> is one of:
;;  - r7rs-library-declaration
;;  - r7rs
;;  - r5rs
;; and <define-library-file> is the file containing the
;; enclosing library declaration, or nil for programs.
(defun scheme-code-context (file)
  (cond
   ((scheme-inside-define-library-p)
    (list (if (scheme-inside-library-begin-p)
              'r7rs
            'r7rs-library-declaration)
          file))
   ((not *scheme-use-r7rs*)
    (if (scheme-inside-module-p)
        (list 'r5rs (buffer-file-name (current-buffer)))
      (list 'r5rs nil)))
   (t
    (or *scheme-current-context*
        (let ((res (scheme-code-context/uncached file)))
          ;; cache if we don't expect the context to change
          (when (and (car res) (cadr res))
            (setq *scheme-current-context* res))
          res)))))

(defun scheme-code-context/uncached (file)
  ;; check for including library files, starting with the same named .sld
  (let* ((sld (concat (file-name-sans-extension file)
                      *scheme-r7rs-extension*))
         (include-type (scheme-library-include-type sld file)))
    (unless (or include-type (equalp file sld))
      (let ((dir (file-name-directory file))
            (count 0)
            (sld-pat
             (concat (regexp-quote *scheme-r7rs-extension*) "\\'")))
        (while (and (< count *scheme-max-decl-file-search-depth*)
                    (not (member dir '(nil "" "/" "./"))))
          (let ((sld-ls (directory-files dir t sld-pat)))
            (while (consp sld-ls)
              (setq sld (pop sld-ls))
              (setq include-type (scheme-library-include-type sld file))
              (when include-type
                (setq sld-ls '()
                      dir "/"))))
          (incf count)
          (setq dir (file-name-directory
                     (replace-regexp-in-string "/+\\'" "" dir))))))
    (if include-type
        (list include-type sld)
      (list 'r7rs nil))))

(defun scheme-current-env ()
  (let* ((file (buffer-file-name (current-buffer)))
         (lang+base (scheme-code-context file))
         (lang (car lang+base))
         (base (cadr lang+base)))
    ;; base language
    (case lang
      ((r7rs-library-declaration)
       (let ((enclosing (scheme-enclosing-2-sexp-prefixes)))
         (cond
          ((and (eq 'import (caddr enclosing))
                (> (cadr enclosing) 1)
                (memq (car enclosing) '(only except)))
           (let* ((lib (save-excursion
                         (when (eq ?w (char-syntax (char-before (point))))
                           (scheme-beginning-of-sexp))
                         (dotimes (i (- (cadr enclosing) 1))
                           (backward-sexp))
                         (scheme-nth-sexp-at-point 0)))
                  (exports (and lib (scheme-module-exports/compute lib))))
             (list exports)))
          ((memq (caddr enclosing) '(import only except))
           (list (mapcar #'list
                         (append
                          (and (= 0 (cadr enclosing))
                               '(only except rename prefix drop-prefix))
                          (mapcar #'intern
                                  (scheme-library-completions
                                   (scheme-preceding-sexps)))))))
          ((eq 'import (car enclosing)) nil)
          ((eq 'export (car enclosing))
           (let* ((imports (ignore-errors (scheme-current-imports)))
                  (globals (ignore-errors
                             (scheme-current-globals (list imports))))
                  (exports (scheme-current-exports)))
             (list (mapcar #'list (remove-if #'(lambda (x) (memq (car x) exports))
                                             (append imports globals))))))
          (t (list *scheme-r7rs-lib-decl-info*)))))
      ((r5rs)
       (let* ((env (if base
                       '(((import
                           (special list scheme-available-modules))))
                     (list (scheme-r5rs-info))))
              (base (cdr (assq (scheme-current-implementation)
                               *scheme-implementation-exports*))))
         (if (and base (not in-mod-p)) (push base env))
         env))
      (t ; r7rs
       (let* ((env '())
              (can-import-p (not (scheme-inside-define*-p)))
              (in-import-p
               (and can-import-p
                    (eq 'import (caddr (scheme-enclosing-2-sexp-prefixes))))))
         (cond
          (in-import-p
           (push (mapcar #'list
                         (mapcar #'intern
                                 (scheme-library-completions
                                  (scheme-preceding-sexps))))
                 env))
          (t
           ;; imports
           (let ((imports (ignore-errors (scheme-file-imports (or base file)))))
             (if imports (push imports env)))
           ;; top-level defs
           (let ((top (ignore-errors (scheme-current-globals))))
             (if top (push top env)))
           ;; current local vars
           (let ((locals (ignore-errors (scheme-current-local-vars env))))
             (if locals (push locals env)))
           ;; define-library/import
           (unless env
             (push '((define-library (syntax clause))) env))
           (when can-import-p
             (push '((import (special list scheme-available-modules))) env))))
         env)))))

(defun scheme-env-filter (pred env)
  (mapcar #'car
          (apply #'concatenate
                 'list
                 (mapcar #'(lambda (e) (remove-if-not pred e)) env))))

;; checking return values:
;;   a should be capable of returning instances of b
(defun scheme-type-match-p (a b)
  (let ((a1 (scheme-translate-type a))
        (b1 (scheme-translate-type b)))
    (and (not (eq a1 'undefined))   ; check a *does* return something
         (or (eq a1 b1)             ; and they're the same
             (eq a1 'object)        ; ... or a can return anything
             (eq b1 'object)        ; ... or b can receive anything
             (if (symbolp a1)
                 (if (symbolp b1)
                     (case a1           ; ... or the types overlap
                       ((number complex real rational integer)
                        (memq b1 '(number complex real rational integer)))
                       ((port input-port output-port)
                        (memq b1 '(port input-port output-port)))
                       ((pair list)
                        (memq b1 '(pair list)))
                       ((non-procedure)
                        (not (eq 'procedure b1))))
                   (and
                    (consp b1)
                    (if (eq 'or (car b1))
                        ;; type unions
                        (find-if
                         #'(lambda (x)
                             (scheme-type-match-p
                              a1 (scheme-translate-type x)))
                         (cdr b1))
                      (let ((b2 (scheme-translate-special-type b1)))
                        (and (not (equal b1 b2))
                             (scheme-type-match-p a1 b2))))))
               (and (consp a1)
                    (case (car a1)
                      ((or)
                       ;; type unions
                       (find-if
                        #'(lambda (x)
                            (scheme-type-match-p (scheme-translate-type x) b1))
                        (cdr a1)))
                      ((lambda)
                       ;; procedures
                       (or (eq 'procedure b1)
                           (and (consp b1)
                                (eq 'lambda (car b1))
                                (scheme-param-list-match-p (cadr a1)
                                                           (cadr b1)))))
                      (t
                       ;; other special types
                       (let ((a2 (scheme-translate-special-type a1))
                             (b2 (scheme-translate-special-type b1)))
                         (and (or (not (equal a1 a2)) (not (equal b1 b2)))
                              (scheme-type-match-p a2 b2)))))))))))

(defun scheme-param-list-match-p (p1 p2)
  (or (and (symbolp p1) (not (null p1)))
      (and (symbolp p2) (not (null p2)))
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (scheme-param-list-match-p (cdr p1) (cdr p2)))))

(defun scheme-translate-special-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((list string) (car x))
      ((set special) (cadr x))
      ((flags) 'integer)
      (t x))))

(defun scheme-nth* (n ls)
  (while (and (consp ls) (> n 0))
    (setq n (- n 1)
          ls (cdr ls)))
  (and (consp ls) (car ls)))

(defun scheme-file->lines (file)
  (and (file-readable-p file)
       (scheme-with-find-file file
         (goto-char (point-min))
         (let ((res '()))
           (while (not (eobp))
             (let ((start (point)))
               (forward-line)
               (push (buffer-substring-no-properties start (- (point) 1))
                     res)))
           (reverse res)))))

(defun scheme-passwd-file-names (file &optional pat)
  (delete
   nil
   (mapcar
    #'(lambda (line)
        (and (not (string-match "^[  ]*#" line))
             (or (not pat) (string-match pat line))
             (string-match "^\\([^:]*\\):" line)
             (match-string 1 line)))
    (scheme-file->lines file))))

(defun scheme-host-file-names (file)
  (scheme-append-map
   #'(lambda (line)
       (let ((i (string-match "#" line)))
         (if i (setq line (substring line 0 i))))
       (cdr (split-string line)))
   (scheme-file->lines file)))

(defun scheme-ssh-known-hosts-file-names (file)
  (scheme-append-map
   #'(lambda (line)
       (split-string (car (split-string line)) ","))
   (scheme-file->lines file)))

(defun scheme-ssh-config-file-names (file)
  (scheme-append-map
   #'(lambda (line)
       (and (string-match "^ *Host" line)
            (cdr (split-string line))))
   (scheme-file->lines file)))

(defun scheme-complete-user-name (trans sym)
  (if (string-match "apple" (emacs-version))
      (append (scheme-passwd-file-names "/etc/passwd" "^[^_].*")
              (delete "Shared" (directory-files "/Users" nil "^[^.].*")))
    (scheme-passwd-file-names "/etc/passwd")))

(defun scheme-complete-host-name (trans sym)
  (append (scheme-host-file-names "/etc/hosts")
          (scheme-ssh-known-hosts-file-names "~/.ssh/known_hosts")
          (scheme-ssh-config-file-names "~/.ssh/config")))

;; my /etc/services is 14k lines, so we try to optimize this
(defun scheme-complete-port-name (trans sym)
  (and (file-readable-p "/etc/services")
       (scheme-with-find-file "/etc/services"
         (goto-char (point-min))
         (let ((rx (concat "^\\(" (regexp-quote (if (symbolp sym)
                                                    (symbol-name sym)
                                                  sym))
                           "[^  ]*\\)"))
               (res '()))
           (while (not (eobp))
             (if (not (re-search-forward rx nil t))
                 (goto-char (point-max))
               (let ((str (match-string-no-properties 1)))
                 (if (not (equal str (car res)))
                     (push str res)))
               (forward-char 1)))
           res))))

(defun scheme-complete-file-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir "."))))
    (if dir
        (mapcar #'(lambda (f) (concat dir f)) res)
      res)))

(defun scheme-complete-directory-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir ".")))
         (res2 (if dir (mapcar #'(lambda (f) (concat dir f)) res) res)))
    (remove-if-not #'file-directory-p res2)))

(defun scheme-string-completer (type)
  (case type
    ((filename)
     '(scheme-complete-file-name file-name-nondirectory))
    ((directory)
     '(scheme-complete-directory-name file-name-nondirectory))
    (t
     (cond
      ((and (consp type) (eq 'string (car type)))
       (cadr type))
      ((and (consp type) (eq 'or (car type)))
       (car (delete nil (mapcar #'scheme-string-completer (cdr type)))))))))

(defun scheme-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

(defun scheme-complete-variable-name (trans sym)
  (dabbrev-expand nil))

(defun scheme-smart-complete (&optional arg)
  (interactive "P")
  (if scheme-indent-before-complete-p
      (lisp-indent-line))
  (cond
   ((and scheme-complete-from-end-of-symbol-p
         (not (eobp))
         (eq ?w (char-syntax (char-after)))
         (not (bobp))
         (eq ?w (char-syntax (char-before))))
    (let ((orig-start (point)))
      (forward-sexp 1)
      (let ((ls (scheme-get-completions arg)))
        (cond
         ((not (or (try-completion (car ls) (cadr ls) (cadddr ls))
                   (and (caddr ls)
                        (try-completion (car ls) (caddr ls) (cadddr ls)))))
          (goto-char orig-start)
          (insert " ")
          (forward-char -1)
          (apply #'scheme-do-completion (scheme-get-completions arg)))
         (t
          (message "found %s completions" (length (cadr ls)))
          (apply #'scheme-do-completion ls))))))
   (t
    (apply #'scheme-do-completion (scheme-get-completions arg)))))

;; normalize the base completions to a list of string lists
(defun scheme-get-completions (&optional arg)
  (let ((res (scheme-get-base-completions arg)))
    (cons (car res)
          (cons (mapcar #'(lambda (x)
                            (cond
                             ((symbolp x) (list (symbol-name x)))
                             ((stringp x) (list x))
                             (t x)))
                        (cadr res))
                (cddr res)))))

(defun scheme-get-base-completions (&optional arg)
  (let* ((end (point))
         (start (save-excursion (skip-syntax-backward "w_") (point)))
         (sym (buffer-substring-no-properties start end))
         (in-str-p (scheme-in-string-p))
         (x (save-excursion
              (if in-str-p (scheme-beginning-of-string))
              (scheme-enclosing-2-sexp-prefixes)))
         (inner-proc (car x))
         (inner-pos (cadr x))
         (outer-proc (caddr x))
         (outer-pos (cadddr x))
         (env (save-excursion
                (if in-str-p (scheme-beginning-of-string))
                (scheme-current-env)))
         (outer-spec (scheme-env-lookup env outer-proc))
         (outer-type (scheme-translate-type (cadr outer-spec)))
         (inner-spec (scheme-env-lookup env inner-proc))
         (inner-type (scheme-translate-type (cadr inner-spec))))
    (cond
     ;; return all env symbols when a prefix arg is given
     (arg
      (list sym (scheme-env-filter #'(lambda (x) t) env)))
     ;; allow different types of strings
     (in-str-p
      (let* ((param-type
              (and (consp inner-type)
                   (eq 'lambda (car inner-type))
                   (scheme-lookup-type (cadr inner-type) inner-pos)))
             (completer (or (scheme-string-completer param-type)
                            '(scheme-complete-file-name
                              file-name-nondirectory))))
        (list sym
              (scheme-apply-string-completer completer sym))))
     ;; outer special
     ((and (consp outer-type)
           (eq 'special (car outer-type))
           (cadddr outer-type))
      (list sym (funcall (cadddr outer-type) sym)))
     ;; inner special
     ((and (consp inner-type)
           (eq 'special (car inner-type))
           (caddr inner-type))
      (list sym (funcall (caddr inner-type) sym)))
     ;; completing inner procedure, complete procedures with a
     ;; matching return type
     ((and (consp outer-type)
           (eq 'lambda (car outer-type))
           (not (zerop outer-pos))
           (scheme-nth* (- outer-pos 1) (cadr outer-type))
           (or (zerop inner-pos)
               (and (>= 1 inner-pos)
                    (consp inner-type)
                    (eq 'lambda (car inner-type))
                    (let ((param-type
                           (scheme-lookup-type (cadr inner-type) inner-pos)))
                      (and (consp param-type)
                           (eq 'lambda (car param-type))
                           (eq (caddr inner-type) (caddr param-type)))))))
      (let ((want-type (scheme-lookup-type (cadr outer-type) outer-pos)))
        (list
         sym
         (scheme-env-filter
          #'(lambda (x)
              (let ((type (cadr x)))
                (or (memq type '(procedure object nil))
                    (and (consp type)
                         (or (and (eq 'syntax (car type))
                                  (not (eq 'undefined (caddr type))))
                             (and (eq 'lambda (car type))
                                  (scheme-type-match-p (caddr type)
                                                       want-type)))))))
          env))))
     ;; completing a normal parameter
     ((and inner-proc
           (not (zerop inner-pos))
           (consp inner-type)
           (eq 'lambda (car inner-type)))
      (let* ((param-type (scheme-lookup-type (cadr inner-type) inner-pos))
             (set-or-flags
              (or (and (consp param-type)
                       (case (car param-type)
                         ((set) (cddr param-type))
                         ((flags) (cdr param-type))))
                  ;; handle nested arithmetic functions inside a flags
                  ;; parameter
                  (and (not (zerop outer-pos))
                       (consp outer-type)
                       (eq 'lambda (car outer-type))
                       (let ((outer-param-type
                              (scheme-lookup-type (cadr outer-type)
                                                  outer-pos)))
                         (and (consp outer-param-type)
                              (eq 'flags (car outer-param-type))
                              (memq (scheme-translate-type param-type)
                                    '(number complex real rational integer))
                              (memq (scheme-translate-type (caddr inner-type))
                                    '(number complex real rational integer))
                              (cdr outer-param-type))))))
             (base-type (if set-or-flags
                            (if (and (consp param-type)
                                     (eq 'set (car param-type)))
                                (scheme-translate-type (cadr param-type))
                              'integer)
                          param-type))
             (base-completions
              (scheme-env-filter
               #'(lambda (x)
                   (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                        (scheme-type-match-p (cadr x) base-type)))
               env))
             (str-completions
              (let ((completer (scheme-string-completer base-type)))
                (and
                 completer
                 (scheme-apply-string-completer completer sym)))))
        (list
         sym
         (append set-or-flags base-completions)
         str-completions)))
     ;; completing a function
     ((zerop inner-pos)
      (list
       sym
       (scheme-env-filter
        #'(lambda (x)
            (or (null (cdr x))
                (memq (cadr x) '(procedure object nil))
                (and (consp (cadr x))
                     (memq (caadr x) '(lambda syntax special)))))
        env)))
     ;; complete everything
     (t
      (list sym (scheme-env-filter #'(lambda (x) t) env))))))

(defun scheme-complete-or-indent (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (func
          (if (or (and (not (bobp))
                       (eq ?w (char-syntax (char-before))))
                  (and (not (and (looking-at "\\s-*$")
                                 (looking-back ")")))
                       (save-excursion
                            (beginning-of-line)
                            (re-search-forward "\\S-" end t))
                       (case scheme-complete-empty-tab-behavior
                         ((indent) nil)
                         ((beep) (beep))
                         (t t))))
              'scheme-smart-complete
            'lisp-indent-line)))
    (funcall func arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional indentation handling

(defvar calculate-lisp-indent-last-sexp)

;; Copied from scheme-indent-function, but ignore
;; scheme-indent-function property for local variables.
(defun scheme-smart-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring-no-properties
                        (point)
                        (progn (forward-sexp 1) (point))))
             (function-sym (intern-soft function))
             (method (and (not (assq function-sym (scheme-current-local-vars)))
                          (get function-sym 'scheme-indent-function))))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional eldoc function

(defun scheme-translate-dot-to-optional (ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (cons (car ls) res))
      (setq ls (cdr ls)))
    (if (not (null ls))
        (setq res (cons ls (cons :optional res))))
    (reverse res)))

(defun scheme-optional-in-brackets (ls)
  ;; put optional arguments inside brackets (via a vector)
  (if (memq :optional ls)
      (let ((res '()))
        (while (and (consp ls) (not (eq :optional (car ls))))
          (push (pop ls) res))
        (reverse (cons (apply #'vector (cdr ls)) res)))
    ls))

(defun scheme-base-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((string list) (car x))
      ((set) (or (cadr x) (car x)))
      ((flags) 'integer)
      ((lambda) 'procedure)
      ((syntax) 'syntax)
      (t x))))

(defun scheme-sexp-to-string (sexp)
  (with-output-to-string (princ sexp)))

(defun scheme-get-current-symbol-info ()
  (let* ((sym (eldoc-current-symbol))
         (fnsym0 (eldoc-fnsym-in-current-sexp))
         (fnsym (if (consp fnsym0) (car fnsym0) fnsym0))
         (env (save-excursion
                (if (scheme-in-string-p) (scheme-beginning-of-string))
                (scheme-current-env)))
         (spec (or (and sym (scheme-env-lookup env sym))
                   (and fnsym (scheme-env-lookup env fnsym)))))
    (and (consp spec)
         (consp (cdr spec))
         (let ((type (cadr spec)))
           (concat
            (cond
             ((nth 3 spec)
              "")
             ((and (consp type)
                   (memq (car type) '(syntax lambda)))
              (concat
               (if (eq (car type) 'syntax)
                   "syntax: "
                 "")
               (scheme-sexp-to-string
                (cons (car spec)
                      (scheme-optional-in-brackets
                       (mapcar #'scheme-base-type
                               (scheme-translate-dot-to-optional
                                (cadr type))))))
               (if (and (consp (cddr type))
                        (not (memq (caddr type) '(obj object))))
                   (concat " => " (scheme-sexp-to-string (caddr type)))
                 "")))
             ((and (consp type) (eq (car type) 'special))
              (scheme-sexp-to-string (car spec)))
             (t
              (scheme-sexp-to-string type)))
            (if (and (not (nth 3 spec)) (nth 4 spec)) " - " "")
            (or (nth 4 spec) ""))))))

(provide 'scheme-complete)

;;; scheme-complete.el ends here
;;; Local Variables:
;;; eval: (put 'scheme-with-find-file 'lisp-indent-hook 1)
;;; End:
