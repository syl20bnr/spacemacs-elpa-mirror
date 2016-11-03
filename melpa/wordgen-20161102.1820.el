;;; wordgen.el --- Random word generator -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/wordgen.el
;; Package-Version: 20161102.1820
;; Package-X-Original-Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2015-2016, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Commentary:

;; Generate random words using user-provided rules.
;;
;; Example:
;; (wordgen
;;  '((result (concat-reeval [(2 1) (5 2) (4 3)] syl))
;;    (syl (++ c v coda))
;;    (c [(4 "p") (5 "t") (5 "k") (3 "m") (4 "n") (3 "s") (4 "l") (3 "r")])
;;    (v ["a" "e" "i" "o" "u"])
;;    (coda [(4 "") "m" "n"]))
;;  :word-count 5)
;;
;; => ("komlamkim" "kepa" "mennem" "ne" "palu")
;;
;; See the function `wordgen' for complete description.

;;; Code:
(eval-when-compile (require 'pcase))
(eval-when-compile (require 'cl-lib))


;;; Public interface

;;;###autoload
(cl-defun wordgen (ruleset &key (word-count 1) (starting-rule nil) (seed nil))
  "Generate random words using rules described in RULESET.

The result is a list of WORD-COUNT random words.
Each word is the result of evaluation of STARTING-RULE.
The default STARTING-RULE is `result'.

SEED is a sequence of integers (a string, a list of integers, etc.) used to
initialize the pseudo-random number generator. When nil, a default seed is
obtained from /dev/urandom or, if that fails, from the current time.

RULESET is a list of lists of the form (RULE-NAME EXPRESSION), where RULE-NAME
is a symbol.
Rules always evaluate to a string.

EXPRESSION can be one of the following:
 * A symbol RULE-NAME, which evaluates the rule named RULE-NAME and returns its
   result.

 * An integer, which evaluates to itself.

 * A string, which evaluates to itself.

 * A vector [CHOICE-ELEMENT ...], which randomly chooses one of its elements and
   evaluates it.

   CHOICE-ELEMENT can be either a list (WEIGHT EXPRESSION), or just an
   EXPRESSION. In the latter case, the WEIGHT is assumed to be 1.
   WEIGHT is an integer used to control the probability of picking an element:
   P(element) = P(weight) / P(sum of all weights).

 * A list (++ EXPR-1 EXPR-2 ... EXPR-N), which evaluates all its arguments from
   left to right and concatenates the results.

 * A list (replicate TIMES EXPR), which evaluates expressions TIMES and EXPR,
   then returns a string of EXPR repeated TIMES times. For example:
   \(replicate 3 \"foo\") evaluates to \"foofoofoo\".

 * A list (concat-reeval TIMES EXPR), which evaluates the expression
   TIMES, then evaluates EXPR TIMES times, concatenating the results. For
   example:
   \(concat-reeval 2 [\"a\" \"b\"]) may evaluate to \"aa\", \"ab\", \"ba\"
   or \"bb\".

 * A list (lisp FUNC), where FUNC is an expression evaluating to a Lisp
   function. The function is called with two arguments (RULES RNG), where RULES
   is the compiled RULESET as returned by `wordgen-compile-ruleset' and RNG is
   an instance of the pseudo-random number generator that can be passed to
   `wordgen-prng-next-int'.
   It's likely a bad idea to pass a `lambda' as FUNC, because FUNC is copied
   straight into generated Emacs Lisp code, so the `lambda' will be compiled
   during RULESET compilation using `wordgen's internal settings. In particular,
   this means `lexical-binding' set to t.
   To be safe, store the `lambda' in a variable first and pass the variable name
   here.
   To return a string back to `wordgen' code, use `wordgen-print-string' instead
   of returning the string directly.
   To get a string from `wordgen-call-rule-by-name', use
   `wordgen-with-output-to-string'.

Any other EXPRESSION is unrecognized and results in an error being signaled at
RULESET compilation time.

Type errors, like (replicate \"foo\" 3) or (++ \"foo\" 10), also result in an
error being signaled during RULESET compilation."
  (let ((compiled-ruleset (wordgen-compile-ruleset ruleset))
        (rng (wordgen--prng-create-from-bytes (or seed (wordgen--get-default-seed))))
        (result '()))
    (dotimes (_ word-count)
      (push (wordgen-evaluate-ruleset compiled-ruleset rng starting-rule) result))
    result))

(defun wordgen-compile-ruleset (ruleset)
  "Compile RULESET to executable form.
RULESET should be a rule set of the same form as in `wordgen', which see."
  (let ((rules (make-hash-table :test #'eq)))
    (dolist (rule ruleset)
      (pcase rule
        (`(,rule-name ,rule-expr)
         (when (gethash rule-name rules)
           (error "Redefinition of rule %S" rule-name))
         (puthash rule-name
                  (wordgen--compile-elisp-to-lambda
                   (wordgen--expr-compile
                    (wordgen--typecheck-rule-body
                     (wordgen--parse-expression rule-expr))))
                  rules))
        (_
         (error "Invalid rule %S" rule))))
    rules))

(defvar wordgen--output-strings '())

(defmacro wordgen-with-output-to-string (&rest body)
  "Execute BODY, returning the strings passed to `wordgen-print-string'.
The strings are concantenated in the order of the `wordgen-print-string' calls."
  (declare (debug t) (indent defun))
  `(let ((wordgen--output-strings '()))
     (progn ,@body)
     (apply #'concat (nreverse wordgen--output-strings))))

;; NB: `cl-defsubst', not `defsubst', as the former generates slightly better
;; bytecode. This comes at the cost of the arguments being immutable, but
;; `wordgen-print-string' doesn't need to change its argument anyway.
(cl-defsubst wordgen-print-string (string)
  "Print a STRING.
The string is printed to the innermost enclosing
`wordgen-with-output-to-string'."
  (push string wordgen--output-strings)
  "")

(defun wordgen-evaluate-ruleset (ruleset rng &optional starting-rule)
  "Evaluate the RULESET using pseudo-random generator RNG.
Evaluation starts from STARTING-RULE; if that's nil, it starts from `result'."
  (wordgen-with-output-to-string
    (wordgen-call-rule-by-name ruleset rng (or starting-rule 'result))))

(defun wordgen-call-rule-by-name (ruleset rng rule-name)
  "Call a rule using its name.
RULESET is the rule set we're using.
RNG is the pseudo-random number generator.
RULE-NAME is the name of the rule to call."
  (let ((rule-desc (gethash rule-name ruleset)))
    (unless rule-desc
      (error "Rule named %S not found" rule-name))
    (funcall rule-desc ruleset rng)))


;;; Intermediate representation

;; There's no `:named', because we're doing our own type tagging.
(cl-defstruct
    (wordgen--expr
     (:constructor nil)
     (:copier nil)
     (:predicate nil)
     (:type vector))
  "Base struct for intermediate representation expression objects."
  (subclass-type :read-only t)
  (type)
  (original-form :read-only t))

(eval-and-compile
  (defvar wordgen--known-expr-types '()))

(defmacro wordgen--define-derived-expr-type (name-type ctor &rest slots)
  "Define a `cl-defstruct' for a derived IR expression type.

NAME-TYPE is a list (NAME TYPE), where NAME is a symbol used to generate the
type name by prepending \"wordgen--expr-\"; TYPE is the value to initialize
`wordgen--expr''s type slot with.
CTOR is passed directly as `:constructor' to `cl-defstruct'.
SLOTS are passed directly to `cl-defstruct'."
  (declare (indent 2) (doc-string 3))
  (let* ((name (nth 0 name-type))
         (type (nth 1 name-type))
         (struct-name (intern (concat "wordgen--expr-" (symbol-name name)))))
    `(progn
       (eval-and-compile
         (cl-pushnew ',name wordgen--known-expr-types :test #'eq))
       (cl-defstruct
           (,struct-name
            (:include wordgen--expr
                      (subclass-type ',name)
                      (type ,type))
            (:copier nil)
            (:constructor nil)
            (:constructor ,@ctor)
            (:type vector))
         ,@slots))))

(defmacro wordgen--expr-case (expr type-var &rest clauses)
  "Eval EXPR and choose a clause according to its type.

With TYPE-VAR non-nil, the symbol TYPE-VAR is bound to the type of EXPR.

Each one of CLAUSES looks like (TYPE BODY...), where TYPE is either a symbol
representing a wordgen IR type (see `wordgen--define-derived-expr-type') or a
list of such symbols.

If the type EXPR is `eq' to TYPE (when TYPE is a symbol) or is an element of it
according to `memq' (when TYPE is a list), the corresponding BODY is evaluated.

If the CLAUSES are not exhaustive, an error is signaled at macro expansion
time."
  (declare (indent 2) (debug (form sexp &rest (sexp body))))
  (let ((types-handled '())
        (type-sym (or type-var (make-symbol "type")))
        (cond-clauses '()))
    (while clauses
      (pcase-let* ((`(,clause . ,next) clauses)
                   (last-clause (null next)))
        (unless (consp clause)
          (error "%S is not a valid `wordgen--expr-case' clause" clause))
        (pcase-let ((`(,type-list . ,body) clause))
          (unless (listp type-list)
            (cl-callf list type-list))
          (dolist (type type-list)
            (unless (memq type wordgen--known-expr-types)
              (error "Unknown expression type `%S'" type))
            (when (memq type types-handled)
              (error "Duplicate case for type `%S'" type))
            (push type types-handled))
          (push
           `(,(cond
               ;; Since the cases are guaranteed to be exhaustive (this macro
               ;; signals otherwise), we can omit the condition for the last
               ;; clause.
               (last-clause t)
               ((null (cdr type-list)) `(eq ,type-sym ',(car type-list)))
               (t `(memq ,type-sym ',type-list)))
             ,@body)
           cond-clauses))
        (setq clauses next)))
    (dolist (type wordgen--known-expr-types)
      (unless (memq type types-handled)
        (error "Expression type `%S' not handled" type)))
    `(let ((,type-sym (wordgen--expr-subclass-type ,expr)))
       (cond
        ,@(nreverse cond-clauses)))))

(wordgen--define-derived-expr-type (string 'string)
    (wordgen--expr-string-make (value original-form))
  (value :read-only t))

(wordgen--define-derived-expr-type (integer 'integer)
    (wordgen--expr-integer-make (value original-form))
  (value :read-only t))

(wordgen--define-derived-expr-type (choice nil)
    (wordgen--expr-choice-make (children-count total-weight children original-form))
  "CHILDREN is a list of lists (EXPR WEIGHT RUNNING-WEIGHT).
CHILDREN is sorted according to RUNNING-WEIGHT, ascending."
  ;; The children count is trivial to get from the original vector, so store it
  ;; to avoid calling `length' on lists later.
  (children-count :read-only t)
  (total-weight :read-only t)
  (children :read-only t))

(wordgen--define-derived-expr-type (rule-call 'string)
    (wordgen--expr-rule-call-make (rule-name original-form))
  (rule-name :read-only t))

(wordgen--define-derived-expr-type (concat 'string)
    (wordgen--expr-concat-make (children original-form))
  (children :read-only t))

(wordgen--define-derived-expr-type (replicate 'string)
    (wordgen--expr-replicate-make (reps subexpr original-form))
  (reps :read-only t)
  (subexpr :read-only t))

(wordgen--define-derived-expr-type (concat-reeval 'string)
    (wordgen--expr-concat-reeval-make (reps subexpr original-form))
  (reps :read-only t)
  (subexpr :read-only t))

(wordgen--define-derived-expr-type (lisp-call nil)
    (wordgen--expr-lisp-call-make (func original-form))
  (func :read-only t))

(defun wordgen--parse-expression (expression)
  "Compile wordgen EXPRESSION to intermediate representation."
  (pcase expression
    ((pred stringp) (wordgen--parse-string expression))
    ((pred integerp) (wordgen--parse-integer expression))
    ((pred vectorp) (wordgen--parse-choice-expr expression))
    ((pred symbolp) (wordgen--parse-rule-call expression))
    (`(++ . ,_) (wordgen--parse-concat expression))
    (`(replicate . ,_) (wordgen--parse-replicate expression))
    (`(concat-reeval . ,_) (wordgen--parse-concat-reeval expression))
    (`(lisp . ,_) (wordgen--parse-lisp-call expression))
    (_ (error "Invalid expression %S" expression))))

(defun wordgen--parse-string (string)
  "Compile a STRING literal to intermediate representation."
  (wordgen--expr-string-make string string))

(defun wordgen--parse-integer (integer)
  "Compile an INTEGER literal to intermediate representation."
  (wordgen--expr-integer-make integer integer))

(defun wordgen--parse-choice-expr (vec)
  "Compile a choice expression VEC to intermediate representation."
  (when (= 0 (length vec))
    (error "Empty choice expression"))
  (let ((children-count (length vec))
        (children '())
        (running-total-weight 0))
    (dotimes (i children-count)
      (let ((child (aref vec i)))
        (push (pcase child
                ((and `(,weight ,child-expr)
                      (guard (integerp weight)))
                 (when (<= weight 0)
                   (error "Weight %d is not positive" weight))
                 `(,child-expr ,weight ,(cl-incf running-total-weight weight)))
                (_
                 `(,child 1 ,(cl-incf running-total-weight))))
              children)))
    (let ((weight-gcd (wordgen--gcd-choice-weights children)))
      (when (/= weight-gcd 1)
        (dolist (child children)
          (cl-callf / (nth 1 child) weight-gcd)
          (cl-callf / (nth 2 child) weight-gcd))
        (cl-callf / running-total-weight weight-gcd)))
    ;; Actually compile the children now.
    (dolist (child children)
      (cl-callf wordgen--parse-expression (car child)))
    (wordgen--expr-choice-make
     children-count running-total-weight (nreverse children) vec)))

(defun wordgen--gcd-choice-weights (children)
  "Find the greatest common divisor of the weights of CHILDREN."
  (catch 'return
    (let ((gcd (nth 1 (car children))))
      (dolist (child (cdr children))
        (let ((weight (nth 1 child)))
          (while (/= weight 0)
            (cl-psetq gcd weight
                      weight (% gcd weight))))
        (when (= gcd 1)
          (throw 'return 1)))
      gcd)))

(defun wordgen--parse-rule-call (rule)
  "Compile a RULE call to intermediate representation."
  (wordgen--expr-rule-call-make rule rule))

(defun wordgen--parse-concat (expression)
  "Compile a concat expression to intermediate representation.
EXPRESSION is the whole (++ ...) list."
  (wordgen--expr-concat-make (mapcar #'wordgen--parse-expression (cdr expression))
                             expression))

(defun wordgen--parse-replicate (expression)
  "Compile a replicate expression to intermediate representation.
EXPRESSION is the whole (replicate ...) list."
  (pcase (cdr expression)
    (`(,reps ,child)
     (wordgen--expr-replicate-make
      (wordgen--parse-expression reps)
      (wordgen--parse-expression child)
      expression))
    (_
     (error "Invalid replicate expression %S: expects 2 arguments, %d given"
            expression (length (cdr expression))))))

(defun wordgen--parse-concat-reeval (expression)
  "Compile a concat-reeval expression to intermediate representation.
EXPRESSION is the whole (concat-reeval ...) list."
  (pcase (cdr expression)
    (`(,reps ,child)
     (wordgen--expr-concat-reeval-make
      (wordgen--parse-expression reps)
      (wordgen--parse-expression child)
      expression))
    (_
     (error "Invalid concat-reeval expression %S: expects 2 arguments, %d given"
            expression (length (cdr expression))))))

(defun wordgen--parse-lisp-call (expression)
  "Compile a Lisp function call expression to intermediate representation.
EXPRESSION is the whole (lisp ...) list."
  (pcase (cdr expression)
    (`(,func)
     (wordgen--expr-lisp-call-make func expression))
    (_
     (error "Invalid Lisp expression %S: expects 1 argument, %d given"
            expression (length (cdr expression))))))


;;; IR-based type checking

(defun wordgen--typecheck-rule-body (expression)
  "Verify that the rule body EXPRESSION type checks and return it."
  (wordgen--expr-expect-type expression 'string)
  (wordgen--expr-typecheck-children expression)
  expression)

(defun wordgen--expr-expect-type (expr type)
  "Verify that the EXPR is of TYPE.
If type checking fails, an error is raised.

The returned value is unspecified."
  (let ((expr-type (wordgen--expr-type expr)))
    (cond
     ((eq expr-type type)
      nil)
     ((null expr-type)
      (wordgen--expr-case expr nil
        (choice
         (pcase-dolist (`(,child . ,_) (wordgen--expr-choice-children expr))
           (wordgen--expr-expect-type child type))
         ;; If we're still here, `type' is the correct type, so store it.
         (setf (wordgen--expr-type expr) type))
        (lisp-call
         ;; So we later know what to do when compiling.
         (setf (wordgen--expr-type expr) type))
        ;; These types won't even reach this point.
        ((integer string rule-call concat replicate concat-reeval)
         nil)))
     (t
      (error "Expected type `%S', but %S is of type `%S'"
             type (wordgen--expr-original-form expr) expr-type)))))

(defun wordgen--expr-typecheck-children (expr)
  "Verify that all children of EXPR are of the correct type."
  (wordgen--expr-case expr nil
    (choice
     (pcase-dolist (`(,child . ,_) (wordgen--expr-choice-children expr))
       (wordgen--expr-typecheck-children child)))
    (concat
     (dolist (child (wordgen--expr-concat-children expr))
       (wordgen--expr-expect-type child 'string)
       (wordgen--expr-typecheck-children child)))
    (replicate
     (let ((reps (wordgen--expr-replicate-reps expr))
           (form (wordgen--expr-replicate-subexpr expr)))
       (wordgen--expr-expect-type reps 'integer)
       (wordgen--expr-typecheck-children reps)
       (wordgen--expr-expect-type form 'string)
       (wordgen--expr-typecheck-children form)))
    (concat-reeval
     (let ((reps (wordgen--expr-concat-reeval-reps expr))
           (form (wordgen--expr-concat-reeval-subexpr expr)))
       (wordgen--expr-expect-type reps 'integer)
       (wordgen--expr-typecheck-children reps)
       (wordgen--expr-expect-type form 'string)
       (wordgen--expr-typecheck-children form)))
    ((integer string rule-call lisp-call)
     nil)))


;;; Expression compiler

;; Wordgen code is made executable by converting its expressions to Emacs Lisp
;; forms, which then are joined together (which is trivial, as Emacs Lisp is a
;; Lisp), put into a lambda, and finally compiled by Emacs's own byte compiler.
;; It's done this way because it's probably the easiest way in Lisp and it's
;; reasonably fast.
;;
;; The generated functions take two arguments: (RULES RNG).
;; RULES is the compiled rule set, as returned by `wordgen-compile-ruleset'.
;; RNG is the pseudo-random number generator, as returned by
;; `wordgen--prng-create'.

(defvar wordgen--compile-to-bytecode t
  "If non-nil, compile the generated lambdas to Emacs bytecode.
Should be t at all times, except when debugging.")

(defun wordgen--compile-elisp-to-lambda (expression)
  "Compile Emacs Lisp form EXPRESSION to a Emacs Lisp bytecode.
With `wordgen--compile-to-bytecode' nil, an uncompiled `lambda' form is returned
instead."
  (let ((func `(lambda (rules rng) ,expression)))
    (if wordgen--compile-to-bytecode
        ;; We have to silence `byte-compile-log-warning' as it can log some
        ;; warnings to *Compile-Log* even though we set `byte-compile-warnings'
        ;; to nil.
        (cl-letf (((symbol-function #'byte-compile-log-warning) #'ignore)
                  (lexical-binding t)
                  (byte-compile-warnings nil)
                  (byte-compile-verbose nil)
                  (byte-optimize t)
                  (byte-compile-generate-call-tree nil))
          (byte-compile func))
      ;; Not compiling, just return the lambda.
      func)))

(defun wordgen--expr-compile (expr)
  "Compile EXPR to an Emacs Lisp form."
  (wordgen--expr-case expr nil
    (integer
     (wordgen--expr-integer-value expr))
    (string
     `(wordgen-print-string ,(wordgen--expr-string-value expr)))
    (rule-call
     `(wordgen-call-rule-by-name rules rng ',(wordgen--expr-rule-call-rule-name expr)))
    (concat
     `(progn
        ,@(mapcar #'wordgen--expr-compile (wordgen--expr-concat-children expr))))
    (choice
     (wordgen--expr-choice-compile expr))
    (replicate
     (wordgen--expr-replicate-compile expr))
    (concat-reeval
     (wordgen--expr-concat-reeval-compile expr))
    (lisp-call
     (wordgen--expr-lisp-call-compile expr))))

(defun wordgen--expr-choice-compile (choice)
  "Compile a CHOICE expression to an Emacs Lisp form."
  (let ((total-weight (wordgen--expr-choice-total-weight choice))
        (children (wordgen--expr-choice-children choice))
        (children-count (wordgen--expr-choice-children-count choice)))
    ;; We have three strategies available to us.
    ;; * lookup table: used when the weights are relatively dense.
    ;; * binary search: used when the weights are sparse.
    ;; * cond-based linear search: used when the are few subexpressions.
    ;; The conditions used here are mere heuristics that work good enough.
    (cond
     ((and (> children-count 2)
           (> 10 (/ total-weight children-count)))
      (wordgen--compile-choice-dense children total-weight))
     ((> children-count 5)
      (wordgen--compile-choice-sparse children total-weight))
     (t
      (wordgen--compile-choice-tiny children total-weight)))))

(defun wordgen--compile-choice-dense (children total-weight)
  "Compile a choice expression into a dense table lookup.
CHILDREN and TOTAL-WEIGHT are the slots of `wordgen--expr-choice'."
  (let* ((same-type (wordgen--choice-children-same-eval-type-p children))
         (vec (wordgen--build-vector total-weight children))
         (aref-form `(aref ,vec (wordgen-prng-next-int ,(1- total-weight) rng))))
    (pcase same-type
      (`integer aref-form)
      (`string `(wordgen-print-string ,aref-form))
      (`lambda `(funcall ,aref-form rules rng))
      (_
       ;; The types are mixed, go through `wordgen--eval-choice-subexpression'.
       ;; Note: this let is actually useful, as it lets the compiler macro on
       ;; `wordgen--eval-choice-subexpression' generate optimal bytecode.
       `(let ((x ,aref-form))
          (wordgen--eval-choice-subexpression x rules rng))))))

(defun wordgen--compile-choice-tiny (children total-weight)
  "Compile a choice expression into a series of conditionals.
CHILDREN and TOTAL-WEIGHT are the slots of `wordgen--expr-choice'."
  `(let ((number (wordgen-prng-next-int ,(1- total-weight) rng)))
     (cond
      ,@(mapcar
         (lambda (child)
           (pcase-let ((`(,expr _ ,limit) child))
             `(,(if (= limit total-weight) t `(< number ,limit))
               ,(wordgen--expr-compile expr))))
         children))))

(defun wordgen--compile-choice-sparse (children total-weight)
  "Compile a choice expression into binary search.
CHILDREN and TOTAL-WEIGHT are the slots of `wordgen--expr-choice'."
  (let ((vec
         (apply
          #'vector
          (mapcar
           (lambda (child)
             (pcase-let ((`(,expr ,weight ,running-weight) child))
               (list (- running-weight weight)
                     running-weight
                     (wordgen--build-choice-subexpression expr))))
           children))))
    `(wordgen--choice-binary-search
      ,vec (wordgen-prng-next-int ,(1- total-weight) rng) rules rng)))

(defun wordgen--build-vector (length subexprs)
  "Build a vector of LENGTH elements using SUBEXPRS.

For each (EXPR WEIGHT . _) element of SUBEXPRS, the result of
\(`wordgen--build-choice-subexpression' EXPR) appears WEIGHT times in the
returned vector."
  (let ((result (make-vector length nil))
        (i 0))
    (pcase-dolist (`(,expr ,weight . ,_) subexprs)
      (let ((built-expr (wordgen--build-choice-subexpression expr)))
        (dotimes (_ weight)
          (aset result i built-expr)
          (cl-incf i))))
    result))

(defun wordgen--build-choice-subexpression (subexpr)
  "Generate a vector element for a expression SUBEXPR.

Strings and integers are returned unchanged, other forms are wrapped in a lambda
and compiled.

See also `wordgen--eval-choice-subexpression', which evaluates the returned
object."
  (wordgen--expr-case subexpr nil
    (string
     (wordgen--expr-string-value subexpr))
    (integer
     (wordgen--expr-integer-value subexpr))
    ((concat rule-call lisp-call replicate concat-reeval choice)
     (wordgen--compile-elisp-to-lambda (wordgen--expr-compile subexpr)))))

(cl-defsubst wordgen--eval-choice-subexpression (subexpr rules rng)
  "Eval a SUBEXPR compiled by `wordgen--build-choice-subexpression'.

RULES and RNG are passed unchanged to the compiled form."
  (cond
   ((stringp subexpr)
    (wordgen-print-string subexpr))
   ((integerp subexpr)
    subexpr)
   (t
    (funcall subexpr rules rng))))

(defun wordgen--choice-children-same-eval-type-p (children)
  "Test whether all CHILDREN of a choice expression are of the same type.

\"Type\" here means the type of the object returned by
`wordgen--build-choice-subexpression', i.e. it's the type of the Lisp object in
the vector.

If all CHILDREN are string literals, integer literals, or lambdas, symbols
'string, 'integer or 'lambda are returned; otherwise, the result is nil."
  (catch 'return
    (let (type)
      (pcase-dolist (`(,child . ,_) children)
        (let ((child-type
               (wordgen--expr-case child child-type
                 ((integer string)
                  child-type)
                 ((concat choice rule-call lisp-call replicate concat-reeval)
                  'lambda))))
          ;; Mixed-type list.
          (when (and type (not (eq child-type type)))
            (throw 'return nil))
          (setq type child-type)))
      type)))

(defun wordgen--choice-binary-search (vec number rules rng)
  "Find the range in VEC in which NUMBER is, using binary search.

VEC is a sorted vector of (BEGIN END FORM), where [BEGIN..END) is a numeric
range and FORM is its corresponding compiled form returned from
`wordgen--build-choice-subexpression'.

RULES and RNG are passed unchanged to the compiled forms."
  (catch 'return
    (let ((low 0)
          (high (length vec)))
      (while t
        (let* ((half (+ low (/ (- high low) 2)))
               (guess (aref vec half))
               (guess-low (nth 0 guess)))
          (cond
           ((and (<= guess-low number)
                 (< number (nth 1 guess)))
            (let ((form (nth 2 guess)))
              (throw 'return (wordgen--eval-choice-subexpression form rules rng))))
           ((> guess-low number)
            (setq high half))
           (t
            (setq low (1+ half)))))))))

(defun wordgen--expr-replicate-compile (replicate)
  "Compile a REPLICATE expression to an Emacs Lisp form."
  (let ((times-compiled
         (wordgen--expr-compile (wordgen--expr-replicate-reps replicate)))
        (form-compiled
         (wordgen--expr-compile (wordgen--expr-replicate-subexpr replicate))))
    `(let ((times ,times-compiled))
       (when (> times 0)
         (let ((string (wordgen-with-output-to-string ,form-compiled)))
           (dotimes (_ times)
             (wordgen-print-string string))))
       "")))

(defun wordgen--expr-concat-reeval-compile (concat-reeval)
  "Compile a CONCAT-REEVAL expression to an Emacs Lisp form."
  (let ((times-compiled
         (wordgen--expr-compile (wordgen--expr-concat-reeval-reps concat-reeval)))
        (form-compiled
         (wordgen--expr-compile (wordgen--expr-concat-reeval-subexpr concat-reeval))))
    `(let ((times ,times-compiled))
       (if (<= times 0)
           ""
         ;; Run the loop N-1 times, so the result of the if will be the same as
         ;; that of last expr evaluation.
         (dotimes (_ (1- times))
           ,form-compiled)
         ,form-compiled))))

(defun wordgen--expr-lisp-call-compile (lisp-call)
  "Compile a LISP-CALL expression to an Emacs Lisp form."
  (let* ((type (wordgen--expr-type lisp-call))
         (predicate (wordgen--expr-type-to-predicate-name type)))
    `(let ((x (funcall ,(wordgen--expr-lisp-call-func lisp-call) rules rng)))
       (unless (,predicate x)
         (error "Lisp call inferred to be of type `%S', but `%S' is nil"
                ',type ',predicate))
       x)))

(defun wordgen--expr-type-to-predicate-name (type)
  "Convert expression TYPE to corresponding Emacs Lisp predicate."
  (pcase type
    (`string #'stringp)
    (`integer #'integerp)
    (_ (error "Unknown type %S" type))))


;;; PRNG helpers

(eval-and-compile
  (when (< most-positive-fixnum #xFFFFFFFF)
    (error "This package requires Lisp integers to be at least 32-bit"))
  (defconst wordgen--prng-array-size 128)
  (defconst wordgen--prng-optimal-bytes-size (* 4 wordgen--prng-array-size)))

(defun wordgen--get-default-seed ()
  "Get the PRNG default seed.
When available, uses /dev/urandom."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (pcase (ignore-errors
             (call-process "dd" nil '(t nil) nil
                           "if=/dev/urandom"
                           (eval-when-compile (concat "bs=" (number-to-string wordgen--prng-optimal-bytes-size)))
                           "count=1"))
      (`0 (buffer-string))
      (_ (current-time-string)))))


;;; PRNG engine

(defun wordgen--prng-create (passed-array)
  "Create a PRNG seeded using PASSED-ARRAY.
PASSED-ARRAY must be a vector of 128 32-bit integers."
  (let ((index 0)
        (array (copy-sequence passed-array)))
    (lambda ()
      (cl-macrolet
          ((as-index
            (index)
            `(logand ,index ,(- (1- wordgen--prng-array-size))))
           (xorshift
            (shift value)
            (let ((value-symbol (make-symbol "value")))
              `(let ((,value-symbol ,value))
                 (logxor ,value-symbol
                         ,(if (> shift 0)
                              ;; We're shifting left, so the result may be bigger than 32
                              ;; bits; truncate it.
                              `(logand (lsh ,value-symbol ,shift) #xFFFFFFFF)
                            `(lsh ,value-symbol ,shift)))))))
        (let* ((result
                (logxor (xorshift -12 (xorshift 17 (aref array index)))
                        (xorshift -15 (xorshift 13 (aref array (as-index (+ index 33))))))))
          (aset array index result)
          (setq index (as-index (1+ index)))
          result)))))

(defun wordgen--prng-create-from-bytes (bytes)
  "Create a prng seeded from BYTES.
BYTES should be a sequence of integers."
  (let* ((array (make-vector wordgen--prng-array-size 0))
         (bytes-word-len (/ (length bytes) 4))
         (i 0)
         (max-words (min wordgen--prng-array-size bytes-word-len)))
    (while (< i max-words)
      (let ((word 0))
        (dotimes (k 4)
          (setq word (lsh word 8))
          (setq word (+ word (logand #xFF (elt bytes (+ k (* i 4)))))))
        (aset array i word))
      (cl-incf i))
    ;; No more words in bytes, fill the state using a linear congruential
    ;; generator.
    (let ((seed (if (= i 0) 1 (aref array (1- i)))))
      (while (< i wordgen--prng-array-size)
        (setq seed (logand #xFFFFFFFF (+ 1013904223 (* 1664525 seed))))
        (aset array i seed)
        (cl-incf i)))
    (wordgen--prng-create array)))

(defun wordgen--prng-next-int-small (limit rng)
  "Return a 32-bit pseudo-random integer in interval [0, LIMIT] using RNG.
LIMIT must be a non-negative integer smaller than 2^32-1."
  (let* ((range (1+ limit))
         (scaling-factor (/ #xFFFFFFFF range))
         (actual-limit (* range scaling-factor))
         (result nil))
    (while (< actual-limit (setq result (funcall rng))))
    (/ result scaling-factor)))

(defun wordgen-prng-next-int (limit rng)
  "Return a pseudo-random integer in interval [0, LIMIT] using RNG.
LIMIT must be a non-negative integer."
  (cond
   ((< limit #xFFFFFFFF) (wordgen--prng-next-int-small limit rng))
   ((= limit #xFFFFFFFF) (funcall rng))
   (t (let ((result nil))
        (while (> (setq result (+ (lsh (wordgen-prng-next-int (lsh limit -32) rng) 32)
                                  (funcall rng)))
                  limit))
        result))))

(provide 'wordgen)
;;; wordgen.el ends here
