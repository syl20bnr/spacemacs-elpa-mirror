;;; objc-font-lock.el --- Highlight Objective-C method calls.

;; Copyright (C) 2013-2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; Package-Version: 20141021.1122
;; Created: 2013-11-26
;; Version: 0.0.4
;; URL: https://github.com/Lindydancer/objc-font-lock

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package highlights Objective-C method calls.

;; Background:
;;
;; Objective-C use a syntax for method calls it has inherited from
;; Smalltalk. Unfortunately, this syntax is frustratingly hard to read
;; -- when browsing through code, method calls tend to "disappear" as
;; they are different from normal C function calls.
;;
;; By highlighting method calls, it is possible to read the same piece
;; of code faster and more accurate.
;;
;; By default, the open and close bracket is highlighted using a
;; bright *warning face*, the entire method call is highligthed using
;; the standard *highlight face*, and each Objective-C function name
;; component is highlighted using the *font-lock function name* face.
;;
;; For a more sober appearance, you can configure the package to, for
;; example, only highlight the function name components.
;;
;; The following screenshot demonstrates the highlighting effect of
;; this package:
;;
;; ![See doc/demo.png for screenshot](doc/demo.png)

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'objc-font-lock-mode)
;;    (objc-font-lock-global-mode 1)

;; Customization:
;;
;; Method calls are highlighted as follows:
;;
;;                             Controlling variable:           Default:
;;     [expr func: expr]
;;     ^               ^-- objc-font-lock-bracket-face       Warning face
;;           ^^^^--------- objc-font-lock-function-name-face Function name face
;;     ^^^^^^^^^^^^^^^^^-- objc-font-lock-background-face    Highlight
;;
;; To change the face used, change the face variable. By setting it to
;; "nil", the corresponding part of the method call will not be
;; highlighted. For example:
;;
;;     ;; Don't highlight brackets.
;;     (setq objc-font-lock-bracket-face nil)
;;     ;; Use `secondary-selection' (a builtin face) as background.
;;     (setq objc-font-lock-background-face 'secondary-selection)

;; Under the hood:
;;
;; This package parse the content of brace pairs to decide if they
;; form an Objective-C method call, and what the parts are. It is
;; neither aware of the context in which the brackets appear, nor what
;; included symbols corresponds to. Despite these limitations, this
;; package highlight most typical constructs used in real-world
;; Objective-C.

;; Implementation:
;;
;; An Objective-C method call is on one the following forms:
;;
;;     [expression func]
;;     [expression func: expression func: expression....]
;;
;; This package use font-lock rules with a function performing the
;; matcher (rather than an regexp). The matcher will find a bracket
;; pair (skipping those that are used for plain C arrays or are
;; located in a string or comment). Anchored sub-rules will match and
;; highlight individual parts. The function names an brackets are
;; highlighted using one rule placed before the standard Font Lock
;; rules. The background is highlighted using another placed last.
;;
;; To recognize an expression, the Emacs built-in expression parser,
;; `forward-sexp', is used for plain trivial expressions (like
;; identifiers and numbers) and expressions inside parentheses. On top
;; of this, this package checks for complex expressions by looking for
;; casts and infix operators.
;;
;; The pre- and postincrement and -decrement operators don't fall into
;; the above pattern. However, any code containing them should be
;; fontified exactly like they would, if they weren't there. Hence,
;; they are treated as "whitespace".
;;
;; Without knowing if an identifier denotes a type or not, a construct
;; like `[(alpha)beta]' is ambiguous. It can either be interpreted as
;; a method call, where the object `alpha' is sent the message `beta'.
;; However, it can also be seen as an array subscript, where the
;; expression `beta' is cast to the type `alpha'. This package treats
;; any construct that looks like a cast as though it is a cast.

;;; Code:

(defgroup objc-font-lock nil
  "Highlight method calls in Objective-C."
  :group 'faces)


(defface objc-font-lock-background
  '((t :inherit highlight))
  "The default face used to highlight an entire Objective-C method call."
  :group 'objc-font-lock)

(defcustom objc-font-lock-background-face 'objc-font-lock-background
  "The face used to highlight an entire Objective-C method call.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'objc-font-lock)


(defface objc-font-lock-bracket
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight brackets in Objective-C method calls."
  :group 'objc-font-lock)

(defcustom objc-font-lock-bracket-face       'objc-font-lock-bracket
  "The face used to highlight brackets in Objective-C method calls.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'objc-font-lock)


(defface objc-font-lock-function-name
  '((t :inherit font-lock-function-name-face))
  "\
The default face used to highlight function names in Objective-C method calls."
  :group 'objc-font-lock)

(defcustom objc-font-lock-function-name-face 'objc-font-lock-function-name
  "The face used to highlight function names in Objective-C method calls.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'objc-font-lock)


;;;###autoload
(defcustom objc-font-lock-modes '(objc-mode)
  "List of major modes where Objc Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'objc-font-lock)


;; ------------------------------
;; The modes
;;

;;;###autoload
(define-minor-mode objc-font-lock-mode
  "Minor mode that highlights Objective-C method calls."
  :group 'objc-font-lock
  (if objc-font-lock-mode
      (objc-font-lock-add-keywords)
    (objc-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode objc-font-lock-global-mode objc-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p objc-font-lock-modes)
      (objc-font-lock-mode 1)))
  :group 'objc-font-lock)


;; ------------------------------
;; Font-lock rules and setup function
;;

(defvar objc-font-lock-prepend-keywords
  ;; --------------------
  ;; Brackets and function names.
  '((objc-font-lock-match-bracket
     (1 objc-font-lock-bracket-face)
     (3 objc-font-lock-bracket-face)
     (objc-font-lock-match-bracket-content
      ;; PRE-MATCH-FORM:
      (objc-font-lock-match-bracket-pre-match)
      ;; POST-MATCH-FORM:
      (objc-font-lock-match-bracket-post-match)
      ;; Highlight.
      (0 objc-font-lock-function-name-face nil t)))))


(defvar objc-font-lock-append-keywords
  ;; --------------------
  ;; Brackets and function names.
   '((objc-font-lock-match-bracket
      (objc-font-lock-match-line
       ;; PRE-MATCH-FORM:
       (progn
         (goto-char (match-beginning 0))
         (match-end 0))
       ;; POST-MATCH-FORM:
       (goto-char (match-end 1))
       (0 objc-font-lock-background-face append)))))


(defun objc-font-lock-add-keywords ()
  "Add font-lock keywords to highlight Objective-C method calls."
  (setq font-lock-multiline t)
  (font-lock-add-keywords
   nil
   objc-font-lock-prepend-keywords)
  ;; --------------------
  ;; Background.
  ;;
  ;; Separate rule, to ensure that nested method calls work.
  (font-lock-add-keywords
   nil
   objc-font-lock-append-keywords
   'append))


(defun objc-font-lock-remove-keywords ()
  "Remove font-lock keywords for highlighting Objective-C method calls."
  (font-lock-remove-keywords nil objc-font-lock-prepend-keywords)
  (font-lock-remove-keywords nil objc-font-lock-append-keywords))


;; --------------------
;; Method call search function.

(defun objc-font-lock-match-bracket (lim)
  "Search for an Objective-C method call.

Return non-nil if one is found. The point will be placed after
the start brace. The match data will be filled with the following:

   0 -- The entire method call
   1 -- The start brace
   2 -- The object expression
   3 -- The end brace.

Note that this function is intended to be used by font-lock, and
assumes that comments and string have been fontified."
  (let ((ok nil)
        (beg-of-expr nil)
        (end-of-expr nil))
    ;; Look for "[":s, but skip those that are used for array constructs.
    (while
        (progn
          (setq ok (re-search-forward "\\[" lim t))
          (and ok
               ;; Continue searching, if this bracket isn't suitable.
               (or (objc-font-lock-is-in-comment-or-string)
                   ;; When the `[' doesn't match a `]', skip it.
                   ;; Without this, code after the method call would
                   ;; be fontfied, typically up the the next closing
                   ;; brace.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (condition-case nil
                         (progn
                           (forward-sexp)
                           (not (eq (char-before) ?\] )))
                       (error nil)))
                   ;; Skip array subscripts.
                   ;;
                   ;; Record the start end end location of the first
                   ;; complex expression, in case this wasn't an array
                   ;; construct.
                   (save-excursion
                     (objc-font-lock-skip-whitespace-etc)
                     (setq beg-of-expr (point))
                     (objc-font-lock-skip-complex-expression)
                     (setq end-of-expr (point))
                     (objc-font-lock-skip-whitespace-etc)
                     (eq (following-char) ?\]))))))
    (if ok
        (let* ((beg (- (point) 1))
               (end (save-excursion
                     (goto-char beg)
                     (forward-sexp)
                     (point))))
          ;; This synthesized match data will be in place for the
          ;; highlights of the keyword as well as when the pre- and
          ;; post-match-forms of the anchored rule are executed.
          (set-match-data (list
                           ;; Match 0: Full range
                           beg end
                           ;; Match 1: [
                           beg (+ beg 1)
                           ;; Match 2: expr
                           beg-of-expr end-of-expr
                           ;; Match 3: ]
                           (- end 1) end))))
    ok))


(defun objc-font-lock-is-in-comment-or-string ()
  "Return non-nil if point is in comment or string.

This assumes that Font Lock is active and has fontified comments
and strings."
  (let ((props (text-properties-at (point)))
        (faces '()))
    (while props
      (let ((pr (pop props))
            (value (pop props)))
        (if (eq pr 'face)
            (setq faces value))))
    (unless (listp faces)
      (setq faces (list faces)))
    (or (memq 'font-lock-comment-face faces)
        (memq 'font-lock-string-face faces))))


;; ----------
;; Function names anchored sub-rule.

(defun objc-font-lock-match-bracket-pre-match ()
  "Pre-match form of `objc-font-lock-match-bracket-content' font-lock rule."
  ;; Place point after the object expression.
  (goto-char (match-end 2))
  ;; Search limit end before the `]'.
  (match-beginning 3))


(defun objc-font-lock-match-bracket-post-match ()
  "Post-match form of `objc-font-lock-match-bracket-content' font-lock rule."
  ;; Place point after `['.
  (goto-char (match-end 1)))


(defun objc-font-lock-match-bracket-content (lim)
  "Find function name part of Objective-C method call.

The point is moved to end of the argument."
  (objc-font-lock-skip-whitespace-etc)
  (cond ((looking-at "\\<[a-zA-Z_][a-zA-Z0-9_]*\\>")
         (goto-char (match-end 0))
         (objc-font-lock-skip-whitespace-etc)
         (cond ((eq (following-char) ?:)
                (forward-char)
                (objc-font-lock-skip-complex-expression)
                t)
               ((eq (following-char) ?\])
                t)
              (t
               nil)))
        (t
         nil)))


;; ----------
;; Background anchored sub-rule

(defun objc-font-lock-match-line (limit)
  "Match line at point."
  (if (>= (point) limit)
      nil
    (set-match-data (list (save-excursion
                            (skip-chars-forward " \t")
                            (point))
                          (min limit (line-end-position))))
    (forward-line)
    t))


;; ------------------------------
;; Parse functions.
;;

(defun objc-font-lock-skip-complex-expression ()
  "Move forward to the end of a complex expression.

An expression is a sequence of simple expressions separated by
infix operators. Casts may precede the simple expressions."
  (objc-font-lock-skip-whitespace-etc)
  (objc-font-lock-skip-sexp-with-cast)
  (while
      (progn
        (objc-font-lock-skip-whitespace-etc)
        ;; Include all C infix operators.
        ;;
        ;; Note: forward-sexp can't handle floating point constructs
        ;; like "0.5", however, the dot is seen as an infix operator.
        ;;
        ;; Note: "||" are handled as the first "|" is found and the
        ;; second is swallowed by the
        ;; `objc-font-lock-skip-sexp-with-cast' function.
        ;; Unfortunately, this also accepts nonsense constructs like
        ;; "|\".
        ;;
        ;; Note: Objective-C block syntax ^ is supported by ^ being
        ;; treated as a normal infix operator and brase blocks simply
        ;; skipped.
        (cond ((memq (following-char)
                     '(?+ ?- ?* ?/ ?% ?& ?^ ?| ?, ?. ?< ?> ?? ?: ?= ?!))
               (forward-char)
               (objc-font-lock-skip-sexp-with-cast))
              ((memq (following-char) '(?\( ?\[ ?\{))
               ;; A function call or array construct.
               (objc-font-lock-skip-sexp-with-parens))
              (t nil)))))


(defun objc-font-lock-skip-sexp-with-parens()
  "Skip over an parenthesized expression.
Return t on success or nil on failure."
  (condition-case nil
      (progn
        (forward-sexp)
        t)
    (error nil)))


(defun objc-font-lock-skip-sexp-with-cast ()
  "Skip over an primitive or parenthesized expression, handle casts.
Return t on success or nil on failure."
  ;; Objective-C strings (@"XZY") are not handled by forward-sexp.
  (when (eq (following-char) ?@)
    (forward-char)
    (objc-font-lock-skip-whitespace-etc))
  (condition-case nil
      (progn
        (while (objc-font-lock-looking-at-simple-cast)
          (forward-sexp))
        (forward-sexp)
        t)
    (error nil)))


;; Note: Doesn't include function pointers, brackets, Obj-C++
;; reference types. etc.
(defun objc-font-lock-looking-at-simple-cast ()
  "Return non-nil if point is at a simple cast expression."
  (save-match-data
    (if (eq (following-char) ?\( )
        (save-excursion
          (let ((res t))
            (forward-char)
            (while
                (progn
                  (objc-font-lock-skip-whitespace-etc)
                  (cond ((eobp)
                         (setq res nil)
                         nil)
                        ((looking-at "[a-zA-Z_][a-zA-Z0-9_]*")
                         (goto-char (match-end 0))
                         t)
                        ((eq (following-char) ?*)
                         (forward-char)
                         t)
                        ((eq (following-char) ?\) )
                         nil)
                        (t
                         (setq res nil)
                         nil))))
            res))
      nil)))


(defun objc-font-lock-skip-whitespace-etc ()
  "Move point past whitespace, comments, and other things that can be ignored.

Currecntly, `++' and `--' as well as `sizeof' are ignored, as an
expression containing those should be treated exactly like a
similar expression without those constructs."
  (while
      (progn
        ;; Note: argument is number of comment to skip. Using
        ;; `buffer-size', as recommended in the Emacs elisp manual, to
        ;; skip all comments.
        (forward-comment (buffer-size))
        (cond ((or (and (eq (following-char) ?+)
                        (eq (char-after (+ (point) 1)) ?+))
                   (and (eq (following-char) ?-)
                        (eq (char-after (+ (point) 1)) ?-)))
               (forward-char 2)
               t)
              ;; "sizeof" normally look like a function call (which
              ;; the normal system can handle). However, it can also
              ;; be used without the paretheses, like "sizeof int". To
              ;; avoid this case, "sizeof" is treated as "whitespace".
              ((looking-at "\\(sizeof\\>\\)[^_]")
               (goto-char (match-end 1))
               t)
              (t
               nil)))))


;; ------------------------------
;; The end
;;

(provide 'objc-font-lock)


;;; objc-font-lock.el ends here.
