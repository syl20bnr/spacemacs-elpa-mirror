Suggested key bindings
======================

You can eval the following key definitions to try things out while
reading this introduction.  These are the bindings I use
personally:

  (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
  (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
  (define-key global-map          [(control ?J)] #'el-search-jump-to-search-head)
  (define-key global-map          [(control ?N)] #'el-search-continue-in-next-buffer)

  (define-key el-search-read-expression-map [(control ?S)] #'exit-minibuffer)

  (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)

  (with-eval-after-load 'dired
    (define-key dired-mode-map [(control ?S)] #'el-search-dired-marked-files))

These bindings may not work in a console (if you have an idea for
official bindings that fit better into the Emacs ecosystem, please
mail me).

The binding in `isearch-mode-map' lets you switch to "el-search"
from isearch reusing already the given input.  The binding in
`el-search-read-expression-map' allows you to hit C-S twice to
start a search using the last search pattern, similar to isearch.

Don't be afraid of the long introduction, it's only verbose (sorry)
but not complicated.


Introduction
============

The main user entry point is `el-search-pattern'.  This command
prompts for a `pcase' pattern and searches the current buffer for
matching expressions by iteratively `read'ing buffer contents.  For
any match, point is put at the beginning of the expression found
(unlike isearch which puts point at the end of matches).

Why is it based on `pcase'?  Because pattern matching (and the
ability to combine destructuring and condition testing) is well
suited for this task.  In addition, pcase allows to add specialized
pattern types and to combine them with other patterns in a natural
and transparent way out of the box.

When searching, it doesn't matter how code is actually formatted.
Comments are ignored, and strings are treated as atomic objects,
their contents are not being searched.


Example 1: if you enter

   97

at the prompt, this will find any occurrence of the number 97 in
the code, but not 977 or (+ 90 7) or "My string containing 97".
But it will find anything `eq' to 97 after reading, e.g. #x61 or
?a.


Example 2: If you enter the pattern

  `(defvar ,_)

you search for all defvar forms that don't specify an init value.

The following pattern will search for defvar forms with a docstring
whose first line is longer than 70 characters:

  `(defvar ,_ ,_
     ,(and (pred stringp) s
           (guard (< 70 (length (car (split-string s "\n")))))))


Convenience
===========

For pattern input, the minibuffer is put into `emacs-lisp-mode'.

Any input PATTERN is silently transformed into (and exp PATTERN)
so that you can always refer to the whole currently tested
expression via the variable `exp'.


Example 3:

If you wanted to search a buffer for symbols that are defined in
"cl-lib", you could use this pattern

  (guard (and (symbolp exp)
              (when-let ((file (symbol-file exp)))
                (string-match-p "cl-lib\\.elc?$" file))))


,----------------------------------------------------------------------
| Q: "But I hate `pcase'!  Can't we just do without?"                 |
|                                                                     |
| A: Respect that you kept up until here! Just use (guard CODE), where|
| CODE is any normal Elisp expression that returns non-nil when and   |
| only when you have a match.  Use the variable `exp' to refer to     |
| the currently tested expression.  Just like in the last example!    |
`----------------------------------------------------------------------


It's cumbersome to write out the same complicated pattern
constructs in the minibuffer again and again.  You can define your
own `pcase' pattern types for the purpose of el-searching with
`el-search-defpattern'.  It is just like `pcase-defmacro', but the
effect is limited to this package (i.e. it uses a separate name
space).  See C-h f `el-search-pattern' for a list of predefined
pattern forms.

Some additional pattern definitions can be found in the file
"el-search-x" which is part of this package.


Replacing
=========

You can replace expressions with command `el-search-query-replace'.
You are queried for a (pcase) pattern and a replacement expression.
For each match of the pattern, the replacement expression is
evaluated with the bindings created by the pcase matching in
effect, and printed to a string to produce the replacement.

Example: In some buffer you want to swap the two expressions at the
places of the first two arguments in all calls of function `foo',
so that e.g.

  (foo 'a (* 2 (+ 3 4)) t)

becomes

  (foo (* 2 (+ 3 4)) 'a t).

This will do it:

   M-x el-search-query-replace RET
   `(foo ,a ,b . ,rest) RET
   `(foo ,b ,a . ,rest) RET

Type y to replace a match and go to the next one, r to replace
without moving, SPC to go to the next match and ! to replace all
remaining matches automatically.  q quits.  n is like SPC, so that
y and n work like in isearch (meaning "yes" and "no") if you are
used to that.

It is possible to replace a match with multiple expressions using
"splicing mode".  When it is active, the replacement expression
must evaluate to a list, and is spliced instead of inserted into
the buffer for any replaced match.  Use s to toggle splicing mode
in a `el-search-query-replace' session.


Multi Searching
===============

"el-search" is capable of performing "multi searches" - searches
spanning multiple files or buffers.  When no more matches can be
found in the current buffer, the search automatically switches to
the next buffer.  Examples for search commands starting a multi
search are `el-search-buffers' (search all living elisp mode
buffers), `el-search-directory' (search all elisp files in a
specified directory), `el-search-emacs-elisp-sources' (search all
Emacs elisp sources) and `el-search-dired-marked-files'.  Actually,
every search is internally a multi search.

You can pause any (multi) search by just doing something different,
the state of the search is automatically saved. You can continue
searching by calling `el-search-jump-to-search-head': this command
jumps to the last match and re-activates the search.

`el-search-continue-in-next-buffer' skips all remaining matches in
the current buffer and continues searching in the next buffer.

Matches found in the current buffer are recorded; use
`el-search-previous-match' to revisit them in reverse order (this
is actually the poor-man's version of a backward search, since a
real backward el-search would be slow).

There is no multi query-replace currently implemented; I don't know
if it would be that useful as a separate command anyway.  If you
want to query-replace in multiple buffers or files, call an
appropriate multi-search command, and every time a first match is
found in any buffer, start an ordinary `el-search-query-replace';
after finishing, check that everything is ok, save etc, and resume
the multi search with one of the above commands.

There is currently nothing like `occur' for el-search.  However,
you can get a list of matches in the form
(file-name-or-buffer . match-position) with

 (el-search-all-matches (el-search-make-search pattern stream))

where PATTERN is the search pattern and STREAM is a stream of
buffers or files (typical ways to construct such a STREAM are to
call the `stream' function on a list of buffers, or to use
`el-search-stream-of-directory-files').

For example,

  (el-search-all-matches
   (el-search-make-search
    ''require
    (seq-filter
     (lambda (buffer)
        (with-current-buffer buffer (derived-mode-p 'emacs-lisp-mode)))
     (stream (buffer-list)))))

would return a list of matches for the symbol require in all elisp
mode buffers.


Multiple multi searches
=======================

Every search is collected in a history.  You can resume older
searches from the position of the last match by calling
`el-search-jump-to-search-head' with a prefix argument.  That let's
you select an older search to resume and switches to the buffer and
position where this search had been suspended.

There is no special command to restart a prior search from the
beginning.  I suggest to use `repeat-complex-command'.


Writing replacement rules for semi-automatic code rewriting
===========================================================

When you want to rewrite larger code parts programmatically, it is
often useful to define dedicated patterns for performing the
replacement.  Here is an example:

You heard that in many situations, `dolist' is faster than an
equivalent `mapc'.  You use `mapc' quite often in your code and
want to query-replace many occurrences in your stuff.  Instead of
using an ad hoc replacing rule, it's cleaner to define a dedicated
named pattern using `el-search-defpattern'.  Make this pattern
accept an argument and use this argument to bind the replacement
expression to a variable you specify.  In our case, the pattern
could look like this:

  (el-search-defpattern el-search-mapc->dolist (new)
    (let ((var  (make-symbol "var"))
          (body (make-symbol "body"))
          (list (make-symbol "list")))
      `(and `(mapc (lambda (,,var) . ,,body) ,,list)
            (let ,new `(dolist (,,var ,,list) . ,,body)))))

The first condition in the `and' performs the matching and binds
the essential parts of the `mapc' form to variables.  The second,
the `let' part, binds the pattern specified argument NEW (as said,
typically just a variable to receive the rewritten code) to the
rewritten expression - in our case, a `dolist' form is constructed
with the remembered code parts filled in.

Now, in `el-search-query-replace', you just specify the following
rule:

  (el-search-mapc->dolist replacement) -> replacement

And when you want to replace in multiple buffers or files, call an
appropriate multi el-search command, e.g. `el-search-directory',
and specify

  (el-search-mapc->dolist replacement)

as search pattern.



Bugs, Known Limitations
=======================

- Replacing: in some cases the reader syntax of forms is changing
due to reading+printing.  "Some" because we can handle this problem
in most cases.

- Similarly: Comments are normally preserved (where it makes
sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)

in a content like

  (foo
    a
    ;; comment
    b)

the comment will be lost.


 Acknowledgments
 ===============

Thanks to Stefan Monnier for corrections and advice.


 Notes for developers
 ====================

We use the following model for multi buffer/file searches: a search
(object) is represented by a struct "el-search-object" that
contains a stream of matches M and a search head object H that
contains a compiled matcher according to the given search pattern,
the buffer and buffer position where to continue searching, and the
stream of buffers or files yet to search.

When elements are requested from M, H is updated accordingly.  H
can be manipulated directly to influence how M will find further
elements when requested (useful for skipping buffers on the fly).


TODO:

- The default keys are not available in the terminal

- Handle buffers killed/files closed when resuming a search

- Make el-search-previous-match behave correctly when a buffer has
  been modified and data is outdated

- Make the non-command `el-search-forward' accept an &optional
  LIMIT argument

- Make searching work in comments, too? (->
  `parse-sexp-ignore-comments').  Related: should the pattern
  `symbol' also match strings that contain matches for a symbol so
  that it's possible to replace also occurrences of a symbol in
  docstrings?

- Implement an occur like interface?

- Port this to non Emacs Lisp modes?  How?  Would it already
  work using only syntax tables, sexp scanning and font-lock?

- For query-replace, maybe we should save the original buffer
  string in a buffer-local variable, and make that ediff'able
  against the new version.  Or should we even save multiple
  versions when appropriate?

- Replace: pause and warn when replacement might be wrong
  (ambiguous reader syntaxes; lost comments, comments that can't
  non-ambiguously be assigned to rewritten code)