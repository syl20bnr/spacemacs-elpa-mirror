This package implements an expression based interactive search tool
for Emacs Lisp files and buffers.  The pattern language used is a
superset of `pcase' patterns.

"el-search" is multi file/buffer search capable.  It is designed to
be fast and easy to use.  It offers an occur-like overview of
matches and can do query-replace based on the same set of patterns.
All searches are added to a history and can be resumed or restarted
later.  Finally, it allows you to define your own kinds of search
patterns and your own multi-search commands.


Suggested key bindings
======================

After loading this file, you can eval
(el-search-install-shift-bindings) to install a set of key bindings
to try things out (no other setup is needed).  Here is an overview
of the most important bindings that this function will establish -
most are of the form Control-Shift-Letter:

  C-S (el-search-pattern)
    Start a search in the current buffer/go to the next match.

  C-R (el-search-pattern-backwards)
    Search backwards.

  C-% (el-search-query-replace)
    Do a query-replace.

  M-x el-search-directory
    Prompt for a directory name and start a multi search for all
    Emacs-Lisp files in that directory.  With prefix arg,
    recursively search files in subdirectories.

  C-S in Dired (el-search-dired-marked-files)
    Like above but uses the marked files and directories.

  C-O (el-search-occur)
    Pop up an occur buffer for the current search.

  C-O (from a search pattern prompt)
    Execute this search command as occur.

  C-N (el-search-continue-in-next-buffer)
    Skip over current buffer or file.

  C-D (el-search-skip-directory)
    Prompt for a directory name and skip all subsequent files
    located under this directory.

  C-A (el-search-from-beginning) Go back to the first match in this
    buffer or (with prefix arg) completely restart the current
    search from the first file or buffer.

  C-J (el-search-jump-to-search-head)
    Resume the last search from the position of the last visited
    match, or (with prefix arg) prompt for an old search to resume.

  C-H (el-search-this-sexp)
    Grab the symbol or sexp under point and initiate an el-search
    for other occurrences.


These bindings may not work in a console (if you have a good idea
for nice alternative bindings please mail me).

The setup you'll need for your init file is trivial: just define
the key bindings you want to use (all important commands are
autoloaded) and you are done.  You can either just copy
(el-search-install-shift-bindings) to your init file to use the
above bindings or use its definition as a template for your own key
binding definitions.


Usage
=====

The main user entry point `el-search-pattern' (C-S) is analogue to
`isearch-forward'.  You are prompted for a `pcase'-style search
pattern using an `emacs-lisp-mode' minibuffer.  After hitting RET
it searches the current buffer from point for matching expressions.
For any match, point is put at the beginning of the expression
found (unlike isearch which puts point at the end of matches).  Hit
C-S again to go to the next match etc.

Syntax and semantics of search patterns are identical to that of
`pcase' patterns, plus additionally defined pattern types
especially useful for matching parts of programs.  The (only)
difference to the `pcase' macro is that the expressions found in
buffers and files are tried to be matched instead of a given
expression.

It doesn't matter how code is actually formatted.  Comments are
ignored, and strings are treated as atomic objects (their contents
are not being searched).


Example 1: if you enter

   97

at the prompt, el-search will find any occurrence of the integer 97
in the code, but not 97.0 or 977 or (+ 90 7) or "My string
containing 97" or symbol_97.  OTOH it will find any printed
representation of 97, e.g. #x61 or ?a.


Example 2: If you enter the pattern

  `(defvar ,_)

you search for all `defvar' forms that don't specify an init value.

The following pattern will search for `defvar's with a docstring
whose first line is longer than 70 characters:

  `(defvar ,_ ,_
     ,(and (pred stringp)
           s
           (guard (< 70 (length (car (split-string s "\n")))))))

Put simply, el-search is a tool to match representations of
symbolic expressions written in a buffer or file.  Most of the
time, but not necessarily, this is Elisp code.  El-search has no
semantic understanding of the meaning of these s-exps as a program.
For example, if you define a macro `my-defvar' that expands to
`defvar' forms, the pattern `(defvar ,_) will not match any
equivalent `my-defvar' form, it just matches any lists of two
elements with the first element being the symbol `defvar'.

You can define your own pattern types with `el-search-defpattern'
which is analogue to `defmacro'.  See C-h f `el-search-pattern' for
a list of predefined additional pattern types, and C-h f pcase for
the basic pcase patterns.

Some more pattern definitions can be found in the file
"el-search-x" which is part of this package but not automatically
loaded.


Multi Searching
===============

"el-search" is capable of performing "multi searches" - searches
spanning multiple files or buffers.  When no more matches can be
found in the current file or buffer, the search automatically
switches to the next.  Examples for search commands starting a
multi search are `el-search-buffers' (search all living elisp mode
buffers), `el-search-directory' (search all elisp files in a
specified directory), `el-search-emacs-elisp-sources' (search all
Emacs elisp sources) and `el-search-dired-marked-files'.  Actually,
every search is internally a multi search.

You can pause any (multi) search by just doing something different
(no quitting is needed), the state of the search is automatically
saved.  You can later continue searching by calling
`el-search-jump-to-search-head' (C-J): this command jumps to the
last match and re-activates the search.

`el-search-continue-in-next-buffer' (C-N) skips all remaining
matches in the current buffer and continues searching in the next
buffer.  `el-search-skip-directory' even skips all subsequent files
under a specified directory.


El-Occur
========

To get an occur-like overview buffer, you can use the usual
commands to initiate a search.  You can either hit C-O from a
pattern prompt instead of RET to confirm your input and start the
search as noninteractive occur search.  Alternatively, you can
always call `el-search-occur' (C-O) to start an occur for the
latest started search.

The *El Occur* buffer uses an adjusted emacs-lisp-mode.  RET on a
match gives you a pop-up window displaying the position of the
match in that buffer or file.  With S-tab you can (un)collapse all
file sections like in `org-mode' to see only file names and the
number of matches, or everything.  Tab folds and unfolds
expressions (this uses hideshow; initially, all expressions are
folded to one line) and sections at the beginning of headlines.


Multiple multi searches
=======================

Every search is collected in a history.  You can resume older
searches from the position of the last match by calling
`el-search-jump-to-search-head' (C-J) with a prefix argument.  That
let's you select an older search to resume and switches to the
buffer and position where this search had been suspended.


Query-replace
=============

You can replace expressions with command `el-search-query-replace'.
You are queried for a pattern and a replacement expression.  For
each match of the pattern, the replacement expression is evaluated
with the bindings created by pattern matching in effect, and
printed to a string to produce the replacement.

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
without moving, SPC or n to go to the next match and ! to replace
all remaining matches automatically.  q quits.  And ? shows a quick
help summarizing all of these keys.

It is possible to replace a match with more than one expression
using "splicing mode".  When it is active, the replacement
expression must evaluate to a list, and is spliced into the buffer
for any match.  Use s from the prompt to toggle splicing mode in an
`el-search-query-replace' session.


Multi query-replace
===================

To query-replace in multiple files or buffers at once, call
`el-search-query-replace' directly after starting a search whose
search domain is the set of files and buffers you want to treat.
Answer "yes" to the prompt asking whether you want the started
search drive the query-replace.  The user interface is
self-explanatory.

It is always possible to resume an aborted query-replace session
even if you did other stuff in the meantime (including other
`el-search-query-replace' invocations).  Since internally every
query-replace is driven by a search, call
`el-search-jump-to-search-head' (maybe with a prefix arg) to make
that search current, and invoke `el-search-query-replace' (with the
default bindings, this would be C-J C-%).  This will continue the
query-replace session from where you left.


Advanced usage: Replacement rules for semi-automatic code rewriting
===================================================================

When you want to rewrite larger code parts programmatically, it can
often be useful to define a dedicated pattern type to perform the
replacement.  Here is an example:

You heard that in many situations, `dolist' is faster than an
equivalent `mapc'.  You use `mapc' quite often in your code and
want to query-replace many occurrences in your stuff.  Instead of
using an ad hoc replacing rule, it's cleaner to define a dedicated
named pattern type using `el-search-defpattern'.  Make this pattern
accept an argument and use it to bind a replacement expression to a
variable you specify.  In query-replace, specify that variable as
replacement expression.

In our case, the pattern could look like this:

  (el-search-defpattern el-search-mapc->dolist (new)
    (let ((var  (make-symbol "var"))
          (body (make-symbol "body"))
          (list (make-symbol "list")))
      `(and `(mapc (lambda (,,var) . ,,body) ,,list)
            (let ,new `(dolist (,,var ,,list) . ,,body)))))

The first condition in the `and' performs the matching and binds
the essential parts of the `mapc' form to helper variables.  The
second, the `let', part, binds the specified variable NEW to the
rewritten expression - in our case, a `dolist' form is constructed
with the remembered code parts filled in.

Now after this preparatory work, for `el-search-query-replace' you
can simply specify (literally!) the following rule:

  (el-search-mapc->dolist repl) -> repl



Bugs, Known Limitations
=======================

- Replacing: in some cases the read syntax of forms is changing due
to reading-printing.  "Some" because we can handle this problem in
most cases.

- Similarly: Comments are normally preserved (where it makes
sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)

in a content like

  (foo
    a
    ;; comment
    b)

the comment will be lost.

- Emacs bug#29857: 27.0.50; error: "Loading `nil': old-style
  backquotes detected!"


 Acknowledgments
 ===============

Thanks to Stefan Monnier for corrections and advice.


BUGS:

- l is very slow for very long lists.  E.g. C-S-e (l "test")


TODO:

- The default keys are not available in the terminal

- Make searching work in comments, too? (->
  `parse-sexp-ignore-comments').  Related: should the pattern
  `symbol' also match strings that contain matches for a symbol so
  that it's possible to replace occurrences of a symbol in
  docstrings?

- Port this package to non Emacs Lisp modes?  How?  Would it
  already work using only syntax tables, sexp scanning and
  font-lock?

- Replace: pause and warn when replacement might be wrong
  (ambiguous reader syntaxes; lost comments, comments that can't
  non-ambiguously be assigned to rewritten code)

- There could be something much better than pp to format the
  replacement, or pp should be improved.


NEWS:

Please see the NEWS file in this directory.