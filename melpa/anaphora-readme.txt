Quickstart

    (require 'anaphora)

    (awhen (big-long-calculation)
      (foo it)      ; `it' is provided as
      (bar it))     ; a temporary variable

    ;; anonymous function to compute factorial using `self'
    (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))

Explanation

Anaphoric expressions implicitly create one or more temporary
variables which can be referred to during the expression.  This
technique can improve clarity in certain cases.  It also enables
recursion for anonymous functions.

To use anaphora, place the anaphora.el library somewhere
Emacs can find it, and add the following to your ~/.emacs file:

    (require 'anaphora)

The following macros are made available

    `aand'
    `ablock'
    `acase'
    `acond'
    `aecase'
    `aetypecase'
    `aif'
    `alambda'
    `alet'
    `aprog1'
    `aprog2'
    `atypecase'
    `awhen'
    `awhile'
    `a+'
    `a-'
    `a*'
    `a/'

The following macros are experimental

    `anaphoric-set'
    `anaphoric-setq'

See Also

    M-x customize-group RET anaphora RET
    http://en.wikipedia.org/wiki/On_Lisp
    http://en.wikipedia.org/wiki/Anaphoric_macro

Notes

Partially based on examples from the book "On Lisp", by Paul
Graham.

When this library is loaded, the provided anaphoric forms are
registered as keywords in font-lock.  This may be disabled via
customize.

Compatibility and Requirements

    GNU Emacs version 24.4-devel     : yes, except macros marked experimental
    GNU Emacs version 24.3           : yes, except macros marked experimental
    GNU Emacs version 23.3           : yes, except macros marked experimental
    GNU Emacs version 22.2           : yes, except macros marked experimental
    GNU Emacs version 21.x and lower : unknown

Bugs

TODO

    better face for it and self

License

All code contributed by the author to this library is placed in the
public domain.  It is the author's belief that the portions adapted
from examples in "On Lisp" are in the public domain.  At least 10
lines of code have been adapted from the Emacs 'cl package (in the
function `anaphoric-setq').

Regardless of the copyright status of individual functions, all
code herein is free software, and is provided without any express
or implied warranties.
