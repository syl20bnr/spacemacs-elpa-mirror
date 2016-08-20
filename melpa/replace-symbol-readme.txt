This file implements a set of functions for replacing symbols
within a sexp or a buffer. A symbol for this purpose is anything
for which (progn (forward-sexp) (backward-sexp)) is idempotent and
for which (progn (down-list) (backward-up-list)) is *not*
idempotent. It is case sensitive at the moment, but that may change
in the future.

I have used this to rename variables, functions, types, etc. in
both Lisp and C. Your mileage may of course vary.

This file was written by Brian Mastenbrook (brian AT mastenbrook
DOT net) and is placed in the public domain.

M-x replace-symbol-in-sexp from to
M-x replace-symbol-in-buffer from to
