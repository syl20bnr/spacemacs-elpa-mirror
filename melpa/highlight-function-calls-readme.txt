This package highlights function symbols in function calls.  This
makes them stand out from other symbols, which makes it easy to see
where calls to other functions are.  Optionally, macros and special
forms can be highlighted as well.  Also, a list of symbols can be
excluded from highlighting; by default, ones like +/-, </>, error,
require, etc. are excluded.  Finally, the `not' function can be
highlighted specially.

Just run `highlight-function-calls-mode' to activate, or you can
add that to your `emacs-lisp-mode-hook' to do it automatically.
