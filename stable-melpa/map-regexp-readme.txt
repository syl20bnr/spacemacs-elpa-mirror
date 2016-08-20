This library defines several forms that search forward from
point for a regular expression and call a function for each
match to do something with the match data.

Equivalents of `mapc', `mapcar', and `cl-mapcan' are defined.
Anaphoric variants that expect an expression instead of a function
are also available.  Instead an expression `mr-amapcar-regexp'
also accepts an integer (or list of integers); it then returns a
list of match strings (resp. a list of lists of match strings).

If that isn't enough use `mr-loop-regexp' which supports all of
`cl-loop's accumulation clauses.
