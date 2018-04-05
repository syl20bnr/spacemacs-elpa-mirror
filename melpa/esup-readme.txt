The most recent code is always at http://github.com/jschaf/esup

`esup' profiles your Emacs startup time by examining all top-level
S-expressions (sexps).  `esup' starts a new Emacs process from Emacs to
profile each SEXP.  After the profiled Emacs is complete, it will exit and
your Emacs will display the results.

`esup' will step into `require' and `load' forms at the top level of a file,
but not if they're enclosed in any other statement.
