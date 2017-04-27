lcs.el is a library for other Emacs Lisp programs not useful by
itself.

This library provides functions to find the Longest Common Sequence
(LCS) of two sequences. This is used to create a unified diff of to
two lists. See `lcs-unified-diff' for a useful function to be
called.

The code is more or less a literal translation of (part of)
Gauche's util/lcs.scm module to Emacs Lisp.
