Sequence-manipulation functions that complement basic functions
provided by subr.el.

All functions are prefixed with "seq-".

All provided functions work on lists, strings and vectors.

Functions taking a predicate or iterating over a sequence using a
function as argument take the function as their first argument and
the sequence as their second argument.  All other functions take
the sequence as their first argument.

All functions are tested in test/automated/seq-tests.el
