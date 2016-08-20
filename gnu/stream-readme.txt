This library provides an implementation of streams. Streams are
implemented as delayed evaluation of cons cells.

Functions defined in `seq.el' can also take a stream as input.

streams could be created from any sequential input data:
- sequences, making operation on them lazy
- a set of 2 forms (first and rest), making it easy to represent infinite sequences
- buffers (by character)
- buffers (by line)
- buffers (by page)
- IO streams
- orgmode table cells
- ...

All functions are prefixed with "stream-".
All functions are tested in test/automated/stream-tests.el

Here is an example implementation of the Fibonacci numbers
implemented as in infinite stream:

(defun fib (a b)
 (stream-cons a (fib b (+ a b))))
(fib 0 1)