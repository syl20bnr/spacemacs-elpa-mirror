This package provides `syntax-subword' minor mode, which extends
`subword-mode' to make word editing and motion more fine-grained.
Basically, it makes syntax changes, CamelCaseWords, and the normal
word boundaries the boundaries for word operations.  Here's an
example of where the cursor stops using `forward-word' in
`emacs-lisp-mode':

(defun FooBar (arg) "doc string"
|     |      |    |     |      |  standard
|     |   |  |    |     |      |  subword-mode
||    ||  |  |||  ||||  ||     || syntax-subword-mode
||     |      ||  | ||   |     |  vim

As you can see, syntax boundaries are places where the syntax
changes, i.e. we change from a bracket to a keyword, to a space, to
an argument, to a space, etc.  This makes word movement much more
fine-grained, to the point that you almost never need to operate by
single characters anymore.  Vim's word operations are similar to
this mode's.

Stops on spaces can be eliminated by setting
`syntax-subword-skip-spaces' to non-nil.
