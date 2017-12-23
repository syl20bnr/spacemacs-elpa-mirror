This is a package for sending code to the IPython interpreter.
It provides functionality similar to the `python-shell-send-*'
functions in python.el, but is able to send code regions
containing IPython magic (such as `!ls' or `%timeit'),
whereas python.el only has limited support for this.

The functions provided by ipython-shell-send are
`ipython-shell-send-region', `ipython-shell-send-buffer',
and `ipython-shell-send-defun'. They are essentially equivalent
to their `python-shell-send-*' equivalents in `python.el',
except better able to handle IPython magic.

Note to use the ipython-shell-send, you must make sure
to start an IPython shell when calling `run-python'.
