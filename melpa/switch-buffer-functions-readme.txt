This package provides a hook variable `switch-buffer-functions'.
This hook will be run when the current buffer has been changed.

When functions are hooked, they will be called with the previous buffer and
the current buffer.  For example, if you eval:

(add-hook 'switch-buffer-functions
          (lambda (prev cur) (message "%S -> %S" prev cur)))

then the message like "#<buffer *Messages*> -> #<buffer init.el<.emacs.d>>"
will be displayed to the echo area each time when you switch the current
buffer.
