shx or "shell-extras" extends comint-mode.  It parses simple markup in the
output stream (enabling plots and graphics to be embedded in the shell) and
adds several command-line functions which plug into Emacs (for example, use
:e <filename> to edit a file).

See <https://github.com/riscy/shx-for-emacs/blob/master/README.org> for more.

This version tested with Emacs 25.2.1

Manual install:

1. Move shx.el to a directory in your load-path or add
   this to your .emacs:
   (add-to-list 'load-path "~/path/to/this-file/")
2. Next add this line to your .emacs:
   (require 'shx)

By default, shx runs automatically in all comint-mode buffers, but you
can always use M-x shx RET to create a new shell session using shx.

Use M-x customize-group RET shx RET to see customization options.
