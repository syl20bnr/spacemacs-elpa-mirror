shx ("shell-extras") extends comint-mode: it parses markup in the output
stream, enabling plots and graphics to be embedded, and adds command-line
functions which plug into Emacs (e.g., use :e <filename> to edit a file).

See <https://github.com/riscy/shx-for-emacs/blob/master/README.org> for more.

This version tested with Emacs 25.2.1

Manual install:

1. Move shx.el to a directory in your load-path or add this to your .emacs:
   (add-to-list 'load-path "~/path/to/this-file/")
2. Add this line to your .emacs:
   (require 'shx)

Type M-x shx RET to create a new shell session using shx.  If you like shx,
you can enable shx in every comint-mode buffer with (shx-global-mode 1).

Use M-x customize-group RET shx RET to see customization options.
