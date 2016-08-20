Here is an Emacs implementation of incremental git-blame.  When you
turn it on while viewing a file, the editor buffer will be updated by
setting the background of individual lines to a color that reflects
which commit it comes from.  And when you move around the buffer, a
one-line summary will be shown in the echo area.

Installation:

To use this package, put it somewhere in `load-path' (or add
directory with git-blame.el to `load-path'), and add the following
line to your .emacs:

   (require 'git-blame)

If you do not want to load this package before it is necessary, you
can make use of the `autoload' feature, e.g. by adding to your .emacs
the following lines

   (autoload 'git-blame-mode "git-blame"
             "Minor mode for incremental blame for Git." t)

Then first use of `M-x git-blame-mode' would load the package.

Compatibility:

It requires GNU Emacs 21 or later and Git 1.5.0 and up

If you'are using Emacs 20, try changing this:

           (overlay-put ovl 'face (list :background
                                        (cdr (assq 'color (cddddr info)))))

to

           (overlay-put ovl 'face (cons 'background-color
                                        (cdr (assq 'color (cddddr info)))))
