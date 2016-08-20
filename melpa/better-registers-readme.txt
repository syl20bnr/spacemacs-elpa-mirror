This package provides a row of commands, and keyboard shortcuts to
make the powerful concept of registers even more powerful (most
important it lets you save macros there), and to save them to a
file. It is collected as a minor mode.

The code requires the cl.el (common-lisp-wannabe extensions) package
It is not tested under xemacs

Installation:

dump this file somwhere on your load path. Then insert the
following into your .emacs (or other file loaded at startup) (of
course removing the ;;'s

Better registers!
(require 'better-registers)
(better-registers-install-save-registers-hook)
(load better-registers-save-file)

The last two line are unnecessary if you do not want to have your
registers contents saved across sessions.

Note that this package is pretty harsh to your std shortcuts you
probably should edit them (or at least see them through this file
to suit your needs. I e.g. personally never used backwards searches,
so C-r was doomed in my hands. I use only enter for indented
linebreaks, so C-j also seemed as a nice candidate for something
more useful - you might want to bind:
(global-set-key "\r" 'newline-and-indent)
