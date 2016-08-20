To use this, put something like the following in your .emacs:

    (when (and (not window-system)
               (string-match "^xterm" (getenv "TERM")))
      (require 'xterm-title)
      (xterm-title-mode 1))

This package requires the function `format-mode-line', which does not
appear in Emacs 21.3 or earlier versions.  In fact, as of 2004-06-21
only the Emacs development sources in CVS have this function.

Updates of xterm-title.el may be retrieved via
http://www.splode.com/~friedman/software/emacs-lisp/
