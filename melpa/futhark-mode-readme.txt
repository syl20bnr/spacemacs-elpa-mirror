Futhark is a small programming language designed to be compiled to
efficient GPU code.  This Emacs mode provides syntax highlighting and
conservative automatic indentation for Futhark source code.

Define your local keybindings in `futhark-mode-map'.  Add startup
functions to `futhark-mode-hook'.

Manual installation: To load futhark-mode automatically on Emacs
startup, put this file in your load path and require the mode,
e.g. something like this:

  (add-to-list 'load-path "~/.emacs.d/futhark-mode")
  (require 'futhark-mode)

In this case, you have to create the directory
"~/.emacs.d/futhark-mode" and store this file in that directory.

This will also tell your Emacs that ".fut" files are to be handled by
futhark-mode.
