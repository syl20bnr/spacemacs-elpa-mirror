   Extensions to `window.el'.


 ***** NOTE: The following functions defined in `window.el' have
             been REDEFINED HERE:

 `count-windows' -
    Only use arg MINIBUF if current frame has a minibuffer.

 `special-display-popup-frame' (Emacs 24+) - Fit the frame.

 `quit-window' - Call `delete-window' inside `condition-case'.

 This file should be loaded after loading the standard GNU file
 `window.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "window" '(require 'window+))
