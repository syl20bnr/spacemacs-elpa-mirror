   Extensions to `novice.el'.


 ***** NOTE: The following functions defined in `novice.el' have
             been REDEFINED HERE:

 `disable-command', `enable-command' -
    These now use `completing-read' in the interactive spec, with,
    as default, `symbol-nearest-point'.


 This file should be loaded after loading the standard GNU file
 `novice.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "novice" '(require 'novice+))
