   Extensions to `macros.el'.


 ***** NOTE: The following functions defined in `macros.el' have
             been REDEFINED HERE:

 `apply-macro-to-region-lines' -
    Make sure that `delete-selection-mode' is deactivated.  (See
    `delsel.el'.)  Otherwise, character self-insertion by the
    keyboard macro could cause the region to be deleted (replaced
    by the inserted text) when `apply-macro-to-region-lines' was
    finished (e.g. via a delete-selection `pre-command-hook').

 `insert-kbd-macro', `name-last-kbd-macro' -
    These functions now use `completing-read' in interactive spec,
    with, as default, `symbol-nearest-point'.


 This file should be loaded after loading the standard GNU file
 `macros.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "macros" '(require 'macros+))
