 This enhances the functionality of library `dired-details.el'.


   NOTE: If you use Emacs 24.4 or later, and if you use library
         `dired+.el', then you do not need this library or library
         `dired-details.el'.

         Instead, you can use `dired-hide-details-mode'.  Library
         `dired+.el' enhances `dired-hide-details-mode' to give it
         the same features as `dired-details+.el' provides.  In
         that case, use `(require 'dired+.el)', and use option
         `diredp-hide-details-propagate-flag' instead of option
         `dired-details-propagate-flag'.


 `dired-details+.el' enhances `dired-details.el' in these ways:

 1. It shrink-wraps Dired's frame whenever you show or hide
    details.  For this enhancement, you will need library
    `autofit-frame.el'.

 2. It updates the listing whenever you create new files or
    directories or rename them.

 3. It adds user option `dired-details-propagate-flag' which, if
    non-nil, propagates the last state you chose to the next Dired
    buffer you open.

 4. It binds both `)' and `(' to `dired-details-toggle'.

 Perhaps #2 corresponds to this TO-DO item in `dired-details.el':

   * add a hook for dired-add-file to hide new entries as necessary


 ***** NOTE: The following function defined in `dired-details.el'
             has been REDEFINED HERE:

 `dired-details-activate' - If `dired-details-propagate-flag' is
                            non-nil, then use the last state.


 Put this in your initialization file (~/.emacs):

  (require 'dired-details+)

 I also recommend customizing `dired-details-hidden-string' to use
 the value "" instead of the default "[...]" - less wasted space.

 Note: This library also calls `dired-details-install', activates
 show/hide and binds key `)'.
