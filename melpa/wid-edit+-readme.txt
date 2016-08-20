 Extensions to standard library `wid-edit.el'.

 New widgets defined here:

   `conditional-key-definition', `key-definition'.

 Functions defined here:

   `widgetp-color-match', `widgetp-color-validate',
   `widgetp-define-key-from-key-def',
   `widgetp-display-Completions', `widgetp-keyboard-quit',
   `widgetp-key-def-set', `widgetp-remove-Completions',
   `widgetp-rgb-hex-string-p'.


 ***** NOTE: The following widgets defined in `wid-edit.el'
             have been REDEFINED HERE:

 `color' - Added :match and :validate values.
           Increased :size value to max color name length.

 `editable-field' - `C-g' also removes *Completions*.


 ***** NOTE: The following functions defined in `wid-edit.el' have
             been REDEFINED HERE:

 `widget-color-complete' - Made compatible with Emacs 20.
                           Don't use `facemenu-color-alist'.
                           Delete *Completions* window when done.
                           Keep focus in Customize frame.

 `widget-color-notify'   - Remove *Completions* if color changes.
