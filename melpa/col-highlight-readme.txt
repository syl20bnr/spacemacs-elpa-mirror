 This library highlights the current column.  When you move the
 cursor, the highlighting follows (tracks the cursor), as long as
 the highlighting stays on.

 Command `column-highlight-mode' toggles this highlighting on and
 off.

 If you use `column-highlight-mode' twice in succession (I bind it
 to `C-+'), you can flash the highlighting to show you the current
 column temporarily.  An alternative way to flash-highlight is to
 use command `flash-column-highlight' (once).  It shows the
 highlighting for just a second or two (see option
 `col-highlight-period').

 You can also have current-column highlighting come on
 automatically, when Emacs is idle.  Command
 `toggle-highlight-column-when-idle' toggles this mode.  Command
 `col-highlight-set-interval' changes the number of idle seconds to
 wait before highlighting.

 You can use option `col-highlight-overlay-priority' to make the
 vline (i.e., column) highlighting appear on top of other overlay
 highlighting that might exist.

 You can use option `col-highlight-show-only' to restrict
 current-column highlighting to a section of text of a particular
 kind: paragaph, sentence, page, defun, etc.


 To use this file, you must also have library `vline.el'.
 Put this in your Emacs init file (~/.emacs):

   (require 'col-highlight) ; Load this file (and `vline')

 If you want to turn on continual current-column highlighting by
 default, then add this to your init file:

   (column-highlight-mode 1)

 If you want to turn on automatic idle highlighting of the current
 column, then add this to your init file:

   (toggle-highlight-column-when-idle 1)

 If you want to use a different wait interval, before idle
 highlighting begins, then set it in your init file using
 `col-highlight-set-interval':

   (col-highlight-set-interval 6) ; Wait 6 idle secs.

 Note that `column-highlight-mode' is intentionally a global minor
 mode.  If you want a local minor mode, so that highlighting
 affects only a particular buffer, you can use `vline-mode' (in
 `vline.el').


 See also:

 * Library `hl-line+.el', which offers the same functionality, but
   for the current line instead of the current column.

 * Library `crosshairs.el', which combines the features of
   `col-highlight.el' and `hl-line+.el', providing a crosshair
   highlighting effect.  It requires `col-highlight.el' and
   `hl-line+.el'.

 * Library `cursor-chg.el' or library `oneonone.el', to change the
   cursor type when Emacs is idle.

 User options defined here:

   `col-highlight-period', `column-highlight-mode',
   `col-highlight-overlay-priority', `col-highlight-show-only',
   `col-highlight-vline-face-flag'.

 Faces defined here:

   `col-highlight'.

 Commands defined here:

   `col-highlight-flash', `col-highlight-set-interval',
   `col-highlight-toggle-when-idle', `column-highlight-mode',
   `flash-column-highlight', `toggle-highlight-column-when-idle'.

 Non-interactive functions defined here:

   `col-highlight-highlight', `col-highlight-unhighlight'.

 Internal variables defined here:

   `col-highlight-face', `col-highlight-idle-interval',
   `col-highlight-idle-timer', `col-highlight-when-idle-p'.


 ***** NOTE: The following function defined in `vline.el' has
             been REDEFINED HERE:

   `vline-show' - Respect options `col-highlight-overlay-priority'
                  and `col-highlight-show-only'.
