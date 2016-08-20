 This library extends standard library `hl-line.el' in these ways:

 1. As an alternative to turning on `hl-line' highlighting at all
    times, you can turn it on only when Emacs is idle.  To do that,
    use command `toggle-hl-line-when-idle' and customize
    `global-hl-line-mode' to nil.

 2. As another alternative, you can turn it on for only a few
    seconds.  To do that, use command `flash-line-highlight' and
    customize `global-hl-line-mode' to nil.

 3. It provides a face, `hl-line', that you can customize, instead
    of using option `hl-line-face'.

    I suggested #3 to the Emacs developers, and it was added to
    Emacs 22, but with a different default value.  If you use
    library `crosshairs.el', you might want to customize this to a
    value similar to what is used there, so that the horizontal and
    vertical highlights will be the same.

 4. Option `hl-line-overlay-priority' is provided, so that you can
    make hl-line highlighting appear on top of other overlay
    highlighting that might exist.

 To use this library, put this in your Emacs init file (~/.emacs):

   (require 'hl-line+) ; Load this file (it will load `hl-line.el')

 To turn on `global-hl-line-mode' only when Emacs is idle, by
 default, add this line also to your init file:

   (toggle-hl-line-when-idle 1) ; Highlight only when idle

 You can use command `toggle-hl-line-when-idle' to turn idle
 highlighting on and off at any time.  You can use command
 `hl-line-when-idle-interval' to change the number of idle seconds
 to wait before highlighting.


 See also these libraries:

 * `col-highlight.el', which highlights the current column.

 * `crosshairs.el', which highlights the current line and the
   current column, at the same time.  It requires libraries
   `col-highlight.el' and `hl-line+.el'.

 * `hl-spotlight.el', which extends `hl-line.el' by spotlighting
   the lines around the cursor.

 * `cursor-chg.el' or library `oneonone.el', to change the cursor
   type when Emacs is idle.


 Faces defined here:

   `hl-line'.

 User options defined here:

   `hl-line-flash-show-period',
   `hl-line-inhibit-highlighting-for-modes',
   `hl-line-overlay-priority'.

 Commands defined here:

   `flash-line-highlight', `hl-line-flash',
   `hl-line-toggle-when-idle', `hl-line-when-idle-interval',
   `toggle-hl-line-when-idle'.

 Non-interactive functions defined here:

   `hl-line-highlight-now', `hl-line-unhighlight-now'.

 Internal variables defined here:

   `hl-line-idle-interval', `hl-line-idle-timer',
   `hl-line-when-idle-p'.


 ***** NOTE: The following non-interactive functions defined in
             `hl-line.el' have been ADVISED HERE (to respect option
             `hl-line-overlay-priority'):

   `global-hl-line-highlight', `hl-line-highlight'.
