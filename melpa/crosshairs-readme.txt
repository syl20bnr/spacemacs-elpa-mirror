 This library highlights the current line and the current column.
 It combines the features of libraries `hl-line.el', `hl-line+.el',
 and `col-highlight.el', which let you highlight the line or column
 individually.  See those libraries for more information, in
 particular for user options that affect the behavior.

 If you want the horizontal and vertical highlighting to look the
 same, then:

 1. Set option `col-highlight-vline-face-flag' to non-nil.
 2. Customize faces `col-highlight' and `hl-line' to look the same.

 Command `crosshairs-mode' toggles this highlighting on and off.
 You can do this twice in succession to flash the crosshairs to
 show you where the cursor is.  An alternative way to
 flash-highlight is to use command `flash-crosshairs' (once).

 Command `crosshairs-highlight' shows crosshairs highlighting until
 your next action (next command, technically).  Command
 `crosshairs-unhighlight' turns off crosshairs highlighting due to
 `crosshairs-highlight'.

 With no prefix arg, command `crosshairs' is
 `crosshairs-highlight'.  With a prefix arg, it is
 `crosshairs-mode'.

 You can also have crosshairs highlighting come on automatically,
 when Emacs is idle.  Command `toggle-crosshairs-when-idle' toggles
 this mode.

 You can use command `flash-crosshairs' to do what its name says
 when you switch buffers or windows.  Here is how one user did it
 (rejoin the split URL):
 http://unix.stackexchange.com/questions/83167/emacs-finding-the-
 cursor-in-multiple-windows


 See also:

 * Library `hl-line+.el', which highlights the current line.

 * Library `col-highlight.el', which highlights the current column.

 * Library `cursor-chg.el' or library `oneonone.el', to change the
   cursor type when Emacs is idle.


 User options defined here:

   `crosshairs-mode'.

 Commands defined here:

   `crosshairs', `crosshairs-flash', `crosshairs-highlight',
   `crosshairs-mode', `crosshairs-toggle-when-idle',
   `crosshairs-unhighlight', `flash-crosshairs',
   `toggle-crosshairs-when-idle'.

 Internal variables defined here:

   `crosshairs-flash-col-timer', `crosshairs-flash-line-timer',
   `crosshairs-highlight-when-idle-p'.

 Suggested alternative key bindings:

     (global-set-key [(control ?+)] 'crosshairs)
  or (global-set-key [(control ?+)] 'crosshairs-mode)
  or (global-set-key [(control ?+)] 'crosshairs-flash)
