 Commands `text-scale-decrease', `text-scale-increase', and
 `text-scale-adjust' (bound to `C-x C--', `C-x C-+', `C-x C-=', and
 `C-x C-0') let you resize the text in the current buffer by
 changing its scale factor.  When you shrink or enlarge the
 apparent text size this way, however, the window takes no notice
 of it.  In particular, although shrinking text can result in extra
 horizontal space at the right, window commands do not see this
 space as extra.

 With this library, user option `text-scale-resize-window' lets you
 automatically resize the selected window (horizontally,
 vertically, or both) when text is resized, so that the way the
 window fits the buffer text remains relatively constant.
 Shrinking the text in one window shrinks that window, giving more
 space to adjacent windows.

 If you also use library `fit-frame.el', then one-window frames
 also respond to text resizing by scaling.  If not, then the
 text-scale commands have no effect on frame size for one-window
 frames.

 See also:

 * Library `zoom-frm.el', which provides commands `zoom-in' and
   `zoom-out', which let you zoom the text in a buffer (as in text
   scaling) or the text in an frame.  In the latter case, the
   default font of the frame is enlarged or shrunk dynamically.

 * Library `doremi-frm.el', which provides commands
   `doremi-buffer-font-size+' and `doremi-frame-font-size+', which
   provide another way to zoom incrementally.

 To use library `face-remap+.el', put it in your `load-path' and
 put this sexp in your init file (~/.emacs):

  (require 'face-remap+)


 Options (user variables) defined here:

   `text-scale-resize-window'.


 ***** NOTE: The following standard functions defined in `face-remap.el'
             have been REDEFINED HERE:

   `text-scale-increase' -- Possibly resize the window or frame.
