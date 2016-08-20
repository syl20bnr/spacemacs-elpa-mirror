   Automatically resize one-window frames to fit.

 Functions are provided here to automatically resize each frame to
 fit its selected window, when there is no other window in the
 frame.  Some standard Emacs primitive functions are redefined to
 do this: `display-buffer' (prior to Emacs 23 only),
 `switch-to-buffer', and `pop-to-buffer' (prior to Emacs 23 only).

 Prior to Emacs 23, automatic frame resizing is also provided here
 implicitly for functions `switch-to-buffer-other-window' and
 `switch-to-buffer-other-frame', since they ultimately use
 `display-buffer', and so the version of it defined here.  (Command
 `switch-to-buffer' does not use `display-buffer' - it is redefined
 separately here, for all Emacs versions.)

 Put the following in your Emacs initialization file (`~/.emacs'),
 in order to provide for automatic frame resizing:

   (require 'autofit-frame)
   (add-hook 'after-make-frame-functions 'fit-frame)

 The second line here causes newly created frames to be fitted to
 their buffer (window).  Even if you load `auto-fit-frames.el', you
 will still need to do this, because `display-buffer' and so on are
 not called when a new frame is created.

 To automatically fit frames that show a temporary buffer in their
 sole window, add this to your initialization file also:

   (add-hook 'temp-buffer-show-hook                ; Emacs < 24.4
             'fit-frame-if-one-window 'append)

   (add-hook 'temp-buffer-window-show-hook         ; Emacs 24.4+
             'fit-frame-if-one-window 'append)

 User option (variable) `autofit-frames-flag' turns on and off the
 automatic resizing defined here.  Setting it to nil turns it off:
 (setq autofit-frames-flag nil).  You can also bind it to nil to
 temporarily inhibit frame resizing in Lisp code:

        (let ((autofit-frames-flag nil))...)


 New user option (variable) defined here: `autofit-frames-flag'.

 New function defined here (useful as a `temp-buffer-show-hook'):

   `fit-frame-if-one-window'.


 ***** NOTE: The following function defined in `window.el' has been
             REDEFINED HERE:

 `window--display-buffer-1' (Emacs 23+) -
    Resize frame to fit sole window if `autofit-frames-flag'.


 ***** NOTE: The following EMACS PRIMITIVES are REDEFINED HERE:

 `display-buffer' (prior to Emacs 23 only) -
    1) Use `read-buffer' in interactive spec.
    2) Resize frame to fit sole window if `autofit-frames-flag'
       (and provided buffer was not yet displayed).
    3) Raise the frame.
    4) Restore point in buffer - fixes unknown Emacs 22 bug.

 `pop-to-buffer' (prior to Emacs 23 only) -
    Use the `display-buffer' defined here.

 `switch-to-buffer' (all Emacs versions) -
    1) Use `read-buffer' in interactive spec.
    2) If current window is dedicated, then use another window.
    3) Resize frame to fit sole window if `autofit-frames-flag'
       (unless BUFFER is already the `current-buffer').


 This file loads file `fit-frame.el', which provides the main
 functionality behind the automatic frame resizing.  See it for
 user options to do such things as customize default frame sizes.

 The reason for separating the code here from that in
 `fit-frame.el' is to let you load that code but not load the code
 here, if you do not want to redefine Emacs primitives.

 This file was formerly called `shrink-fit-all.el', then
 `auto-resize-frames.el'.


 See also these files for other frame commands:

    `doremi-frm.el'    - Incrementally adjust frame properties
                         using arrow keys and/or mouse wheel.

    `frame-cmds.el'    - Miscellaneous frame and window commands.

    `thumb-frm.el'     - Shrink frames to a thumbnail size and
                         restore them again.

    `zoom-frm.el'      - Zoom a frame, so that its font becomes
                         larger or smaller.
