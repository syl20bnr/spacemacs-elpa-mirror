 Commands to zoom into and out of text.  They zoom a frame or a
 buffer, so that the text appears larger or smaller.

 Commands `zoom-in', `zoom-out', and `zoom-in/out' do both kinds of
 zooming.  They can behave like command `text-scale-adjust',
 zooming a buffer wherever it is displayed, or they can zoom an
 entire single frame (all of its windows).  Hit `C-u' at any time
 while using these commands to toggle between buffer and frame
 zooming.

 Because it is a more general replacement for `text-scale-adjust',
 I suggest you bind `zoom-in/out' to the keys bound by default to
 `text-scale-adjust': `C-x C-+', `C-x C-=', `C-x C--', and `C-x
 C-0'.

 It is also handy to use a mouse button or wheel for zooming, hence
 the mouse binding suggestions.  For example, binding `zoom-in' and
 `zoom-out' to mouse wheel events gives you the zooming effect you
 are perhaps used to in a Web browser.

 User option `zoom-frame/buffer' determines which kind of zooming
 (frame or buffer) is used by default.  You can customize this
 option, but (in Emacs 23 or later) you can also toggle it just by
 providing a prefix arg (`C-u') to `zoom-in/out', `zoom-in', or
 `zoom-out'.

 Note about saving changes made dynamically using the commands
 defined here:

   Some of the commands defined here change frame properties.  You
   can save any changes you have made, by using Customize.  To
   visit a Customize buffer of all unsaved changes you have made,
   use command `customize-customized'.

   Frame parameter changes, such as font size, can be saved for
   future use by all frames or all frames of a certain kind.  For
   that, you must change the frame parameters of the correponding
   frame-alist variable.

   There is no single variable for saving changes to parameters of
   the current frame.  Instead, there are several different
   frame-alist variables, which you can use to define different
   kinds of frames.  These include: `default-frame-alist',
   `initial-frame-alist', and `special-display-frame-alist'.  The
   complete list of such frame alist variables is available using
   function `frame-alist-var-names', defined in library
   `frame-cmds.el'.

   Example: Suppose you change the font size of a frame and want to
   make that the default font size for new frames in the future.
   You will need to update the value of variable
   `default-frame-alist' to use the `font' parameter setting of the
   changed frame.

   You can easily copy one or all parameter values from any given
   frame to any frame alist (such as `default-frame-alist'), by
   using the commands `set-frame-alist-parameter-from-frame' and
   `set-all-frame-alist-parameters-from-frame'.  Those commands are
   defined in library `frame-cmds.el'.


 Commands defined here:

   `toggle-zoom-frame', `zoom-all-frames-in',
   `zoom-all-frames-out', `zoom-frm-in', `zoom-frm-out',
   `zoom-frm-unzoom', `zoom-in', `zoom-in/out' (Emacs 23+),
   `zoom-out'.


 User options (variables) defined here:

   `frame-zoom-font-difference', `zoom-frame/buffer' (Emacs 23+).


 Put this in your init file (`~/.emacs'): (require 'zoom-frm)

 Suggested key bindings:

   Emacs 23 and later:

   (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
   (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
   (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
   (define-key ctl-x-map [(control ?0)] 'zoom-in/out)

   Any Emacs version:

   (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                       (vector (list 'control
                                     mouse-wheel-down-event))
                     [C-mouse-wheel])    ; Emacs 20, 21
                   'zoom-in)
   (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
     (global-set-key (vector (list 'control mouse-wheel-up-event))
                     'zoom-out))

   (global-set-key [S-mouse-1]    'zoom-in)
   (global-set-key [C-S-mouse-1]  'zoom-out)
   ;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
   (global-set-key [S-down-mouse-1] nil)

 Some of the commands are not autoloaded by default, because this
 library works with old as well as recent Emacs releases.  The
 commands that are not autoloaded are not usable in older releases.
 You can autoload such commands yourself.  For example, if you use
 Emacs 23 or later, you can add this to your init file, to autoload
 `zoom-in/out':

 (autoload 'zoom-in/out "zoom-frm"
           "Zoom current frame or buffer in or out" t)

 The first two of the mouse bindings mean that in Emacs 22 or later
 you can hold the Control key and rotate the mouse wheel to zoom in
 and out, just as you might do in a Web browser.

 (In Emacs 20 and 21, Control plus mouse wheeling zooms in, but to
 zoom out you need to use `C--' before wheeling with Control.  This
 is because Emacs 20 and 21 do not have separate events for the
 mouse wheel directions, and it is the prefix arg, not the wheel
 direction, that determines the effect.)


 See also these files for other frame commands:

    `autofit-frame.el' - Automatically fit each frame to its
                         selected window.  Uses `fit-frame.el'.

    `fit-frame.el'     - 1) Fit a frame to its selected window.
                         2) Incrementally resize a frame.

    `doremi-frm.el'    - Incrementally adjust frame properties
                         using arrow keys and/or mouse wheel.

    `frame-cmds.el'    - Miscellaneous frame and window commands.

    `thumb-frm.el'     - Shrink frames to a thumbnail size and
                         restore them again.
