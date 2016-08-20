   Resize a frame.  In particular, fit a frame to its buffers.

 Commands and user options (variables) are provided here to resize
 (shrink-wrap) a frame to fit its displayed buffers, its selected
 buffer, or the `fill-column' width.

 The command to fit a frame is `fit-frame'.  The main user options
 for this command are `fit-frame-inhibit-fitting-flag' and
 `fit-frame-max-*[-percent]'.  You can use a prefix argument to
 control the behavior of command `fit-frame'.

 To take full advantage of the functionality provided here, load
 the companion library `autofit-frame.el', to modify primitives
 `display-buffer' and `switch-to-buffer' so they automatically fit
 all frames that have a single window.  Library `autofit-frame.el'
 loads library `fit-frame.el'.

 Put this in your initialization file (`~/.emacs'):

   (require 'fit-frame)
   (add-hook 'after-make-frame-functions 'fit-frame)

 The second line here causes newly created frames to be fitted to
 their buffer.  Even if you load library `autofit-frame.el', you
 will still need to do this, because `display-buffer' and
 `switch-to-buffer' are not called when a new frame is created.

 Command `fit-frame' does *not* take the following into account,
 when determining the proper frame size:

  - font sizes, other than the default frame font
  - characters that have special widths

 NOTE: If you also use library `frame-cmds.el', and you are on MS
 Windows, then load that library after `fit-frame.el'.  This is
 because the commands `maximize-frame' and `restore-frame' defined
 there are more general and non-Windows-specific.

 Suggested key bindings:

  (global-set-key [(control ?x) (control ?_)] 'fit-frame)
  (global-set-key [vertical-line down-mouse-1]
                  'fit-frame-or-mouse-drag-vertical-line)

 Customize the menu-bar.  Uncomment this to try it out.

  (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
  (define-key global-map [menu-bar frames]
    (cons "Frames" menu-bar-frames-menu)))
  (define-key menu-bar-frames-menu [fit-frame]
    '("Fit This Frame" . fit-frame))


 Commands defined here:

   `fit-frame', `fit-frame-or-mouse-drag-vertical-line',
   `fit-frame-maximize-frame', `fit-frame-minimize-frame',
   `fit-frame-restore-frame', `fit-frame-to-image',
   `maximize-frame', `minimize-frame', `restore-frame',

 User options (variables) defined here:

   `fit-frame-crop-end-blank-flag', `fit-frame-empty-height',
   `fit-frame-empty-special-display-height',
   `fit-frame-empty-special-display-width',
   `fit-frame-empty-width', `fit-frame-fill-column-margin',
   `fit-frame-inhibit-fitting-flag', `fit-frame-max-height',
   `fit-frame-max-height-percent', `fit-frame-max-width',
   `fit-frame-max-width-percent', `fit-frame-min-height',
   `fit-frame-min-width', `fit-frame-skip-header-lines-alist'.

 Non-interactive functions defined here:

   `fit-frame-fringe-width', `fit-frame-max-frame-size',
   `fit-frame-max-height', `fit-frame-max-width',
   `fit-frame-max-window-size', `fit-frame-same-column-windows',
   `fit-frame-same-row-windows', `fit-frame-thumbnail-factor'.


 See also these files for other frame commands:

    `autofit-frame.el' - See above.

    `frame-cmds.el' - Various frame and window commands, including
                      commands to incrementally resize frames and
                      better, non-Windows-specific commands to
                      maximize and restore frames.

    `doremi-frm.el' - Incrementally adjust frame properties
                      using arrow keys and/or mouse wheel.

 This file was formerly called `shrink-fit.el', then
 `resize-frame.el', and command `fit-frame' was formerly called
 `shrink-frame-to-fit', then `resize-frame'.

 TO DO:

 Emacs needs a command similar to `fit-frame' for windows, that is,
 a command that will fit the existing windows of a frame to their
 buffers, as well as possible.  That could be then be used in
 combination with `fit-frame'.
