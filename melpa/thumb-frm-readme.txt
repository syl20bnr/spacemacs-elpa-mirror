   Shrink frames to a thumbnail size and restore them again.

 User option `thumfr-frame-parameters' defines the appearance of
 the thumbnail frames.  If you change it and set the new definition
 for the current session you can immediately see the effect in all
 of your thumbnail frames.

 By default, `thumfr-frame-parameters' gives you thumbnail frames
 that:

  * have no menu bar or tool bar
  * have a 6x6-pixel scroll bar
  * are 30% transparent when not selected (opaque when selected)

 Commands `thumfr-toggle-thumbnail-frame', `thumfr-thumbify-frame',
 and `thumfr-dethumbify-frame' thumbify and dethumbify an
 individual frame.  If option `thumfr-thumbify-dont-iconify-flag'
 is non-nil (the default), then keys (e.g. `C-z') that normally
 iconify and deiconify will instead thumbify and dethumbify.

 Command `thumfr-thumbify-other-frames', alias `thumfr-fisheye',
 shrinks all frames except the selected frame to a thumbnail size.
 The thumbnail frames are stacked from top to bottom, left to right
 on your display.  This provides a kind of "fisheye" view of the
 frames you are using.  Command `thumfr-dethumbify-all-frames'
 restores all thumbnail frames to full size.

 Command `thumfr-stack-thumbnail-frames' neatly stacks all of the
 thumbnail frames along a display edge.  You can use it at any
 time, and it is called automatically by `thumfr-fisheye'.  Which
 display edge to stack along (left, right, top, or bottom), and
 which direction (up, down, to-left or to-right), is determined by
 option `thumfr-stack-display-edge'.  The stacking order is
 determined by option `thumfr-sort-function'.  You can turn sorting
 on and off using command
 `thumfr-toggle-sort-thumbnail-frame-stack'.

 By default, this library provides thumbifying and dethumbifying as
 a *replacement* for iconifying and deiconifying.  Whenever option
 `thumfr-thumbify-dont-iconify-flag' is non-nil, loading the
 library changes the standard commands `iconify-frame' and
 `iconify-or-deiconify-frame' so that they use thumbnail frames
 instead of desktop icons.  To prevent this thumbnail behavior, you
 can customize `thumfr-thumbify-dont-iconify-flag' to nil.
 Alternatively, you can deactivate (`ad-deactivate') the advice
 imposed here on these functions, to restore their original
 behavior.

 The original behavior of commands `iconify-frame' and
 `iconify-or-deiconify-frame' is available using commands
 `thumfr-really-iconify-frame' and
 `thumfr-really-iconify-or-deiconify-frame'.  In particular, these
 commands can be used to iconify even if you bind [iconify-frame]
 in `special-event-map' (see below).

 You can iconify or deiconify all thumbnail frames (that is, only
 the thumbnail frames), to get them out of the way and bring them
 back.  Use commands `thumfr-iconify-thumbnail-frames' and
 `thumfr-deiconify-thumbnail-frames' to do this.

 Emacs built-in function `raise-frame' is redefined here to also
 dethumbify.  The original behavior of `raise-frame' is available
 in command `thumfr-only-raise-frame'.

 You can cycle among the visible frames in two ways, applying
 `thumfr-fisheye' to each in turn.  The first way is using commands
 `thumfr-fisheye-previous-frame' and `thumfr-fisheye-next-frame'
 (which you can bind to, for instance, `C-M-prior' and `C-m-next').
 The second way is using command `thumfr-doremi-thumbnail-frames+'
 plus the arrow keys or mouse wheel.

 To be able to use `thumfr-doremi-thumbnail-frames+', you need
 library `doremi-frm.el' (which in turn requires libraries
 `hexrgb.el', `faces+.el', `doremi.el', and perhaps `ring+.el',
 depending on your Emacs version).  The only libraries strictly
 required by `thumb-frm.el' are `frame-fns.el' and `frame-cmds.el'.

 A more comprehensive, lower-level way of substituting thumbifying
 for iconifying is to do the following in your init file:

   (define-key special-event-map [iconify-frame]
               'thumfr-thumbify-frame-upon-event)

 In effect, this thumbifies any frame as soon as it is iconified,
 no matter how it was iconified.  In particular, this will let you
 use the window-manager "minimize" frame button (usually at the
 upper left or right frame corner) to thumbify.

 Be aware of this fact/feature, which is true of any binding on
 keymap `special-event-map': The event interrupts any key sequence
 in progress, to invoke its command, and then the key sequence as a
 whole is processed, ignoring the special event.

 For example, assuming that event `iconify-frame' occurs whenever
 you use a window-manager button (e.g. in the title bar) or menu
 item to minimize (iconify) a frame, if you do `C-x
 <click-the-minimize-button> b', then: (1) as soon as you click the
 minimize button the frame is thumbified, and (2) when you hit `b'
 the key sequence `C-x b' is processed (e.g. `switch-to-buffer'
 prompts you for a buffer name).  I mention this not because it is
 a useful feature but in order to avoid confusion.

 If you do bind `thumfr-thumbify-frame-upon-event' to
 `iconify-frame', be aware that `thumfr-thumbify-dont-iconify-flag'
 will no longer have any effect: Emacs will *always* thumbify
 instead of iconify (except for functions `really-iconify-*frame',
 which are designed to counter this).  If you try this behavior and
 then wish to cancel it, to once again allow iconification, use
 this code:

 In Emacs 20 or prior:
   (define-key special-event-map [iconify-frame] 'ignore-event)

 In Emacs 21 or later:
   (define-key special-event-map [iconify-frame] 'ignore)


 Other user options (variables) not mentioned above are these:

   `thumfr-font-difference'             - Zoom of thumbnail frames.
   `thumfr-rename-when-thumbify-flag'   - Rename frame to buffer.
   `thumfr-stack-display-edge'         - Display edge for stacking.
   `window-mgr-title-bar-pixel-width'   - Thickness of title bar.


 IMPORTANT:

   Thumbnail frames are *FULLY FUNCTIONAL*.  In particular, their
   buffers are *NOT* read-only in any way.  You can edit their
   buffers normally, even if you can't see what you're doing :-).

   You can also scroll and search their buffers.  You can thumbify
   the frame of a progressive output buffer, to monitor the output
   from 4,000 meters as it is produced.  In other words, you can
   interactive with thumbnail frames in the usual ways.  They are
   not special; they are just small.

 Functions defined here:

   `set-frame-parameter' (Emacs < 22),
   `thumfr-culled-thumbnail-frames',
   `thumfr-deiconify-thumbnail-frames', `thumfr-delete-if-not',
   `thumfr-dethumbify-all-frames', `thumfr-dethumbify-frame',
   `thumfr-doremi-thumbnail-frames+', `thumfr-fisheye',
   `thumfr-fisheye-next-frame', `thumfr-fisheye-previous-frame',
   `thumfr--frame-parameters-:set-function',
   `thumfr-iconify-thumbnail-frames', `thumfr-only-raise-frame',
   `thumfr-nset-difference', `thumfr-next-stack-position',
   `thumfr-really-iconify-frame',
   `thumfr-really-iconify-or-deiconify-frame', `thumfr-remove-if',
   `thumfr-remove-if-not', `thumfr-set-difference',
   `thumfr-stack-thumbnail-frames', `thumfr-sort-by-name',
   `thumfr-sort-by-window-id', `thumfr-thumbify-frame',
   `thumfr-thumbnail-frame-p', `thumfr-thumbnail-frames',
   `thumfr-thumbify-frame-upon-event',
   `thumfr-thumbify-other-frames', `thumfr-thumfr-parameter-p',
   `thumfr-toggle-sort-thumbnail-frame-stack',
   `thumfr-toggle-thumbnail-frame'.


 User options (variables) defined here:

   `thumfr-font-difference', `thumfr-thumbify-dont-iconify-flag',
   `thumfr-frame-parameters', `thumfr-rename-when-thumbify-flag',
   `thumfr-sort-function', `thumfr-stack-display-edge',
   `window-mgr-title-bar-pixel-width'.


 Internal variables defined here:

   `thumfr-last-row-show', `thumfr-last-sort-function',
   `thumfr-next-stack-xoffset', `thumfr-next-stack-yoffset'.


 ***** NOTE: These EMACS functions have been ADVISED or REDEFINED:

 `iconify-frame' - Thumbify if `thumfr-thumbify-dont-iconify-flag'.
 `iconify-or-deiconify-frame' - Similar to `iconify-frame', plus
                                dethumbify if already a thumbnail.
 `raise-frame' - Dethumbify also, if a thumbnail.


 Put this in your init file (`~/.emacs'): (require 'thumb-frm)

 Suggested key bindings:

  (global-set-key [(shift mouse-3)]
                  'thumfr-toggle-thumbnail-frame)
  (global-set-key [(shift control mouse-3)]
                  'thumfr-thumbify-other-frames)
  (global-set-key [(shift control ?z)]
                  'thumfr-thumbify-other-frames)
  (global-set-key [(shift control ?p)]
                  'thumfr-fisheye-previous-frame)
  (global-set-key [(shift control ?n)]
                  'thumfr-fisheye-next-frame)
  (global-set-key [(control meta ?z)]
                  'thumfr-really-iconify-or-deiconify-frame)

  ;; Make the window-manager "minimize" button thumbify instead.
  (define-key special-event-map [iconify-frame]
              'thumfr-thumbify-frame-upon-event)

  ;; Add `thumfr-doremi-thumbnail-frames+' to the Do Re Mi commands
  ;; - see library `doremi-frm.el'.
  (unless (fboundp 'doremi-prefix)
    (defalias 'doremi-prefix (make-sparse-keymap))
    (defvar doremi-map (symbol-function 'doremi-prefix)
      "Keymap for Do Re Mi commands."))
  (define-key global-map "\C-xt"  'doremi-prefix)
  (define-key global-map "\C-xte"
              'thumfr-doremi-thumbnail-frames+) ; "Eye"

  Keep in mind also that if `thumfr-thumbify-dont-iconify-flag' is
  non-nil then keys bound to (de-)iconifying commands, such as
  `C-z', will instead (de)thumbify.  (This is not true, however, if
  `thumfr-thumbify-frame-upon-event' is bound to `iconify-frame'.
  Such a binding causes `thumfr-thumbify-dont-iconify-flag' not to
  have any effect - Emacs *always* thumbifies instead of
  iconifying, except for commands like `really-iconify-*frame'.)


 See also these libraries for other frame commands:

    `autofit-frame.el' - Automatically fit each frame to its
                         selected window.  Uses `fit-frame.el'.

    `fit-frame.el'     - 1) Fit a frame to its selected window.
                         2) Incrementally resize a frame.

    `doremi-frm.el'    - Incrementally adjust frame properties
                         using arrow keys and/or mouse wheel.

    `frame-cmds.el'    - Miscellaneous frame and window commands.

    `zoom-frm.el'      - Zoom a frame, so that its font becomes
                         larger or smaller.

 Acknowledgements (thanks):
   Michael Cadilhac [Michael.Cadilhac-@t-lrde.epita.fr] for a tip
     on using `discard-input' to effectively nullify
     `special-event-map' bindings (used in
     `really-iconify-[or-deiconify]frame').

 TO DO?:

    Make thumbnail frames read-only, to prevent inadvertent
    changes.  How to do so? Could make all buffers in frame's
    buffer-list r-o, but that would affect the buffer on
    non-thumbnail frames too.
