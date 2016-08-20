Extensions to `browse-kill-ring.el'.

 Put this in your init file (~/.emacs):

   (require 'browse-kill-ring+)


 Option `browse-kill-ring-alternative-ring' is an alternative
 selection ring to use, in addition to the `kill-ring'.  You can
 customize the value to any ring of strings you like.

 `browse-kill-ring' lets you use either ring as the selection ring
 to browse and paste.  You can even use both rings, in different
 `browse-kill-ring' display buffers.  In such a buffer (in
 `browse-kill-ring-mode'), `o' pops to the list for the other ring.

 If you have library `second-sel.el' in your `load-path'
 (recommended) it is loaded automatically when you load
 `browse-kill-ring+.el'.  In this case, by default
 `browse-kill-ring-alternative-ring' is the secondary selection
 ring.

 You can customize the set of commands to be recognized as yank
 commands and alternative yank commands - see options
 `browse-kill-ring-yank-commands' and
 `browse-kill-ring-alternative-yank-commands'.  The alternative
 yank commands are commands that yank using a different selection
 ring, for example, the secondary yank commands defined by library
 `second-sel.el'.

 Following a yank command or alternative yank command, `M-y' pops
 and yanks the appropriate type of selection.  A prefix arg N
 chooses the Nth previous selection in the ring.

 Otherwise (not following a yank or alternative yank), `M-y'
 browses the current selection ring.  A prefix arg switches to the
 other selection ring.  If you are in a `browse-kill-ring' buffer,
 then `M-y' switches to the other ring even without a prefix arg.

 If there is no alternative selection ring (e.g., you do not use
 library `second-sel.el'), then `M-y' either pops (following a
 yank) or browses (not following a yank) the `kill-ring'.

 FWIW, I customize `browse-kill-ring-quit-action' to be
 `browse-kill-ring-quit-deletes-window/frame'.  This is similar to
 the default behavior, except that if the window is dedicated, then
 the frame is deleted.

 Icicles offers an alternative: completion vs browsing.

   You might want to use the Icicles (library `icicles.el') binding
   of `M-y' as an alternative to the `browse-kill-ring+.el'
   behavior of `M-y'.  If you do that you can of course still use
   `browse-kill-ring+.el' to browse the selection rings.  Just bind
   `browse-kill-ring'.

   What Icicles offers is the ability to yank by completing against
   the selection rings, instead of browsing them.


 Commands defined here:

   `browse-kill-ring-copy-to-other-ring',
   `browse-kill-ring-switch-to-other-kill-ring',
   `toggle-browse-kill-ring-display-style'.

 User options defined here:

   `browse-kill-ring-alternative-push-function',
   `browse-kill-ring-alternative-ring',
   `browse-kill-ring-alternative-yank-commands',
   `browse-kill-ring-yank-commands'.

 Non-interactive functions defined here:

   `browse-kill-ring-current-ring',
   `browse-kill-ring-quit-deletes-window/frame',
   `browse-kill-ring-remove-dups',
   `browse-kill-ring-target-overlay-at'.

 Internal variables defined here:

   `browse-kill-ring-current-ring', `browse-kill-ring-mode-map'.


 ***** NOTE: The following functions defined in `browse-kill-ring.el'
             have been REDEFINED HERE:

   `browse-kill-ring', `browse-kill-ring-append-insert-and-move',
   `browse-kill-ring-current-string',
   `browse-kill-ring-default-keybindings',
   `browse-kill-ring-delete', `browse-kill-ring-do-append-insert',
   `browse-kill-ring-do-insert',
   `browse-kill-ring-insert-and-move',
   `browse-kill-ring-do-prepend-insert', `browse-kill-ring-edit',
   `browse-kill-ring-edit-finish', `browse-kill-ring-forward',
   `browse-kill-ring-prepend-insert-and-move',
   `browse-kill-ring-mode' `browse-kill-ring-setup'.


 ***** NOTE: The following functions defined in `second-sel.el'
             have been ADVISED HERE:

   `add-secondary-to-ring'.

 Key bindings defined here for `browse-kill-ring-mode-map':

    `t'     - `toggle-browse-kill-ring-display-style'
    `TAB'   - `browse-kill-ring-forward'
    `S-TAB' - `browse-kill-ring-previous'

 NOTE: This library automatically calls
 `browse-kill-ring-default-keybindings'.  If you do not want that,
 then comment out this call (at the end of the file).


 TO DO:

 `browse-kill-ring-edit': membership of selection text is not
 sufficient, since there can be duplicates.
