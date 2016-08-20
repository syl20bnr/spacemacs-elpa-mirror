   Extensions to `mouse.el'.

 Command `mouse-flash-position' highlights the character after the
 mouse pointer position, even as you drag it.  This can help make
 it clearer exactly where a `yank' will occur when you use
 `mouse-2'.  When you press `mouse-2', if the highlighted position
 is not exactly what you want, just keep `mouse-2' held while you
 move to the right location.  To enable this behavior, bind
 `mouse-flash-position' to `down-mouse-2'.

 Command `mouse-flash-position-or-M-x' is the same thing as
 `mouse-flash-position', except that it has a special behavior in
 the echo area (that is, the minibuffer space when the minibuffer
 is inactive).  In the echo area, it calls `M-x'.  To enable this
 behavior, bind `mouse-flash-position-or-M-x' to `down-mouse-2'.

 Command `mouse-scan-lines' tracks the mouse position, highlighting
 the line at that position.  It is handy in buffers like Dired that
 are essentially tables with columns - it helps you to align
 entries that are in the same row.

 Command `mouse-scan-lines-or-M-:' is the same thing as
 `mouse-scan-lines', except that it has a special behavior in the
 echo area.  In the echo area, it calls `M-:'.  To enable this
 behavior, bind `mouse-scan-lines-or-M-:' to `S-down-mouse-2'.

 See also library `second-sel.el' for enhancements to
 `mouse-drag-secondary' and `mouse-secondary-save-then-kill' that
 use a separate ring, `secondary-selection-ring', instead of the
 `kill-ring'.


 Faces defined here:

   `mouse-flash-position', `mouse-scan-lines'.

 Commands defined here:

   `mouse-flash-position', `mouse-flash-position-or-M-x',
   `mouse-M-:', `mouse-scan-lines', `mouse-scan-lines-or-M-:'.

 Non-interactive functions defined here:

   `mouse-flash-posn-track', `mouse-move-flash-posn-overlay'.

 Constants defined here:

   `mouse-flash-posn-overlay', `mouse-scan-lines-overlay'.


 ***** NOTE: The following functions defined in `mouse.el' have
             been REDEFINED HERE:

 `mouse-drag-region'     - If click echo area and `*Messages*' is
                           displayed, do `M-x', delete `*Messages*'
 `mouse-tear-off-window' - Don't delete window if it is alone in
                           frame.  Instead, clone frame and window.
 `mouse-yank-secondary' - Error if (x-get-selection 'SECONDARY)=nil


 Do this in your init file (~/.emacs or ~/_emacs):

  (require 'mouse+)


 Suggested bindings:

  The first sexp is NECESSARY for Emacs 24 or later, if you want to
  take advantage of the `mouse-drag-region' behavior defined here
  wrt buffer `*Messages*' and `M-x'.

  ;; Do not use `view-echo-area-messages' for `mouse-1'.   Use
  ;; version of `mouse-drag-region' defined here, which does more.
  (when (> emacs-major-version 23)
    (define-key minibuffer-inactive-mode-map [down-mouse-1] nil)
    (define-key minibuffer-inactive-mode-map [mouse-1]      nil))

  (global-set-key [down-mouse-2]        'mouse-flash-position-or-M-x)
  (global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
  (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
