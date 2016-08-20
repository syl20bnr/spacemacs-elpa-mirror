Save and restore parameters of Emacs frames.

Just call `frame-restore' in your `init.el':

   (frame-restore-mode)

Note that since r113242 the built-in Desktop Save mode will restore frames.
If you are using a Emacs snapshot build later than this revision, you are
*strongly* advised to use Desktop Save mode instead:

   (desktop-save-mode)

Frame Restore mode will display a bold warning if enabled in an Emacs build
whose Desktop Save mode can restore frames.
