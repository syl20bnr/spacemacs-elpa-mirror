 Faces defined here:

   `pp^L-highlight'.

 User options defined here:

   `pp^L-^L-string', `pp^L-^L-string-function',
   `pp^L-^L-string-post', `pp^L-^L-string-pre',
   `pretty-control-l-mode'.

 Commands defined here:

   `pp^l', `pretty-control-l-mode', `refresh-pretty-control-l'.

 Non-interactive functions defined here:

  `pp^L-^L-display-table-entry', `pp^L-make-glyph-code'.


 To use this library, add this to your initialization file
 (~/.emacs or ~/_emacs):

   (require 'pp-c-l)           ; Load this library.

 To turn on this mode by default, then either customize option
 `pretty-control-l-mode' to non-nil or add this line also to your
 init file:

   (pretty-control-l-mode 1)   ; Turn on pretty display of `^L'.

 For most of the user options defined here, if you change the value
 then you will need to re-enter `pretty-control-l-mode', for the
 new value to take effect.
