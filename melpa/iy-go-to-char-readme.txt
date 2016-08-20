This package defines the function `iy-go-to-char' which behaves like "f" in
vim, and `iy-go-up-to-char` like "t" in vim.  It reads a char and go the
next Nth occurence of the char.  User can continue such search using that
char key.

To use, make sure this file is on your `load-path' and put the
following in your .emacs file:

    (require 'iy-go-to-char)

To make `iy-go-to-char' works better with `multiple-cursors', add
`iy-go-to-char-start-pos' to `mc/cursor-specific-vars' when mc is loaded:

    (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

Then you can bind functions like:

    (global-set-key (kbd "C-c f") 'iy-go-to-char)
    (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
    (global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
    (global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

Or if you prefer up-to (vim "t") versions:

    (global-set-key (kbd "C-c f") 'iy-go-up-to-char)
    (global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)

You also can bind go-to methods and up-to methods to different keys.

Except repeating the char key, followings keys are defined before
quitting the search (which can be disabled by setting
`iy-go-to-char-override-local-map' to nil):

   X   -- where X is the char to be searched. Repeating it will search
          forward the char. Can be disabled through
          `iy-go-to-char-continue-when-repeating'

   ;   -- search forward the char, customizable:
          `iy-go-to-char-key-forward', `iy-go-to-char-use-key-forward'

   ,   -- search backward the char, customizable:
          `iy-go-to-char-key-backward', `iy-go-to-char-use-key-backward'

   C-g -- quit

   C-s -- start `isearch-forward' using char as initial search
          string

   C-r -- start `isearch-backward' using char as initial search
          string

   C-w -- quit and kill region between start and current point.  If region is
          activated before search, then use the original mark instead of the
          start position.

   M-w -- quit and save region between start and current point.  If region is
          activated before search, use the mark instead of start position.

All other keys will quit the search.  Then the key event is
intepreted in the original environment before search.

if the search quits because of error or using "C-g", point is set
back to the start position.  Otherwise, point is not change and the
start position is set as marker.  So you can use "C-x C-x" back to
that position.

`iy-go-to-char-backward' search backward by default.  Also the search can
cross lines.  To continue search last char, use `iy-go-to-char-continue' and
`iy-go-to-char-continue-backward'.
