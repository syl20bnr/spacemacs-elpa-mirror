   Extensions to `dired-sort-menu.el'

Francis J. Wright <F.J.Wright@maths.qmw.ac.uk> wrote library
`dired-sort-menu.el' (http://centaur.maths.qmw.ac.uk/Emacs/).
Library `dired-sort-menu+.el' modifies `dired-sort-menu.el' to play
better with other libraries from Drew Adams.

Changes:

  1. The toggles for reverse sorting, `ls-lisp-ignore-case' and
     `ls-lisp-dirs-first', are bound respectively to "a", "c", and
     "W" in the dired map, instead of "r", "c" and "b".

  2. We don't define `dired-sort-menu-toggle-ignore-case' and
     `dired-sort-menu-toggle-dirs-first' unless they can be used.

  3. `handle-delete-frame' is protected against nil `buffer-name'.


 ***** NOTE: The following functions defined in `dired.el' have
             been REDEFINED or ADVISED HERE:

 `dired-sort-dialogue' -
   1. Fit frame.  2. Do not add `dired-sort-dialogue-auto-kill-1'
   to `kill-buffer-hook'.
 `dired-sort-dialogue-close' - Just `kill-buffer'.
 `handle-delete-frame' - Do nothing if `buffer-name' is nil.
