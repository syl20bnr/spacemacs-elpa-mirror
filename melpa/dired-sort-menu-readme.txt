This package, which is intended for use with GNU Emacs 20/21,
implements a menu and a dialogue that consist of a set of radio
buttons and toggles to control sort order in dired mode.  The menu
is added as a submenu to the Immediate menu, and bound to S-mouse-2
as a popup menu.  The dialogue can be accessed via the menu or the
dired key binding `C-d' or the command `dired-sort-dialogue', which
*must* be run in a dired buffer!  The dialogue uses the Emacs
widget library, which does not require GUI support but uses it if
available.  The dialogue can be used with a character-based
display; the main keys to use are <TAB> and <RET> if a mouse is not
available or does not work.  With a GUI the dialogue pops up its
own frame by default; otherwise it uses part of the dired window.
This is controlled by the customizable user option
`dired-sort-dialogue-own-frame'.

The menu and dialogue support the `ls' sort switches [tSXUucrR].
These are all implemented by GNU `ls'; the subset [tSucr] are
implemented by the standard Emacs 20 `ls-lisp' emulation and all
are implemented by the Emacs 21 `ls-lisp' (see above URL).
Changing a main sort option turns off reverse sorting; toggling
reverse sorting does not change the main sort option.  Setting any
prefix argument before selecting a sort order reverses it
(e.g. press `C-u' or `C-number' or `M-number' BEFORE accessing the
menu bar).  The menu also supports two Emacs 21 `ls-lisp' switches:
`ls-lisp-ignore-case' ignores case in alphanumeric sorts and
`ls-lisp-dirs-first' lists all directories first.  (These latter
two switches improve compatibility with Microsoft Windows
Explorer.)

The toggles for reverse sorting, `ls-lisp-ignore-case' and
`ls-lisp-dirs-first', are bound respectively to the keys "r", "c"
and "b" in the dired map.

A `Configuration' submenu allows a dired sort configuration to be
saved or restored, the current and saved configurations to be
swaped (bound to "T" for "toggle"), or the default to be restored.
The saved configuration is saved as a customization, which can also
be changed directly.  This submenu also allows
`dired-sort-dialogue-own-frame' to be toggled.

If a `dired-sort-menu' option causes an error (but not if it is
just ignored) then it is automatically disabled via the
customization facility.  This is useful because not all `ls' and
`ftp' programs support the same sort options.

The dialogue allows the setting of arbitrary combinations of dired
sort options together, without requiring multiple menu accesses.
Each dired buffer has its own dialogue.  This dialogue can be left
displayed, in which case it is automatically updated to reflect any
changes to its dired buffer that are made by other means.  For
tidiness, a dialogue buffer is automatically killed whenever it or
its dired buffer becomes invisible.

To do:
  test `dired-sort-menu-invalid-options' facility;
  per-host `dired-sort-menu-invalid-options-remote';
  cascade multiple dialogue frames?

Installation:

Put this file somewhere where Emacs can find it (i.e. in one of the
directories in your `load-path' such as `site-lisp'), optionally
byte-compile it (recommended), and put this in your .emacs:

(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))
