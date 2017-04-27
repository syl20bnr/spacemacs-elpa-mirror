Font-lock menus.  `font-menus.el', enhanced, & fixed for Emacs 24+.

This is library `font-menus.el', by Francis J. Wright, modified so
that it continues to work with GNU Emacs 24 and later (as well as
older versions), and with minor enhancements.

Enhancements:

* Added group `font-lock-menus', and renamed everything to use
  package prefix `flm-'.

* User option `flm-font-lock-menu-wrap': non-nil means wrap around
  when changing levels, instead of just raising an error.  This has
  no effect when the commands are called from a menu, but you can
  bind commands `flm-font-lock-fontify-more' and
  `flm-font-lock-fontify-less' to keys.

  If the non-nil value is `off' (the default value) then font-lock
  mode is turned off (absolute minimum font-locking) when cycling
  wraps around.  Any other non-nil value means cycle only among
  font-lock states.  If the value is `off' then you can cycle
  on/off even if there is only one font-lock level.

TO DO:

* Be able to associate *names* with numerical levels, in a given mode.
* Be able to easily customize sets of keywords (probably hard to do).

-------------------------------------------------------------------

Here is the original Commentary, by F.J. Wright:

This package is intended for use with GNU Emacs 20 and adds
submenus to the Edit menu to control font lock mode and provide
font display.

Installation:

Put this file somewhere where Emacs can find it (i.e. in one of the
directories in your `load-path' such as `site-lisp'), optionally
byte-compile it, and put this in your .emacs:

 (require 'font-lock-menus)

Font Display:

Extracted from font-lock.el for GNU Emacs 20.3 and
`font-lock-menu.el' for GNU Emacs 19, both by Simon Marshal
<simon@gnu.ai.mit.edu> and revised to use easymenu and run as a
stand-alone package by Francis J. Wright.  (It would be better put
back into font-lock.el!)
