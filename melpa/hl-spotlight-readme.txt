 This library extends standard library `hl-line.el'.  It provides
 local and global modes to highlight several lines surrounding
 point using a different face, `hl-spotlight'.  You can enlarge or
 shrink this spotlight highlighting, using command
 `hl-spotlight-enlarge'.  You can repeat, to enlarge or shrink the
 spotlight incrementally, by using `C-x z z z z...'.

 Spotlight highlighting can be used together with library
 `centered-cursor-mode.el', which keeps point (hence also the
 spotlight) centered in the window.  This can be helpful when
 reading text (as opposed to code).  This is controlled by user
 option `hl-spotlight-keep-centered-flag'. You can obtain library
 `centered-cursor-mode.el' here:
 http://www.emacswiki.org/emacs/centered-cursor-mode.el.

 If you want the spotlight to automatically move down the page
 progressively, use command `hl-spotlight-scan'.  You might find
 this useful for some kinds of reading.  Or not.  ;-)

 To use this library, put this in your Emacs init file (~/.emacs):

   (require 'hl-spotlight) ; Load this file (it will load `hl-line.el')


 See also library `hl-line+.el', which extends `hl-line.el' in
 other ways.


 Faces defined here:

   `hl-spotlight'.

 User options defined here:

   `hl-spotlight-height', `hl-spotlight-keep-centered-flag',
   `hl-spotlight-scan-period'.

 Commands defined here:

   `global-hl-spotlight-mode', `hl-spotlight-enlarge',
   `hl-spotlight-mode', `hl-spotlight-scan'.

 Non-interactive functions defined here:

   `hl-spotlight-down', `hl-spotlight-limits'.

 Internal variables defined here:

   `hl-spotlight-old-state', `hl-spotlight-scan-timer'.
