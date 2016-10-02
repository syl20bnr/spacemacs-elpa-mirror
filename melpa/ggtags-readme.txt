A package to integrate GNU Global source code tagging system
(http://www.gnu.org/software/global) with Emacs.

Usage:

`ggtags' is similar to the standard `etags' package. These keys
`M-.', `M-,' and `C-M-.' should work as expected in `ggtags-mode'.
See the README in https://github.com/leoliu/ggtags for more
details.

All commands are available from the `Ggtags' menu in `ggtags-mode'.

NEWS 0.8.12 (2016-10-02):

- Work with Emacs 25
- `ggtags-navigation-mode' is more discreet in displaying lighter
  when `ggtags-enable-navigation-keys' is set to nil
- `ggtags-make-project' tries harder to find TAG files respecting
  `GTAGSDBPATH'
- Fix error "Selecting deleted buffer"
  https://github.com/leoliu/ggtags/issues/89

See full NEWS on https://github.com/leoliu/ggtags#news
