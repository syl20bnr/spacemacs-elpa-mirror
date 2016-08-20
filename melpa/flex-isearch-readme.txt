This was inspired by and based on Tomohiro Matsuyama's fuzzy.el.

This file defines a minor mode that allows flex matching in
isearch-mode.  The flex matching is very similar to the flex
matching in ido mode (see `ido-enable-flex-matching').  Given an
isearch string, it is transformed to a regexp that matches the
original and also similar strings (basically, strings that contain
the original characters in the same order, but possibly with other
characters in between).  You can redefine
`flex-isearch-compile-regexp' to change this behavior.

When `flex-isearch-mode' is enabled, the `flex-isearch-auto'
variable controls when the flex matching is activated.  See its
docstring for details.  Also, `isearch-forward' and
`isearch-backward' are advised so that double prefix args (C-u C-u
C-s) will use flex search.

Le Wang has improved this by providing a better
`flex-isearch-compile-regexp' and the advice on
`isearch-toggle-regexp'.
