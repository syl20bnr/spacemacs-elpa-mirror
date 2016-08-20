This package provides a minor mode for formatting an Emacs buffer
as slides. This package relies on `narrowed-page-navigation-mode'
to actually navigate from slide to slide, and instead provides
syntax for comments that are rendered as slide elements.

The syntax comes pre-configured for Idris or Haskell. For other
languages, set `live-code-talks-title-regexp',
`live-code-talks-image-regexp', and
`live-code-talks-comment-regexp', preferably as file variables.
For your presentation, consider also overriding
`face-remapping-alist' to get the proper fonts for your screen.
