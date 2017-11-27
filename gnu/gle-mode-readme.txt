This is a major mode for files using the GLE (Graphics Layout Engine)
language.  See http://glx.sourceforge.net/
[ Not sure why the site uses "glx", while everything else seems to use
  "gle" instead.  ]

It provides:
- Rudimentary code highlighting.
- Automatic indentation.
- Flymake support (requires Emacs-26's fymake).
- Imenu support

TODO
- Fix highlighting of function calls?
- provide a completion-at-point-function
- auto-complete the `end`s and `next`s