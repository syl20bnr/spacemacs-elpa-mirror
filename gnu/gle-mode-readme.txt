This is a major mode for files using the GLE (Graphics Layout Engine)
language.  See http://glx.sourceforge.net/
[ Not sure why the site uses "glx", while everything else seems to use
  "gle" instead.  ]

It provides:
- Rudimentary code highlighting
- Automatic indentation
- Flymake support (requires Emacs-26's fymake)
- Imenu support
- Electric bloc names (after begin/end)
- Completion of bloc names
- Skeletons/templates to insert or close blocs

TODO
- Fix highlighting of function calls?
- provide more completion