This is an Emacs minor mode that tries to enable useful folding for
XML files, copying a lot from AUCTeX's tex-fold.el.  It presupposes
that nxml-mode is the major-mode.

The most useful entry points for users are `noxml-fold-dwim', and
`noxml-fold-region'.

Since this mode uses overlays, it does *not* scale: for very
long/deeply nested XML, you should only fold what's within view, or
make use of `narrow-to-region'.
