This library implements support for some Org Mode link types in
other major modes.  Links can be opened and edited like in Org
Mode.

To use enable `global-orglink-mode' and customize
`orglink-activate-in-modes'.  Or use the buffer local
`orglink-mode'.  Do the latter now to linkify these examples:

  [[Code]]
  [[Code][start of code]]
  [[define-derived-mode orglink-mode][orglink-mode]]
  <mailto:jonas@bernoul.li>
  man:info
  <info:man>
  https://github.com/tarsius/orglink
  TODO support Emacs Lisp xref links like (in) `help-mode'.
  TODO support footnote.el links (only)

If you like this you might also like `orgstruct-mode', [[https://github.com/tj64/outshine][outshine]],
[[https://github.com/tarsius/hl-todo][hl-todo]], and [[https://github.com/tarsius/org-elisp-help][org-elisp-help]].
