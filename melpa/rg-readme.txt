This package is a frontend to ripgrep (rg) and works in a similar
way to Emacs built in `rgrep' command.  It depends on and reuses parts
of built in grep with adjustments to ripgrep and is compatible with
`wgrep'.  The `rg' results buffer has bindings for modification of
the last search for quick reruns with refined parameters.
Possible refinements are: toggle case insensitive search, toggle
'--no-ignore' flag, change directory, change file pattern and change
search string. See `rg-mode' for details.

Install the package and bind the main entry point `rg':
(eval-after-load
  (global-set-key (kbd "M-s") 'rg))

ripgrep has built in type aliases that can be selected on
invocation of `rg'.  Customize `rg-custom-type-aliases' to add your
own aliases:
(setq rg-custom-type-aliases
  '(("foo" .    "*.foo *.bar")
    ("baz" .    "*.baz *.qux")))
