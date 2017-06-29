This package is a frontend to ripgrep (rg) and works in a similar
way to Emacs built in `rgrep' command.  It depends on and reuse parts
of built in grep with adjustments to ripgrep and is compatible with
`wgrep'.

Install the package and bind the main entry point `rg':
(eval-after-load
  (global-set-key (kbd "M-s") 'rg))

There are also other entry points for easy searching:
`rg-project' - Search in a project.
`rg-dwim' - Handsfree search.  Search thing at point in project in
files matching the type alias of the current buffer file name.

The `rg' results buffer has bindings for modification of
the last search for quick reruns with refined parameters.
Possible refinements are: toggle case insensitive search, toggle
'--no-ignore' flag, change directory, change file pattern and change
search string.  See `rg-mode' for details.

This package (just as grep.el) use the setting of
`case-fold-search' variable to decide whether to do a case
sensitive search or not.  The behavior is similar to the ripgrep
'--smart-case' flag in that the search will be case insensitive if
`case-fold-search' is non nil and search pattern is all lowercase.
Otherwise it's case sensitive

ripgrep has built in type aliases that can be selected on
invocation of `rg'.  Customize `rg-custom-type-aliases' to add your
own aliases:
(setq rg-custom-type-aliases
  '(("foo" .    "*.foo *.bar")
    ("baz" .    "*.baz *.qux")))

The `rg-define-toggle' macro can be used to define a toggleable
flag for the rg command line.  Such flags can then be toggled from
the results buffer to repeat the search with updated flags.

The default configuration of this package is compatible with `wgrep'.
If grouped mode and/or show columns is enabled you need to install
the wgrep-ag package from MELPA and configure it like this:
(add-hook 'rg-mode-hook 'wgrep-ag-setup)
