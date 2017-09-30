This package is a frontend to ripgrep (rg) and works in a similar
way to Emacs built in `rgrep' command or external `ag' if you like.
It depends on and reuse parts of built in grep with adjustments to
ripgrep and is compatible with `wgrep'.

Install the package and and use the default key bindings:
(rg-enable-default-bindings)

The default key binding prefix is C-c s but can be changed by
supplying a prefix of choice to the above function call:
(rg-enable-default-bindings "\M-s")

`rg' is the main entry point but there are functions for easy
searching:
`rg-project' - Search in a project.
`rg-dwim' - Hands free search.  Search thing at point in project in
files matching the type alias of the current buffer file name.

The search results buffer has bindings for modification of
the last search for quick reruns with refined parameters.
Possible refinements are: toggle case insensitive search, toggle
'--no-ignore' flag, change directory, change file pattern and change
search string.  See `rg-mode' for details.

This package by default use the setting of
`case-fold-search' variable to decide whether to do a case
sensitive search or not, similar to the '--smart-case' rg flag.
This can be changed by changing the `rg-ignore-case' variable.

ripgrep has built in type aliases that can be selected on
invocation of `rg'.  Customize `rg-custom-type-aliases' to add your
own aliases:
(setq rg-custom-type-aliases
  '(("foo" .    "*.foo *.bar")
    ("baz" .    "*.baz *.qux")))

The `rg-define-toggle' macro can be used to define a toggle-able
flag for the rg command line.  Such flags can then be toggled from
the results buffer to repeat the search with updated flags.

The two `rg-save-search' functions will allow for saving search
result buffers with or without custom naming.
`rg-list-searches' will display a list of all search buffers with
search info and allow jumping to results.

The default configuration of this package is compatible with `wgrep'.
If grouped mode and/or show columns is enabled you need to install
the wgrep-ag package from MELPA and configure it like this:
(add-hook 'rg-mode-hook 'wgrep-ag-setup)
