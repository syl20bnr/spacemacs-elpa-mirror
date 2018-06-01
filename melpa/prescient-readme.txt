prescient.el is a general-purpose library for sorting and filtering
candidates.

The algorithm of prescient.el is very simple. You enter a query, or
multiple queries separated by spaces (two spaces match a literal
space), and each query filters the candidates by matching either a
substring (e.g. "scient" matches "prescient-frequency-threshold")
or initialism (e.g. "ft" also matches the same). Then, candidates
are sorted to prioritize recently chosen candidates, followed by
frequently chosen candidates, with the remaining candidates sorted
by length.

prescient.el aims to replace a number of other packages, including
IDO, Smex, Flx, Historian, and Company-Statistics. It also replaces
the sorting and filtering functionalities of completion frameworks
such as Helm and Ivy.

To use prescient.el for Ivy, see ivy-prescient.el. To use
prescient.el for Company, see company-prescient.el. In either case,
you will most likely want your usage statistics to be saved across
Emacs sessions; to do this, enable `prescient-persist-mode' in your
init-file or interactively.

For more information, see https://github.com/raxod502/prescient.el.
