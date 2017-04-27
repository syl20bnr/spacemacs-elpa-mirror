This package uses the functions in flycheck-dmd-dub to obtain a list
of all dependencies of a D dub project and sets the ac-dcd-flags
variables in order to pass in to DCD the appropriate -I flags.

Usage:

(add-hook 'd-mode-hook 'ac-dcd-dub-set-include-path)
