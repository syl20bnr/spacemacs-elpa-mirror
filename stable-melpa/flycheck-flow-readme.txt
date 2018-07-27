This package adds support for flow to flycheck.  It requires
flow>=0.20.0.

To use it, add to your init.el:

(require 'flycheck-flow)
(add-hook 'javascript-mode-hook 'flycheck-mode)

You want to use flow in conjunction with other JS checkers.
E.g. to use with gjslint, add this to your init.el
(flycheck-add-next-checker 'javascript-gjslint 'javascript-flow)

For coverage warnings add this to your init.el
(flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
