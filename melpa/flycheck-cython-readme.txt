This package adds support for cython to flycheck.  It requires
cython>=0.23.0.

To use it, add to your init.el:

(require 'flycheck-cython)
(add-hook 'cython-mode-hook 'flycheck-mode)
