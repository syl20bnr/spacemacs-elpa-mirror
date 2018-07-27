This package adds support for mypy to flycheck.  To use it, add
to your init.el:

(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)

If you want to use mypy you probably don't want pylint or
flake8. To disable those checkers, add the following to your
init.el:

(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
