This package provides a way to run multiple syntax checkers on Python
code.  The list of supported checkers includes:

- pylint
- flake8
- pep8
- pyflakes
- mypy (for both Python 2 and 3)

This is an alternative way of running multiple Python syntax checkers in
flycheck that doesn't depend on flycheck's chaining mechanism.  flycheck is
opinionated about what checkers should be run (see
https://github.com/flycheck/flycheck/issues/185), and chaining is difficult
to get right (e.g. see https://github.com/flycheck/flycheck/issues/836).
This package assumes that the user knows what they want, and can configure
their checkers accordingly -- if they want to run both flake8 and pylint,
that's fine.

This also allows us to run multiple syntax checkers in parallel, rather than
sequentially.

Usage:

In your `init.el':
(require 'flycheck-pycheckers) ; Not necessary if using ELPA package
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
