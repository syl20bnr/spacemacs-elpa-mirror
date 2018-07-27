This package adds support for dialyzer to flycheck.  To use it, add
to your init.el:

(require 'flycheck-dialyzer)
(add-hook 'erlang-mode-hook 'flycheck-mode)
