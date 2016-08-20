This package adds support for elixir to flycheck.  It requires
elixir>=1.2.3.
Warning: running the checker will effectively execute the buffer,
therefore it may be unsafe to run.  See
https://github.com/flycheck/flycheck/issues/630

To use it, add to your init.el:

(require 'flycheck-elixir)
(add-hook 'elixir-mode-hook 'flycheck-mode)
