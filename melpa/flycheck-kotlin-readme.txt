This package adds support for kotlin to flycheck. To use it, add
to your init.el:

(require 'flycheck-kotlin)
(add-hook 'kotlin-mode-hook 'flycheck-mode)
