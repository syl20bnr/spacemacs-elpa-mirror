LilyPond syntax checking support for Flycheck.

You can add the following to your init file:

(add-hook 'LilyPond-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck '(require 'flycheck-lilypond)
