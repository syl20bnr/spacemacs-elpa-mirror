Enables the kill-ring to interact with the clipboard when running
Emacs from a Mac OSX terminal (without losing full kill-ring
functionality). All I did was modify xclip.el to work with pbcopy
and pbpaste. The real thanks go to Leo Shidai Liu, the author of
xclip.el.

Activate via:
(require 'pbcopy)
(turn-on-pbcopy)
