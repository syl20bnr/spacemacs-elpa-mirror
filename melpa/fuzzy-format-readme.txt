This minor mode, fuzzy format mode, provides check indent format (tabs or spaces) of current buffer.
And it set indent-tabs-mode and format code automatically.
This minor mode take a neutral stance in the war of tabs indent and spaces indent...

Install
Put this file into load-path'ed directory, and byte compile it if
desired.          And put the following expression into your ~/.emacs.

(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)


Change Log
0.1.1: Fix advice bug
0.1.0: display error line (it's fuzzy).
0.0.9: Fix bug.
0.0.8: new function fuzzy-format-check-pair.
0.0.7: fuzzy-format-set-indent-mode bug fix.
0.0.6: fuzzy-format-auto-format bug fix.
0.0.5: new valiable fuzzy-format-default-indent-tabs-mode.
0.0.4: change from fuzzy-format-no-check-modes to fuzzy-format-check-modes.
0.0.3: set mode-line-buffer-identification. refactor.
0.0.2: use defcustom, defgroup.
0.0.1: fuzzy-format.el 0.0.1 released.
