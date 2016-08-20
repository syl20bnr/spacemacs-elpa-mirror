This package highlighting `cl' functions.

Please byte compile this package on batch mode instead of runtime
byte compiling. Because this package using load state for classify
functions to `cl only' and `cl and other package'.

Installation:
Run followings to byte compile this package:

  $ emacs -Q -batch -f batch-byte-compile highlight-cl.el

Put followings to your .emacs:

  (require 'highlight-cl)
  (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
  (add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)
