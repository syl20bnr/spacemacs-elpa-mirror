This package provides functionality for correcting words via custom
interfaces. There are two functions for this: `flyspell-correct-word-generic'
to correct word at point and `flyspell-correct-previous-word-generic' to
correct any visible word before point. In most cases second function is more
convenient, so don't forget to bind it.

  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

When invoked, it will show the list of corrections suggested by Flyspell.
Most interfaces also allow you to save new word to your dictionary, accept
this spelling in current buffer or for a whole session.

Default interface is implemented using `completing-read', but it's highly
advised to use `flyspell-correct-ido' (which comes bundled with this package)
or any interface provided by following packages: `flyspell-correct-ivy',
`flyspell-correct-helm' and `flyspell-correct-popup'.

In order to use `flyspell-correct-ido' interface instead of default
`flyspell-correct-dummy', place following snippet in your 'init.el' file.

  (require 'flyspell-correct-ido)

It's easy to implement your own interface for `flyspell-correct'. Checkout
documentation for `flyspell-correct-interface' variable.

For more information about this and related packages, please refer to
attached README.org file.
