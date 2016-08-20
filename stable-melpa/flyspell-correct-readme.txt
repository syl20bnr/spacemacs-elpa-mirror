This package provides functionality for correcting words via custom
interfaces. There are two functions for this: `flyspell-correct-word-generic'
to correct word at point and `flyspell-correct-previous-word-generic' to
correct any visible word before point. In most cases second function is more
convenient, so don't forget to bind it.

(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

When invoked, it will show the list of corrections suggested by Flyspell.
Most interfaces also allow you to save new word to your dictionary, accept
this spelling in current buffer or for a whole session.

Since this package does not provide any interface for correcting words, it's
better to use one of the following packages: `flyspell-correct-ivy',
`flyspell-correct-helm' and `flyspell-correct-popup'. The all depend on
`flyspell-correct' and just provide interface for it's functionality.

But one can easily implement it's own interface for `flyspell-correct'.
Checkout documentation for `flyspell-correct-interface' variable.

For more information about this and related package, please read attached
README.org file.
