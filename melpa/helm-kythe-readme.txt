`helm-kythe.el' is a `helm' interface of Google Kythe.

This package is enlightened by `helm-gtags.el'.

Usage
For C++:
(add-hook 'c++-mode-hook 'helm-kythe-mode)

For Haskell:
(add-hook 'haskell-mode-hook 'helm-kythe-mode)

If eldoc-mode is enabled, when the point is at a reference, the `snippet' of its definition will be displayed in the echo area..
