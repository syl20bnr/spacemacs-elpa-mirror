Extends nlinum to provide current-line-number highlighting, plus other fixes.

It also tries to stave off a nlinum glitch where line numbers disappear
(usually in buffers that have been open a while).

Installation:

M-x package-install RET nlinum-hl

  (require 'nlinum-hl)
  (add-hook 'nlinum-hook #'nlinum-hl-mode))

Alternatively, use `use-package':

  (use-package nlinum-hl
    :after nlinum
    :config
    (add-hook 'nlinum-hook #'nlinum-hl-mode))
