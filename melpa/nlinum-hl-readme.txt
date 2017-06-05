Extends nlinum to provide current-line-number highlighting, and tries to
mitigate disappearing line numbers (a known issue with nlinum).

Installation:

M-x package-install RET nlinum-hl

  (require 'nlinum-hl)
  (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))

Alternatively, use `use-package':

  (use-package nlinum-hl
    :after nlinum
    :config
    (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))
