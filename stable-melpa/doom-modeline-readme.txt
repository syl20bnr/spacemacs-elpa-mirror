This package offers a modern modeline them which is extraced from DOOM Emacs
(https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).
It's also the part of Centaur Emacs (https://github.com/seagle0128/.emacs.d).

The DOOM modeline was designed for minimalism, and offers:
1. A match count panel (for evil-search, iedit and evil-substitute)
2. An indicator for recording a macro
3. Local python/ruby version in the major-mode
4. A customizable mode-line height (see doom-modeline-height)
5. An error/warning count segment for flycheck

Installation:
From melpa, `M-x package-install RET doom-modeline RET`.
In `init.el`,
(require 'doom-modeline)
(doom-modeline-init)
or
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))
