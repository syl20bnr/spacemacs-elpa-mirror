An opinionated UI plugin/pack of themes extracted from my emacs.d, inspired
by the One Dark/Light UI and syntax themes in Atom.

Includes optional dimming of non-source buffers, a neotree theme with font
icons, and (soon) a mode-line config.

Currently available colorschemes:
+ doom-one: inspired by Atom One Dark
+ doom-dark: based on Molokai

Soon to come:
+ doom-one-light: inspired by Atom One Light
+ doom-tron: doom-one, but with daylerees' Tron Legacy colorscheme
+ doom-peacock: doom-one, but with daylerees' Peacock colorscheme


## Configuration

+ global
    + `doom-enable-bold` (default: `t`): if nil, bolding will be disabled
    across all faces.
    + `doom-enable-italic` (default: `t`): if nil, italicization will be
    disabled across all faces.
+ doom-one
    + `doom-one-brighter-modeline` (default: `nil`): If non-nil, the
    mode-line background is slightly brighter.
    + `doom-one-brighter-comments` (default: `nil`): If non-nil, comments
    are brighter and easier to see.


## Installation

1. Install from MELPA `M-x package-install RET doom-themes`, or clone
   the repo somewhere in your `load-path`.

2. If you want the neotree theme, download and install the fonts included
   with all-the-icons.

3. `(require 'doom-themes)` and then load the theme you want.

Example configuration:

  (require 'doom-themes)
  (load-theme 'doom-one t) ;; or doom-dark, etc.

  ;;; OPTIONAL
  ;; brighter source buffers
  (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; Custom neotree theme
  (require 'doom-neotree)
