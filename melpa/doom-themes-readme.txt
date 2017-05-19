DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
emacs.d, inspired by the One Dark/Light UI and syntax themes in Atom.

Includes optional dimming of non-source buffers, a neotree theme with font
icons, and (soon) a mode-line config.

Currently available colorschemes:
+ doom-one: inspired by Atom One Dark
+ doom-vibrant: a more vibrant take on doom-one
+ doom-molokai: based on molokai
+ doom-tomorrow-night: Chris Kempson's Tomorrow Night (dark)

Soon to come:
+ doom-tomorrow-day: Chris Kempson's Tomorrow Day (light)
+ doom-one-light: inspired by Atom One Light
+ doom-tron: daylerees' Tron Legacy colorscheme
+ doom-peacock: daylerees' Peacock colorscheme
+ doom-spacegrey: I'm sure you've heard of it
+ doom-mono-dark: A minimalistic, custom colorscheme
+ doom-mono-light: A minimalistic, custom colorscheme


## Configuration

+ global
    + `doom-themes-enable-bold` (default: `t`): if nil, bolding will be disabled
    across all faces.
    + `doom-themes-enable-italic` (default: `t`): if nil, italicization will be
    disabled across all faces.

  Each colorscheme has their own sub-options, and can be looked up via
  `customize'.

Example:

  (require 'doom-themes)
  ;;; Settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-one t) ;; or doom-molokai, etc.

  ;;; OPTIONAL
  ;; brighter source buffers (that represent files)
  (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  ;; ...if you use auto-revert-mode
  (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  ;; And you can brighten other buffers (unconditionally) with:
  (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Enable nlinum line highlighting
  (doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode
