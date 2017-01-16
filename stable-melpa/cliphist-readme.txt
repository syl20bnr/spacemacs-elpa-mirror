Read clipboard items from following clipboard managers,
  - Parcellite (http://parcellite.sourceforge.net) at Linux
  - ClipIt (http://clipit.sourceforge.net) at Linux
  - Flycut (https://github.com/TermiT/Flycut) on OSX

Usage:
  Make sure clipboard manager is running.
  `M-x cliphist-paste-item' to paste item from history
  `M-x cliphist-select-item' to select item
  In popup, press `C-n' or `C-p' to navigate, other keys
  to filter.

  By default, we use popup.el to provides candidates window.
  But you can use ivy-mode instead by installing Swiper and
  `(setq cliphist-use-ivy t)'.

  You can customize the behavior of cliphist-select-item,
    (setq cliphist-select-item-callback
       (lambda (num str) (cliphist-copy-to-clipboard str)))

  If `cliphist-cc-kill-ring' is true, the selected/pasted string
  will be inserted into kill-ring

You can tweak =cliphist-linux-clipboard-managers= to tell cliphist
how to detect clipboard manager:
  `(setq cliphist-linux-clipboard-managers '("clipit" "parcellite"))'
