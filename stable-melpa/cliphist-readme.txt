Read clipboard items from following clipboard managers,
  - Parcellite (http://parcellite.sourceforge.net) at Linux
  - ClipIt (http://clipit.sourceforge.net) at Linux
  - Flycut (https://github.com/TermiT/Flycut) on OSX

Usage:
  Make sure clipboard manager is running.
  `M-x cliphist-paste-item' to paste item from history
  `C-u M-x cliphist-paste-item' rectangle paste item
  `M-x cliphist-select-item' to select item

You can customize the behavior of cliphist-select-item,
    (setq cliphist-select-item-callback
       (lambda (num str) (cliphist-copy-to-clipboard str)))

  If `cliphist-cc-kill-ring' is true, the selected/pasted string
  will be inserted into kill-ring

You can tweak =cliphist-linux-clipboard-managers= to tell cliphist
how to detect clipboard manager:
  `(setq cliphist-linux-clipboard-managers '("clipit" "parcellite"))'
