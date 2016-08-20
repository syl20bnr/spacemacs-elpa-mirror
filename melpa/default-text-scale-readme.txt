This package provides commands for increasing or decreasing the
default font size in all GUI Emacs frames -- it is like an
Emacs-wide version of `text-scale-mode'.

Usage:

    (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
    (global-set-key (kbd "C-M--") 'default-text-scale-decrease)
