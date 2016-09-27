Installation:

Put cbm.el in your `load-path' and require it:

(require 'cbm)

It is recommended to bind `cbm-cycle', `cbm-switch-buffer' and
`cbm-find-org-agenda-file' to a key:

(global-set-key (kbd "C-;") #'cbm-cycle)
(global-set-key (kbd "C-'") #'cbm-switch-buffer)
(global-set-key (kbd "C-c o") #'cbm-find-org-agenda-file)
(define-key rcirc-mode-map (kbd "M-i") #'cbm-rcirc-switch-to-channel)

Usage:

This package provides useful commands for switching to similar
buffers. It's particularly handy for switching between buffers in
the same major mode.
