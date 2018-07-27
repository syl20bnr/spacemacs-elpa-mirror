Load this package

(require 'cycle-resize)

You can then call these two methods, once you have at least 2 windows:

M-x cycle-resize-window-vertically
M-x cycle-resize-window-horizontally

and eventually bind some keys like:

(global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
(global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)

You also can configure the dimensions (in %) the package will cycle through
By default, it is: 80% -> 50% -> 20% -> 50%, and so on...

(setq cycle-resize-steps '(80 50 20 50))
