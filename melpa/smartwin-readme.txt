Smartwin is a window for shell buffers, temp buffers and etc.

This minor mode let shell like buffers share a window, called smart window,
the smart window is always at the bottom of Emacs window.  Besides that, when
point move into or out the smart window, it will be enlarged or shrinked
automaticly.  Warning: this package can not work with popwin.el.

To use smartwin, place this file to load path and add this to .emacs:

(require 'smartwin)
(smartwin-mode 1)

or run M-x smartwin-mode to toggle it.

To switch between buffers of smart window, you can bind keys like:
    (define-key smartwin-mode-map (kbd "C-c s") 'smartwin-switch-buffer)

Then try run M-x shell or or eshell, if you want to show more buffers in
smart window, please customize variable: smartwin-buffers.
