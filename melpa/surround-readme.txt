This package emulates surround.vim by Tim Pope.
The functionality is wrapped into a minor mode. To enable
it globally, add the following lines to ~/.emacs:

    (require 'surround)
    (global-surround-mode 1)

Alternatively, you can enable surround-mode along a major mode
by adding `turn-on-surround-mode' to the mode hook.

This package uses Evil as its vi layer. It is available from:

    http://gitorious.org/evil
