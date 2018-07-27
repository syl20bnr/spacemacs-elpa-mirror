This package provides key bindings to quickly switch between
predefined places, and move text to these places. With the default
binding, use M-g 1 to store current point to register 1, use M-1 to
go to it later; this works for 1~5. With an active region, M-1 will
copy text to the place stored in register 1. 6~8 stores / recalls
window configuration by default.

To use, put into your init.el:
(require 'register-channel)
(register-channel-mode 1)
