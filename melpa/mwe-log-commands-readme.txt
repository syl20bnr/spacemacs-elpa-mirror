This add-on can be used to demo Emacs to an audience.  When
activated, keystrokes get logged into a designated buffer, along
with the command bound to them.

To enable, use e.g.:

(add-hook 'LaTeX-mode-hook (function mwe:log-keyboard-commands))

To see the log buffer, call M-x mwe:open-command-log-buffer.
