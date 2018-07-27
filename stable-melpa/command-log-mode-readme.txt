This add-on can be used to demo Emacs to an audience.  When
activated, keystrokes get logged into a designated buffer, along
with the command bound to them.

To enable, use e.g.:

(require 'command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

To see the log buffer, call M-x clm/open-command-log-buffer.

The key strokes in the log are decorated with ISO9601 timestamps on
the property `:time' so if you want to convert the log for
screencasting purposes you could use the time stamp as a key into
the video beginning.
