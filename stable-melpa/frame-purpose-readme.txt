`frame-purpose' makes it easy to open purpose-specific frames that only show certain buffers,
e.g. by buffers' major mode, their filename or directory, etc, with custom frame/X-window
titles, icons, and other frame parameters.

Note that this does not lock buffers to certain frames, and it does not prevent frames from
showing any buffer.  It works by overriding the `buffer-list' function so that, within a
purpose-specific frame, any code that uses `buffer-list' (e.g. `list-buffers', `ibuffer', Helm
buffer-related commands) will only show buffers for that frame's purpose.  You could say that,
within a purpose-specific frame, you would have to "go out of your way" to show a buffer that's
not related to that frame's purpose.
