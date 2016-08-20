A few modifications to David Ponce's most excellent swbuff package

(1) Fix the display timer so that it doesn't interfere with other
packages e.g speedbar and ispell.

(2) Maintain buffer list ordering so that only the first and last
buffer in a sequence are effected.

(3) Exclude buffers whose major mode matches
`swbuff-exclude-mode-regexp' from the buffer list but include any
buffers that match `swbuff-include-buffer-regexps' (a couterpoint
to `swbuff-exclude-buffer-regexps'). Also If
`swbuff-this-frame-only' is non-nil exclude buffers displayed in
other visible frames.

(4) New hook `swbuff-pre-switch-hook' for things you may want to do
before switching buffers.

(5) New function `swbuff-kill-this-buffer' which useful for
selectively cleaning out your buffer list.

(6) If `swbuff-start-with-current-centered' is non-nil buffer list
display starts with the current buffer roughly in the middle of the
display ordering.  This encourages the use of
`swbuff-previous-buffer' to get to buffers which would otherwise
appear at the end of the list.

(7) New variables `swbuff-left' and `swbuff-right' as an
alternative to `swbuff-header', `swbuff-trailer' and
`swbuff-separator'.  This allows you to place brackets around the
buffer name.

(8) Display buffer name matching `swbuff-special-buffers-re' using
`swbuff-special-buffers-face'.

(9) Added variable `swbuff-modeline-format' to make the modeline of the
status window configurable (Thanks to Matthias Wedel).
