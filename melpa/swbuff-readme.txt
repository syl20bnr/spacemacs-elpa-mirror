This package provides the commands `swbuff-switch-to-next-buffer'
and `swbuff-switch-to-previous-buffer' to respectively switch to
the next or previous buffer in the buffer list.

The `swbuff-exclude-buffer-regexps' defines a list of regular
expressions for excluded buffers.  The default setting excludes
buffers whose name begin with a blank character.  To exclude all the
internal buffers (that is *scratch*, *Message*, etc...) you could
use the following regexps '("^ .*" "^\\*.*\\*").

Switching buffers pops-up a status window at the bottom of the
selected window.  The status window shows the list of switchable
buffers where the switched one is hilighted using
`swbuff-current-buffer-face'.  This window is automatically
discarded after any command is executed or after the delay
specified by `swbuff-clear-delay'.

The bufferlist is sorted by how recently the buffers were used.  If
you prefer a fixed (cyclic) order set `swbuff-recent-buffers-first'
to nil.

When the status window disappears because of the clear-delay you
still stay in switching mode.  The timeout is only a visual
thing.  If you want it to have the same effect as using the buffer,
set `swbuff-clear-delay-ends-switching' to t.

The leftmost item in the status window is the active buffer before
switching started.  If you want the buffer /after/ switching started
there, set `swbuff-display-original-buffer-first' to nil.

To install and use, put this file on your Emacs-Lisp load path and
add the following into your ~/.emacs startup file:

(require 'swbuff)
