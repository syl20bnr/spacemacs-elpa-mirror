Navigating the cursor across long lines of text by keyboard in Emacs can be
cumbersome, since commands like `forward-char', `backward-char',
`forward-word', and `backward-word' move sequentially, and potentially
require a lot of repeated executions to arrive at the desired position.  This
package provides the binary line navigation minor-mode (`bln-mode'), to
address this issue.  It defines the commands `bln-forward-half' and
`bln-backward-half', which allow for navigating from any position in a line to
any other position in that line by recursive binary subdivision.

For instance, if the cursor is at position K, invoking `bln-backward-half' will
move the cursor to position K/2. Successively invoking `bln-forward-half'
(without moving the cursor in between invocations) will move the cursor to
K/2 + K/4, whereas a second invocation of `bln-backward-half' would move the
cursor to K/2 - K/4.

Below is an illustration of how you can use binary line navigation to reach
character `e' at column 10 from character `b' at column 34 in four steps:

                  ________________|     `bln-backward-half'
         ________|                      `bln-backward-half'
        |___                            `bln-forward-half'
           _|                           `bln-backward-half'
..........e.......................b.....

This approach requires at most log(N) invocations to move from any position
to any other position in a line of N characters.  Note that when you move in
the wrong direction---by mistakenly invoking `bln-backward-half' instead of
`bln-forward-half' or vice versa---you can interrupt the current binary
navigation sequence by moving the cursor away from its current position (for
example, by `forward-char'). You can then start the binary navigation again
from that cursor position.

By default the commands `bln-backward-half' and `bln-forward-half' are bound to M-[
and M-], respectively.  Depending on your keyboard layout, these keys may not
be very convenient.  For more convenient binary line navigation, you could
bind to more convenient keys, like M-j and M-k (at the expense of losing the
default bindings for `indent-new-comment-line', and `kill-sentence',
respectively):

(global-set-key (kbd "M-j") 'bln-backward-half)
(global-set-key (kbd "M-k") 'bln-forward-half)
