When two windows view the same buffer at the same time, and one
window is switched to another buffer and back, point is now the
same as in the other window, not as it was before we switched away.
This mode tries to work around this problem by storing and
restoring per-window positions for each buffer.

To enable this, just run (winpoint-mode 1)
