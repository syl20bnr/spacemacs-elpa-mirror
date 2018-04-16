This package implements NHexl mode, a minor mode for editing files
in hex dump format.  The mode command is called `nhexl-mode'.

This minor mode implements similar functionality to `hexl-mode',
but using a different implementation technique, which makes it
usable as a "plain" minor mode.  It works on any buffer, and does
not mess with the undo log or with the major mode.

In theory it could also work just fine even on very large buffers,
although in practice it seems to make the display engine suffer.

It also comes with:

- `nhexl-nibble-edit-mode': a "nibble editor" minor mode.
  where the cursor pretends to advance by nibbles (4-bit) and the
  self-insertion keys (which only work for hex-digits) will only modify the
  nibble under point.

- `nhexl-overwrite-only-mode': a minor mode to try and avoid moving text.
  In this minor mode, not only self-inserting keys overwrite existing
  text, but commands like `yank' and `kill-region' as well.