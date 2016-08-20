 In the mode-line, show the value of the character after point.

 Use minor mode `mlc-char-in-mode-line-mode' to show the value of
 the character after point.  Use global minor mode
 `mlc-char-in-mode-line-mode-global' to do this in every buffer.

 The character is shown (in the mode-line) highlighted with face
 `mlc-mode-line-char-format', followed by `=', followed by the
 hexadecimal Unicode code point for the character, highlighted with
 face `mlc-mode-line-char-format-code'.

 For example, with point before the character `e' it shows:

   e=000065

 with `e' and `000065' highlighted.

 You thus have a dynamically updated view at all times of what the
 character at the cursor is.

 In addition:

  * If you click `mouse-1' on the lighter then full information
    about the character is shown in buffer `*Help*', including the
    font and faces used (for the character in the buffer, not for
    the lighter in the mode-line).

  * If you click `mouse-2' on the lighter then the character is
    copied to the secondary selection.

  * If you click `mouse-3' on the lighter then the character is
    shown enlarged in a tooltip.


 Commands defined here:

   `mlc-char-in-mode-line-mode', `mlc-char-in-mode-line-mode-global'.

 Faces defined here:

   `mlc-mode-line-char-format', `mlc-mode-line-char-format-code'.

 Non-interactive functions defined here:

   `mlc-copy-char-to-second-sel',
   `mlc-turn-on-char-in-mode-line-mode'.

 Internal variables defined here:

   `mlc-mode-line-char-format',
   `mlc-char-in-mode-line-mode-initialized'.
