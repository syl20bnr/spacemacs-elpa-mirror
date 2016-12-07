A minor mode for inputing math symbols and Unicode symbols.

Call `global-xah-math-input-mode' to toggle on/off for all buffers.
Call `xah-math-input-mode' to toggle on/off for current buffer.

In lisp code:
(global-xah-math-input-mode 1) ; turn on globally
(global-xah-math-input-mode 0) ; turn off globally
(xah-math-input-mode 1) or (xah-math-input-mode-on) ; turn on for current buffer
(xah-math-input-mode 0) or (xah-math-input-mode-off) ; turn off for current buffer

Type “inf”, then press 【Shift+Space】 `xah-math-input-change-to-symbol', then it becomes “∞”.
Other examples:
“a” ⇒ “α”.
“p” ⇒ “π”.
“/=” ⇒ “≠”.
“>=” ⇒ “≥”.
“=>” ⇒ “⇒”.
“->” ⇒ “→”.
“and” ⇒ “∧”.
etc.

Call `xah-math-input-list-math-symbols' to see all abbrevs.

Home page: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
