This is a input method to exchange your number keys with the symbols above them.
For example, when you press `1', `!' will be entered.
If `!' was bound to function other than `self-insert-command', it will be
called.  But if evil is installed, it's only worked at insert-state.
Pressing `S+1' will enter `1', etc.
It's a input method, so it only works on insert mode in current buffer,
so chords like `C-x 2' will act as normal, and the key won't be translated
in minibuffer.
