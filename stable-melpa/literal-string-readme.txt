Literal String Mode is a minor mode for editing multi-line literal
strings in a dedicated buffer.

When enabled, edit the literal string at point using C-c '
(literal-string-edit-string), this will copy the (unescaped and
deindented) content of the string to a dedicated literal string
editing buffer that has Literal String Editing Mode (a minor mode)
enabled.

To exit the current literal string buffer copy the edited string
back into the original source buffer with correct quoting and
escape sequences, press C-c '
(literal-string-edit-string-exit).

To discard your changes to the editing buffer, press C-c C-k
(literal-string-edit-string-abort)

To enable literal-string-mode in your preferred programming modes,
turn it on using the relevant mode hooks.
