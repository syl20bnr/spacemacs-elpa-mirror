`easy-escape-minor-mode' composes double backslashes (escape characters) into
single backslashes, and highlights them to improve readability.

For example, `easy-escape` displays "\\(?:\\_<\\\\newcommand\\_>\\s-*\\)?"
as "\(?:\_<\\newcommand\_>\s-*\)?".  The underlying text is not modified.

The default it to use a single \ character instead of two, but the character
used and its color can be customized using `easy-escape-face' and
`easy-escape-character' (which see).

Suggested setup:
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
or
  (add-hook 'prog-mode-hook 'easy-escape-minor-mode)

NOTE: If you find the distinction between the fontified double-slash and the
single slash too subtle, try the following:

* Adjust the foreground of `easy-escape-face'
* Set `easy-escape-character' to a different character.
