Commentary:

Highlights the background at a given character column.

Commands `column-marker-1', `column-marker-2', and
`column-marker-3' each highlight a given column (using different
background colors, by default).

- With no prefix argument, each highlights the current column
  (where the cursor is).

- With a non-negative numeric prefix argument, each highlights that
  column.

- With plain `C-u' (no number), each turns off its highlighting.

- With `C-u C-u', each turns off all column highlighting.

If two commands highlight the same column, the last-issued
highlighting command shadows the other - only the last-issued
highlighting is seen.  If that "topmost" highlighting is then
turned off, the other highlighting for that column then shows
through.

Examples:

M-x column-marker-1 highlights the column where the cursor is, in
face `column-marker-1'.

C-u 70 M-x column-marker-2 highlights column 70 in face
`column-marker-2'.

C-u 70 M-x column-marker-3 highlights column 70 in face
`column-marker-3'.  The face `column-marker-2' highlighting no
longer shows.

C-u M-x column-marker-3 turns off highlighting for column-marker-3,
so face `column-marker-2' highlighting shows again for column 70.

C-u C-u M-x column-marker-1 (or -2 or -3) erases all column
highlighting.

These commands use `font-lock-fontify-buffer', so syntax
highlighting (`font-lock-mode') must be turned on.  There might be
a performance impact during refontification.


Installation: Place this file on your load path, and put this in
your init file (`.emacs'):

(require 'column-marker)

Other init file suggestions (examples):

Highlight column 80 in foo mode.
(add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80)))

Use `C-c m' interactively to highlight with face `column-marker-1'.
(global-set-key [?\C-c ?m] 'column-marker-1)


Please report any bugs!
