This minor mode highlights indentation levels via font-lock. Indent widths
are dynamically discovered, which means this correctly highlights in any
mode, regardless of indent width, even in languages with non-uniform
indentation such as Haskell. This mode works properly around hard tabs and
mixed indentation, and it behaves well in large buffers.

To install, put this file in your load-path, and require it:

  (require 'highlight-indent-guides)

Then, do M-x highlight-indent-guides-mode to enable it. To enable it
automatically in most programming modes, use the following:

  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

To set the display method, use:

  (setq highlight-indent-guides-method METHOD)

Where METHOD is either 'fill, 'column, or 'character.

To change the colors used for highlighting, use:

  (set-face-background 'highlight-indent-guides-odd-face "color")
  (set-face-background 'highlight-indent-guides-even-face "color")

To change the character and face used for drawing guide lines, use:

  (setq highlight-indent-guides-character ?ch)
  (set-face-foreground 'highlight-indent-guides-character-face "color")
