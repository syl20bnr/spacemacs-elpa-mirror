Provides an auto-complete source for nim using the "nim" executable.
To enable this auto-complete source in nim-mode, use code like the
following:

(eval-after-load 'nim-mode
  '(add-hook 'nim-mode-hook 'ac-nim-enable))
