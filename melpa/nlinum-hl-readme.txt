`nlinum-hl' is an nlinum extension that tries to mitigate disappearing line
numbers in buffers that have been open a while (a known issue with nlinum).

Installation:

M-x package-install RET nlinum-hl

  (require 'nlinum-hl)
  (add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

The `post-gc-hook' hook works flawlessly for me. In case this isn't true for
everyone, here are some alternatives:

  ;; whenever Emacs loses/gains focus
  (add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)

  ;; when idling
  (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

  ;; when switching windows
  (advice-add #'select-window :before #'nlinum-hl-do-flush)
  (advice-add #'select-window :after  #'nlinum-hl-do-flush)
