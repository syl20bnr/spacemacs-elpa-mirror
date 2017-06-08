`nlinum-hl' is an nlinum extension that tries to mitigate disappearing line
numbers in buffers that have been open a while (a known issue with nlinum).

Installation:

M-x package-install RET nlinum-hl

  (require 'nlinum-hl)

By itself, `nlinum-hl` does nothing. You'll need to attach one of its
functions or hooks somewhere. I leave it to you to decide where, as it
depends on how bad the problem is for you. Here are some examples:

  ;; Runs occasionally, though unpredictably
  (add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

  ;; after X amount of idle time
  (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

  ;; whenever Emacs loses/gains focus
  (add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)

  ;; when switching windows
  (advice-add #'select-window :before #'nlinum-hl-do-flush)
  (advice-add #'select-window :after  #'nlinum-hl-do-flush)
