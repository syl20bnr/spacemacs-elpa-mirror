 Whenever "lambda" appears as a separate word, it is displayed using the
 Greek letter.

 Put this in your init file (~/.emacs), to turn this display on for
 the modes in `pretty-lambda-auto-modes', which by default are the
 usual Lisp modes:

  (require 'pretty-lambdada)
  (pretty-lambda-for-modes)

 You can toggle pretty-lambda display on/off in any buffer, using
 command `pretty-lambda-mode'.  Use `global-pretty-lambda-mode' to
 toggle the display in all buffers.

 Three alternative ways to turn on pretty-lambda display for a
 specific buffer mode:

 1. (add-hook 'my-mode-hook 'turn-on-pretty-lambda-mode)
 2. (pretty-lambda 'my-mode)
 3. (add-hook 'my-mode-hook 'pretty-lambda)

 The first way uses minor mode `pretty-lambda-mode', so you can
 easily toggle pretty-lambda display.  The last two just turn on
 the display.  To turn it off, use `turn-off-pretty-lambda-mode'.


 User options defined here:

   `global-pretty-lambda-mode', `pretty-lambda-auto-modes',
   `pretty-lambda-mode'.

 Commands defined here:

   `global-pretty-lambda-mode', `pretty-lambda-for-modes',
   `pretty-lambda-mode'

 Non-interactive functions defined here:

   `pretty-lambda', `turn-off-pretty-lambda-mode',
   `turn-on-pretty-lambda-mode'.
