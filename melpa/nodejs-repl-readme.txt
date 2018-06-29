This program is derived from comint-mode and provides the following features.

 * TAB completion same as Node.js REPL
 * file name completion in string
 * incremental history search


Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
and add the following line to your .emacs:

   (require 'nodejs-repl)

Type M-x nodejs-repl to run Node.js REPL.
See also `comint-mode' to check key bindings.

You can define key bindings to send JavaScript codes to REPL like below:

    (add-hook 'js-mode-hook
              (lambda ()
                (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
                (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
                (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
                (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
