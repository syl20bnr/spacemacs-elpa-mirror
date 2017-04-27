This package can automatically add whitespaces before and after operators.
e.g, "=" becomes " = ", "+=" becomes " += ". which will be handy for
writing C-style sources.

To use, set all the operators smart at one time, then ajust some at
your wish e.g.

(defun my-c-mode-common-hook()
  (smart-insert-operator-hook)

  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-set-key (kbd "*") 'c-electric-star))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

Besides `smart-insert-operator' when writing, there's also
`smart-beautify-operator' to beautify ugly codes, be careful there
are still some unresolved issues in `smart-beautify-operator'!

Put this file into your load-path and the following into your ~/.emacs:
  (require 'smart-operator)
