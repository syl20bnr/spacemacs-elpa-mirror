Someone like to reuse the current dired buffer to visit
another directory, so that you just need open one dired
buffer. but the bad point is ,you can't  easily go
forward and back in different dired directory. this file
can remember dired directory you have visited and list them
using `helm.el'.

Installation:

(require 'savehist)
(add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
(savehist-mode 1)
(eval-after-load 'dired
  '(progn (autoload 'helm-dired-history-view "helm-dired-history" "call `helm' to show dired history." t nil)
          (define-key dired-mode-map "," 'helm-dired-history-view)))
