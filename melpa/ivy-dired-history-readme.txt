remember dired directory you have visited and list them
using `ivy.el'.

integrating dired history feature into commands like
dired-do-copy and dired-do-rename. What I think of is that when
user press C (copy) or R (rename) mode, it is excellent to have
an option allowing users to select a directory from the history
list.



Installation:

(require 'savehist)
(add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
(savehist-mode 1)

(with-eval-after-load 'dired
  (require 'ivy-dired-history)
if you are using ido,you'd better disable ido for dired
(define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
  (define-key dired-mode-map "," 'dired))
