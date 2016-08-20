anything-sage provides 3 commands.
Bind them to some keys: e.g.
(defun anything-sage-set-up ()
  (local-set-key (kbd "C-c C-i") 'anything-sage-shell)
  (local-set-key (kbd "C-c C-d") 'anything-sage-shell-describe-object-at-point)
  (local-set-key (kbd "M-r") 'anything-sage-command-history))
(add-hook 'sage-shell-mode-hook 'anything-sage-set-up)
