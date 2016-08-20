Load this script

  (require 'scratch-pop)

and you can popup a scratch buffer with "M-x scratch-pop". If a
scratch is already displayed, then another one is created. You may
also bind some keys to "scratch-pop" if you want.

  (global-set-key "C-M-s" 'scratch-pop)

You can backup scratches by calling `scratch-pop-backup-scratches'
after setting `scratch-pop-backup-directory', and then restore
backup by calling `scratch-pop-restore-scratches'.

  (scratch-pop-backup-directory)
  (scratch-pop-restore-scratches)

It's good idea to put `scratch-pop-backup-directory' into
`kill-emacs-hook' so that scratches are automatically saved when
killing emacs.

  (add-hook 'kill-emacs-hook 'scratch-pop-backup-scratches)
