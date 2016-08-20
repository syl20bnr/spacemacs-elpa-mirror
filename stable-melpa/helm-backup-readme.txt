To store change every time you save a file add :
(add-hook 'after-save-hook 'helm-backup-versioning)
or from Emacs you can do :
M-x customize-variable RET after-save-hook RET [INS] helm-backup-versioning

To retrieve file backup, from buffer call `helm-backup' :
M-x helm-backup
for convenience you can define key binding as follow :
(global-set-key (kbd "C-c b") 'helm-backup)
