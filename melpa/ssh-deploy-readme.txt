`ssh-deploy' enables automatic deploys on explicit-save, manual
uploads, downloads, differences, remote terminals and remote directory browsing
via key-pair password-less authorized SSH connections.  To do this it uses `tramp',
`tramp-term', `scp', `ediff' and `ztree'.
By setting the variables (globally or per directory):
`ssh-deploy-root-local`,`ssh-deploy-root-remote`,
`ssh-deploy-on-explicit-save` you can setup a directory for
SSH deploy.

- To setup a hook on explicit save do this:
    (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))

- To set key-bindings do something like this:
    (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
    (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
    (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
    (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
    (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))

Now when your in a directory which is deployed via SSH you can access these features.

Please see README.md from the same repository for documentation.
