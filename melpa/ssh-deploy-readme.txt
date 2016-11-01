`ssh-deploy' enables automatic deploys on explicit-save, manual
uploads, downloads, differences, remote terminals and remote directory browsing
via key-pair password-less authorized SSH connections and password-based FTP connections.
To do this it uses `tramp',`tramp-term', `scp', `curl', `ediff' and `ztree'.
By setting the variables (globally or per directory):
`ssh-deploy-root-local',`ssh-deploy-root-remote',
`ssh-deploy-on-explicit-save' you can setup a directory for
SSH or FTP deployment.

- To setup a hook on explicit save do this:
    (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))

- To set key-bindings do something like this:
    (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
    (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
    (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
    (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
    (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))

An illustrative example for SSH, /Users/Chris/Web/Site1/.dir.locals.el
((nil . (
  (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
  (ssh-deploy-root-remote . "/ssh:web@myserver.com:/var/www/site1/")
  (ssh-deploy-on-explicity-save . t)
)))

An example for FTP, /Users/Chris/Web/Site2/.dir.locals.el:
((nil . (
(ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
(ssh-deploy-root-remote . "/ftp:myuser:mypassword@myserver.com:/site2/")
(ssh-deploy-on-explicit-save . nil)
)))

Now when you are in a directory which is deployed via SSH or FTP you can access these features.

Please see README.md from the same repository for documentation.
