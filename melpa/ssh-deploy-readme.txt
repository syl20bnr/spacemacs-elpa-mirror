`ssh-deploy' enables automatic deploys on explicit-save, manual uploads,
downloads, differences, remote terminals, detection of remote changes and remote directory browsing via TRAMP.
To do this it progressively uses `tramp', `tramp-term', `ediff', `async` and `ztree'.
By setting the variables (globally or per directory):
`ssh-deploy-root-local',`ssh-deploy-root-remote', `ssh-deploy-on-explicit-save'
you can setup a directory for SSH or FTP deployment.

For asynchronous transfers you need to setup `~/.netrc' or equivalent for automatic authentication.

Example contents of `~/.netrc':
machine myserver.com login myuser port ftp password mypassword

Set permissions to this file to `700' with you as the owner.

- To setup a upload hook on save do this:
    (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))

- To setup automatic storing of base revisions and download of external changes do this:
    (add-hook 'find-file-hook (lambda() (if ssh-deploy-automatically-detect-remote-changes (ssh-deploy-remote-changes-handler)) ))

- To set key-bindings do something like this:
    (global-set-key (kbd "C-c C-z f") (lambda() (interactive)(ssh-deploy-upload-handler-forced) ))
    (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
    (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
    (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
    (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
    (global-set-key (kbd "C-c C-z r") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
    (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))

An illustrative example for `SSH' deployment, /Users/Chris/Web/Site1/.dir.locals.el
((nil . (
  (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/site1/")
  (ssh-deploy-on-explicity-save . t)
)))

An example for `FTP' deployment, /Users/Chris/Web/Site2/.dir.locals.el:
((nil . (
  (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
  (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/var/www/site2/")
  (ssh-deploy-on-explicit-save . nil)
)))

Now when you are in a directory which is deployed via SSH or FTP you can access these features.

Please see README.md from the same repository for documentation.
