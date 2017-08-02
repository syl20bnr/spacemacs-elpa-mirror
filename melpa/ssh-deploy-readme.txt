`ssh-deploy' enables automatic deploys on explicit-save, manual uploads, renaming,
deleting, downloads, file differences, remote terminals, detection of remote changes and remote directory browsing via TRAMP.

To do this it progressively uses `tramp-term' and `async'.
By setting the variables (globally or per directory):
`ssh-deploy-root-local',`ssh-deploy-root-remote', `ssh-deploy-on-explicit-save'
you can setup a directory for `SSH' or `FTP' deployment.

For asynchronous transfers you need to setup `~/.netrc' or key-based authorization or equivalent for automatic authentication.

Example contents of `~/.netrc' for `FTP':
machine myserver.com login myuser port ftp password mypassword

Set permissions to this file to `700' with you as the owner.

- To setup a upload hook on save do this:
    (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))

- To setup automatic storing of base revisions and download of external changes do this:
    (add-hook 'find-file-hook (lambda() (if ssh-deploy-automatically-detect-remote-changes (ssh-deploy-remote-changes-handler)) ))

- To avoid the directory variables warning add this:
       (put 'ssh-deploy-root-local 'safe-local-variable 'identity)
       (put 'ssh-deploy-root-remote 'safe-local-variable 'identity)
       (put 'ssh-deploy-on-explicit-save 'safe-local-variable 'identity)
       (put 'ssh-deploy-async 'safe-local-variable 'identity)

- To set key-bindings do something like this:
    (global-set-key (kbd "C-c C-z f") (lambda() (interactive)(ssh-deploy-upload-handler-forced) ))
    (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
    (global-set-key (kbd "C-c C-z D") (lambda() (interactive)(ssh-deploy-delete-handler) ))
    (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
    (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
    (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
    (global-set-key (kbd "C-c C-z T") (lambda() (interactive)(ssh-deploy-remote-terminal-eshell-handler) ))
    (global-set-key (kbd "C-c C-z R") (lambda() (interactive)(ssh-deploy-rename-handler) ))
    (global-set-key (kbd "C-c C-z e") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
    (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))

An illustrative example for `SSH' deployment, /Users/Chris/Web/Site1/.dir.locals.el:
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


Here is a list of other variables you can set globally or per directory:

* `ssh-deploy-root-local' The local root that should be under deployment *(string)*
* `ssh-deploy-root-remote' The remote root that should be under deployment, should follow a `/protocol:user@host:path` format *(string)*
* `ssh-deploy-debug' Enables debugging messages *(boolean)*
* `ssh-deploy-revision-folder' The folder used for storing local revisions *(string)*
* `ssh-deploy-automatically-detect-remote-changes' Enables automatic detection of remote changes *(boolean)*
* `ssh-deploy-on-explicit-save' Enabled automatic uploads on save *(boolean)*
* `ssh-deploy-exclude-list' A list defining what paths to exclude from deployment *(list)*
* `ssh-deploy-async' Enables asynchronous transfers (you need to have `async.el` installed as well) *(boolean)*

Please see README.md from the same repository for documentation.
