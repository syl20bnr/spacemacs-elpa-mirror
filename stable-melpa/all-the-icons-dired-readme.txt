To use this package, simply add this to your init.el:
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

To manually install, add this to your init.el before the hook mentioned above.
(add-to-load-path (expand-file-name "~/path/to/all-the-icons-dired"))
(load "all-the-icons-dired.el")
