Auto virtualenvwrapper activates virtualenv automatically when called.
To use auto-virtualenvwrapper set hooks for `auto-virtualenvwrapper-activate'

For example:
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
(add-hook 'projectile-after-switch-project-hook #'auto-virtualenvwrapper-activate)
