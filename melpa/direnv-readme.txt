direnv <http://direnv.net/> is an environment switcher for the shell.  Users
define a .envrc file in their directories, and when they change into those
directories,  direnv loads those environment variables.  When they change
out, direnv unloads them.

This is particularly useful for defining project-specific environment
variables.

This module integrates direnv with Emacs.

The main function is 'direnv-load-environment', which exports the
environment difference from direnv and sets in globally within Emacs,
making the direnv environment settings available to any subprocess.  When
you change to a different buffer, the environment variable changes are
reverted, thanks to the magic of direnv.

One way to use this is as a hook in find-file and buffer-list-update-hook:

   (add-hook 'find-file-hook 'direnv-load-environment)
   (add-hook 'buffer-list-update-hook 'direnv-load-environment)
