Why do I need this?

Sometimes you have need to sync private emacs configuration files
between different PCs. These normally include corporation project
configuration, corporation code, personal project code, etc.

How private works?

Private works by encrypt private configuration files with a password
you specified.

Usage:

Put every private configuration into a private configuration directory
default to '~/.emacs.d/private'.
and make git ignoring this directory.
(private-require &optional package)
This function requires all your private configuration files.
If package name is specified, require just that package.
(private-find-configuration-file)
This function creates or visits a private configuration file.
(private-backup)
This function backups all your private configuration files.
So it's safe to put any private project related configuraiton on the
public git repo.
(private-recover)
This function recovers all your private configuration files.
(private-clear-backup)
This function removes all your private configuration backup files.
