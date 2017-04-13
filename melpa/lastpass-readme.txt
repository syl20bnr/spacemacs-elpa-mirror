This package contains a wrapper for the LastPass command line utility
lpass and a custom auth-source.

Auth-source functionality relies on an adviced function, `auth-source-backend-parse'.
Can for example be used to get smtpmail to use LastPass instead of authinfo file.
To use this auth-source, LastPass account name must be set to match the host.
One way to achieve this is to keep a separate group in LastPass called auth-source
where all hosts are stored.  To enable LastPass auth-source, run
`lastpass-auth-source-enable'.

Several functions used for interacting with lpass in Emacs are
made available to the user through the "M-x" interface.
Such functions are:
- `lastpass-login'
- `lastpass-logout'
- `lastpass-status'
- `lastpass-getpass'
- `lastpass-addpass'
- `lastpass-visit-url'
These functions can also used in elisp when configuring Emacs.

A lpass manager is available by running `lastpass-list-all'.
This function will list all passwords and a major mode takes care of
setting som neat keybindings to some neat functions.  All these functions
are shown in the lpass buffer, and is self-explanatory.

For more information, see the readme at https://github.com/storvik/emacs-lastpass
