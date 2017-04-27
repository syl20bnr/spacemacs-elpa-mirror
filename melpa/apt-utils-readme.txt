Package to interface Emacs with APT.  Start things off using e.g.:
M-x apt-utils-show-package RET emacs21 RET

Other packages (dependencies, conflicts etc.) can be navigated
using apt-utils-{next,previous}-package,
apt-utils-choose-package-link or apt-utils-follow-link.  Return to
the previous package with apt-utils-view-previous-package.
ChangeLog and README files for the current package can easily be
accessed with, for example, apt-utils-view-changelog.

For normal (i.e., not virtual) packages, the information can be
toggled between `package' and `showpkg' displays using
apt-utils-toggle-package-info; the latter is useful for the
"Reverse Depends".

View the key bindings with describe-mode (bound to ? by default).
