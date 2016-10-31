A pretty simple (but, at least for me, effective) backend for xref
library, using GNU Global.

Prerequisites:

* GNU Global.
* Emacs version >= 25.1

Disclaimers:

Because the xref API in Emacs 25.1 is experimental, it's likely to
change in ways that will break this package.  I will try to
keep up with API changes.

Installing:

Add something like the following to your init.el file:
(add-to-list 'xref-backend-functions 'gxref-xref-backend)

TODO:

Generate DB at root
project.el integration.
Use process-environment to pass GTAGSROOT/GTAGSLABEL/etc.
