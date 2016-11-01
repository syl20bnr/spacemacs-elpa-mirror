A pretty simple (but, at least for me, effective) backend for Emacs 25 xref
library, using GNU Global.


Prerequisites:
--------------

* GNU Global.
* Emacs version >= 25.1

Installation using package.el:
------------------------------

Gxref is now available on [MELPA](https://melpa.org/#/getting-started).
Once you get MELPA set up, you can install gxref by typing
M-x package-install RET gxref RET

Manual Installation:
--------------------

Clone the repository, and add it to your load path, or just download
gxref.el and put it in a directory that is already in your load path.

Usage:
------

Add something like the following to your init.el file:

(add-to-list 'xref-backend-functions 'gxref-xref-backend)

This will add gxref as a backend for xref functions.  The backend will
be used whenever a GNU Global project is detected - that is, whenever
a GTAGS file exists at the current directory or above it.

by default, xref functions are bound as follows:


| Function              | Binding  |
|-----------------------+----------|
| xref-find-definitions | M-.      |
| xref-find-references  | M-?      |
| xref-find-apropos     | C-M-.    |


Configuration/Customization
---------------------------

gxref can be customized in several ways.  use

M-x customize-group RET gxref RET

to start.

Additionally, the following buffer-local variables can be defined to tweak
the behavior:

 - gxref-gtags-root -

   Explicitly set the project root.  This will make gxref backend
   available in the buffer even if it is not under a GTAGS project.

 - gxref-gtags-conf

   The GTAGS/GLOBAL configuration file to use.

 - gxref-gtags-label

   GTAGS/GLOBAL Configuration label

 - gxref-gtags-lib-path

   the library path.  Passed to GNU Global using the GTAGSLIBPATH
 environment variable.

## Disclaimers:

Because the xref API in Emacs 25.1 is experimental, it's likely to
change in ways that will break this package.  I will try to
keep up with API changes.



## Missing/TODO:

* Generate DB at root
* project.el integration.
