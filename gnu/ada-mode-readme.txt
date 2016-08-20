Emacs Ada mode version 5.2.0

Ada mode requires Emacs 24.2 or greater

See ada-mode.texi (or a compiled version) for help on using and
customizing Ada mode, upgrading from previous versions, and notes for
Ada mode developers.

Ada mode is provided as a Gnu ELPA package; to install the package,
add to ~./emacs:

(package-initialize)

then invoke M-x list-packages, install Ada mode.

Note that you must have the Emacs lisp sources installed; that's a
separate package in some distributions.

To install Ada mode, gpr mode, and associated utilities from this distribution:

Unzip to a convenient place (we will use ~/ada-mode in the following).

In a shell:
$ cd ~/ada-mode/build/wisi
$ make byte-compile

Edit your ~/.emacs, add:

(add-to-list 'load-path (expand-file-name "~/ada-mode"))


Ada mode will be automatically loaded when you open a file with a
matching extension (default *.ads, *.adb).


By default ada-mode uses gnat find for cross-reference functions.
There is support for an alternative: gpr_query. See ada-mode.texi
section Installation for more.

(end of file)
