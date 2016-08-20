To install, put this file in your load-path, byte-compile it, and add
the following to your .emacs:

  (load "escreen")
  (escreen-install)

If you are using Emacs 19, you may have trouble loading this program
because of the customs syntax officially introduced in Emacs 20.  In
that case, first load cust-stub.el, available from

    http://www.splode.com/~friedman/software/emacs-lisp/

Updates to escreen.el will also be made available on that page.

Inspired by:
  * wicos.el, written by Heikki Suopanki <suopanki@phoenix.oulu.fi>
  * `screen', written by Oliver Laumann, Juergen Weigert,
    and Michael Schroeder.

Todo:
  * make escreen-menu display list of screens for all frames
  * ability to lock window configurations against change or deletion
  * ability to renumber screens
  * symbolic names for screens (a la wicos.el); partly started
  * switching active screen from pull-down menu from menubar
  * switching active screen from escreen menu
  * persistance of screens across instances of emacs
    [look at johnw's work on this; depends on additional non-standard
    packages but perhaps those parts can be reimplemented inline.]
