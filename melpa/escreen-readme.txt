To install, put this file in your load-path, byte-compile it, and add
the following to your .emacs:

  (load "escreen")
  (escreen-install)

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
