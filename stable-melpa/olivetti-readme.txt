Olivetti
========

Olivetti is a simple Emacs minor mode for a nice writing environment.

Features
--------

- Set a desired text body width to automatically resize window margins
  to keep the text comfortably in the middle of the window.
- Text body width can be the number of characters (an integer) or a
  fraction of the window width (a float between 0.0 and 1.0).
- Interactively change body width with:
  `olivetti-shrink` C-c [
  `olivetti-expand` C-c ]
  and `olivetti-set-width`.
- If `olivetti-body-width` is an integer, the text body width will scale
  with use of `text-scale-mode`, whereas if a fraction (float) then the
  text body width will remain at that fraction.
- Optionally remember the state of `visual-line-mode` on entry and
  recall its state on exit.
- Optionally hide the mode-line for distraction-free writing.

Requirements
------------

- Emacs 24.4

Installation
------------

Olivetti is available through [MELPA][] and [MELPA-stable][]. I
encourage installing the stable version.

Alternately, download the [latest release][] and put it in your
`load-path`.

[melpa]: https://melpa.org/ "MELPA"
[melpa-stable]: https://stable.melpa.org/ "MELPA"
[latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

Known Bugs
----------

- `linum-mode` currently has a bug that overwrites margin settings,
  making it incompatible with Olivetti. More information here:
  <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20674>

Please report bugs on GitHub [Issues][] page.

[issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

History
-------

See [Releases][].

[releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"


