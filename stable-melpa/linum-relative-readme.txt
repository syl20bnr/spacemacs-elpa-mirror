[![MELPA](http://melpa.org/packages/linum-relative-badge.svg)](http://melpa.org/#/linum-relative)
[![MELPA Stable](http://stable.melpa.org/packages/linum-relative-badge.svg)](http://stable.melpa.org/#/linum-relative)

![Screenshot](https://github.com/coldnew/linum-relative/raw/master/screenshot/screenshot1.jpg)

linum-relative lets you display relative line numbers for current buffer.


Installation:

If you have `melpa` and `emacs24` installed, simply type:

    M-x package-install linum-relative

And add the following to your .emacs

    (require 'linum-relative)

Setup & Tips:

The non-interactive function *linum-on* (which should already be built into recent GNU Emacs distributions), turns on side-bar line numbering:

    (linum-on)

and alternatively, by enabling `linum-mode`:

    M-x linum-mode

Relative line numbering should already be enabled by default (by installing this package), following *linum-on* or enabling *linum-mode*. One can also use the *linum-relative-toggle* interactive function to switch between relative and non-relative line numbering:

    M-x linum-relative-toggle
