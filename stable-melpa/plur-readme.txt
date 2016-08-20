* Plur

This package introduces a new syntax =...{subexp1,subexp2,...}...= to search and
replace a group of words.  Three commands are provided by this package:

- ~plur-isearch-forward~
- ~plur-query-replace~
- ~plur-replace~

** Replace example

To replace "mouse" with "cat" and "mice" with "cats" using:

#+BEGIN_SRC undefined
M-x plur-query-replace RET m{ouse,ice} RET cat{,s} RET
#+END_SRC

For more examples,

- Facility to Building

facilit{y,ies}  building{,s}

- Mouse to Trackpad

m{ouse,ice}  trackpad{,s}

- Swap Emacs and Vim

{emacs,vim}  {vim,emacs}

** Search example

To search "mouse" and "mice" using:

#+BEGIN_SRC undefined
M-x plur-isearch-forward RET m{ouse,ice}
#+END_SRC

** Requirements

- Emacs 24.4 or higher

** Installation

*** MELPA

Plur is available from [[https://melpa.org][Melpa]]. You can install it using:

#+BEGIN_SRC undefined
M-x package-install RET plur RET
#+END_SRC

*** Manually

Make sure plur.el is saved in a directory in you ~load-path~ and load it. Add something
like

#+BEGIN_SRC emacs-lisp
(add-to-list 'load-path "path/to/plur/")
(require 'plur)
#+END_SRC

to your init file.

** Acknowledge

This package is inspired by [[https://github.com/tpope/vim-abolish][vim-abolish]].
