                     ______________________________

                      ORG-TABLE-STIKCY-HEADER-MODE

                              Junpeng Qiu
                     ______________________________


Table of Contents
_________________

1 Overview
2 Usage
3 Demo


A minor mode to show the sticky header for org-mode tables.


1 Overview
==========

  Similar to `semantic-stickyfunc-mode', this package uses the header
  line to show the table header when it is out of sight.


2 Usage
=======

  To install manually:
  ,----
  | (add-to-list 'loat-path "/path/to/org-table-sticky-header.el")
  `----

  `M-x org-table-sticky-header-mode' to enable the minor mode in an
  org-mode buffer.

  To automatically enable the minor mode in all org-mode buffers, use
  ,----
  | (add-hook 'org-mode-hook 'org-table-sticky-header-mode)
  `----


3 Demo
======

  [./screenshots/demo.gif]
