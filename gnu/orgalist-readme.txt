This library provides Org mode's plain lists in non-Org buffers, as
a minor mode.

More specifically, it supports syntax for numbered, unnumbered,
description items, checkboxes, and counter cookies.  See (info
"(org) Plain Lists") and (info "(org) Checkboxes") for details
about the syntax of such constructs.

The following features are supported:

- Item insertion (M-<RET>)
- Navigation (M-<UP>, M-<DOWN>)
- Indentation (M-<LEFT>, M-<RIGHT>, M-S-<LEFT>, M-S-<RIGHT>, <TAB>)
- Re-ordering (M-S-<UP>, M-S-<DOWN>)
- Toggling checkboxes (C-c C-c)
- Cycling bullets (C-c -)
- Sorting items (C-c ^)
- Filling items (M-q)
- Auto filling (when Auto Fill mode is enabled)

Note that the bindings above are only available when point is in an
item (for M-<RET>, M-<UP>, M-<DOWN> and M-q) or exactly at an item.

The library also implements radio lists:

Call the `orgalist-insert-radio-list' function to insert a radio list
template in HTML, LaTeX, and Texinfo mode documents.  Sending and
receiving radio lists works is the same as for radio tables (see
Org manual for details) except for these differences:

- Orgalist minor mode must be active;
- Use the "ORGLST" keyword instead of "ORGTBL";
- `M-x orgalist-send-list' works only on the first list item.

Built-in translator functions are: `org-list-to-latex',
`org-list-to-html' and `org-list-to-texinfo'.  They use the
`org-list-to-generic' translator function.  See its documentation for
parameters for accurate customizations of lists.  Here is a LaTeX
example:

  % BEGIN RECEIVE ORGLST to-buy
  % END RECEIVE ORGLST to-buy
  \begin{comment}

  #+ORGLST: SEND to-buy org-list-to-latex
  - a new house
  - a new computer
    + a new keyboard
    + a new mouse
  - a new life
  \end{comment}

`M-x orgalist-send-list' on "a new house" inserts the translated
LaTeX list in-between the "BEGIN RECEIVE" and "END RECEIVE" marker
lines.