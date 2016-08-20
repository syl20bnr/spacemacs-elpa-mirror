CriticMarkup for Emacs
======================

`cm-mode' is a minor mode that provides support for CriticMarkup in Emacs.

CriticMarkup is a way for authors and editors to track changes to
documents in plain text. It defines the following patterns for marking
changes:

- Addition {++ ++}
- Deletion {-- --}
- Substitution {~~ ~> ~~}
- Comment {>> <<}
- Highlight {== ==}{>> <<}

`cm-mode' provides the following functionality:

- font lock support
- key bindings to insert CriticMarkup.
- 'follow changes' mode: automatically record changes to the buffer.
- accept/reject changes interactively.
- automatically add author tag.
- navigation to move between changes.


Key bindings
------------

`C-c * a' : add text
`C-c * d' : delete text
`C-c * s' : make a substitution
`C-c * c' : add a comment
`C-c * i' : accept/reject change at point
`C-c * I' : accept/reject all changes interactively
`C-c * *' : move forward out of a change
`C-c * f' : move forward to the next change
`C-c * b' : move backward to the previous change
`C-c * C' : set author
`C-c * F' : activate follow changes mode


Usage
-----

See README.md for details.
