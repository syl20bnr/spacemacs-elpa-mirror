Side Notes
==========

Quickly display your quick side notes in quick side window.

Side notes live in a file in the current directory defined by custom option
`side-notes-file', which defaults to "notes.txt".

Installation
------------

Add the following to you init file:

(define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes)
