In vi/vim/evil-mode in normal-state, the cursor is viewed as being
ON a character rather than BETWEEN characters.  This breaks
smartparens particularly when it tries to put you past the last
character of the line, since normal-state will push point back one
character so it can be viewed as on the last character rather than
past it.  Additionally, most vim/evil movements go to the beginning
of a text object, with an alternative version that goes to the end.
Emacs tends to go past the end of things when it goes forward, but
to the beginning when it goes back.

This provides movements for normal-state that go forward and back
to specifically the beginning or end of sexps, as well as up or
down.
