This package provides a minor mode that will do two things
after a successful recompile:
1) bury the *compilation* buffer, and
2) restore your window configuration to how it looked when you
issued the last recompile, ignoring successive compilations to
squash bugs.

Usage:

(bury-successful-compilation 1)
