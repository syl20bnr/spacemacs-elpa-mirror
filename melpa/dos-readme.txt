Major mode for editing Dos scripts (batch files). Provides syntax
highlighting, a basic template, access to Dos help pages, imenu/outline
navigation, and the ability to run scripts from within Emacs. The syntax
groups for highlighting are:

Face                          Example
dos-label-face                :LABEL
font-lock-comment-face        rem
font-lock-builtin-face        copy
font-lock-keyword-face        goto
font-lock-warning-face        cp
font-lock-constant-face       [call] prog
font-lock-variable-name-face  %var%
font-lock-type-face           -option

Usage:

See documentation of function `dos-mode'.

Separate package `dos-indent' (Matthew Fidler) provides rudimentary
indentation, see http://www.emacswiki.org/emacs/dos-indent.el.

Acknowledgements:

Inspired by `batch-mode' (Agnar Renolen) and `cmd-mode' (Tadamegu Furukawa).
