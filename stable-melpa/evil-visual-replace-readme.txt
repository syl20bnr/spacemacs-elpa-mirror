Provides versions of the `query-replace' (M-%) and `replace-regexp' (C-M-%)
commands which work for evil-mode visual-state, including visual blocks
(rectangular regions).  The native Emacs versions don't understand evil's
visual blocks, and treat them as normal regions.

Note that these commands are specifically intended for visual state and have
barely been tested in non-visual states.  Rather than globally replacing
the native commands, it is recommended to rebind them in
`evil-visual-state-map'.

Install:

(evil-visual-replace-visual-bindings)
