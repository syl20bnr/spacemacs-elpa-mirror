A cheap hack of hl-line.el to highlight the thingy at point instead
of the line at point. Sort of inspired by a post by Kent Pitman to
CLL in which he ranted about font-locking. You can find the latest
version here: <URL:http://oconnor.cx/elisp/hl-sexp.el>

An overlay is used, active only on the selected window.  Hooks are
added to `pre-command-hook' and `post-command-hook' to activate and
deactivate (by deleting) the overlay.  `hl-sexp-unhighlight', on
`pre-command-hook', deactivates it unconditionally in case the
command changes the selected window.  (It does so rather than
keeping track of changes in the selected window).
`hl-sexp-highlight', on `post-command-hook', activates it again
across the window width.

You could make variable `hl-sexp-mode' buffer-local to avoid
highlighting specific buffers.
