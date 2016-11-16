org-board uses `org-attach' and `wget' to provide a bookmarking and
web archival system directly from an Org file.  Any `wget' switch
can be used in `org-board', and presets (like user agents) can be
set for easier control.  Every snapshot is logged and saved to an
automatically generated folder, and snapshots for the same link can
be compared using the `ztree' package (optional dependency).

Commands defined here:

`org-board-archive', `org-board-archive-dry-run',
`org-board-delete-all', `org-board-open', `org-board-new',
`org-board-diff', `org-board-cancel'.

Variables defined here:

`org-board-wget-program', `org-board-wget-switches',
`org-board-wget-show-buffer', `org-board-log-wget-invocation',
`org-board-archive-date-format', `org-board-agent-header-alist',
`org-board-domain-regexp-alist', `org-board-default-browser'.

Keymap defined here:

`org-board-keymap'.

Functions advised here:

`org-thing-at-point', with `org-board-thing-at-point'.
