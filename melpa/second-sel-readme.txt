   Commands that use the secondary selection.


 Commands defined here:

   `isearch-yank-secondary', `primary-to-secondary',
   `rotate-secondary-selection-yank-pointer',
   `secondary-yank|select|move|swap', `secondary-save-then-kill',
   `secondary-swap-region', `secondary-to-primary',
   `set-secondary-start', `yank-pop-commands',
   `yank-pop-secondary', `yank-secondary'.

 User options defined here:

   `secondary-selection-ring-max',
   `secondary-selection-yank-commands',
   `secondary-selection-yank-secondary-commands',

 Non-interactive functions defined here:

   `add-secondary-to-ring', `current-secondary-selection',
   `second-sel-msg'.

 Internal variables defined here:

   `secondary-selection-ring',
   `secondary-selection-ring-yank-pointer',
   `secondary-selection-save-posn', `yank-undo-function'.


 ***** NOTE: The following functions defined in `mouse.el'
             have been REDEFINED or ADVISED HERE:

   `mouse-drag-secondary', `mouse-save-then-kill-delete-region',
   `mouse-secondary-save-then-kill'.


 Suggested key bindings:

  (global-set-key (kbd "C-M-y")               'secondary-yank|select|move|swap)
  (define-key esc-map "y"                     'yank-pop-commands)
  (define-key isearch-mode-map (kbd "C-M-y")  'isearch-yank-secondary)
  (global-set-key (kbd "C-x C-M-SPC")         'set-secondary-start)
  (global-set-key (kbd "C-x C-M-<return>")    'secondary-save-then-kill)


 You can enhance what `second-sel.el' offers in these ways:

 1. Use library `browse-kill-ring+.el'.

    This lets you use `M-y' at the top level to browse either the
    `kill-ring' or the `secondary-selection-ring', or both, to
    choose a selection to yank.

    And `M-y' following a yank from either of these rings replaces
    that yank with the next (or prefix-argth) ring entry.  IOW, it
    does a `yank-pop' or a `yank-pop-secondary', as appropriate.

    (If you use `browse-kill-ring+.el', load `second-sel.el'
    first.)

 2. Use Icicles (library `icicles.el').

    If you do that then the behavior is similar to that provided by
    `browse-kill-ring+.el', except that `M-y' at the top level lets
    you choose a selection from either ring using completion
    against the ring entries.  And during completion you can use
    `S-delete' to delete entries from the ring.

    (This is the case by default, but you can customize Icicles to
    not do this by removing the `M-y' binding, if, for example, you
    prefer the `browse-kill-ring+.el' behavior to completing.
