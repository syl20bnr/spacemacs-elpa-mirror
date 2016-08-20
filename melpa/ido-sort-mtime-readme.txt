Display recently modified files at the beginning of Ido's file list.

To activate after installing, add to ~/.emacs:
  (ido-sort-mtime-mode 1)

To display TRAMP files before local ones, use:
  (setq ido-sort-mtime-tramp-files-at-end nil)
(Checking modification time for TRAMP files is not yet supported.)

See also: M-x customize-group RET ido-sort-mtime RET
