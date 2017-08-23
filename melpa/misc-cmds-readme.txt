   Miscellaneous commands (interactive functions).

 Commands defined here:

   `beginning-of-line+', `beginning-of-visual-line+',
   `beginning-or-indentation', `chgrp', `chmod', `chown',
   `clear-regexp-search-history', `clear-regexp-search-ring'
   `clear-search-history', `clear-search-ring',
   `clear-search-histories', `comment-region-lines',
   `compare-windows-repeat', `count-chars-in-region',
   `delete-extra-windows-for-buffer', `delete-lines',
   `delete-window-maybe-kill-buffer.', `end-of-line+',
   `end-of-visual-line+.', `forward-char-same-line',
   `forward-overlay', `goto-previous-mark',
   `indent-rigidly-tab-stops', `indirect-buffer',
   `kill-buffer-and-its-windows', `list-colors-nearest',
   `list-colors-nearest-color-at', `mark-buffer-after-point',
   `mark-buffer-before-point', `mark-line', `mark-whole-word',
   `narrow-to-line', `next-buffer-repeat' (Emacs 22+),
   `old-rename-buffer', `previous-buffer-repeat' (Emacs 22+),
   `quit-window-delete', `recenter-top-bottom',
   `recenter-top-bottom-1', `recenter-top-bottom-2',
   `region-length', `region-to-buffer', `region-to-file',
   `resolve-file-name', `reversible-transpose-sexps',
   `revert-buffer-no-confirm', `selection-length',
   `split-para-at-sentence-ends' (Emacs 21+), `split-para-mode'
   (Emacs 21+), `switch-to-alternate-buffer',
   `switch-to-alternate-buffer-other-window',
   `to-indentation-repeat-backward',
   `to-indentation-repeat-forward', `undo-repeat' (Emacs 24.3+),
   `view-X11-colors'.

 Non-interactive functions defined here:

   `line-number-at-pos', `read-shell-file-command',
   `repeat-command'.


 ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:

   `rename-buffer' - Uses (lax) completion.

 Suggested key bindings:

  (define-key ctl-x-map [home]     'mark-buffer-before-point)
  (define-key ctl-x-map [end]      'mark-buffer-after-point)
  (define-key ctl-x-map "nl"       'narrow-to-line)
  (define-key ctl-x-map "\M-f"     'region-to-file)
  (define-key ctl-x-map [(control ?\;)] 'comment-region-lines)
  (global-set-key [C-S-f1]         'region-to-buffer)
  (global-set-key [C-S-backspace]  'region-to-file)
  (global-set-key [home]           'backward-line-text)
  (global-set-key [f5]             'revert-buffer-no-confirm) ; A la MS Windows
  (substitute-key-definition       'kill-buffer
                                   'kill-buffer-and-its-windows global-map)
  (substitute-key-definition       'recenter 'recenter-top-bottom global-map)
  (substitute-key-definition       'beginning-of-line 'beginning-of-line+ global-map)
  (substitute-key-definition       'end-of-line 'end-of-line+ global-map)
  (substitute-key-definition       'transpose-sexps
                                   'reversible-transpose-sexps global-map)

  The first two of these are needed to remove the default remappings.
  (define-key visual-line-mode-map [remap move-beginning-of-line] nil)
  (define-key visual-line-mode-map [remap move-end-of-line] nil)
  (define-key visual-line-mode-map [home] 'beginning-of-line+)
  (define-key visual-line-mode-map [end]  'end-of-line+)
  (define-key visual-line-mode-map "\C-a" 'beginning-of-visual-line+)
  (define-key visual-line-mode-map "\C-e" 'end-of-visual-line+)
  (global-set-key "\M-m"           'to-indentation-repeat-backward)
  (global-set-key "\M-n"           'to-indentation-repeat-forward)

  (global-set-key [remap mark-word]       'mark-whole-word)
  (global-set-key [remap previous-buffer] 'previous-buffer-repeat)
  (global-set-key [remap next-buffer]     'next-buffer-repeat)
  (global-set-key [remap undo]            'undo-repeat)
