   Miscellaneous non-interactive functions.

 You might want to put this in your `~/.emacs' file, to erase the
 minibuffer when it is inactive and `minibuffer-empty-p':

  (require 'misc-fns)
  (add-hook '<MODE>-hook 'notify-user-of-mode), for each <MODE>.


 Face defined here: `notifying-user-of-mode'.

 User options (variables) defined here:

   `buffer-modifying-cmds', `mode-line-reminder-duration',
   `notifying-user-of-mode-flag'.

 Functions defined here:

   `all-apply-p', `another-buffer', `color-named-at',
   `current-line', `display-in-mode-line', `do-files', `flatten',
   `fontify-buffer', `interesting-buffer-p', `live-buffer-name',
   `make-transient-mark-mode-buffer-local', `mode-ancestors',
   `mode-symbol-p', `mod-signed', `notify-user-of-mode',
   `read-mode-name', `region-or-buffer-limits', `signum',
   `some-apply-p' `string-after-p', `string-before-p',
   `undefine-keys-bound-to', `undefine-killer-commands',
   `unique-name'.
