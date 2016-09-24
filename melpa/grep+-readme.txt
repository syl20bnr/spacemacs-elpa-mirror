 Extensions to standard library `grep.el':

   1. Additional keys are bound here.
   2. Mouse-over is active on the entire hit line, not just on the
      file-name part.
   3. `grep' command provides a default search string in all cases,
      and that default value is better.
   4. Commands are provided to remove commented lines from `grep'
      output and toggle their automatic removal.

 If you want the current `grep' buffer to be renamed automatically
 to reflect the particular `grep' command used, do this:

 (add-hook 'grep-mode-hook 'grepp-rename-buffer-to-last-no-confirm)

 Otherwise, you can use `r' to rename it on demand (command
 `grepp-rename-buffer-to-last').


 Put this in your initialization file (`~/.emacs'):

   (require 'grep+)

 Face suggestions:

   `compilation-info-face':   Blue3' foreground,        no inherit
   `compilation-line-number': DarkGoldenrod foreground, no inherit
   `match':                   SkyBlue background,       no inherit

 User options defined here:

   `grepp-default-comment-line-regexp', `grepp-default-regexp-fn'.

 Commands defined here:

   `choose-grep-buffer', `grepp-choose-grep-buffer',
   `grepp-new-grep-buffer', `grepp-remove-comments',
   `grepp-rename-buffer-to-last', `grepp-toggle-comments',
   `new-grep-buffer', `remove-grep-comments',
   `toggle-grep-comments'.

 Non-interactive functions defined here:

   `grepp-buffers', `grepp-default-regexp-fn',
   `grepp-rename-buffer-to-last-no-confirm'.

 Internal variables defined here:

   `grepp-last-args'.



 ***** NOTE: The following variables defined in `grep.el'
             have been REDEFINED HERE:

 `grep-mode-font-lock-keywords', `grep-regexp-alist' (Emacs < 24)
   - Mouse-over the whole line.



 ***** NOTE: The following minor mode defined in `grep.el'
             has been REDEFINED HERE:

 `grep-mode' - No change.  Redefined here so it uses modified value
               of `grep-regexp-alist'.


 ***** NOTE: The following functions defined in `grep.el'
             have been REDEFINED HERE:

 `grep', `grep-default-command' - Use `grepp-default-regexp-fn'.
