   Extensions to `find-dired.el'.

 A `find' submenu is added to Dired's menu bar, and most of the
 Emacs `find-*' commands have undergone slight improvements.


 User options defined here:

   `find-diredp-default-fn', `find-diredp-max-cmd-line',
   `find-diredp-repeat-reuses-buffer-flag',
   `find-diredp-time-prefix'.

 Commands defined here:

   `find-time-dired'.

 Non-interactive functions defined here:

   `find-diredp--parse-time'.

 Internal variables defined here:

   `find-dired-hook', `find-diredp-repeating-search',
   `menu-bar-run-find-menu'.


 ***** NOTE: The following functions defined in `find-dired.el'
             have been REDEFINED HERE:

 `find-dired' - 1. Interactive spec uses `read-from-minibuffer',
                   `read-file-name', `dired-regexp-history' and
                   `find-diredp-default-fn'.
                2. Runs `find-dired-hook' at end.
                3. Uses `find-diredp-default-fn' for default input.
                4. Buffer named after dir (not named "*Find*").
 `find-dired-filter' - Removes lines that just list a file.
 `find-dired-sentinel' - 1. Highlights file lines.
                         2. Puts `find' in mode-line.
 `find-grep-dired' - Interactive spec uses `read-from-minibuffer',
                     `read-file-name', `dired-regexp-history' and
                     `find-diredp-default-fn'.
 `find-name-dired' - Interactive spec uses `read-from-minibuffer',
                     `read-file-name', `dired-regexp-history' and
                     `find-diredp-default-fn'.


 ***** NOTE: The following variable defined in `find-dired.el'
             has been REDEFINED HERE:

 `find-ls-option'   - Uses `dired-listing-switches' for Windows.
