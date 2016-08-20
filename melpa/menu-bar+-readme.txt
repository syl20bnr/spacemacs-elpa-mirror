   Extensions to `menu-bar.el'.  Redefines the default menu bar.

 Usage:

   This library should be loaded after loading standard library
   `menu-bar.el'.  So, in your `~/.emacs' file, do this:

     (eval-after-load "menu-bar" '(require 'menu-bar+))

   You will also want to do that before loading other libraries
   that might modify the following predefined menu-bar menus:

     `File'
     `Edit'
     `More Manuals'
     `Options'
     `Search'

   This is because those menus correspond to the variables
   mentioned at the end of this commentary as being REDEFINED here.
   If a library modifies one of those variables before you load
   `menu-bar+.el' then those changes will be lost when the variable
   is redefined.

   The following libraries are exceptions to this rule.  If loaded
   before `menu-bar+.el' then they are used by `menu-bar+.el'.  So
   if you use them then load them before loading `menu-bar+.el'.

     `doremi.el'
     `help+.el'
     `help-fns+.el'
     `thumb-frm.el'
     `w32-browser-dlgopen.el'

 Main differences:

   1. Menus "Search", "Frames" and "Do Re Mi" were added.
   2. Menus "File", "Edit", & "Help" were changed.
   3. Menu order was changed.
   4. Buffer-local menus are separated from global menus via "||".


 User options defined here:

   `menu-barp-select-buffer-function'.

 Commands defined here:

   `describe-menubar', `fill-paragraph-ala-mode',
   `menu-bar-create-directory', `menu-bar-next-tag-other-window'
   (Emacs 20), `menu-bar-select-frame' (Emacs 20),
   `menu-bar-word-search-backward' (Emacs 22+),
   `menu-bar-word-search-forward' (Emacs 22+),
   `nonincremental-repeat-search-backward' (Emacs 22+),
   `nonincremental-repeat-search-forward' (Emacs 22+),
   `nonincremental-repeat-word-search-backward' (Emacs < 22),
   `nonincremental-repeat-word-search-forward' (Emacs < 22),

 Macros defined here:

   `menu-bar-make-toggle-any-version'.

 Non-interactive functions defined here:

   `menu-barp-nonempty-region-p'.

 Variables defined here:

   `menu-bar-apropos-menu', `menu-bar-describe-menu',
   `menu-bar-divider-menu', `menu-bar-doremi-menu',
   `menu-bar-edit-fill-menu', `menu-bar-edit-region-menu',
   `menu-bar-edit-sort-menu', `menu-bar-emacs-lisp-manual-menu',
   `menu-bar-emacs-manual-menu', `menu-bar-frames-menu',
   `menu-bar-i-search-menu' (Emacs < 22),
   `menu-bar-search-replace-menu', `menu-bar-search-tags-menu',
   `menu-bar-whereami-menu', `yank-menu'.


 ***** NOTE: The following functions defined in `menu-bar.el' have
             been REDEFINED HERE:

 `kill-this-buffer' - Deletes buffer's windows as well, if
                      `sub-kill-buffer-and-its-windows'.

 `menu-bar-options-save' - Added options are saved (>= Emacs 21).

 `menu-bar-select-buffer' (Emacs 20-22) - Uses -other-frame.


 ***** NOTE: The following variables defined in `menu-bar.el' have
             been REDEFINED HERE:

 `menu-bar-edit-menu', `menu-bar-file(s)-menu',
 `menu-bar-manuals-menu', `menu-bar-options-menu',
 `menu-bar-search-menu'.
