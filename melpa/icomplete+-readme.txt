 Extensions to `icomplete.el'.

 * Better display of candidates, including highlighting them and showing how many there are.
 * Shows key bindings for command candidates, optionally including menu bindings.
 * Does not bind keys for cycling in `icomplete-mode'.  Defines a separate mode for that, so you
   can use Icomplete with or without those key bindings.  (Emacs 24.4+)
 * Support for Icicles:
   . Respect current Icicles sort order, which you can cycle using `C-,'.
   . When you change direction cycling candidates, show the number of other cycle candidates.


 Macros defined here (but identical to those in Emacs 23):

   `with-local-quit', `with-no-input'.

 Commands define here:

   `icompletep-cycling-mode'.

 Faces defined here:

   `icompletep-choices', `icompletep-determined',
   `icompletep-keys', `icompletep-nb-candidates'.

 User options defined here:

   `icompletep-exact-separator',
   `icompletep-include-menu-items-flag' (Emacs 23+),
   `icompletep-prospects-length' (Emacs < 23),
   `icomplete-show-key-bindings'.

 Non-interactive functions defined here:

   `icomplete-get-keys' (Emacs > 24.2),
   `icompletep-completion-all-sorted-completions',
   `icompletep-remove-if'.


 ***** NOTE: The following functions defined in `icomplete.el'
             have been REDEFINED OR ADVISED HERE:

   `icomplete-get-keys' (Emacs < 24.3) -
      1. Respect `icompletep-include-menu-items-flag'.
      2. Do not wrap with `<...>'.
      3. If string of keys would be too long then shorten it.

   `icomplete-completions' -
      1. Prepend the total number of candidates.
      2. For file-name completion, respect `completion-ignored-extensions'.
      3. With Icicles, sort candidates using `icicle-reversible-sort' and show number of
         remaining cycle candidates.  You can cycle the sort order using `C-,'.
      4. Show candidates in a different face.
      5. Optionally show and highlight key bindings, truncating if too long.

   `icomplete-exhibit' -
      1. Save match-data.
      2. Do not insert if input begins with `(' (e.g. `repeat-complex-command').
      3. Ensure that the insertion does not deactivate mark.

   `icomplete-mode' - Advised to provide Icomplete+ doc.


 This file should be loaded after loading the standard GNU file
 `icomplete.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "icomplete" '(progn (require 'icomplete+)))

 Usage notes:

 * Starting with Emacs 23 you can get icompletion of things like
   file names also.  See variable (non-option)
   `icomplete-with-completion-tables'.  If you set it to the
   (undocumented) value `t' then icompletion is available anytime
   the completion COLLECTION parameter is a function, which
   includes file-name completion.

 * Starting with Emacs 24 you can specify the kinds of completion
   you want by customizing option `completion-category-overrides'
   for file names, buffer names, bookmark names, and so on.

 * Starting with Emacs 24.4, Icomplete mode automatically binds
   keys that are otherwise useful in the minibuffer (for Isearch,
   symbol completion, etc.) to its own keys for cycling among
   icompletion candidates.  This is a BAD idea - see Emacs bug
   #13602.  Icomplete+ fixes this by having a separate mode that
   binds Icomplete keys, making that optional.  This is analogous
   to the difference between `cua-selection-mode' and `cua-mode'.

   So with Icomplete+, just turning on Icomplete mode does not
   co-opt those keys taking them away from you for use in the
   minibuffer.  If you really want to do that then turn on
   `icomplete-cycling-mode' in addition to `icomplete-mode'.  And
   in that case, consider also choosing different keys to bind in
   `icomplete-minibuffer-map' from those that are bound by default.

 (The first two features above are not particular to Icomplete+ -
 they are available also for vanilla Icomplete.)
