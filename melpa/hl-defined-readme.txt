   Highlight defined or undefined symbols in Emacs-Lisp.

 `hdefd-highlight-mode' is a minor mode that highlights, in the
 current buffer, symbols that are known to be defined as Emacs-Lisp
 functions or variables or both.  Alternatively, it can highlight
 symbols that are not known to be defined as functions or
 variables.

 The current buffer should be in Emacs-Lisp mode.

 Command `hdefd-highlight-mode' toggles highlighting on/off.  The
 highlighting respects option `hdefd-highlight-type'.

 Command `hdefd-cycle' cycles highlighting among the available
 types and off, as follows: functions & variables > functions >
 variables > undefined > off .  It does this by changing the
 current value of option `hdefd-highlight-type'.

 You can of course customize the faces used for highlighting.  You
 might want, for instance, to have face `hdefd-functions' inherit
 from face `font-lock-function-name-face', and `hdefd-variables'
 inherit from `font-lock-variable-name-face'.  This is not the
 default because I don't find it so useful.


 Put this in your init file:

   (require 'hl-defined)

 If you want to turn on this highlighting automatically whenever
 you enter Emacs-Lisp mode then you can do this in your init file:

   (require 'hl-defined)
   (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

 User option `hdefd-highlight-type' controls what gets highlighted.


 Faces defined here:

   `hdefd-functions', `hdefd-variables', `hdefd-undefined'.

 User options defined here:

   `hdefd-highlight-type'.

 Commands defined here:

   `hdefd-highlight-mode'.

 Non-interactive functions defined here:

   `hdefd-highlight'.

 Internal variables defined here:

   `hdefd-face'.
