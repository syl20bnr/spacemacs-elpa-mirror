=============
 Get Started
=============

Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
and add to the following lines to your ~/.emacs.d/init.el.

   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
   (autoload 'jsx-mode "jsx-mode" "JSX mode" t)

See also init.el.example.


==============
 Key Bindings
==============

In `jsx-mode', the following keys are bound by default.

C-c C-c     comment-region (Comment or uncomment each line in the region)
C-c c       jsx-compile-file (Compile the current buffer)
C-c C       jsx-compile-file-async (Compile the current buffer asynchronously)
C-c C-r     jsx-run-buffer (Run the current buffer)


TODO:
* support imenu
* fix a bug that any token after implements is colored
  e.g. 'J' will be colored in the code like 'class C implements I { J'
* support indentations for lambda statment
* support auto-complete
