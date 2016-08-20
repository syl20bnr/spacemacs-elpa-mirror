inferior-lisp adapted for Clojure.

If you're installing manually, you'll need to:

* drop the file somewhere on your load path (perhaps ~/.emacs.d)
* Add the following lines to your .emacs file:

   (autoload 'inf-clojure "inf-clojure" "Run an inferior Clojure process" t)
   (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
