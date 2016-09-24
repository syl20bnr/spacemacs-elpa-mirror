* Enable parinfer-mode.
~M-x parinfer-mode~

or
#+BEGIN_SRC emacs-lisp
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
#+END_SRC
Not work in Cider REPL now.

* Toggle Indent and Paren mode.
Use ~parinfer-toggle-mode~.

I recommand to add a keybinding for ~parinfer-toggle-mode~, since it will be used frequently.
#+BEGIN_SRC emacs-lisp
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode)
#+END_SRC
When the first time, you switch to Indent Mode, if your code will be modified by parinfer,
You will see a confirm message in minibuffer.  Type ~y~ for continue, ~n~ to stay in paren mode.

Use ~parinfer-diff~ to see how parinfer will change the buffer with Ediff.

Normally, after indenting the whole buffer with ~C-x h~ ~C-M-\~, you can switch to Indent Mode safely.

* Work with Evil?
Not yet, some works are needed.  Will come soon.

* Performance.
On each text modification, the current top-level form will be computed.
When switching to Indent mode, whole buffer will be computed.
No performance issue now.

* Hooks?
~parinfer-mode-enable-hook~ and ~parinfer-mode-disable-hook~.

* Preview cursor scope?
Not support yet.

* Credits
- [[https://github.com/oakmac][oakmac]] :: Bring Parinfer to Emacs.
- [[https://github.com/tumashu][tumashu]] :: Help me a lot in writing this plugin.

* License
Licensed under the GPLv3.
