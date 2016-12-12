Main functions are three

  1. For Ruby -> string-inflection-ruby-style-cycle  (foo_bar => FOO_BAR => FooBar => foo_bar)
  2. For Java -> string-inflection-java-style-cycle  (fooBar  => FOO_BAR => FooBar => fooBar)
  3. Other    -> string-inflection-all-cycle         (foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => foo_bar)

Setting example

  (require 'string-inflection)
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'string-inflection-ruby-style-cycle) ; Vz Editor-like key binding

  or

  (require 'string-inflection)
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)

  (defun my-string-inflection-cycle-auto ()
    "switching in major-mode"
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      (string-inflection-ruby-style-cycle))))
