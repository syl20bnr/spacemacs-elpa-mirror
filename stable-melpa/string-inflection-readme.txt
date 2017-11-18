Main functions are three

  1. For Ruby -> string-inflection-ruby-style-cycle  (foo_bar => FOO_BAR => FooBar => foo_bar)
  2. For Java -> string-inflection-java-style-cycle  (fooBar  => FOO_BAR => FooBar => fooBar)
  3. For All  -> string-inflection-all-cycle         (foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => foo_bar)


Setting Example 1

  (require 'string-inflection)
  (global-unset-key (kbd "C-q"))
  ;; C-q C-u is the key bindings similar to Vz Editor.
  (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)

  (defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle))))


Setting Example 2

  (require 'string-inflection)

  ;; default
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)

  ;; for ruby
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))

  ;; for java
  (add-hook 'java-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))

You may also consider setting `string-inflection-skip-backward-when-done' to
`t' if you don't like `string-inflect' moving your point to the end of the
word
