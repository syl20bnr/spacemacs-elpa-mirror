* Toggle Indent and Paren mode.
Use ~parinfer-toggle-mode~.

I recommand to add a keybinding for ~parinfer-toggle-mode~, since it will be used frequently.
#+BEGIN_SRC emacs-lisp
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode)
#+END_SRC
When the first time, you switch to Indent Mode, if your code will be modified by parinfer,
You will see a confirm message in minibuffer. Type ~y~ for continue, ~n~ to stay in paren mode.

Use ~parinfer-diff~ to see how parinfer will change the buffer with Ediff.

Some keybindings in Ediff:
| Key  | Description                                               |
|------+-----------------------------------------------------------|
| ~q~  | Quit diff.                                                |
| ~b~  | B->A in Ediff, this can apply change to your origin code. |
| ~ra~ | Restore A in Ediff, this can revert change.               |
| ~n~  | Move to next difference.                                  |
| ~p~  | Move to previous difference.                              |

Normally, after indenting the whole buffer with ~C-x h~ ~C-M-\~, you can switch to Indent Mode safely.

* FAQ
** Project status.
I'm already using parinfer-mode for Clojure And Elisp. It should be stable and should work as expected.
If there's any bug or uncomfortable stuff, open an issue please.

** I found command XXX break matched parens!
If XXX is a built-in or wild used command, please open a issue, I'll do a fix.

Alternatively, you can fix it too. There're two macros.

*** parinfer-run
This macro will run the BODY code, then invoke parinfer to fix parentheses(if we are in indent-mode).
#+BEGIN_SRC emacs-lisp
  ;; This is a sample, parinfer-mode have already remap yank with parinfer-yank.

  (defun parinfer-yank ()
    (interactive)
    (parinfer-run
     (call-interactively 'yank)))

  ;; Replace yank to parinfer-yank.
  (define-key parinfer-mode-map [remap yank] 'parinfer-yank)
#+END_SRC

*** parinfer-paren-run
This macro will always run BODY in paren-mode, avoid changing the S-exp struct.
#+BEGIN_SRC emacs-lisp
  ;; This is a sample, parinfer-mode already remap delete-indentation with parinfer-delete-indentation.

  (defun parinfer-delete-indentation ()
    (interactive)
    (parinfer-paren-run
     (call-interactively 'delete-indentation)))

  ;; Replace delete-indentation to parinfer-indentation.
  (define-key parinfer-mode-map [remap delete-indentation] 'parinfer-delete-indentation)
#+END_SRC

** Parinfer-mode toggle indent mode is changing the indentation.
The indentation of code should not be changed by indent mode. When you meet this, your code probably have indentation with *TAB*.

Currently Parinfer can not handle tab indentation, you can change all tab indentation to whitespace for current buffer with ~M-x parinfer-untabify-buffer~.

** Use with Evil?
Parinfer mode only works in insert-state.

But there's already a plugin called [[https://github.com/luxbock/evil-cleverparens][evil-cleverparens]] , that handles parentheses nicely for evil normal or visual states.

If you are using evil, try using ~evil-cleverparens~ + ~parinfer-mode~ .

** Use in Cider REPL?
Not yet, I simply use ~electric-pair-mode~ for auto pairs.
#+BEGIN_SRC emacs-lisp
  (add-hook 'cider-repl-mode-hook #'electric-pair-mode)
#+END_SRC

** Performance.
On each text modification, the current & previous top-level form will be computed.
When switching to Indent mode, whole buffer will be computed.
No performance issue now.

** Hooks?
~parinfer-mode-enable-hook~ and ~parinfer-mode-disable-hook~.

** Preview cursor scope?
Not support yet.

* Credits
- [[https://github.com/shaunlebron][shaunlebron]] :: Create Parinfer.
- [[https://github.com/oakmac][oakmac]] :: Bring Parinfer to Emacs.
- [[https://github.com/tumashu][tumashu]] :: Help me a lot in writing this plugin.
- [[https://github.com/purcell][purcell]] & [[https://github.com/syohex][syohex]] :: Advice and Tips for writing emacs plugin.
* License
Licensed under the GPLv3.
