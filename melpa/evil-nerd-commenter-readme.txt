This program emulates nerd-commenter.vim by Marty Grenfell.

It helps you comment/uncomment multiple lines without selecting them.

`M-x evilnc-default-hotkeys` assigns hotkey `M-;` to `evilnc-comment-or-uncomment-lines`

`M-x evilnc-comment-or-uncomment-lines` comment or uncomment lines.

`M-x evilnc-quick-comment-or-uncomment-to-the-line` will comment/uncomment
from current line to specified line.
The last digit(s) of line number is parameter of the command.

For example, `C-u 9 evilnc-quick-comment-or-uncomment-to-the-line` comments
code from current line to line 99 if you current line is 91.

Though this program could be used *independently*, though I highly recommend
using it with Evil (https://bitbucket.org/lyro/evil/)

Evil makes you take advantage of power of Vi to comment lines.
For example, you can press key `99,ci` to comment out 99 lines.

Setup:

Use case 1,
If you use comma as leader key, as most Vim users do, setup is one liner,
(evilnc-default-hotkeys)

Use case 2,
If you use evil-leader and its default leader key,
insert below setup into your ~/.emacs instead,

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator
  )

Use case 3,
For certain major modes, you need manual setup to override its original
keybindings,

(defun matlab-mode-hook-config ()
  (local-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))
(add-hook 'matlab-mode-hook 'matlab-mode-hook-config)

See https://github.com/redguardtoo/evil-nerd-commenter for more use cases.
