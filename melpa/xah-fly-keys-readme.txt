xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

It is a modal mode like vi, but key choices are based on statistics of command call frequency.

--------------------------------------------------
MANUAL INSTALL

put the file xah-fly-keys.el in ~/.emacs.d/lisp/
create the dir if doesn't exist.

put the following in your emacs init file:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)
(xah-fly-keys 1)

--------------------------------------------------
HOW TO USE

M-x xah-fly-keys to toggle the mode on/off.

Important command/insert mode switch keys:

xah-fly-command-mode-activate (press 【<home>】 or 【F8】 or 【Alt+Space】 or 【menu】)

xah-fly-insert-mode-activate  (when in command mode, press letter 【u】 key)

When in command mode:
【u】 activates insertion mode
【Space】 is a leader key. For example, 【SPACE p】 calls query-replace. Press 【SPACE C-h】 to see the full list.
【Space Space】 also activates insertion mode.
【Space Enter】 calls execute-extended-command or smex (if smex is installed).
【a】 calls execute-extended-command or smex (if smex is installed).

The leader key sequence basically replace ALL emacs commands that starts with C-x key.

When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

C-c for major mode commands.
C-g for cancel.
C-q for quoted-insert.
C-h for getting a list of keys following a prefix/leader key.

Leader key

You NEVER need to press Ctrl+x

Any emacs commands that has a keybinding starting with C-x, has also a key sequence binding in xah-fly-keys. For example,
【C-x b】 switch-to-buffer is 【SPACE u】
【C-x C-f】 find-file is 【SPACE c .】
【C-x n n】 narrow-to-region is 【SPACE n n】
The first key we call it leader key. In the above examples, the SPACE is the leader key.

When in command mode, the 【SPACE】 is a leader key.

globally, the leader key is the 【f9】 key.

the following stardard keys with Control are supported:

 ;; 【Ctrl+tab】 'xah-next-user-buffer
 ;; 【Ctrl+shift+tab】 'xah-previous-user-buffer
 ;; 【Ctrl+v】 'yank
 ;; 【Ctrl+w】 'xah-close-current-buffer
 ;; 【Ctrl+z】 'undo
 ;; 【Ctrl+n】 'xah-new-empty-buffer
 ;; 【Ctrl+o】 'find-file
 ;; 【Ctrl+s】 'save-buffer
 ;; 【Ctrl+shift+s】 'write-file
 ;; 【Ctrl+shift+t】 'xah-open-last-closed
 ;; 【Ctrl++】 'text-scale-increase
 ;; 【Ctrl+-】 'text-scale-decrease
 ;; 【Ctrl+0】 (lambda () (interactive) (text-scale-set 0))))

On the Mac, I highly recommend using a app called Sail to set your capslock to send Home. So that it acts as xah-fly-command-mode-activate. You can set capslock or one of the cmd key to Home. See http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

I recommend you clone xah-fly-keys.el, and modify it, and use your modified version. Don't worry about upgrade. The main point is use it for your own good. (I still make key tweaks every week, for the past 3 years.)

If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

For detail about design and other info, see home page at
http://ergoemacs.org/misc/ergoemacs_vi_mode.html

If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.


