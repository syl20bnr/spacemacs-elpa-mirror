Just put ctags-update.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
...
(add-hook 'lisp-mode-hook  'turn-on-ctags-auto-update-mode)

then when you save a file ,`ctags-auto-update-mode' will recursively searches each
parent directory for a file named 'TAGS'. if found ,it will use
`exuberant-ctags' update TAGS,
it would not be updated if last time calling `ctags-update' is not 5 minute age(default).

if you want to update TAGS only when you want.
you can
    (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
and
    M-x : ctags-update
with prefix `C-u' ,then you can generate a new TAGS file in your
selected directory.

on windows ,you can custom `ctags-update-command' like this:
(when (equal system-type 'windows-nt)
  (setq ctags-update-command (expand-file-name  "~/.emacs.d/bin/ctags.exe")))
