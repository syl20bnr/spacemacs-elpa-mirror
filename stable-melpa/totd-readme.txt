totd.el provides a daily morning "tip" - a command, bound to a key,
selected at random from keymaps. To enable it, install via MELPA, then
add:

(totd-start)

to your `~/.emacs.d/init.el` file.

To display a tip right now, run `M-x totd`.

By default, totd will display a random command that is bound to a key
in `global-map`. To add maps (e.g. `org-mode-map`) to use a source for
your daily tip, customize `totd-keymaps` by adding a symbol name.

Inspired by http://emacswiki.org/emacs/TipOfTheDay
