Description:

`dired-toggle' provide an simple way to toggle dired buffer for
current directory(similar to `dired-jump'), and the target buffer
is specified so that it will be divided from other dired buffers,
for example the dired buffer only shown in a special name which
could be edit through the variable `dired-toggle-buffer-name', and
it will include a minor mode named `dired-toggle-mode' to ensure
all the actions(such as change directory and open a selected file)
under the dired buffer will not influence other dired buffers and
works as we expected.

You could custom the toggled window's size and side through
variable `dired-toggle-window-size' and `dired-toggle-window-side'.

Source: https://github.com/fasheng/dired-toggle

Tips: For a good user experience you may want to use
`dired-details.el' or `dired-hide-details-mode' if use Emacs 24.4
or later.


Usage:

Just add the following to your .emacs:

(global-set-key (kbd "<f5>") 'dired-toggle)

You could also custom functions after `dired-toggle-mode' enabled,
for example enable `visual-line-mode' for our narrow dired buffer:

(add-hook 'dired-toggle-mode-hook
          (lambda () (interactive)
            (visual-line-mode 1)
            (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
            (setq-local word-wrap nil)))


Default key-bindings:

| "q"       | dired-toggle-action-quit         |
| "RET"     | dired-toggle-action-find-file    |
| "^"       | dired-toggle-action-up-directory |
| "C-c C-u" | dired-toggle-action-up-directory |
