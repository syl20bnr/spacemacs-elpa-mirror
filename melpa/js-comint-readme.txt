js-comint.el is a comint mode for Emacs which allows you to run a
compatible javascript repl like Node.js/Spidermonkey/Rhino inside Emacs.
It also defines a few functions for sending javascript input to it
quickly.

Usage:
 Put js-comint.el in your load path
 Add (require 'js-comint) to your .emacs or ~/.emacs.d/init.el

 Optionally, set the `inferior-js-program-command' string
 and the `inferior-js-program-arguments' list to the executable that runs
 the JS interpreter and the arguments to pass to it respectively.
 E.g., the default is:
 (setq inferior-js-program-command "node")
 (setq inferior-js-program-arguments '("--interactive"))

 E.g. Set up the Rhino JAR downloaded from
 https://github.com/mozilla/rhino
 (setq inferior-js-program-command "java")
 (setq inferior-js-program-arguments '("-jar" "/absolute/path/to/rhino/js.jar"))

 Do: `M-x run-js'
 Away you go.

 If you have nvm, you can select the versions of node.js installed and run
 them.  This is done thanks to nvm.el.
 Please note nvm.el is optional. So you need *manually* install it.
 To enable nvm support, run `js-do-use-nvm'.
 The first time you start the JS interpreter with run-js, you will be asked
 to select a version of node.js
 If you want to change version of node js, run `js-select-node-version'

 You can add  the following couple of lines to your .emacs to take advantage of
 cool keybindings for sending things to the javascript interpreter inside
 of Steve Yegge's most excellent js2-mode.

  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go)))
