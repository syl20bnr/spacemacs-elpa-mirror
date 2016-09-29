This package provide commands for looking up the web of word under cursor.

xah-lookup-word-on-internet
xah-lookup-google           ; 【C-h 7】 or 【F1 7】
xah-lookup-wikipedia        ; 【C-h 8】 or 【F1 8】
xah-lookup-word-definition  ; 【C-h 9】 or 【F1 9】
xah-lookup-word-dict-org
xah-lookup-answers.com
xah-lookup-wiktionary

If there's a text selection (a phrase you want to lookup), these commands will act on the selection.

If you prefer to use emacs 24.4's builtin eww browser, put the following in your emacs init
(require 'eww)
(setq xah-lookup-browser-function 'eww) ; or 'browse-url

For commands that lookup English word definition, you can specify browser separately.
(setq xah-lookup-dictionary-browser-function 'eww) ; or 'browse-url

To change/add keys, put the following in your emacs init.
(define-key help-map (kbd "7") 'xah-lookup-google)
Change the command to the one you want, or `nil' to reset.

You can also create your own lookup command to lookup perl, ruby, php, clojure, etc.
See: http://ergoemacs.org/emacs/xah-lookup.html

Like it?
Buy Xah Emacs Tutorial
http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html
