A minimalists' Emacs extension to search http://www.bing.com/dict. Support
English to Chinese and Chinese to English.

## Setup

You can install via [melpa](http://melpa.org).

If installing this package manually:

    (add-to-list 'load-path "/path/to/bing-dict.el")
    (require 'bing-dict)

## Usage
You can call `bing-dict-brief` to get the explanations of you query. The results
will be shown in the echo area.

You should probably give this command a key binding:

    (global-set-key (kbd "C-c d") 'bing-dict-brief)

## Customization
You can set the value of `bing-dict-add-to-kill-ring` to control whether the
result should be added to the `kill-ring` or not. By default, the value is
`nil`. If set to `t`, the result will be added to the `kill-ring' and you
are able to use `C-y` to paste the result.
