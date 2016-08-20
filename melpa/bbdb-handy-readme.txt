## Introduce ##

bbdb-handy is a BBDB tool, when in headers (TO: and CC:) of message-mode buffer,
Type TAB key will will pop up a BBDB window as email-address chooser.

![snapshot1.gif](snapshots/snapshot1.gif)

## Download ##

    https://github.com/tumashu/bbdb-handy

## Install ##

1. Config melpa: http://melpa.org/#/getting-started
2. M-x package-install RET bbdb-handy RET
3. Add code to your emacs config file:（for example: ~/.emacs）：

```lisp
(require 'bbdb-handy)
```

## Usage ##

### The easiest way ###

Add the below line to your emacs config file.

```
(bbdb-handy-enable)
```

### The manual way ###

The function `bbdb-handy-enable' only rebind some key in `bbdb-mode-map'
and `message-mode-map', user can do this job by hand, for example:

```
bbdb-mode
(define-key bbdb-mode-map "g" 'bbdb-handy-display-all-records)
(define-key bbdb-mode-map "q" 'bbdb-handy-quit-window)
(define-key bbdb-mode-map "p" 'bbdb-handy-bbdb-push-mail)
(define-key bbdb-mode-map "\C-s" 'bbdb-handy-search-records)
(define-key bbdb-mode-map "b" 'bbdb-handy-search-records)
(define-key bbdb-mode-map "\C-c\C-c" 'bbdb-handy-push-mail)
(define-key bbdb-mode-map (kbd "RET") 'bbdb-handy-push-mail-and-quit-window)
Message-mode
(define-key message-mode-map "\t" 'bbdb-handy-message-tab)
```
