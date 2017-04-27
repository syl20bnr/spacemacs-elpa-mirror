Provides support for editing GNU Cfengine files, including
font-locking, Imenu and indentation, but with no special keybindings.

The CFEngine 3.x support doesn't have Imenu support but patches are
welcome.

By default, CFEngine 3.x syntax is used.

You can set it up so either `cfengine2-mode' (2.x and earlier) or
`cfengine3-mode' (3.x) will be picked, depending on the buffer
contents:

(add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-auto-mode))

OR you can choose to always use a specific version, if you prefer
it:

(add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine3-mode))
(add-to-list 'auto-mode-alist '("^cf\\." . cfengine2-mode))
(add-to-list 'auto-mode-alist '("^cfagent.conf\\'" . cfengine2-mode))

It's *highly* recommended that you enable the eldoc minor mode:

(add-hook 'cfengine-mode-hook 'turn-on-eldoc-mode)

This is not the same as the mode written by Rolf Ebert
<ebert@waporo.muc.de>, distributed with cfengine-2.0.5.  It does
better fontification and indentation, inter alia.
