Smex has really nice handling of command histories, favouring
commands which have been executed most frequently in the past.

This package provides an Ivy interface to those internals via the
`ivy-smex' command, which is meant as a replacement for
`execute-extended-command' or `smex':

(global-set-key (kbd "M-x") 'ivy-smex)
