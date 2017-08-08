This file contains definitions of CFML submode classes.

Usage:

(require 'mmm-mode)
(require 'cfml-mode)

(add-to-list 'magic-mode-alist
             '("<cfcomponent" . cfml-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . cfml-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . cfml-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . cfml-cfscript-mode))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)

Optional settings:

(setq mmm-submode-decoration-level 0)
