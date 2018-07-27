This file contains definitions of CFML submode classes.

Usage:

(require 'mmm-mode)
(require 'cfml-mode)

(add-to-list 'magic-mode-alist
             '("<cfcomponent" . cftag-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . cfml-cfscript-mode))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfml-cftag)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfml-cftag)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfml-js)

Optional settings:

(setq mmm-submode-decoration-level 0)
