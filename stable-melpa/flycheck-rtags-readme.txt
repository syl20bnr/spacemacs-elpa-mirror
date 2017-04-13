C, C++ and Objective-c support for Flycheck, using rtags.


Usage:

(require 'flycheck-rtags)


Optional explicitly select the RTags Flycheck checker for c or c++ major mode.
Turn off Flycheck highlighting, use the RTags one.
Turn off automatic Flycheck syntax checking rtags does this manually.
(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-highlighting-mode nil))
c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
