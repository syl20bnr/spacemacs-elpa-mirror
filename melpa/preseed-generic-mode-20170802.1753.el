;;; preseed-generic-mode.el --- Debian preseed file major mode
;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun
;; License: GPL version 3, or any later version

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; URL: https://github.com/suntong001/preseed-generic-mode
;; Package-Version: 20170802.1753

;;; Commentary:

;; This is Debian preseed custom mode.

;;; Code:

;;;_* require
(eval-when-compile (require 'generic-x))

;;;_* mode definition
(define-generic-mode 
    preseed-generic-mode		;; name of the mode
  '(?#)					;; comments start with '#'
  '("d-i"
    "boolean"
    "string"
    "select"
    "xserver-xorg"
    "in-target")			;; keywords
  '(
    ;; order is important
    ("^\\s-*\\(d-i\\|xserver-xorg\\)\\s-+\\([-A-Za-z0-9_]+\\)/\\([^ \t]+\\)\\s-+"
     (2 font-lock-constant-face)
     (3 font-lock-variable-name-face))
    ("\\<\\(true\\|false\\|auto\\|atomic\\|finish\\)\\>"
     1 font-lock-builtin-face)
    )

  '("preseed\\.cfg\\'")			;; files for which to activate this mode 
   nil					;; other functions to call
  "Mode for Debian preseed files"	;; doc string for this mode
)

(provide 'preseed-generic-mode)

;;;_* end
;;; preseed-generic-mode.el ends here
