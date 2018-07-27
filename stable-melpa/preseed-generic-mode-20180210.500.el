;;; preseed-generic-mode.el --- Debian preseed file major mode
;; -*- emacs-lisp -*-

;; Copyright (C) 2015-2018 Tong Sun
;; License: GPL version 3, or any later version

;; Author: Tong Sun <suntong@users.sourceforge.net>
;; URL: https://github.com/suntong/preseed-generic-mode
;; Package-Version: 20180210.500

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}

;;; Commentary:

;;{{{ Introduction

;; Preface
;;
;;      This package provides a major mode for Debian preseed files.
;;      It provides basic font-locking for preseed files, built upon generic-mode.

;;}}}
;;{{{ Installation

;;  Installation
;;
;;      To install the Preseed-Generic mode
;;
;;      o  Put this file (preseed-generic-mode.el) on your Emacs 
;;         `load-path' (or extend the load path to include the
;;         directory containing this file) and optionally byte compile it.
;;      o  Or install it directly from the MELPA repository
;;

;;}}}
;;{{{ DOCUMENTATION

;;  Usage
;;
;;      To use the Preseed-Generic mode, put somewhere in your init file,
;;
;;          ;; for Debian preseed files
;;          (require 'preseed-generic-mode)
;;
;;      To have the preseed files opened with syntax highlighting 
;;      automatically turn on, put the following at the top of the files:
;;
;;          # -*- Preseed-Generic -*-

;;}}}

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
