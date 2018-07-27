;;; gams-ac.el --- auto-complete source file for GAMS mode -*- lexical-binding: t -*-

;; Author: Shiro Takeda
;; Maintainer: Shiro Takeda
;; Copyright (C) 2018 Shiro Takeda
;; First Created: Tue Jan 23, 2018
;; Time-stamp: <2018-04-23 18:24:30 st>
;; Version: 1.1
;; Package-Version: 20180423.926
;; Keywords: languages, tools, gams-mode, auto-complete
;; URL: https://github.com/ShiroTakeda/gams-ac
;; Package-Requires: ((emacs "24") (auto-complete "1.0") (gams-mode "4.0"))

;; This file is not part of any Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides auto-complete feature in GAMS mode.  To use this
;; package, you first need GAMS mode (gams-mode.el) and auto-complete mode.  You
;; can install GAMS mode and auto-complete mode from MELPA.
;;

;; Put this file into your load-path and add the following into your init.el
;; file.
;;
;;   (require 'gams-ac)
;;   (gams-ac-after-init-setup)
;;

;; If you want to add more keywords, for example, "computable", "general",
;; "equilibrium", Add the following into your init.el.
;;
;;  (setq gams-ac-source-user-keywords-list
;;         '("computable" "general" "equilibrium"))

;;; Code:

(require 'auto-complete)
(require 'gams-mode)

(defvar gams-ac-source-user-keywords-list nil
  "A list of user keywords for auto-complete source in GAMS mode.")

(defun gams-ac-user-keywords ()
  gams-ac-source-user-keywords-list)
(defvar gams-ac-source-user-keywords
  '((candidates . gams-ac-user-keywords)
    (cache))
  "Source for user keywords, eg. command, option and variable")

;; `gams-statement-alist' and `gams-alist-to-list' are defined in gams-mode.el.
(defun gams-ac-basic-commands ()
  (gams-alist-to-list gams-statement-alist))
(defvar gams-ac-source-basic-commands
  '((candidates . gams-ac-basic-commands)
    (cache))
  "Source for standard GAMS commands created from gams-statement-alist.")

;; `gams-dollar-control-alist' and `gams-alist-to-list' are defined in gams-mode.el.
(defun gams-ac-dollar-control ()
  (mapcar #'(lambda (x) (concat "$" x)) 
          (gams-alist-to-list gams-dollar-control-alist)))
(defvar gams-ac-source-dollar-control
  '((candidates . gams-ac-dollar-control)
    (cache))
  "Source for GAMS dollar control commands created from gams-dollar-control-alist.")

(defvar gams-ac-sources
  '(gams-ac-source-user-keywords
    gams-ac-source-basic-commands
    gams-ac-source-dollar-control)
  "Auto-complete source for GAMS mode.")

;;;###autoload
(defun gams-ac-setup ()
  "Set up `auto-complete' for GAMS mode."
  (setq ac-sources (append gams-ac-sources ac-sources)))

;;;###autoload
(defun gams-ac-after-init-setup ()
  "A function that should be executed in the init file."
  (add-to-list 'ac-modes 'gams-mode)
  (add-hook 'gams-mode-hook 'gams-ac-setup))

(provide 'gams-ac)

;;; gams-ac.el ends here

