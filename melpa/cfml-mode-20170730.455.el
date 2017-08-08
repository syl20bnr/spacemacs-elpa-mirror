;;; cfml-mode.el --- CFML editing support

;; Copyright 2017 Andrew Myers

;; Author: Andrew Myers <am2605@gmail.com>
;; URL: https://github.com/am2605/cfml-mode
;; Package-Version: 20170730.455
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (mmm-mode "0.5.4"))

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains definitions of CFML submode classes.

;; Usage:

;; (require 'mmm-mode)
;; (require 'cfml-mode)

;; (add-to-list 'magic-mode-alist
;;              '("<cfcomponent" . cfml-mode))
;; (add-to-list 'magic-mode-alist
;;              '("<!---" . cfml-mode))
;; (add-to-list 'auto-mode-alist
;;              '("\\.cfm\\'" . cfml-mode))
;; (add-to-list 'auto-mode-alist
;;              '("\\.cfc\\'" . cfml-cfscript-mode))

;; (setq mmm-global-mode 'maybe)
;; (mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
;; (mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)

;; Optional settings:

;; (setq mmm-submode-decoration-level 0)


;;; Code:

(require 'sgml-mode)

(mmm-add-classes
 '((html-cfm
    :submode cfml-cfscript-mode
    :front "<cfscript>"
    :back "[ \t]*</cfscript>")))

(defun cfml-indent-to-previous ()
  "Insert a newline character then indent the new line just like the previous line."
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
    (indent-relative-maybe)))

;;;###autoload
(define-derived-mode cfml-cfscript-mode js-mode "cfscript"
  (local-set-key (kbd "RET") 'cfml-indent-to-previous)
  (local-set-key (kbd "S-<tab>") (lambda () (interactive) (unindent-by-removing-n-spaces sgml-basic-offset)))
  ;; (setq indent-line-function 'tab-to-tab-stop)
  ;;(setq indent-line-function 'sgml-indent-line)
  (electric-indent-local-mode -1)
  (add-to-list 'mmm-save-local-variables 'js--quick-match-re)
  (add-to-list 'mmm-save-local-variables 'js--quick-match-re-func))
;;;###autoload
(define-derived-mode cfml-mode html-mode "CFML"
  (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
  (local-set-key (kbd "RET") 'cfml-indent-to-previous)
  ;;  (local-set-key (kbd "S-<tab>") (lambda () (interactive) (unindent-by-removing-n-spaces sgml-basic-offset)))
  ;; (setq indent-line-function 'tab-to-tab-stop)
  (setq indent-line-function 'sgml-indent-line)
  (setq comment-start "<!---")
  (setq comment-end " --->")
  (setq-local comment-start-skip "<!---[ \t]*")
  (setq-local comment-end-skip "[ \t]*---[ \t\n]*>")
  (electric-indent-local-mode -1))

(provide 'cfml-mode)

;;; cfml-mode.el ends here
