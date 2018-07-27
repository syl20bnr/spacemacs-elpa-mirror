;;; outlined-elisp-mode.el --- outline-minor-mode settings for emacs lisp

;; Copyright (C) 2012 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Version: 1.0.5
;; Package-Version: 20131108.1127
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; This is an outline-minor-mode settings for emacs lisp. For example,
;; "outlined" emacs lisp code look like this.
;;
;;   ;; * general settings
;;   ;; ** show-paren-mode
;;
;;   (show-paren-mode 1)
;;   (setq show-paren-delay 0)
;;
;;   ;; ** cua-mode
;;
;;   (cua-mode t)
;;   (setq cua-enable-cua-keys nil)
;;
;;   ;; * emacs-lisp-mode settings
;;
;;   (add-hook 'emacs-lisp-mode-hook 'outlined-elisp-find-file-hook)

;; To install, put code like
;;
;;   (require 'outlined-elisp-mode)
;;
;; in your .emacs file. You can activate outlined-elisp-mode with
;; "M-x outlined-elisp-mode" command. Or, if you put code like
;;
;;   (add-hook 'emacs-lisp-mode-hook 'outlined-elisp-find-file-hook)
;;
;; in your .emacs file, outlined-elisp-mode is automatically activated
;; when one of the first 300 lines seem to be heading of outlined-elisp.
;; You can also change the trigger, and the range of search.
;;
;;   (setq outlined-elisp-trigger-pattern ";; \\+\\+ outlined-elisp \\+\\+")
;;   (setq outlined-elisp-trigger-limit 3)

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 added outlined-elisp-startup-folded
;; 1.0.2 now allows indents before heading
;; 1.0.3 changed default outlined-elisp-trigger-pattern
;; 1.0.4 minor fix
;; 1.0.5 changed deafult outlined-elisp-trigger-pattern

;;; Code:

;; * constants

(defconst outlined-elisp-version "1.0.5")

;; * suppress byte-compiler

(declare-function outline-level "outline")
(declare-function hide-sublevels "outline")

;; * customs

(defgroup outlined-elisp nil
  "outline-minor-mode settings for emacs lisp"
  :group 'emacs)

(defcustom outlined-elisp-regexp "^[\s\t]*;;\s[*]+\s"
  "regexp that matches headings"
  :group 'outlined-elisp)

(defcustom outlined-elisp-top-level 4
  "the minimum length of heading"
  :group 'outlined-elisp)

(defcustom outlined-elisp-trigger-pattern "^;;\s\\*+\s"
  "trigger pattern for outlined-elisp-mode"
  :group 'outlined-elisp)

(defcustom outlined-elisp-trigger-limit 300
  "the maximum length of search for trigger"
  :group 'outlined-elisp)

(defcustom outlined-elisp-startup-folded t
  "if outlined-elisp should fold on startup"
  :group 'outlined-elisp)

;; * mode variable

(define-minor-mode outlined-elisp-mode
  "outline-minor-mode settings for emacs lisp"
  :init-value nil
  :lighter " OLisp"
  :global nil
  (if outlined-elisp-mode
      (progn (outline-minor-mode 1)
             (make-local-variable 'outline-regexp)
             (setq outline-regexp outlined-elisp-regexp)
             (make-local-variable 'outline-level)
             (setq outline-level
                   (lambda ()
                     (- (outline-level) outlined-elisp-top-level)))
             (when outlined-elisp-startup-folded (hide-sublevels 1)))
    (outline-minor-mode -1)
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)))

(defadvice outline-minor-mode (after deactivate-outlined-elisp-mode-automatically activate)
  (when (and (not outline-minor-mode) outlined-elisp-mode)
    (outlined-elisp-mode -1)))

;; * trigger

(defun outlined-elisp-get-first-n-lines (num)
  (let* ((begin 1)
         (end (point-at-eol begin)))
    (while (and (> (setq num (1- num)) 0)
                (> (point-max) (1+ end)))
      (setq end (point-at-eol (1+ end))))
    (buffer-substring-no-properties begin end)))

(defun outlined-elisp-find-file-hook ()
  (when (string-match
         outlined-elisp-trigger-pattern
         (outlined-elisp-get-first-n-lines outlined-elisp-trigger-limit))
    (outlined-elisp-mode)))

;; * provide

(provide 'outlined-elisp-mode)

;;; outlined-elisp-mode.el ends here
