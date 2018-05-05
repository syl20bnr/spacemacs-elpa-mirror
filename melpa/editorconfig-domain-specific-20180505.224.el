;;; editorconfig-domain-specific.el --- Apply brace style and other "domain-specific" EditorConfig properties -*- lexical-binding: t -*-
;;
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/editorconfig-emacs-domain-specific
;; Package-Version: 20180505.224
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (editorconfig "0.6.0"))
;; Keywords: editorconfig util
;; License: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An EditorConfig hook that applies the unofficial indent_brace_style
;; setting for C-like programming languages.
;;
;; To use:
;;
;; (add-hook 'editorconfig-custom-hooks 'editorconfig-domain-specific)
;;
;;; Code:

;; Thanks to @10sr. editorconfig-custom-majormode.el taught me how to
;; make an editorconfig hook.

(require 'cc-mode)
(require 'cl-lib)

;; TODO: I believe the critical ingredient is that all these modes are
;; somehow derived from cc-mode. Can we check that directly?  Using
;; derived-mode-p does not work.
(defvar editorconfig-domain-specific-c-like-modes
  '(c-mode c++-mode java-mode js-mode objc-mode php-mode))

;;;###autoload
(defun editorconfig-domain-specific (hash)
  "Apply brace style and other \"domain-specific\" EditorConfig properties

Properties are applied from HASH to the current buffer (but only
when they make sense for the current major mode).  Unknown
properties and invalid property values are ignored.

To use:

  (add-hook 'editorconfig-custom-hooks 'editorconfig-domain-specific)

or do M-x `customize-variable' `editorconfig-custom-hooks' and
insert `editorconfig-domain-specific' there.

Supported EditorConfig properties:

* `indent_brace_style' -- can be one of Allman, GNU, K&R

See:
https://github.com/editorconfig/editorconfig/wiki/EditorConfig-Properties
section `Ideas for Domain-Specific Properties'."
  (when (member major-mode editorconfig-domain-specific-c-like-modes)
    (let ((ibs (gethash 'indent_brace_style hash))
	  (isize (gethash 'indent_size hash)))
      (cond ((cl-equalp ibs "Allman")
	     (c-set-style "bsd"))
	    ((cl-equalp ibs "GNU")
	     (c-set-style "gnu"))
	    ((cl-equalp ibs "K&R")
	     (c-set-style "k&r")))
      (cond ((cl-equalp isize "tab")
	     (setq indent-tabs-mode t)
	     (setq c-basic-offset tab-width))
	    ((not (null isize))
	     (let ((isize (string-to-number isize)))
	       (when (and (integerp isize) (> isize 0))
		 (setq indent-tabs-mode nil)
		 (setq c-basic-offset isize))))))))

(provide 'editorconfig-domain-specific)

;;; editorconfig-domain-specific.el ends here
