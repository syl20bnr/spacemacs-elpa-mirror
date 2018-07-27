;;; auto-complete-distel.el --- Erlang/distel completion backend for auto-complete-mode

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author: Sebastian Weddmark Olsson
;; URL: github.com/sebastiw/distel-completion
;; Package-Version: 20160816.600
;; Version: 1.0.0
;; Package-Requires: ((auto-complete "1.4") (distel-completion-lib "1.0.0"))
;; Keywords: Erlang Distel auto-complete

;; This file is not part of GNU Emacs.

;;; License:

;; BEER-WARE
;; If you like this package and we meet in the future, you can buy me a
;; beer. Otherwise, if we don't meet, drink a beer anyway.

;;; Commentary:

;; Add `auto-complete-distel' to the `ac-sources' list in your .emacs.
;; E.g.
;;   (require 'auto-complete)
;;   (require 'auto-complete-distel)
;;   (add-to-list 'ac-sources 'auto-complete-distel)
;;
;; Customize
;; ------------------
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-get-doc-from-internet t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-valid-syntax "a-zA-Z:_-")

;;; Code:

(require 'auto-complete)
(require 'distel-completion-lib)

(defvar auto-complete-distel
  (list '(prefix . auto-complete-distel-get-start)
	'(candidates . (distel-completion-complete ac-prefix (current-buffer)))
	'(document . distel-completion-get-doc-string)
	'(requires . 0)
	'(symbol . "m"))
    "All it takes to start a auto-complete backend.")

(defun auto-complete-distel-get-start ()
  "Find a valid start of a completion word."
  (save-excursion
    (let ((distance (skip-chars-backward distel-completion-valid-syntax)))
      (when (< distance 0)
        (point)))))

(provide 'auto-complete-distel)

;;; auto-complete-distel.el ends here
