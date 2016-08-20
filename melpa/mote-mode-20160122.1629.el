;;; mote-mode.el --- Mote minor mode
;;
;; Copyright 2012 Leandro López (inkel)
;;
;; Author: Leandro López (inkel) <inkel.ar@gmail.com>
;; URL: http://inkel.github.com/mote-mode/
;; Package-Version: 20160122.1629
;; Version: 1.0.0
;; Package-Requires: ((ruby-mode "1.1"))
;;
;; Mote is the Minimum Operational Template, a very simple and fast
;; template engine for Ruby. For more information about mote go to
;; https://github.com/soveran/mote
;;
;;; Code:
(require 'font-lock)
(require 'ruby-mode)

(defgroup mote nil
  "Mote mode customization group"
  :group 'ruby)

(defun mote-highlight-ruby (limit)
  "Highlight a Ruby expression."
  (when (re-search-forward "^[ \t]*\\(%\\)\\(.*\\)$" limit t)
    (set-text-properties (match-beginning 1) (match-end 1) '(face font-lock-preprocessor-face))
    (mote-fontify-region-as-ruby (match-beginning 2) (match-end 2))))

(defun mote-highlight-assignment (limit)
  "Highlight a Ruby expression inside an assignment."
  (let ((beg (search-forward "{{" limit t))
        (end (search-forward "}}" limit t)))
    (when (and beg end)
      (mote-fontify-region-as-ruby beg (- end 2)))))

(defun mote-fontify-region-as-ruby (beg end)
  "Use Ruby's font-lock variables to fontify region between BEG and END."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords ruby-font-lock-keywords)
            (font-lock-syntax-table ruby-font-lock-syntax-table)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (font-lock-fontify-region beg end)))))

(defvar mote-font-lock-keywords
  '(("{{" . font-lock-preprocessor-face)
    ("}}" . font-lock-preprocessor-face)
    mote-highlight-assignment
    mote-highlight-ruby)
  "Additional syntax highlighting for Mote files.")

(define-minor-mode mote-mode
  "Toggle mote minor mode"
  :lighter " mote"
  :global nil
  :group 'mote
  (if mote-mode
      (font-lock-add-keywords nil mote-font-lock-keywords)
    (font-lock-remove-keywords nil mote-font-lock-keywords))
  (font-lock-fontify-buffer))

(provide 'mote-mode)

;;; mote-mode.el ends here
