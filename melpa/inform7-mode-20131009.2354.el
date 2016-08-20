;;; inform7-mode.el --- Major mode for editing Inform 7 source files
;;
;; Copyright (C) 2013 Fredrik Appelberg
;; 
;; Author: Fredrik Appelberg
;; Version: 0.2
;; Package-Version: 20131009.2354
;; Package-Requires: ((sws-mode "0.1"))
;; Keywords: inform, inform7, interactive fiction

;;; Commentary:

;; A basic major mode for editing Inform 7 story files. It handles highlighting
;; and indentation, but nothing else at the moment. So you'll still need the full
;; Inform IDE for actual development.

;;; Code:

(require 'sws-mode)

(defface inform7-heading-face
  '((t (:inherit font-lock-preprocessor-face :weight bold :height 1.2)))
  "Face for Inform 7 headings"
  :group 'font-lock-highlighting-faces)

(defvar inform7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for inform7 major mode")

(defadvice sws-do-indent-line (around inform7-indent-blank () activate)
  "Ensure proper indentation of blank lines according to Inform7 conventions."
  (if (and (eq major-mode 'inform7-mode) (sws-empty-line-p))
    (if (save-excursion
          (previous-line)
          (looking-at "^.*:[ \t]*$"))
        (indent-to (sws-max-indent))
      (indent-to (sws-previous-indentation)))
    ad-do-it))

(defconst inform7-font-lock-keywords
  `(( ,(regexp-opt '("let" "say" "if" "otherwise") 'words) . font-lock-keyword-face)
    ("^\\(\\(?:Book\\|Chapter\\|Part\\|Section\\|Volume\\) - .*\\)" . 'inform7-heading-face)
    (".\\(\\[.*?\\]\\)." 0 font-lock-variable-name-face t)
    )
  "Highlighting expressions for inform7-mode")

;;;###autoload
(define-derived-mode inform7-mode sws-mode "Inform7"
  "Major mode for editing inform 7 story files."
  (visual-line-mode)
  (set (make-local-variable 'font-lock-defaults) '(inform7-font-lock-keywords nil t)))

(modify-syntax-entry ?\[ "<]" inform7-mode-syntax-table)
(modify-syntax-entry ?\] ">[" inform7-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ni\\'" . inform7-mode))

(provide 'inform7-mode)
;;; inform7-mode.el ends here
