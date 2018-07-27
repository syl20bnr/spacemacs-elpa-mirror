;;; ink-mode.el --- Major mode for writing interactive fiction in Ink

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/ink-mode
;; Package-Version: 20160814.1116
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; `ink-mode' provides basic syntax highlightning for the Ink scripting
;; language, developed by Inkle Studios.  There's also a command `ink-play' to
;; playtest your story from Emacs (bound to C-c C-c by default).

;;; Code:
(require 'rx)

(defgroup ink nil
  "Major mode for writing interactive fiction in Ink."
  :group 'languages)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'ink-play)
    map)
  "Keymap for ink major mode.")

(defconst ink-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // starts a comment
    (modify-syntax-entry ?/ ". 12" st)
    ;; End of line ends a comment
    (modify-syntax-entry ?\n ">" st)
    st))

(defface ink-condition-face
  '((t :inherit font-lock-type-face))
  "Face for conditions in ink-mode.")

(defvar ink-font-lock-keywords
  `(
    ;; Knots, functions and stitches
    ("^=+ *[[:word:]_]+\\(?:(.*)\\)? *=*" .
     font-lock-function-name-face)
    ;; Diverts and threads
    ("\\(?:->\\|<-\\) *[[:word:]_.]+\\(?:(.*)\\)? *$" .
     font-lock-function-name-face)
    ;; Labels
    (,(rx bol (0+ whitespace)
          (1+ (or " " "*" "+" "-"))
          (group "(" (1+ not-newline) ")"))
     1 font-lock-function-name-face)
    ;; Keywords at beginning of line
    (,(rx bol (or "VAR" "CONST" "INCLUDE") word-end) .
     font-lock-keyword-face)
    ;; Vars/constants
    ("^\\(?:VAR\\|CONST\\) +\\([[:word:]_]+\\)" 1
     font-lock-variable-name-face)
    ;; Conditions
    (,(rx bol (0+ whitespace)
          (1+ (or " " "*" "+"))
          (group "{" (1+ not-newline) "}"))
     1 font-lock-type-face)
    ;; Code lines
    (,(rx bol (0+ whitespace)
          "~" (1+ not-newline)) . font-lock-type-face)))

(defvar ink-inklecate-path (executable-find "inklecate")
  "The path to the Inklecate executable.")

(defun ink-play ()
  "Play the current ink buffer."
  (interactive)
  (let ((buffer (comint-check-proc "Ink")))
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Ink*"))
       (current-buffer)))
    (unless buffer
      (switch-to-buffer-other-window
       (apply 'make-comint-in-buffer "Ink" buffer
              ink-inklecate-path nil `("-p" ,(buffer-file-name)))))))

;;;###autoload
(define-derived-mode ink-mode
  prog-mode "Ink"
  "Major mode for editing interactive fiction using the Ink
  scripting language."
  :syntax-table ink-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq font-lock-defaults '(ink-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
;;; ink-mode.el ends here
