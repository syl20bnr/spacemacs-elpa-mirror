;;; dut-mode.el --- Major mode for the Dut programming language

;; Copyright 2017 The dut-mode Authors.  All rights reserved.
;; Use of this source code is governed by a MIT
;; license that can be found in the LICENSE file.

;; Author: The dut-mode Authors
;; Version: 1.0
;; Package-Version: 20170729.1411
;; Keywords: languages gut
;; URL: https://github.com/dut-lang/dut-mode
;; Package-Requires: ((emacs "24"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; dut-mode is a major mode for editing code written in Dut
;; Language.  It is a fork of the original squirrel-mode that provides
;; compatibility with Emacs 24.3 and following versions.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dut\\'" . dut-mode))

(defvar dut-indent-level 4
  "Number of columns for a unit of indentation in dut mode.")

(defconst dut-font-lock-keywords
  (list
   '("\\<\\(base\\|break\\|case\\|catch\\|clone\\|continue\\|const\\|default\\|delete\\|else\\|enum\\|extends\\|for\\|foreach\\|function\\|if\\|in\\|local\\|null\\|resume\\|return\\|switch\\|this\\|throw\\|try\\|typeof\\|while\\|yield\\|constructor\\|instanceof\\|true\\|false\\|static\\|include\\)\\>" . font-lock-builtin-face)
   '("\\<\\(class\\)\\s-+\\(\\w+\\)\\>" (1 font-lock-keyword-face) (2 font-lock-type-face))
   '("\\<\\(function\\)\\s-+\\(\\w+\\)\\>" (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   ))

(defvar dut-mode-syntax-table
  (let ((dut-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" dut-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" dut-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" dut-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" dut-mode-syntax-table)
    dut-mode-syntax-table)
  "Syntax table for dut-mode.")


(defun dut-electric-brace (arg)
  (interactive "P")
  (insert-char last-command-event 1)
  (dut-indent-line)
  (if (looking-back "^\\s *")
      (forward-char)
    (delete-char -1)
    (self-insert-command (prefix-numeric-value arg))))

(defun dut-electric-colon (arg)
  (interactive "P")
  (insert-char ?: 1)
  (dut-indent-line))


(defun dut-insert-block ()
  (interactive)
  (insert-char ?{ 1)
  (dut-indent-line)
  (newline)
  (newline)
  (insert-char ?} 1)
  (dut-indent-line)
  (forward-line -1)
  (dut-indent-line))


(defun dut-indent-line ()
  "Indent current line as dut code"
  (interactive)
  (let ((pos0 (point)) (pos1 (point)))
    (save-excursion
      (beginning-of-line)
      (let ((state (syntax-ppss)) level (indent-old 0) indent-new prev-level)
        (if (nth 8 state)
            () ;; Inside string or comment: do nothing
          (setq prev-level (dut-continuation-indent-level))
          (if prev-level
              (indent-line-to (+ prev-level dut-indent-level)) ;; continuation line
            (setq level (nth 0 state)) ;; get nest level
            (if (looking-at "\\s-*\\(}\\|\\]\\|)\\)")
                (setq level (1- level))
              (if (looking-at "\\s-*\\(case\\|default\\)")
                  (setq level (1- level))))
            (indent-line-to (* dut-indent-level (max 0 level))))
          (setq pos1 (point)))))
    (if (> pos1 pos0)
        (goto-char pos1))))


(defun dut-continuation-indent-level ()
  (save-excursion
    (while (forward-comment -1))
    (if (looking-back ")")
        (condition-case nil
            (progn
              (goto-char (scan-sexps (point) -1))
              (if (looking-at "(")
                  (progn
                    (while (forward-comment -1))
                    (if (looking-back "\\<\\(if\\|catch\\|while\\|for\\|foreach\\)")
                        (current-indentation)))))
          (error nil))
      (if (looking-back "\\<\\(else\\|try\\|in\\|instanceof\\|typeof\\)")
          (current-indentation)
        (if (looking-back "\\(\\+\\+\\|--\\)")
            ()
          (if (looking-back "[-+=~!/*%<>^|&?]")
              (current-indentation)))))))

;;;###autoload
(define-derived-mode dut-mode prog-mode "dut"
  "Major mode for editing dut script"
  (set (make-local-variable 'font-lock-defaults) '(dut-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'dut-indent-line)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'parse-sexp-ignore-comments) t))


(define-key dut-mode-map "{" 'dut-electric-brace)
(define-key dut-mode-map "}" 'dut-electric-brace)
(define-key dut-mode-map ":" 'dut-electric-colon)
(define-key dut-mode-map "\C-c\C-o" 'dut-insert-block)


(provide 'dut-mode)
;;; dut-mode.el ends here
