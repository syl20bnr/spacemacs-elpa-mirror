;;; synquid.el --- Major mode for editing Synquid files

;; Copyright (C) 2016  Clément Pit--Claudel
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/synquid-mode
;; Package-Version: 20160617.2156
;; Keywords: languages
;; Package-Requires: ((flycheck "27") (emacs "24.3"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Synquid programming in Emacs!
;;
;; For information about Synquid, visit
;; https://bitbucket.org/nadiapolikarpova/synquid

;;; Code:

(require 'flycheck)

(defvar synquid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'synquid-synthesize)
    map))

(defvar synquid-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (char (string-to-list "_?0123456789"))
      (modify-syntax-entry char  "_" table))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}1nb" table)
    (modify-syntax-entry ?\} "){4nb" table)
    (modify-syntax-entry ?- ". 123" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `synquid-mode'.")

(defconst synquid-prettify-symbols-alist
  '(("==" . ?≡) ("!=" . ?≠) ("<=" . ?≤)
    (">=" . ?≥) ("&&" . ?∧) ("||" . ?∨)
    ("==>" . ?⇒) ("<==>" . ?⇔) ("->" . ?→)
    ("_v" . "ν") ("\\" . ?λ)))

(defconst synquid--fl-types '("Bool" "Int" "Set"))
(defconst synquid--fl-builtins '("False" "True" "_v"))
(defconst synquid--fl-keywords '("data" "else" "error" "if" "in" "inline"  "let" "match" "measure"
                          "predicate" "qualifier" "termination" "then"  "type" "with" "where"))

(defconst synquid--fl-symbol-re "\\(\\(?:\\sw\\|\\s_\\)+\\)")
(defconst synquid--fl-def-headers '("measure" "termination measure" "predicate"))

(defgroup synquid nil
  "Synquid configuration"
  :group 'languages)

(defface synquid-de-bruijn-face
  '((t :underline t :inherit font-lock-variable-name-face))
  "Face used to highlight De Bruijn indices."
  :group 'synquid)

(defvar synquid--font-lock-keywords
  `((,(regexp-opt synquid--fl-types 'symbols) 0 font-lock-type-face)
    (,(regexp-opt synquid--fl-builtins 'symbols) 0 font-lock-builtin-face)
    (,(regexp-opt synquid--fl-keywords 'symbols) 0 font-lock-keyword-face)
    ("\\_<\\([A-Z]\\sw+\\)\\_>" 1 font-lock-type-face)
    ("\\\\\\|\\_<\\?\\?\\_>" 0 font-lock-variable-name-face)
    ("\\_<\\(_\\)\\([0-9]+\\)\\_>"
     (1 '(face synquid-de-bruijn-face)) ;; display "​"
     (2 'synquid-de-bruijn-face))
    (,(concat "\\_<let " synquid--fl-symbol-re "\\_>") 1 font-lock-variable-name-face)
    (,(concat "^\\(?:" (regexp-opt synquid--fl-def-headers) "\\s-+\\)?"
              synquid--fl-symbol-re "\\s-*\\(::\\|=\\)")
     1 font-lock-function-name-face))
  "Font-lock specs for Synquid mode.")

(defvar synquid--args '("--resolve"))

(flycheck-define-checker synquid
  "Flycheck checker for Synquid files."
  :command ("synquid" (eval synquid--args) source)
  :error-patterns
  ((error bol (file-name) ":" line (?  ":" column) ": "
          (message (or "Parse Error"
                       "Resolution Error"
                       "No Solution. Last candidate failed with error")
                   ":\n" (+ "  " (+ not-newline) "\n"))))
  :modes (synquid-mode))

(add-to-list 'flycheck-checkers 'synquid)

(defun synquid-synthesize ()
  "Feed current file to Synquid and show synthesis results."
  (interactive)
  (let ((synquid-args nil))
    (flycheck-compile 'synquid)))

;;;###autoload
(define-derived-mode synquid-mode prog-mode "Synquid"
  "A major mode for editing Synquid files."
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "--+\\s-*")
  (setq-local font-lock-defaults '(synquid--font-lock-keywords))
  (when (fboundp 'prettify-symbols-mode)
    (setq-local prettify-symbols-alist synquid-prettify-symbols-alist)
    (prettify-symbols-mode))
  (flycheck-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sq\\'" . synquid-mode))

(provide 'synquid)
;;; synquid.el ends here
