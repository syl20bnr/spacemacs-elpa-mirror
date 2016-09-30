;;; faust-mode.el --- Basic faust syntax colorizer for emacs.

;; Copyright (C) 2016 Juan A. Romero

;; Author: Juan A. Romero <rukano@gmail.com>
;; Version: 0.1
;; Package-Version: 20160930.222
;; Maintainer: Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/magnetophon/emacs-faust-mode
;; Keywords: languages

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FAUST Mode (very simple syntax colorizing!)
;; by rukano
;; based on the tutorial on:
;; http://xahlee.org/emacs/elisp_syntax_coloring.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BIG TODOS:
;; Colorize Composition Operators
;; Colorize after keyword {}
;; Colorize arguments (numbers)
;; Colorize [] metadata in string?
;; Run Shell faust w/ custom defaults


;; ROADMAP
;; export option and list possibilities
;; create hotkeys for every compilation
;; view graph

;;; Code:

(defvar faust-keywords
  '("process" "with" "case" "seq" "par" "sum" "prod"
    "include" "import" "component" "library" "environment" "declare"
    "define" "undef" "error" "pragma" "ident"
    "if" "def" "else" "elif" "endif" "line" "warning"))

(defvar faust-functions
  '("mem" "prefix" "int" "float"
    "rdtable" "rwtable" "select2" "select3"
    "ffunction" "fconstant" "fvariable"
    "attach" "acos" "asin" "atan" "atan2" "cos" "sin" "tan" "exp"
    "log" "log10" "pow" "sqrt" "abs" "min" "max" "fmod"
    "remainder" "floor" "ceil" "rint"))

(defvar faust-ui-keywords
  '("button" "checkbox" "vslider" "hslider" "nentry"
    "vgroup" "hgroup" "tgroup" "vbargraph" "hbargraph"))

;; optimize regex for words
;;(defvar faust-math-op-regexp "[=\+()\{\}*-]")
(defvar faust-variables-regexp "[A-Za-z][A-Za-z]*")
(defvar faust-arguments-regexp "[0-9]")
(defvar faust-operator-regexp "\\([~!_@,<>:;]\\)")
(defvar faust-math-op-regexp "[=\+\{\}()/*-]")
(defvar faust-keywords-regexp (regexp-opt faust-keywords 'words))
(defvar faust-function-regexp (regexp-opt faust-functions 'words))
(defvar faust-ui-keywords-regexp (regexp-opt faust-ui-keywords 'words))

;; create the list for font-lock.
(defvar faust-font-lock-keywords
  `(
    (,faust-function-regexp . font-lock-type-face)
    (,faust-ui-keywords-regexp . font-lock-builtin-face)
    (,faust-math-op-regexp . font-lock-function-name-face)
    (,faust-operator-regexp . font-lock-constant-face)
    (,faust-keywords-regexp . font-lock-keyword-face)
;;    (,faust-variables-regexp . font-lock-variable-name-face)
;;    (,faust-arguments-regexp . font-lock-warning-face)
))

;; define the mode
;;;###autoload
(define-derived-mode faust-mode fundamental-mode
  "FAUST mode"
  "Major mode for editing FAUST files (Functional Audio Stream)…"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((faust-font-lock-keywords)))

  ;; set mode-local comment syntax
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) ""))

(modify-syntax-entry ?/  ". 124b" faust-mode-syntax-table)
(modify-syntax-entry ?*  ". 23" faust-mode-syntax-table)
(modify-syntax-entry ?\n "> b" faust-mode-syntax-table)
(modify-syntax-entry ?\^m "> b" faust-mode-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OLD CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: composition operators!
;;(": , :> <: ~ ! _ @ ")
;;(defvar faust-operators "\\([]<>[~:,@!]\\)")
;;(defvar faust-operators "\\([~!_@,<>:;]\\)")

;;(defvar faust-math-op "[=\+()\{\}]") ;; add * and /
;;(defvar faust-math-op "\\([][{}()^\\+*/%-=]\\)")
;;(defvar faust-math-op "\\([][{}()~^<>:=,.\\+*/%-@!]\\)")
;; clear memory
;; (setq faust-keywords nil)
;; (setq faust-functions nil)
;; (setq faust-operators nil)
;; (setq faust-ui-keywords nil)
;; (setq faust-math-op nil)


;; ;; clear memory
;; (setq faust-keywords-regexp nil)
;; (setq faust-functions-regexp nil)
;; (setq faust-operators-regexp nil)
;; (setq faust-ui-keywords-regexp nil)
;; (setq faust-math-op-regexp nil)

(provide 'faust-mode)
;;; faust-mode.el ends here
