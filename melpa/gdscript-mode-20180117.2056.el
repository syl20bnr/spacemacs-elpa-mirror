;;; gdscript-mode.el --- Major mode for editing Godot GDScript files

;; Copyright (C) 2017 Adam Bark

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Adam Bark <adam@adambark.com>
;; Version: 1.0
;; Package-Version: 20180117.2056
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages
;; URL: https://github.com/AdamBark/gdscript-mode

;;; Commentary:
;; This package provides a major mode for editing GDScript files.
;; GDScript is the scripting language used by the game engine Godot
;; <https://godotengine.org>

;;; Code:
(require 'python)

(defvar gdscript-mode-map
  (make-keymap)
  "Keymap for gdscript major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))


(defvar gdscript-font-lock-keywords
  `((,(concat "\\<"
              (regexp-opt '("if" "elif" "else" "for" "do" "while" "switch" "case"
	                    "break" "continue" "pass" "return" "class" "extends" "tool"
	                    "signal" "func" "static" "const" "enum" "var" "onready"
	                    "export" "setget" "breakpoint"))
              "\\>")
     . font-lock-keyword-face)
    ("func +\\([A-Za-z0-9_]+\\)" (1 font-lock-function-name-face))
    ("\\([A-Za-z0-9_.]+\\)\s*=" (1 font-lock-variable-name-face))))

(defvar gdscript-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `gdscript-mode'.")

(defvar gdscript-imenu-generic-expression
  (setq gdscript-imenu-generic-expression
	'((nil "func +\\|class +\\([A-Za-z0-9_]+\\)" 1))))

(defvar gdscript-mode-abbrev-table)

;;;###autoload
(define-derived-mode gdscript-mode prog-mode "GDScript"
  "Major mode for editing Godot GDScript files"
  :syntax-table gdscript-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(gdscript-font-lock-keywords))
  (setq-local indent-line-function 'python-indent-line-function)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local imenu-generic-expression gdscript-imenu-generic-expression))

(provide 'gdscript-mode)
;;; gdscript-mode.el ends here
