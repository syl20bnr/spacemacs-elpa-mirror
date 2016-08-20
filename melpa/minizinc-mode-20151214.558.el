;;; minizinc-mode.el --- Major mode for MiniZinc code -*- lexical-binding: t; -*-

;; Copyright © 2015 Yushi Wang
;; Authors : Yushi Wang <dot_wangyushi@yeah.net>
;; URL : http://github.com/m00nlight/minizinc-mode
;; Package-Version: 20151214.558
;; Keywords : languages minizinc
;; Version : 0.0.1
;; Package-Requires : ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide font-lock for minizinc(http://www.minizinc.org/) code

;; Here are some example of configuration:

;;; Installation:
;;
;; You can either manually install minizinc-mode or automatically
;; install from melpa. Add the following line into your ~/.emacs
;; file or any your emacs start file to install it from the melpa.
;;
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (unless package-archive-contents (package-refresh-contents))
;; (package-initialize)
;;
;; install minizinc-mode
;;
;; M-x package-install
;; minizinc-mode
;;
;; Add the following lines to your emacs after installation.
;; (require 'minizinc-mode)
;; (add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))


;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cc-mode)

(defgroup minizinc nil
  "Major mode for MiniZinc code"
  :group 'languages)

;; (defface font-lock-operator-face
;;   '((t :inherit font-lock-builtin-face))
;;   "Used for operators."
;;   :group 'font-lock-faces)

(defface minizinc-operator-face
  '((t (:foreground "dark orange")))
  "Used for operators."
  :group 'minizinc)

(defvar minizinc-operator-face 'minizinc-operator-face)


(defvar minizinc-keywords
  '("var" "constraint" "solve" "satisfy" "maximize"
    "minimize" "output" "par" "of" "where" "ann"
    "annotation" "any" "array" "function" "include"
    "op" "predicate" "record" "test" "tuple" "type"))

(defvar minizinc-types
  '("float" "int" "bool" "string" "list" "tuple"))

(defvar minizinc-builtins
  '("abort" "abs" "acosh" "array_intersect" "array_union"
    "array1d" "array2d" "array3d" "array4d" "array5d"
    "array6d" "asin" "assert" "atan" "bool2int" "card"
    "ceil" "concat" "cos" "cosh" "dom" "dom_array"
    "dom_size" "fix" "exp" "floor" "index_set"
    "index_set_1of2" "index_set_2of2" "index_set_1of3"
    "int2float" "is_fixed" "join" "lb" "lb_array"
    "length" "ln" "log" "log2" "log10" "min" "max"
    "pow" "product" "round" "set2array" "show" "show_int"
    "show_float" "sin" "sinh" "sqrt" "sum" "tan" "tanh"
    "trace" "ub" "ub_array"
    ))



(defvar minizinc-operators
  (concat "<\\->\\|\\->\\|<-\\|\\\\/\\|xor\\|/\\\\\\|<\\|>="
          "\\|<=\\|==\\|!=\\|<\\|>\\|=\\|in\\|subset\\|union"
          "\\|superset\\|diff\\|symdiff\\|\\.\\.\\|intersect"
          "\\|++\\|+\\|-\\|*\\|/\\|div\\|mod"))


(defvar minizinc-keywords-regex
  (regexp-opt minizinc-keywords 'words))

(defvar minizinc-types-regex
  (regexp-opt minizinc-types 'words))

(defvar minizinc-builtins-regex
  (regexp-opt minizinc-builtins 'words))

(defvar minizinc-operators-regex
  minizinc-operators)

(defvar minizinc-font-lock-keywords
  `(
    ("%.*" . font-lock-comment-face)
    (,minizinc-builtins-regex . font-lock-builtin-face)
    (,minizinc-types-regex . font-lock-type-face)
    (,minizinc-keywords-regex . font-lock-keyword-face)
    (,minizinc-operators-regex . minizinc-operator-face)
    ))

;;;###autoload
(define-derived-mode minizinc-mode java-mode "MiniZinc mode"
  "Major mode for edigint minizinc source file."
  (setq font-lock-defaults '((minizinc-font-lock-keywords)))
  (set (make-local-variable 'c-basic-offset) 0)
  (setq comment-start "%")
  (setq comment-end "")
  )


(setq minizinc-keywords nil)
(setq minizinc-keywords-regex nil)
(setq minizinc-builtins nil)
(setq minizinc-builtins-regex nil)
(setq minizinc-types nil)
(setq minizinc-types-regex nil)
(setq minizinc-operators nil)
(setq minizinc-operators-regex nil)

(provide 'minizinc-mode)

;; Local Variable:
;; coding: utf-8
;; End

;;; minizinc-mode.el ends here
