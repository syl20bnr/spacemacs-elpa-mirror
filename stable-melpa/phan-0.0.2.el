;;; phan.el --- Utility functions for Phan (PHP static analizer)  -*- lexical-binding: t -*-

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@pixiv.com>
;; Created: 28 Jan 2017
;; Version: 0.0.2
;; Package-Version: 0.0.2
;; Keywords: tools php
;; Package-Requires: ((emacs "24") (composer "0.0.8") (f "0.17"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phan is static analizer for PHP.  https://github.com/etsy/phan
;; This package has utilities and major mode for phan log format.


;;; Code:
(require 'composer)

(defgroup phan nil
  "Utilities for Phan (PHP static analizer)"
  :prefix "phan-"
  :group 'tools
  :group 'php)

(defconst phan-php-types
  '("array" "bool" "float" "int" "null" "resource" "string" "void"))

(defconst phan-issues
  '("PhanSyntaxError"
    ;; Issue::CATEGORY_UNDEFINED
    "PhanEmptyFile"
    "PhanParentlessClass"
    "PhanTraitParentReference"
    "PhanUndeclaredClass"
    "PhanUndeclaredClassCatch"
    "PhanUndeclaredClassConstant"
    "PhanUndeclaredClassInstanceof"
    "PhanUndeclaredClassMethod"
    "PhanUndeclaredClassReference"
    "PhanUndeclaredConstant"
    "PhanUndeclaredExtendedClass"
    "PhanUndeclaredFunction"
    "PhanUndeclaredInterface"
    "PhanUndeclaredMethod"
    "PhanUndeclaredProperty"
    "PhanUndeclaredStaticMethod"
    "PhanUndeclaredStaticProperty"
    "PhanUndeclaredTrait"
    "PhanUndeclaredTypeParameter"
    "PhanUndeclaredTypeProperty"
    "PhanUndeclaredVariable"

    ;; Issue::CATEGORY_TYPE
    "PhanNonClassMethodCall"
    "PhanTypeArrayOperator"
    "PhanTypeArraySuspicious"
    "PhanTypeComparisonFromArray"
    "PhanTypeComparisonToArray"
    "PhanTypeConversionFromArray"
    "PhanTypeInstantiateAbstract"
    "PhanTypeInstantiateInterface"
    "PhanTypeInvalidLeftOperand"
    "PhanTypeInvalidRightOperand"
    "PhanTypeMismatchArgument"
    "PhanTypeMismatchArgumentInternal"
    "PhanTypeMismatchDefault"
    "PhanTypeMismatchForeach"
    "PhanTypeMismatchProperty"
    "PhanTypeMismatchReturn"
    "PhanTypeMissingReturn"
    "PhanTypeNonVarPassByRef"
    "PhanTypeParentConstructorCalled"
    "PhanTypeVoidAssignment"

    ;; Issue::CATEGORY_ANALYSIS
    "PhanUnanalyzable"

    ;; Issue::CATEGORY_VARIABLE
    "PhanVariableUseClause"

    ;; Issue::CATEGORY_STATIC
    "PhanStaticCallToNonStatic"

    ;; Issue::CATEGORY_CONTEXT
    "PhanContextNotObject"

    ;; Issue::CATEGORY_DEPRECATED
    "PhanDeprecatedClass"
    "PhanDeprecatedFunction"
    "PhanDeprecatedProperty"

    ;; Issue::CATEGORY_PARAMETER
    "PhanParamReqAfterOpt"
    "PhanParamSpecial1"
    "PhanParamSpecial2"
    "PhanParamSpecial3"
    "PhanParamSpecial4"
    "PhanParamTooFew"
    "PhanParamTooFewInternal"
    "PhanParamTooMany"
    "PhanParamTooManyInternal"
    "PhanParamTypeMismatch"
    "PhanParamSignatureMismatch"
    "PhanParamSignatureMismatchInternal"
    "PhanParamRedefined"

    ;; Issue::CATEGORY_NOOP
    "PhanNoopArray"
    "PhanNoopClosure"
    "PhanNoopConstant"
    "PhanNoopProperty"
    "PhanNoopVariable"
    "PhanUnreferencedClass"
    "PhanUnreferencedMethod"
    "PhanUnreferencedProperty"
    "PhanUnreferencedConstant"

    ;; Issue::CATEGORY_REDEFINE
    "PhanRedefineClass"
    "PhanRedefineClassInternal"
    "PhanRedefineFunction"
    "PhanRedefineFunctionInternal"
    "PhanIncompatibleCompositionProp"
    "PhanIncompatibleCompositionMethod"

    ;; Issue::CATEGORY_ACCESS
    "PhanAccessPropertyPrivate"
    "PhanAccessPropertyProtected"
    "PhanAccessMethodPrivate"
    "PhanAccessMethodProtected"
    "PhanAccessSignatureMismatch"
    "PhanAccessSignatureMismatchInternal"
    "PhanAccessStaticToNonStatic"
    "PhanAccessNonStaticToStatic"

    ;; Issue::CATEGORY_COMPATIBLE
    "PhanCompatibleExpressionPHP7"
    "PhanCompatiblePHP7"

    ;; Issue::CATEGORY_GENERIC
    "PhanTemplateTypeConstant"
    "PhanTemplateTypeStaticMethod"
    "PhanTemplateTypeStaticProperty"
    "PhanGenericGlobalVariable"
    "PhanGenericConstructorTypes"
    ))

(defconst phan-log-warning-keywords
  '("can't be"
    "deprecated"
    "has no return value"
    "only takes"
    "should be compatible"
    "Suspicious"
    "undeclared"))

(defconst phan-log-class-prefix-keywords
  '(":" "but" "class" "for" "function" "is" "method" "property" "return" "takes" "to" "type"
    "Class" "Property" "Method"))

(defconst phan-log-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?$  "_" table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?>  "." table)
    table))

(defconst phan-log-font-lock-keywords
  (list
   (list "^\\([^:]+\\):\\([0-9]+\\)"
         '(1 font-lock-doc-face)
         '(2 font-lock-builtin-face))
   (cons (concat "\\(?:\\`\\|[ |[]\\)\\(" (regexp-opt phan-php-types) "\\)\\(?:[ |,[]\\|$\\)")
         '(1 font-lock-type-face))
   (cons "\\[]" '(0 font-lock-type-face))
   (cons (concat " " (regexp-opt phan-issues) " ")
         '(0 font-lock-keyword-face))
   (cons (concat "\\(?:\\`\\| \\)\\(" (regexp-opt phan-log-warning-keywords) "\\)\\(?: \\|$\\)")
         '(1 font-lock-warning-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group (? "\\") (+ (or "|" (syntax word) (syntax symbol))) "()")))
         '(1 font-lock-function-name-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group "\\" (+ (or "|" (syntax word) (syntax symbol))))))
         '(1 font-lock-type-face))
   (cons " constant \\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons "\\(?:::\\|->\\)\\(\\(?:\\sw\\|\\s_\\)+()\\)"
         '(1 font-lock-function-name-face))
   (cons "::\\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons " Argument [0-9]+ (\\(\\(?:\\sw\\|\\s_\\)+\\))"
         '(1 font-lock-variable-name-face))
   (cons " Call to method \\([^\n\\][^\n ]*\\) "
         '(1 font-lock-function-name-face))
   (cons "\\(?:\\$\\|->\\)\\(\\sw\\|\\s_\\)+"
         '(0 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode phan-log-mode prog-mode "Phan-Log"
  "Major mode for viewing phan formatted log."
  (setq font-lock-defaults '(phan-log-font-lock-keywords))
  (view-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("/phan.*\\.log\\'" . phan-log-mode))

;;;###autoload
(defun phan-find-config-file ()
  "Open Phan config file of the project."
  (interactive)
  (find-file (f-join (composer--find-composer-root default-directory) ".phan/config.php")))

(provide 'phan)
;;; phan.el ends here
