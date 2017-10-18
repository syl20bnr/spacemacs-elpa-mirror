;;; phan.el --- Utility functions for Phan (PHP static analizer)  -*- lexical-binding: t -*-

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@pixiv.com>
;; Created: 28 Jan 2017
;; Version: 0.0.3
;; Package-Version: 20171017.2045
;; Keywords: tools php
;; Package-Requires: ((emacs "24") (composer "0.0.8") (f "0.17"))
;; URL: https://github.com/emacs-php/phan.el

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
;;
;; # Major modes
;;
;; ## phan-log-mode
;;
;; A major mode for viewing Phan log format.
;;
;; # Commands
;;
;; ## phan-find-config-file
;;
;; Open `.phan/config.php' of current directory.
;;


;;; Code:
(require 'composer)
(require 'f)

(defgroup phan nil
  "Utilities for Phan (PHP static analizer)"
  :prefix "phan-"
  :link '(url-link :tag "Phan Wiki" "https://github.com/phan/phan/wiki")
  :link '(url-link :tag "phan.el site" "https://github.com/emacs-php/phan.el")
  :group 'tools
  :group 'php)

(defconst phan-php-types
  '("array" "bool" "float" "int" "null" "resource" "string" "void"))

(defconst phan-issues
  '("PhanSyntaxError"
    ;; Issue::CATEGORY_UNDEFINED
    "PhanAmbiguousTraitAliasSource"
    "PhanClassContainsAbstractMethodInternal"
    "PhanClassContainsAbstractMethod"
    "PhanEmptyFile"
    "PhanParentlessClass"
    "PhanRequiredTraitNotAdded"
    "PhanTraitParentReference"
    "PhanUndeclaredAliasedMethodOfTrait"
    "PhanUndeclaredClass"
    "PhanUndeclaredClassAliasOriginal"
    "PhanUndeclaredClassCatch"
    "PhanUndeclaredClassConstant"
    "PhanUndeclaredClassInstanceof"
    "PhanUndeclaredClassMethod"
    "PhanUndeclaredClassReference"
    "PhanUndeclaredClosureScope"
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
    "PhanUndeclaredTypeReturnType"
    "PhanUndeclaredTypeProperty"
    "PhanUndeclaredVariable"
    "PhanUndeclaredVariableDim"
    "PhanUndeclaredClassInCallable"
    "PhanUndeclaredStaticMethodInCallable"
    "PhanUndeclaredFunctionInCallable"
    "PhanUndeclaredMethodInCallable"

    ;; Issue::CATEGORY_TYPE
    "PhanNonClassMethodCall"
    "PhanTypeArrayOperator"
    "PhanTypeArraySuspicious"
    "PhanTypeSuspiciousIndirectVariable"
    "PhanTypeComparisonFromArray"
    "PhanTypeComparisonToArray"
    "PhanTypeConversionFromArray"
    "PhanTypeInstantiateAbstract"
    "PhanTypeInstantiateInterface"
    "PhanTypeInvalidClosureScope"
    "PhanTypeInvalidLeftOperand"
    "PhanTypeInvalidRightOperand"
    "PhanTypeMagicVoidWithReturn"
    "PhanTypeMismatchArgument"
    "PhanTypeMismatchArgumentInternal"
    "PhanTypeMismatchDefault"
    "PhanMismatchVariadicComment"
    "PhanMismatchVariadicParam"
    "PhanTypeMismatchForeach"
    "PhanTypeMismatchProperty"
    "PhanTypeMismatchReturn"
    "PhanTypeMismatchDeclaredReturn"
    "PhanTypeMismatchDeclaredParam"
    "PhanTypeMissingReturn"
    "PhanTypeNonVarPassByRef"
    "PhanTypeParentConstructorCalled"
    "PhanTypeVoidAssignment"
    "PhanTypeInvalidCallableArraySize"
    "PhanTypeInvalidCallableArrayKey"
    "PhanTypeInvalidCallableObjectOfMethod"

    ;; Issue::CATEGORY_ANALYSIS
    "PhanUnanalyzable"
    "PhanUnanalyzableInheritance"

    ;; Issue::CATEGORY_VARIABLE
    "PhanVariableUseClause"

    ;; Issue::CATEGORY_STATIC
    "PhanStaticCallToNonStatic"

    ;; Issue::CATEGORY_CONTEXT
    "PhanContextNotObject"

    ;; Issue::CATEGORY_DEPRECATED
    "PhanDeprecatedClass"
    "PhanDeprecatedInterface"
    "PhanDeprecatedTrait"
    "PhanDeprecatedFunction"
    "PhanDeprecatedFunctionInternal"
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

    "PhanParamSignatureRealMismatchReturnType"
    "PhanParamSignatureRealMismatchReturnTypeInternal"
    "PhanParamSignaturePHPDocMismatchReturnType"
    "PhanParamSignatureRealMismatchTooManyRequiredParameters"
    "PhanParamSignatureRealMismatchTooManyRequiredParametersInternal"
    "PhanParamSignaturePHPDocMismatchTooManyRequiredParameters"
    "PhanParamSignatureRealMismatchTooFewParameters"
    "PhanParamSignatureRealMismatchTooFewParametersInternal"
    "PhanParamSignaturePHPDocMismatchTooFewParameters"
    "PhanParamSignatureRealMismatchHasParamType"
    "PhanParamSignatureRealMismatchHasParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchHasParamType"
    "PhanParamSignatureRealMismatchHasNoParamType"
    "PhanParamSignatureRealMismatchHasNoParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchHasNoParamType"
    "PhanParamSignatureRealMismatchParamIsReference"
    "PhanParamSignatureRealMismatchParamIsReferenceInternal"
    "PhanParamSignaturePHPDocMismatchParamIsReference"
    "PhanParamSignatureRealMismatchParamIsNotReference"
    "PhanParamSignatureRealMismatchParamIsNotReferenceInternal"
    "PhanParamSignaturePHPDocMismatchParamIsNotReference"
    "PhanParamSignatureRealMismatchParamVariadic"
    "PhanParamSignatureRealMismatchParamVariadicInternal"
    "PhanParamSignaturePHPDocMismatchParamVariadic"
    "PhanParamSignatureRealMismatchParamNotVariadic"
    "PhanParamSignatureRealMismatchParamNotVariadicInternal"
    "PhanParamSignaturePHPDocMismatchParamNotVariadic"
    "PhanParamSignatureRealMismatchParamType"
    "PhanParamSignatureRealMismatchParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchParamType"

    ;; Issue::CATEGORY_NOOP
    "PhanNoopArray"
    "PhanNoopClosure"
    "PhanNoopConstant"
    "PhanNoopProperty"
    "PhanNoopVariable"
    "PhanUnreferencedClass"
    "PhanUnreferencedFunction"
    "PhanUnreferencedMethod"
    "PhanUnreferencedProperty"
    "PhanUnreferencedConstant"

    ;; Issue::CATEGORY_REDEFINE
    "PhanRedefineClass"
    "PhanRedefineClassAlias"
    "PhanRedefineClassInternal"
    "PhanRedefineFunction"
    "PhanRedefineFunctionInternal"
    "PhanIncompatibleCompositionProp"
    "PhanIncompatibleCompositionMethod"

    ;; Issue::CATEGORY_ACCESS
    "PhanAccessPropertyPrivate"
    "PhanAccessPropertyProtected"
    "PhanAccessMethodPrivate"
    "PhanAccessMethodPrivateWithCallMagicMethod"
    "PhanAccessMethodProtected"
    "PhanAccessMethodProtectedWithCallMagicMethod"
    "PhanAccessSignatureMismatch"
    "PhanAccessSignatureMismatchInternal"
    "PhanAccessStaticToNonStatic"
    "PhanAccessNonStaticToStatic"
    "PhanAccessClassConstantPrivate"
    "PhanAccessClassConstantProtected"
    "PhanAccessPropertyStaticAsNonStatic"
    "PhanAccessOwnConstructor"

    "PhanAccessConstantInternal"
    "PhanAccessClassInternal"
    "PhanAccessClassConstantInternal"
    "PhanAccessPropertyInternal"
    "PhanAccessMethodInternal"
    "PhanAccessWrongInheritanceCategory"
    "PhanAccessWrongInheritanceCategoryInternal"
    "PhanAccessExtendsFinalClass"
    "PhanAccessExtendsFinalClassInternal"
    "PhanAccessOverridesFinalMethod"
    "PhanAccessOverridesFinalMethodInternal"
    "PhanAccessOverridesFinalMethodPHPDoc"

    ;; Issue::CATEGORY_COMPATIBLE
    "PhanCompatibleExpressionPHP7"
    "PhanCompatiblePHP7"

    ;; Issue::CATEGORY_GENERIC
    "PhanTemplateTypeConstant"
    "PhanTemplateTypeStaticMethod"
    "PhanTemplateTypeStaticProperty"
    "PhanGenericGlobalVariable"
    "PhanGenericConstructorTypes"

    ;; Issue::CATEGORY_COMMENT
    "PhanInvalidCommentForDeclarationType"
    "PhanMisspelledAnnotation"
    "PhanUnextractableAnnotation"
    "PhanUnextractableAnnotationPart"
    "PhanCommentParamWithoutRealParam"
    "PhanCommentParamOnEmptyParamList"
    "PhanCommentOverrideOnNonOverrideMethod"
    "PhanCommentOverrideOnNonOverrideConstant"
    )
  "Issue names of Phan.

https://github.com/etsy/phan/blob/master/src/Phan/Issue.php
https://github.com/etsy/phan/wiki/Issue-Types-Caught-by-Phan")

(defconst phan-log-warning-keywords
  '("can't be"
    "deprecated"
    "has no return value"
    "not found"
    "only takes"
    "should be compatible"
    "Suspicious"
    "undeclared"
    "unextractable annotation"))

(defconst phan-log-class-prefix-keywords
  '(":" "but" "class" "for" "function" "is" "method" "property" "return" "takes" "to" "type"
    "Class" "Property"))

(defconst phan-log-function-prefix-keywords
  '("Function" "Method"))

(defconst phan-log-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?$  "_" table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?\( "_" table)
    (modify-syntax-entry ?\) "_" table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?.  "_" table)
    (modify-syntax-entry ?:  "." table)
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
   (cons (concat "\\(?:\\`\\| \\)\\(" (regexp-opt phan-log-warning-keywords) "\\)[ $,]")
         '(1 font-lock-warning-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group (? "\\") (+ (or "|" (syntax word) (syntax symbol))) "()")))
         '(1 font-lock-function-name-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group "\\" (+ (or "?" "|" "[]" (syntax word) (syntax symbol))))))
         '(1 font-lock-type-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-function-prefix-keywords) " \\)"
                 (rx (group "\\" (+ (or (syntax word) (syntax symbol))))))
         '(1 font-lock-function-name-face))
   (cons " constant \\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons "\\(?:::\\|->\\)\\(\\(?:\\sw\\|\\s_\\)+()\\)"
         '(1 font-lock-function-name-face))
   (cons "::\\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons " \\(?:Argument [0-9]+\\|annotation for\\) (\\(\\(?:\\sw\\|\\s_\\)+\\))"
         '(1 font-lock-variable-name-face))
   (cons " Argument [0-9]+ (\\(\\(?:\\sw\\|\\s_\\)+\\))"
         '(1 font-lock-variable-name-face))
   (cons " Call to method \\([^\n\\][^\n ]*\\) "
         '(1 font-lock-function-name-face))
   (cons "\\(?:\\$\\|->\\)\\(\\sw\\|\\s_\\)+"
         '(0 font-lock-variable-name-face))))

;; Utility functions
(defun phan--base-dir (directory)
  "Return path to current project root in `DIRECTORY'."
  (or (locate-dominating-file directory ".phan/config.php")
      (composer--find-composer-root directory)))

;; Major modes

;;;###autoload
(define-derived-mode phan-log-mode prog-mode "Phan-Log"
  "Major mode for viewing phan formatted log."
  (setq font-lock-defaults '(phan-log-font-lock-keywords))
  (view-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("/phan.*\\.log\\'" . phan-log-mode))

;; Commands

;;;###autoload
(defun phan-find-config-file ()
  "Open Phan config file of the project."
  (interactive)
  (if (null default-directory)
      (error "A variable `default-directory' is not set")
    (find-file (f-join (phan--base-dir default-directory) ".phan/config.php"))))

(provide 'phan)
;;; phan.el ends here
