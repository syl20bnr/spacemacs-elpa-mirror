;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Serghei Iakovlev

;; Author: Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.0
;; Package-Version: 20170902.345
;; URL: https://github.com/sergeyklay/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;   GNU Emacs major mode for editing Zephir code.  Provides font-locking,
;; indentation, alignment and navigation support.
;;
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are
;; exported to C code that can be compiled and optimized by major C
;; compilers such as gcc/clang/vc++.  Functionality is exposed to the
;; PHP language.  For more information see https://zephir-lang.com
;;
;; Syntax checking:
;;
;;   Flymake support is not provided.  See Flycheck at
;; http://www.flycheck.org for on-the-fly validation and liniting of Zephir
;; code.
;;
;; Bugs:
;;
;;   Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/sergeyklay/zephir-mode/issues
;;
;; History:
;;
;;   History is tracked in the Git repository rather than in this file.
;; See https://github.com/sergeyklay/zephir-mode/blob/master/CHANGELOG.md
;;
;; Movement:
;;
;;   Move to the beginning or end of the current block with
;; `beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.
;;
;; Usage:
;;
;;   Put this file in your Emacs Lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'zephir-mode)
;;
;; To use abbrev-mode, add lines like this:
;;   (add-hook 'zephir-mode-hook
;;     '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))
;;
;; Many options available under Help:Customize
;; Options specific to zephir-mode are in Programming/Languages/Zephir

;;; Code:


;;; Compatibility

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while zephir-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl)))


;;; Requirements

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

(eval-when-compile
  (require 'rx))

(require 'cl-lib)
(require 'pkg-info)


;;; Customization

;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/sergeyklay/zephir-mode")
  :link '(url-link :tag "Zephir Forum" "https://forum.zephir-lang.com")
  :link '(url-link :tag "Zephir Site" "https://zephir-lang.com")
  :link '(emacs-commentary-link :tag "Commentary" "zephir-mode"))

(defvar zephir-zephir-website "https://zephir-lang.com"
  "Official website of Zephir programming language.")

(defvar zephir-github-mode "https://github.com/sergeyklay/zephir-mode"
  "Zephir Mode GitHub Page.")

(defvar zephir-github-zephir "https://github.com/phalcon/zephir"
  "Zephir GitHub Page.")

(defvar zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode.")

(defcustom zephir-indent-tabs-mode t
  "Indentation can insert tabs in Zephir Mode if this is non-nil."
  :type 'boolean
  :group 'zephir
  :safe 'booleanp)


;;; Version information

(defun zephir-mode-version (&optional show-version)
  "Display string describing the version of Zephir Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'zephir-mode)))
    (when show-version
      (message "Zephir Mode version: %s" version))
    version))


;;; Utilities

(defun zephir-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.

Return nil, if there is no special context at POS, or one of

`comment'
     POS is inside a comment

`single-quoted'
     POS is inside a single-quoted string

`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (`?\' 'single-quoted)
        (`?\" 'double-quoted)))))

(defun zephir-in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or comment."
  (not (null (zephir-syntax-context pos))))

(defun zephir-in-listlike (re-open-str)
  "If point is in a listlike, return the position of the opening char of it.
Otherwise return nil.  The RE-OPEN-STR is a regexp string
matching the opening character."
  (save-excursion
    (let ((opoint (nth 1 (syntax-ppss))))
      (when (and opoint
                 (progn
                   (goto-char opoint)
                   (looking-at-p re-open-str)))
        opoint))))


;;; Specialized rx

(eval-when-compile
  (defconst zephir-rx-constituents
    `(
      ;; Identifier.
      (identifier . ,(rx symbol-start
                         (optional ?$)
                         (any "A-Z" "a-z" ?_)
                         (zero-or-more (any "A-Z" "a-z" "0-9" ?_))
                         symbol-end))
      ;; Builtin declaraion.
      (builtin-decl . ,(rx symbol-start
                           (or "class"
                               "interface"
                               "namespace"
                               "abstract"
                               "final")
                          symbol-end))
      ;; Predefined boolean constants
      (bool-const . ,(rx symbol-start
                      (or "true" "false")
                      symbol-end))
      ;; Constants
      (constant . ,(rx symbol-start
                       (any "A-Z" ?_)
                       (+ (any "A-Z" "0-9" ?_))
                       symbol-end))
      ;; Function declaraion.
      (fn-decl . ,(rx symbol-start
                      "function"
                      symbol-end))
      ;; Namespace, class or interface name.
      (classlike . ,(rx symbol-start
                        (optional ?$)
                        (any "A-Z" "a-z" ?_)
                        (zero-or-more (any "A-Z" "a-z" "0-9" ?_))
                        (zero-or-more
                         (and "\\"
                              (any "A-Z" "a-z" ?_)
                              (+ (any "A-Z" "a-z" "0-9" ?_))))
                        symbol-end))
      ;; Visibility modifier
      (visibility . ,(rx (or "public"
                             "protected"
                             "private"
                             "internal"
                             "scoped"
                             "inline")))
      (primitives . ,(rx (and word-start
                              (or "int"   "uint"
                                  "bool"  "boolean"
                                  "float" "double"
                                  "long"  "ulong"
                                  "char"  "uchar"
                                  "string"
                                  "resource"
                                  "var"
                                  "void"
                                  "array")
                              word-end))))
    "Additional special sexps for `zephir-rx'.")

  (defmacro zephir-rx (&rest sexps)
    "Zephir-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`identifier'
     Any valid identifier with optional dollar sign, e.g. function name,
     variable name, etc.

`builtin-dcl'
     Any valid builtin declaraion.

`bool-const'
     Predefined boolean constants.

`constant'
     A valid constant name.
     By convention, constant identifiers are always uppercase.

`fn-decl'
     A function declaraion.

`classlike'
     A valid namespace, class or interface name without leading \.

`visibility'
     Any valid visibility modifier.

`primitives'
     Any valid primitive type keywords.

See `rx' documentation for more information about REGEXPS param."
    (let ((rx-constituents (append zephir-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))


;;; Navigation

(defconst zephir-beginning-of-defun-regexp
  (zephir-rx line-start
             (zero-or-more (syntax whitespace))
             (optional "deprecated" (+ (syntax whitespace)))
             (optional symbol-start
                       (or "abstract" "final")
                       symbol-end
                       (+ (syntax whitespace)))
             (optional visibility (+ (syntax whitespace))
                       (optional "static" (+ (syntax whitespace))))
             (group fn-decl)
             (+ (syntax whitespace))
             (group identifier)
             (zero-or-more (syntax whitespace))
             "(")
  "Regular expression for a Zephir function.")

(defun zephir-beginning-of-defun (&optional arg)
  "Move the beginning of the ARGth PHP function from point.

Implements Zephir version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1))
        (case-fold-search t))
    (while (> arg 0)
      (re-search-backward zephir-beginning-of-defun-regexp nil 'noerror)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (re-search-forward zephir-beginning-of-defun-regexp nil 'noerror))
        (setq arg (1+ arg))))))

(defun zephir-end-of-defun (&optional arg)
  "Move the end of the ARG'th Zephir function from point.

Implements Zephir version of `end-of-defun-function'.  For more
see `zephir-beginning-of-defun'."
  (interactive "p")
  (zephir-beginning-of-defun (- (or arg 1))))


;;; Indentation code


;;; Font Locking

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"      table)
    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\'  "\""     table)
    ;; Comment enders
    (modify-syntax-entry ?\n  "> b"    table)
    ;; Give CR the same syntax as newline
    (modify-syntax-entry ?\^m "> b"    table)
    ;; Set up block and line oriented comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    ;; The parenthesis, braces and brackets
    (modify-syntax-entry ?\(  "()"     table)
    (modify-syntax-entry ?\)  ")("     table)
    (modify-syntax-entry ?\{  "(}"     table)
    (modify-syntax-entry ?\}  "){"     table)
    (modify-syntax-entry ?\[  "(]"     table)
    (modify-syntax-entry ?\]  ")["     table)
    table)
  "Syntax table in use in `zephir-mode' buffers.

This includes setting ' and \" as string delimiters, and setting up
the comment syntax tokens handle both line style \"//\" and block style
\"/*\" \"*/\" comments.")

(defvar zephir-font-lock-keywords
  `(
    ;; Builtin declaration.
    (,(zephir-rx (and line-start
                      (group builtin-decl)))
     1 font-lock-keyword-face)
    ;; Class decclaration.
    ;; Class has its own font lock because it may have "abstract" or "final"
    (,(zephir-rx (optional symbol-start
                           (or "abstract" "final")
                           symbol-end
                           (+ (syntax whitespace)))
                 (group symbol-start "class" symbol-end)
                 (+ (syntax whitespace))
                 (group identifier))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; "namespace Foo", "interface Foo" or "use Foo"
    (,(zephir-rx line-start
                 (group symbol-start
                        (or "namespace" "interface" "use")
                        symbol-end)
                 (+ (syntax whitespace))
                 (group (optional "\\") classlike))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Highlight class name after "use ... as"
    (,(zephir-rx (optional "\\")
                 classlike
                 (+ (syntax whitespace))
                 (group symbol-start "as" symbol-end)
                 (+ (syntax whitespace))
                 (group identifier))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Highlight extends
    (,(zephir-rx classlike
                 (+ (syntax whitespace))
                 (group symbol-start "extends" symbol-end)
                 (+ (syntax whitespace))
                 (group classlike)
                 (optional (+ (syntax whitespace)))
                 (or ";" (group (or "implements" "as"))))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Booleans
    (,(zephir-rx (group bool-const))
     1 font-lock-constant-face)
    ;; null
    (,(rx (group symbol-start "null" symbol-end))
     1 font-lock-constant-face)
    ;; Highlight special variables
    (,(zephir-rx (group symbol-start "this" word-end)
          (zero-or-more "->" identifier))
     1 font-lock-constant-face)
    ;; Visibility
    (,(zephir-rx (group symbol-start visibility symbol-end))
     (1 font-lock-keyword-face))
    ;; Function names, i.e. `function foo'.
    (,zephir-beginning-of-defun-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ;; Type primitives
    (,(zephir-rx (not (any ?_))
                 (group primitives)
                 (not (any ?_)))
     1 font-lock-type-face)
    ;; Constants
    (,(zephir-rx (group constant))
     1 font-lock-constant-face))
  "Font lock keywords for Zephir Mode.")


;;; Alignment


;;; Imenu


;;; Initialization

;;;###autoload
(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code."
  :group 'zephir-mode
  ;; Comment setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  ;; Navigation
  (setq-local beginning-of-defun-function #'zephir-beginning-of-defun)
  (setq-local end-of-defun-function #'zephir-end-of-defun)
  ;; Indentation
  (setq indent-tabs-mode zephir-indent-tabs-mode)
  ;; Font locking
  (setq font-lock-defaults '((zephir-font-lock-keywords) nil nil)))

;;;###autoload
(defun zephir-open-mode-github ()
  "Go to the Zephir Mode GitHub page."
  (interactive)
  (browse-url zephir-github-mode))

;;;###autoload
(defun zephir-open-zephir-github ()
  "Go to the Zephir Documentstion Website."
  (interactive)
  (browse-url zephir-github-zephir))

;;;###autoload
(defun zephir-open-website-home ()
  "Go to the Zephir Website."
  (interactive)
  (browse-url zephir-zephir-website))


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; zephir-mode.el ends here
