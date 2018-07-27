;;; vyper-mode.el --- Major mode for the Vyper programming language -*- lexical-binding: t -*-

;; Author: Alex Stokes <r.alex.stokes@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20180707.1935
;; URL: https://github.com/ralexstokes/vyper-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This library provides support for writing Vyper source code.
;; See README.md for further details.

;;; Code:

(defconst vyper-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Vyper files.")

(defvar vyper-font-lock-keywords
  `(;; keywords
    ,(rx symbol-start
         (or
          "return"
          "assert"
          "@version"
          "contract"
          "for"
          "while"
          "if"
          "else"
          "and"
          "in"
          "not"
          "or"
          "def")
         symbol-end)
    ;; functions
    (,(rx symbol-start
          "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; constants
    (,(rx symbol-start
          (or
           "True"
           "False")
          symbol-end) . font-lock-constant-face)
    ;; decorators
    (,(rx line-start
          (* (any " \t"))
          (group "@" (or
                      "payable"
                      "constant"
                      "internal"
                      "public")))
     (1 font-lock-type-face))
    ;; types
    (,(rx symbol-start
          (or
           "address"
           "bool"
           "decimal"
           "num"
           "bytes32"
           "int128"
           "uint256"
           "bytes"
           "wei_value"
           "timestamp"
           "timedelta")
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (,(rx symbol-start
          (or "self"
              "as_unitless_number"
              "as_wei_value"
              "bitwise_and"
              "bitwise_not"
              "bitwise_or"
              "bitwise_xor"
              "blockhash"
              "ceil"
              "concat"
              "convert"
              "create_with_code_of"
              "ecadd"
              "ecmul"
              "ecrecover"
              "extract32"
              "floor"
              "keccak256"
              "len"
              "max"
              "method_id"
              "min"
              "raw_call"
              "RLPList"
              "sha3"
              "shift"
              "slice"
              "uint256_addmod"
              "uint256_mulmod")
          symbol-end) . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode vyper-mode prog-mode "Vyper mode"
  "Major mode for Vyper"
  :syntax-table vyper-mode-syntax-table
  (setq-local font-lock-defaults
              '(vyper-font-lock-keywords))
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-* "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vy\\'" . vyper-mode))

(provide 'vyper-mode)
;;; vyper-mode.el ends here
