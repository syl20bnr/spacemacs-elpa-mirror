;;; flycheck-ghcmod.el --- A flycheck checker for Haskell using ghcmod  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shen Chao

;; Author: Shen Chao <scturtle@gmail.com>
;; URL: https://github.com/scturtle/flycheck-ghcmod
;; Package-Version: 20150113.2232
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.21-cvs1") (dash "2.0"))
;; Version: DEV

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>

;;; Commentary:

;; A flycheck checker for Haskell using ghcmod.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-ghcmod))

;;; Code:

(require 'dash)
(require 'flycheck)

(defun flycheck-substitute-ghcmod (errors)
  "Substitute \\u0000 to \\n in ghc-mod's output."
  (dolist (err errors)
    (-when-let (message (flycheck-error-message err))
      (setf (flycheck-error-message err)
            (replace-regexp-in-string "\u0000" "\n" message 'fixed-case 'literal))))
  errors)

(flycheck-define-checker haskell-ghcmod-lint
  "Haskell checker using ghc-mod lint."
  :command ("ghc-mod" "lint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            " Warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ":"
          " Error: " (message) line-end))
  :error-filter
  (lambda (errors)
    (-> errors
        flycheck-substitute-ghcmod
        flycheck-dedent-error-messages
        flycheck-sanitize-errors))
  :modes haskell-mode)

(flycheck-define-checker haskell-ghcmod
  "Haskell checker using ghc-mod."
  :command ("ghc-mod" "check" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            "Warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (message) line-end))
  :error-filter
  (lambda (errors)
    (-> errors
        flycheck-substitute-ghcmod
        flycheck-dedent-error-messages
        flycheck-sanitize-errors))
  :modes haskell-mode
  :next-checkers ((t . haskell-ghcmod-lint)))

(add-to-list 'flycheck-checkers 'haskell-ghcmod-lint 'append)
(add-to-list 'flycheck-checkers 'haskell-ghcmod)

(provide 'flycheck-ghcmod)
;;; flycheck-ghcmod.el ends here
