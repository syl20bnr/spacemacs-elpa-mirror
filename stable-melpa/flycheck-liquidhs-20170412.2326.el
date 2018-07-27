;;; flycheck-liquidhs.el --- A flycheck checker for Haskell using liquid (i.e. liquidhaskell)

;; Modified from flycheck-hdevtools.el by Steve Purcell

;; Author: Ranjit Jhala <jhala@cs.ucsd.edu>
;; URL: https://github.com/ucsd-progsys/liquidhaskell/flycheck-liquid.el
;; Package-Version: 20170412.2326
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.15"))
;; Version: 0.0.1
;; X-Original-Version: DEV

;; This file is not part of GNU Emacs.

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

;; Adds a Flycheck syntax checker for Haskell based on liquid-haskell.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-liquidhs))

;;; Code:

(require 'flycheck)

;; (shell-command (concat exe " " paramstr))
;; (message (concat "'" exe "' not found found; please install"))))

(defgroup flycheck-liquid nil
  " LiquidHaskell type popup tooltip."
  :group 'flycheck-options
  :prefix "flycheck-liquid-")

(defcustom flycheck-liquid-use-stack t  ;use stack by default
  "Set popup style."
  :type 'boolean
  :group 'flycheck-liquid)

(defmacro make-liquid-checker (name command)
  (let ((default-reporter
          `((message
            (one-or-more " ") (one-or-more not-newline)
            (zero-or-more "\n"
                          (one-or-more " ")
                          (zero-or-more not-newline)))
          line-end)))

    `(flycheck-define-checker ,name
       "A Haskell refinement type checker using liquidhaskell.

See URL 'https://github.com/ucsd-progsys/liquidhaskell'."
       :command ,command

       :error-patterns
       ((error line-start " " (file-name) ":" line ":" column ":"
               ,@default-reporter)
        (error line-start " " (file-name) ":" line ":" column "-" (one-or-more digit) ":"
               ,@default-reporter)
        (error line-start " " (file-name) ":(" line "," column ")-(" (one-or-more digit) "," (one-or-more digit) "):"
               ,@default-reporter)
        )
       :error-filter
       (lambda (errors)
         (-> errors
             flycheck-dedent-error-messages
             flycheck-sanitize-errors))
       :modes (haskell-mode literate-haskell-mode)
       :next-checkers ((warnings-only . haskell-hlint)))
    ))

(make-liquid-checker haskell-liquid ("liquid" source-inplace))
(make-liquid-checker haskell-stack-liquid ("stack" "exec" "--" "liquid" source-inplace))

(add-to-list 'flycheck-checkers
             (if flycheck-liquid-use-stack
                 'haskell-stack-liquid
               'haskell-liquid))

(provide 'flycheck-liquidhs)
;;; flycheck-liquidhs.el ends here
