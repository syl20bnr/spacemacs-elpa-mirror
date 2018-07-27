;;; gherkin-mode.el --- An emacs major mode for editing gherkin files.

;; Copyright (C) 2017  Craig Andera

;; Author: Craig Andera
;; Keywords: languages
;; Package-Version: 20171224.1353
;; Version: 0.0.1

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; A mode for editing gherkin files.
;;
;; For more about gherkin, see https://github.com/alandipert/gherkin.

;;; Code:

;; TODO: Oh so many things

(defconst gherkin-keywords-re
  (regexp-opt
   '("&" "nil" "t"
     "quote" "fn" "if" "set!" "def" "do" "recur"
     "eq?" "nil?" "car" "cdr" "cons" "list" "eval"
     "apply" "read" "+" "-" "/" "mod" "<" ">"
     "cons?" "symbol?" "number?" "string?" "fn?"
     "gensym" "random" "exit" "println" "sh" "sh!"
     "load-file" "gc" "error" "type" "str")
   'words))

(defconst gherkin-keywords
  `(("<.*>" . font-lock-constant-face)
    ("#.*$" . font-lock-comment-face)
    ,gherkin-keywords-re
    ("\\?\\w+" . font-lock-variable-name-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)))

(define-derived-mode gherkin-mode lisp-mode
  "GK"
  :group 'gherkin-mode
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  ;; Font-lock support
  (setq font-lock-defaults '(gherkin-keywords))
  ;; Key maps
  ;;(define-key gherkin-mode-map (kbd "C-c C-x") 'whatever)
  )

(provide 'gherkin-mode)
;;; gherkin-mode.el ends here
