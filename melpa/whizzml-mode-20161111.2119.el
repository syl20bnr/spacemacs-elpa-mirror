;;; whizzml-mode.el --- Programming mode for editing WhizzML files

;; Copyright (c) 2016 BigML, Inc

;; Author: Jose Antonio Ortega Ruiz <jao@bigml.com>
;; Package-Requires: ((emacs "24.4"))
;; Package-Version: 20161111.2119
;; Version: 0.1
;; Keywords: languages, lisp


;; This file is not part of GNU Emacs.

;; whizzml-mode.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; whizzml-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


;;; Comentary:

;; This package provides a major mode for editing WhizzML source code.

;;; Code:


(require 'subr-x)
(require 'lisp-mode)

(defvar whizzml-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)

    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    ;; (modify-syntax-entry ?' "'   " st)
    ;; (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    ;; (modify-syntax-entry ?, "'   " st)
    ;; (modify-syntax-entry ?@ "'   " st)
    ;; (modify-syntax-entry ?# "' 14" st)
    ;; (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar whizzml-mode-abbrev-table nil)
(define-abbrev-table 'whizzml-mode-abbrev-table ())

(defvar whizzml-builtins
  '("!=" "*" "+" "-" "/" "<" "<=" "=" ">" ">="
    "abort" "abs" "acos" "append" "asin" "assoc" "assoc-in" "atan"
    "bigml--cdf" "bigml--pdf" "boolean?" "butlast"
    "ceil" "concat" "cons" "contains-string?" "contains?" "cos" "cosh" "count"
    "create" "create-rng" "created-resources"
    "delete" "dissoc" "dissoc-in" "div" "drop"
    "empty?" "even?" "exp" "fetch"
    "flatline-listify" "flatline-splice" "flatline-str" "flatline-str-splice"
    "floor" "get" "get-in" "head" "insert" "integer?" "join" "keys"
    "last" "list" "list*" "list?"
    "ln" "log" "log-error" "log-info" "log-warn" "log10" "log2"
    "make-map" "map?" "matches" "matches?" "max" "mean" "merge" "min"
    "negative?" "nil?" "not" "nth" "number?" "odd?" "parse-resource-id"
    "positive?" "pow" "ppr-str" "pr-str" "pretty-whizzml" "procedure?" "rand"
    "rand-int" "range" "re-quote" "real?" "regexp?" "rem" "repeat" "replace"
    "replace-first" "resource-done?" "resource-id?" "resources" "reverse"
    "round" "row-distance" "row-distance-squared" "set-rng-seed" "sin" "sinh"
    "sort" "sort-by-key" "sqrt" "stdev" "str" "string?" "subs"
    "tail" "take" "tan" "tanh" "to-degrees" "to-radians"
    "update" "values" "variance"
    "version" "version-major" "version-micro" "version-minor" "wait" "zero?"))

(defvar whizzml-std-procedures
  '("create-and-wait-anomaly" "create-and-wait-anomalyscore"
    "create-and-wait-association" "create-and-wait-associationset"
    "create-and-wait-batchanomalyscore" "create-and-wait-batchcentroid"
    "create-and-wait-batchprediction" "create-and-wait-batchtopicdistribution"
    "create-and-wait-centroid" "create-and-wait-cluster"
    "create-and-wait-configuration" "create-and-wait-correlation"
    "create-and-wait-dataset" "create-and-wait-ensemble"
    "create-and-wait-evaluation" "create-and-wait-execution"
    "create-and-wait-library" "create-and-wait-logisticregression"
    "create-and-wait-model" "create-and-wait-prediction"
    "create-and-wait-project" "create-and-wait-sample"
    "create-and-wait-script" "create-and-wait-source"
    "create-and-wait-statisticaltest" "create-and-wait-topicdistribution"
    "create-and-wait-topicmodel" "create-anomaly" "create-anomalyscore"
    "create-association" "create-associationset" "create-batchanomalyscore"
    "create-batchcentroid" "create-batchprediction"
    "create-batchtopicdistribution" "create-centroid" "create-cluster"
    "create-configuration" "create-correlation" "create-dataset"
    "create-ensemble" "create-evaluation" "create-execution"
    "create-library" "create-logisticregression" "create-model"
    "create-prediction" "create-project" "create-sample" "create-script"
    "create-source" "create-statisticaltest" "create-topicdistribution"
    "create-topicmodel" "list-anomalies" "list-anomalyscores"
    "list-associations" "list-associationsets" "list-batchanomalyscores"
    "list-batchcentroids" "list-batchpredictions"
    "list-batchtopicdistributions" "list-centroids" "list-clusters"
    "list-configurations" "list-correlations" "list-datasets" "list-ensembles"
    "list-evaluations" "list-executions" "list-libraries"
    "list-logisticregressions" "list-models" "list-predictions"
    "list-projects" "list-samples" "list-scripts" "list-sources"
    "list-statisticaltests" "list-topicdistributions" "list-topicmodels"
    "create" "wait" "fetch" "delete" "update" "wait*" "delete*"
    "create-and-wait" "update-and-wait"))

(setq whizzml-font-lock-keywords
      `(("(\\(define\\)\\>[ \t]*(\\(\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face nil t))
        ("(\\(define\\)\\>[ \t]*\\(\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-variable-name-face nil t))
        ,(concat "(" (regexp-opt '("prog" "loop" "recur" "reduce" "filter"
                                   "iterate" "break" "cond" "flatline"
                                   "for" "if" "lambda" "let"
                                   "and" "or" "map" "list"
                                   "when" "handle" "raise" "try" "catch") t)
                 "\\>")
        (,(concat "\\<" (regexp-opt whizzml-builtins t) "\\>")
         (1 font-lock-function-name-face))
        (,(concat "\\<" (regexp-opt whizzml-std-procedures t) "\\>")
         (1 font-lock-function-name-face))))

(defun whizzml-mode-variables ()
  (set-syntax-table whizzml-mode-syntax-table)
  (setq local-abbrev-table whizzml-mode-abbrev-table)
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'whizzml-indent-function)
  (setq mode-line-process '("" whizzml-mode-line-process))
  (setq-local syntax-propertize-function #'whizzml-syntax-propertize)
  (setq font-lock-defaults
	'(whizzml-font-lock-keywords
	  nil nil
          (("+-*/.<>=!?$%_&~^:" . "w"))
	  beginning-of-defun
	  (font-lock-mark-block-function . mark-defun))))

(defvar whizzml-mode-line-process "")

(defvar whizzml-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "WhizzML")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar whizzml] (cons "WhizzML" map))
    ;; (define-key map [run-whizzml] '("Run Inferior WhizzML" . run-whizzml))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for WhizzML mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")


;;;###autoload
(define-derived-mode whizzml-mode prog-mode "WhizzML"
  "Major mode for editing WhizzML code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{whizzml-mode-map}"
  (whizzml-mode-variables))

(defgroup whizzml nil
  "Editing WhizzML code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom whizzml-mode-hook nil
  "Normal hook run when entering `whizzml-mode'.
See `run-hooks'."
  :type 'hook
  :group 'whizzml)

(defcustom whizzml-program-name "whizzml"
  "Program invoked by the `run-whizzml' command."
  :type 'string
  :group 'whizzml)

(defun whizzml-syntax-propertize (beg end))


(defvar calculate-lisp-indent-last-sexp)

(defun whizzml-mode--letty-args (state)
  (save-excursion
    (when (and (ignore-errors (goto-char (elt state 1))
                              (backward-up-list)
                              (point))
               (< (point) (1- (elt state 1))))
      (let ((prefix (buffer-substring (point) (1- (elt state 1)))))
        (when (string-match "\\(let\\|loop\\|iterate\\)\\'"
                            (string-trim prefix))
          (message "eh: %s" prefix)
          (point))))))

(defun whizzml-indent-function (indent-point state)
  "WhizzML mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `whizzml-indent-function'
\(or the deprecated `whizzml-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond ((and (elt state 2)
                (not (looking-at "\\sw\\|\\s_")))
           ;; car of form doesn't seem to be a symbol
           (if (not (> (save-excursion (forward-line 1) (point))
                       calculate-lisp-indent-last-sexp))
               (progn
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t))
             ;; Indent under the list or under the first sexp on the same
             ;; line as calculate-lisp-indent-last-sexp.  Note that first
             ;; thing on that line has to be complete sexp since we are
             ;; inside the innermost containing sexp.
             (backward-prefix-chars)
             (current-column)))
          ((whizzml-mode--letty-args state) (current-column))
          (t
           (let* ((funname (buffer-substring (point) (progn (forward-sexp 1)
                                                            (point))))
                  (method (or (get (intern-soft funname)
                                   'whizzml-indent-function)
                              (get (intern-soft funname)
                                   'whizzml-indent-hook))))
             (cond ((eq method 'defun)
                    (lisp-indent-defform state indent-point))
                   ((integerp method)
                    (lisp-indent-specform method state
                                          indent-point normal-indent))
                   (method
                    (funcall method state indent-point normal-indent))))))))

(put 'define 'whizzml-indent-function 1)
(put 'prog 'whizzml-indent-function 0)
(put 'iterate 'whizzml-indent-function 1)
(put 'lambda 'whizzml-indent-function 1)
(put 'catch 'whizzml-indent-function 1)
(put 'for 'whizzml-indent-function 1)
(put 'let 'whizzml-indent-function 1)
(put 'loop 'whizzml-indent-function 1)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.whizzml\\'" . whizzml-mode))


(provide 'whizzml-mode)
;;; whizzml-mode.el ends here
