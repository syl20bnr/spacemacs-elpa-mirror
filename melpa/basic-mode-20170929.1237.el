;;; basic-mode.el --- major mode for editing BASIC code

;; Copyright (C) 2017 Johan Dykstrom

;; Author: Johan Dykstrom
;; Created: Sep 2017
;; Version: 0.1.0
;; Package-Version: 20170929.1237
;; Keywords: basic, languages
;; URL: https://github.com/dykstrom/basic-mode
;; Package-Requires: ((emacs "24.3"))

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

;;; Commentary:

;; This package provides a major mode for editing BASIC code. This
;; includes syntax highlighting and indentation.

;; Installation:

;; Install basic-mode from MELPA at https://melpa.org

;; Configuration:

;; You can customize the indentation of code blocks, see variable
;; `basic-indent-offset'. The default value is 4. You can also
;; customize the number of columns to use for line numbers, see
;; variable `basic-line-number-cols'. The default value is 0, which
;; means not using line numbers at all.

;;; Change Log:

;;  0.1.0  2017-09-28  Initial version.

;;; Code:

;; ----------------------------------------------------------------------------
;; Customization:
;; ----------------------------------------------------------------------------

(defgroup basic nil
  "Major mode for editing BASIC code."
  :link '(emacs-library-link :tag "Source File" "basic-mode.el")
  :group 'languages)

(defcustom basic-mode-hook nil
  "*Hook run when entering BASIC mode."
  :type 'hook
  :group 'basic)

(defcustom basic-indent-offset 4
  "*Specifies the indentation level for `basic-indent-line'.
Statements inside a block are indented this number of columns."
  :type 'integer
  :group 'basic)

(defcustom basic-line-number-cols 0
  "*Specifies the number of columns to allocate to line numbers.
This number should include the single space between the line number and
the actual code. Set this variable to 0 if you do not use line numbers."
  :type 'integer
  :group 'basic)

(defcustom basic-trace-flag nil
  "*Non-nil means that tracing is ON. A nil value means that tracing is OFF."
  :type 'boolean
  :group 'basic)

;; ----------------------------------------------------------------------------
;; Variables:
;; ----------------------------------------------------------------------------

(defconst basic-mode-version "0.1.0"
  "The current version of `basic-mode'.")

(defconst basic-increase-indent-keywords (regexp-opt '("else" "ELSE"
                                                       "then" "THEN"))
  "Regexp string of keywords that increase indentation.")

(defconst basic-decrease-indent-keywords (regexp-opt '("else" "ELSE"
                                                       "elseif" "ELSEIF"
                                                       "endif" "ENDIF"
                                                       "end" "END"
                                                       "wend" "WEND"))
  "Regexp string of keywords that decrease indentation.")

(defconst basic-constant-regexp
  (concat "\\_<" (regexp-opt '("false" "FALSE" "true" "TRUE") t) "\\_>")
  "Regexp string of symbols to highlight as constants.")

(defconst basic-builtin-regexp
  (concat "\\_<" (regexp-opt '("abs") t) "\\_>")
  "Regexp string of symbols to highlight as builtins.")

;; ----------------------------------------------------------------------------
;; Mode specific functions:
;; ----------------------------------------------------------------------------

(defun basic-comment-or-string-p ()
  "Return non-nil if point is in a comment or string."
  (let ((text-properties (format "%s" (text-properties-at (point)))))
    (or (string-match "font-lock-comment" text-properties)
        (string-match "font-lock-string" text-properties))))

(defun basic-message (string &rest args)
  "Display a message at the bottom of the screen if tracing is ON.
The message also goes into the `*Messages*' buffer. STRING is a format
control string, and ARGS is data to be formatted under control of the
string. See `format' for details. See `basic-trace-flag' on how to
turn tracing ON and OFF."
  (when basic-trace-flag
    (save-excursion
      (save-match-data

        ;; Get name of calling function
        (let* ((frame-number 0)
               (function-list (backtrace-frame frame-number))
               (function-name nil))
          (while function-list
            (if (symbolp (cadr function-list))
                (setq function-name (symbol-name (cadr function-list)))
              (setq function-name "<not a symbol>"))
            (if (and (string-match "^basic-" function-name)
                     (not (string-match "^basic-message$" function-name)))
                (setq function-list nil)
              (setq frame-number (1+ frame-number))
              (setq function-list (backtrace-frame frame-number))))

          ;; Update argument list
          (setq args (append (list (concat "%s:\t" string) function-name) args)))

        ;; Print message
        (apply 'message args)))))

;; ----------------------------------------------------------------------------
;; Indentation:
;; ----------------------------------------------------------------------------

(defun basic-indent-line ()
  "Indent the current line of code, see function `basic-calculate-indent'."
  (interactive)
  ;; If line needs indentation
  (when (or (not (basic-line-number-indented-correctly-p))
            (not (basic-code-indented-correctly-p)))
    (let* ((original-col (- (current-column) basic-line-number-cols))
           (original-indent-col (basic-current-indent))
           (calculated-indent-col (basic-calculate-indent)))
      (basic-indent-line-to calculated-indent-col)
      ;; Move point to a good place after indentation
      (goto-char (+ (point-at-bol)
                    calculated-indent-col
                    (max (- original-col original-indent-col) 0)
                    basic-line-number-cols)))))

(defun basic-calculate-indent ()
  "Calculate the indent for the current line of code.
The current line is indented like the previous line, with some exceptions:

Code inside a block is indented `basic-indent-offset' extra characters.
Keywords that start a block are defined in `basic-increase-indent-keywords'.
Keywords that end a block are defined in `basic-decrease-indent-keywords'."
  (let ((previous-indent-col (basic-previous-indent))
        (increase-indent-p (basic-increase-indent-p))
        (decrease-indent-p (basic-decrease-indent-p)))
    (max 0 (+ previous-indent-col
              (if increase-indent-p basic-indent-offset 0)
              (if decrease-indent-p (- basic-indent-offset) 0)))))

(defun basic-code-search-backward ()
  "Search backward from point for a line containing code."
  (beginning-of-line)
  (re-search-backward "[^ \t\n]" nil t)
  (while (and (not (bobp)) (basic-comment-or-string-p))
    (re-search-backward "[^ \t\n]" nil t)))

(defun basic-match-symbol-at-point-p (regexp)
  "Return non-nil if the symbol at point does match REGEXP."
  (let ((symbol (symbol-at-point)))
    (when symbol
      (string-match regexp (symbol-name symbol)))))

(defun basic-increase-indent-p ()
  "Return non-nil if indentation should be increased."
  (save-excursion
    (basic-code-search-backward)
    (when (not (bobp))
      (if (basic-match-symbol-at-point-p basic-increase-indent-keywords)
          't
        ;; WHILE needs special handling since there is no DO keyword at EOL
        (beginning-of-line)
        (re-search-forward "[^0-9 \t\n]" (point-at-eol) t)
        (basic-match-symbol-at-point-p (regexp-opt '("while" "WHILE")))))))

(defun basic-decrease-indent-p ()
  "Return non-nil if indentation should be decreased."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^0-9 \t\n]" (point-at-eol) t)
    (basic-match-symbol-at-point-p basic-decrease-indent-keywords)))

(defun basic-current-indent ()
  "Return the indent column of the current code line.
The columns allocated to the line number are ignored."
  (save-excursion
    (beginning-of-line)
    ;; Skip line number and spaces
    (skip-chars-forward "0-9 \t" (point-at-eol))
    (let ((indent (- (point) (point-at-bol))))
      (- indent basic-line-number-cols))))

(defun basic-previous-indent ()
  "Return the indent column of the previous code line.
The columns allocated to the line number are ignored.
If the current line is the first line, then return 0."
  (save-excursion
    (basic-code-search-backward)
    (cond ((bobp) 0)
          (t (basic-current-indent)))))

(defun basic-line-number-indented-correctly-p ()
  "Return non-nil if line number is indented correctly.
If there is no line number, also return non-nil."
  (save-excursion
    (if (not (basic-has-line-number-p))
        t
      (beginning-of-line)
      (skip-chars-forward " \t" (point-at-eol))
      (skip-chars-forward "0-9" (point-at-eol))
      (and (looking-at "[ \t]")
           (= (point) (+ (point-at-bol) basic-line-number-cols -1))))))

(defun basic-code-indented-correctly-p ()
  "Return non-nil if code is indented correctly."
  (save-excursion
    (let ((original-indent-col (basic-current-indent))
          (calculated-indent-col (basic-calculate-indent)))
      (= original-indent-col calculated-indent-col))))

(defun basic-has-line-number-p ()
  "Return non-nil if the current line has a line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t" (point-at-eol))
    (looking-at "[0-9]")))

(defun basic-remove-line-number ()
  "Remove and return the line number of the current line.
After calling this function, the current line will begin with the first
non-blank character after the line number."
  (if (not (basic-has-line-number-p))
      ""
    (beginning-of-line)
    (re-search-forward "\\([0-9]+\\)" (point-at-eol) t)
    (let ((line-number (match-string-no-properties 1)))
      (delete-region (point-at-bol) (match-end 1))
      line-number)))

(defun basic-format-line-number (number)
  "Format NUMBER as a line number."
  (if (= basic-line-number-cols 0)
      number
    (format (concat "%" (number-to-string (- basic-line-number-cols 1)) "s ") number)))

(defun basic-indent-line-to (column)
  "Indent current line to COLUMN, also considering line numbers."
  ;; Remove line number
  (let* ((line-number (basic-remove-line-number))
         (formatted-number (basic-format-line-number line-number)))
    ;; Indent line
    (indent-line-to column)
    ;; Add line number again
    (beginning-of-line)
    (insert formatted-number)))

;; ----------------------------------------------------------------------------
;; BASIC mode:
;; ----------------------------------------------------------------------------

;; Create basic-mode syntax table, and set syntax entries that are specific for
;; BASIC identifiers. Setting this syntax table overwrites the syntax table
;; created by generic mode.
(defvar basic-mode-syntax-table nil
  "Syntax table used while in ‘basic-mode'.")
(unless basic-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_   "w   " table)
    (modify-syntax-entry ?\.  "w   " table)
    (modify-syntax-entry ?'   "<   " table)
    (modify-syntax-entry ?\n  ">   " table)
    (modify-syntax-entry ?\^m ">   " table)
    (setq basic-mode-syntax-table table)))

(defun basic-init-basic-mode ()
  "Initialize ‘basic-mode'."
  (set-syntax-table basic-mode-syntax-table)
  (setq-local syntax-propertize-function (syntax-propertize-rules ("\\(rem\\)" (1 "<"))))
  (setq-local indent-line-function 'basic-indent-line))

(define-generic-mode 'basic-mode
  nil
  ;; Keywords
  (list "and" "AND"
        "else" "ELSE"
        "elseif" "ELSEIF"
        "end" "END"
        "endif" "ENDIF"
        "goto" "GOTO"
        "if" "IF"
        "let" "LET"
        "mod" "MOD"
        "or" "OR"
        "print" "PRINT"
        "then" "THEN"
        "wend" "WEND"
        "while" "WHILE")
  (list
   ;; Line numbers
   (list "^[ \t]*\\([0-9]+\\)" 1 'font-lock-constant-face)
   ;; Constants
   (list basic-constant-regexp 1 'font-lock-constant-face)
   ;; Built-in functions
   (list basic-builtin-regexp 1 'font-lock-builtin-face)
   )
  (list "\\.bas$")
  (list 'basic-init-basic-mode)
  "Major mode for editing BASIC code.

Commands:

key             binding
---             -------

C-c             Prefix Command
TAB             basic-indent-line
")

;; ----------------------------------------------------------------------------

(provide 'basic-mode)

;;; basic-mode.el ends here
