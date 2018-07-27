;;; oberon.el --- Major mode for editing Oberon/Oberon-2 program texts

;; Copyright (C) 2006 Karl Landström

;; Author: Karl Landström <karl@karllandstrom.se>
;; Maintainer: Karl Landström <karl@karllandstrom.se>
;; Version: 2.0 Beta 1
;; Package-Version: 20120715.909
;; Keywords: oberon oberon-2 languages oop

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The main features of this mode are syntactic highlighting (enabled
;; with `font-lock-mode' or `global-font-lock-mode'), automatic
;; indentation and filling of comments.

;; This package has (only) been tested with GNU Emacs 21.4 (the latest
;; stable release).

;; Installation:

;; Put this file in a directory where Emacs can find it (`C-h v
;; load-path' for more info). Then add the following lines to your
;; Emacs initialization file:
 
;;    (add-to-list 'auto-mode-alist '("\\.Mod\\'" . oberon-mode))
;;    (autoload 'oberon-mode "oberon" nil t)
;;    (add-hook 'oberon-mode-hook (lambda () (abbrev-mode t)))
                
;; You may want to change the regular expression on the first line if
;; your Oberon files do not end with `.Mod'. You can also skip the
;; last line if you do not want automatic upcase conversion of
;; predefined words.
    
;; General Remarks:
 
;; Exported names start with `oberon-' of which the names mentioned in
;; the major mode convetions start with `oberon-mode'. Private names
;; start with `obn-'.
 
;; For sake of simplicity certain (sensible) assumptions are made
;; about Oberon program texts. If the assuptions are not satisfied
;; there is no guarantee that the mode will function properly. The
;; assumptions are:

;; * No nested unbalanced control statements on the same line, e.g.
;; `IF p THEN IF q THEN s END'.
;;
;; * No one-line procedure definitions, e.g.
;;
;;      PROCEDURE P; BEGIN END P;

;;; History:

;; See `oberon.el.changelog'.

;;; Code:

(require 'font-lock)
(require 'newcomment)


;; --- Preferences ---

(defcustom oberon-indent-level 3
  "Number of spaces for each indentation step")

(defcustom oberon-auto-indent-flag t
  "If non-nil indent current line when certain words or
  characters are inserted.")

(defcustom oberon-record-type-suffix "Desc"
  "Suffix for record type names in `obn-insert-type-skeleton'.")


;; --- Keymap ---

(defvar oberon-mode-map
  (let ((main-map (make-sparse-keymap))
        (main-menu-map (make-sparse-keymap "Oberon"))
        (skeleton-menu-map (make-sparse-keymap "Templates")))

    ;; Keyboard
    (define-key main-map "(" 'obn-insert-paren-upcase-maybe)
    (when oberon-auto-indent-flag
      (mapc (lambda (key) (define-key main-map key 'obn-insert-and-indent))
            '(";" ":" "|")))
    (define-key main-map "\C-c\C-e" 'oberon-trim-enumeration)
    (define-key main-map "\C-c\C-sm" 'oberon-insert-module-skeleton)
    (define-key main-map "\C-c\C-st" 'oberon-insert-type-skeleton)
    (define-key main-map "\C-c\C-sp" 'oberon-insert-procedure-skeleton)

    ;; Oberon Menu
    (define-key main-map [menu-bar] (make-sparse-keymap))
    (define-key main-map [menu-bar oberon] (cons "Oberon" main-menu-map))
    (define-key main-menu-map [trim-enumeration] 
      '("Trim Enumeration..." . oberon-trim-enumeration))
    (define-key main-menu-map [trim-separator] '("---"))
    (define-key main-menu-map [templates] 
      (cons "Insert Skeleton" skeleton-menu-map))
    (define-key skeleton-menu-map [procedure] 
      '("Procedure..." . oberon-insert-procedure-skeleton))
    (define-key skeleton-menu-map [type] 
      '("Type..." . oberon-insert-type-skeleton))
    (define-key skeleton-menu-map [module] 
      '("Module" . oberon-insert-module-skeleton))
    main-map)
  "Keymap in Oberon mode")

(defconst obn-pre-decl-proc 
  '("ABS" "ASH" "ASSERT" "CAP" "CHR" "COPY" "DEC" "ENTIER" "EXCL" "HALT" 
    "LEN" "LONG" "MAX" "MIN" "NEW" "ODD" "ORD" "INC" "INCL" "SHORT" "SIZE")
  "The predeclared procedures in Oberon")

(defconst obn-pre-decl-proc-re 
  (regexp-opt obn-pre-decl-proc 'words)
  "regular expression matching any predeclared procedure")

(defun obn-insert-paren-upcase-maybe ()
  "Insert left parenthesis and eventually upcase predeclared procedure.
Remark: We want to be able to use common constant and variable
names such as `max' without automatic upcase conversion when a
space or a punctuation character is inserted."
  (interactive)
  (when abbrev-mode
    (let ((p (point))
          word)
      (save-excursion
        (backward-word 1)
        (when (and (save-excursion 
                     (re-search-forward ".*" p)
                     (setq word (match-string-no-properties 0))
                     (string-match obn-pre-decl-proc-re word))
                   (let ((case-fold-search nil))
                     (string= word (downcase word)))
                   (not (nth 8 (parse-partial-sexp (point-min) (point)))))
          (upcase-region (point) p)))))
  (if oberon-auto-indent-flag
      (obn-insert-and-indent "(")
    (insert "(")))


(defun obn-insert-and-indent (key)
  "Run command bound to key and indent current line.  Runs the
command bound to KEY in the global keymap and eventually converts
the identifier to the left to upcase and indents the current
line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (when abbrev-mode (expand-abbrev)) ;in case key (punctuation) is redefined
  (indent-according-to-mode))


;; --- Syntax Table and Parsing ---

(defvar oberon-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?( "()1n" table)
    (modify-syntax-entry ?) ")(4n" table)
    (modify-syntax-entry ?* ". 23n" table)

    ;; Underscores in identifiers are supported by this mode (as a
    ;; trivial extension to Oberon).
    ;; 
    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?- "." table)
    table)
  "syntax table used in Oberon mode")


(defun obn-re-search-forward-inner (regexp &optional bound count)
  "used by `obn-re-search-forward'"
  (let ((parse)
        (saved-point (point-min))
        (nesting))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond 
       ((or (nth 4 parse)          ;inside comment
            (when (and (eq (char-before) ?\() (eq (char-after) ?*))
              (forward-char) t))
        (setq nesting (if (nth 4 parse) (nth 4 parse) 1))
        (while (> nesting 0)
          (re-search-forward "\\((\\*\\|\\*)\\)")
          (setq nesting
                (if (string-match "(\\*" (match-string-no-properties 0))
                    (1+ nesting) 
                  (1- nesting)))))
       ((nth 3 parse)                   ;inside string
        (re-search-forward
         (concat "[^\\]" (string (nth 3 parse))) (point-at-eol) t))
       (t
        (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun obn-re-search-forward (regexp &optional bound noerror count)
  "Searches forward but ignores strings and comments.  Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(obn-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(obn-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(obn-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun obn-re-search-backward-inner (regexp &optional bound count)
  "used by `obn-re-search-backward'"
  (let ((parse)
        (nesting))
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (parse-partial-sexp (point-min) (point)))
      (cond 
       ((nth 4 parse)               ;inside comment
        (setq nesting (nth 4 parse))
        (while (> nesting 0)
          (re-search-backward "\\((\\*\\|\\*)\\)")
          (setq nesting
                (if (string-match "(\\*" (match-string-no-properties 0))
                    (1- nesting) 
                  (1+ nesting)))))
       ((nth 3 parse) (goto-char (nth 8 parse))) ;inside string
       ((and (eq (char-before) ?\() (eq (char-after) ?*))
        (backward-char))
       ((not (looking-at "(\\*\\|[\"']"))
        (setq count (1- count))))))
  (point))


(defun obn-re-search-backward (regexp &optional bound noerror count)
  "Searches backward but ignores strings and comments.
Invokes `re-search-backward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(obn-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(obn-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(obn-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


;; --- Font Lock ---

;; All regexps match text that spans only one line, as fontification
;; is done one line at a time. Moreover it might seem that they are
;; more explicit than nessessary (avoiding .*? etc.), but that
;; is to make sure that we don't enter any comments or strings.

(defvar obn-reserved-words
  '("ARRAY" "BEGIN" "BY" "CASE" "CONST" "DIV" "DO" "ELSE" "ELSIF" "END"
    "EXIT" "FOR" "IF" "IMPORT" "IN" "IS" "LOOP" "MOD" "MODULE" "NIL"
    "OF" "OR" "POINTER" "PROCEDURE" "RECORD" "REPEAT" "RETURN" "THEN"
    "TO" "TYPE" "UNTIL" "VAR" "WHILE" "WITH"))

(defconst obn-basic-types
  '("BOOLEAN" "CHAR" "SHORTINT" "INTEGER" "LONGINT" "REAL" "LONGREAL" "SET"))

(defconst obn-pre-decl-const '("TRUE" "FALSE")
  "predeclared constants")

(defconst obn-forward-decl-re "^[ \t]*\\<PROCEDURE\\>\\s-*\\^"
  "regular expression matching the start of a forward procedure declaration")

(defconst obn-proc-heading-re
  "^[ \t]*PROCEDURE\\>\\s-*\\(([[:word:] \t\n.:]+)\\s-*\\)?\\w")

(defconst obn-module-proc-heading-re 
  (concat obn-proc-heading-re "\\|^[ \t]*MODULE\\>")
  "regular expression matching the start of a (non-forward) procedure
heading")

(defconst obn-font-lock-proc-heading-1-re
  "^[ \t]*\\<PROCEDURE\\>[ \t^]*\\(?:([ \t[:word:]:]+)[ \t]*\\)?\\(\\w+\\)"
  "regular expression matching the start of a procedure heading")

;; Covers the case when the identifier starts on a line and the
;; procedure takes or returns at least one parameter.
(defconst obn-font-lock-proc-heading-2-re 
  "^[ \t]*\\(\\w+\\)[* \t]*(\\(\\([ \t[:word:].,]\\|(\\*.*?\\*)\\)+:[ \t[:word:].,]+\\(;\\|)[ \t]*[:;]\\)\\|[ \t]*[:;]\\)"
  "regular expression matching part of a procedure heading starting
from the procedure identifier")

(defconst obn-font-lock-exp-proc-heading-1-re
  "^[ \t]*\\<PROCEDURE\\>[ \t^]*\\(?:([ \t[:word:]:]+)[ \t]*\\)?\\(\\w+\\)[ \t]*\\*"
  "regular expression matching the start of an exported procedure")

;; Covers the case when the identifier starts on a line and the
;; procedure takes or returns at least one parameter.
(defconst obn-font-lock-exp-proc-heading-2-re 
  "^[ \t]*\\(\\w+\\)[ \t]*\\*[ \t]*(\\(\\([ \t[:word:].,]\\|(\\*.*?\\*)\\)+:[ \t[:word:].,]+\\(;\\|)[ \t]*[:;]\\)\\|[ \t]*[:;]\\)"
  "regular expression matching part of an exported procedure starting
from the procedure identifier")

(defconst obn-reserved-words-re 
  (regexp-opt obn-reserved-words 'words)
  "regular expression matching any Oberon reserved word")

(defconst obn-basic-types-re 
  (regexp-opt obn-basic-types 'words)
  "regular expression matching any Oberon basic type")

(defconst obn-pre-decl-const-re 
  (regexp-opt obn-pre-decl-const 'words)
  "regular expression matching any predeclared constant")

(defconst obn-indent-re
  (concat (regexp-opt '("IMPORT" "CONST" "TYPE" "VAR" "CASE" "FOR" "IF" 
                        "LOOP" "MODULE" "RECORD" "REPEAT" "WHILE" "WITH" 
                        "END" "UNTIL" "BEGIN" "ELSE" "ELSIF")
                      'words)
          "\\|" obn-forward-decl-re
          "\\|" obn-module-proc-heading-re
          "\\||")
  "regular expressions matching code that affects indentation")

(defvar obn-font-lock-keywords-1
  (list   
   "\\<IMPORT\\>"
   (list obn-font-lock-proc-heading-1-re 1 font-lock-function-name-face)
   (list obn-font-lock-proc-heading-2-re 1 font-lock-function-name-face))
  "level one font lock")

(defun obn-type-point-init ()
  "Initialize point before fontification of type declaration.
Stores the position where the font locking should resume."
  (if (and (null (nth 4 (parse-partial-sexp (point-at-bol) 
                                            (point))))
           (save-excursion             ;not inside type decl. section?
             (let ((case-fold-search nil))
               (prog1
                   (obn-re-search-backward obn-indent-re nil t)
                 (while (looking-at "END\\|VAR")
                   (obn-re-search-backward obn-indent-re 
                                           nil t)))
               (not (looking-at "TYPE\\|RECORD")))))
      (end-of-line)
    (back-to-indentation)))

(defvar obn-font-lock-keywords-2-diff
  (list obn-reserved-words-re
        (cons obn-basic-types-re font-lock-type-face)
        (cons obn-pre-decl-const-re font-lock-constant-face))
  "additional rules for level two font lock")


(defvar obn-font-lock-keywords-3-diff
  ;; The order of the declarations are significant!
  (list
   (list (concat obn-pre-decl-proc-re "[ \t]*(") 1 font-lock-builtin-face)

   ;; TYPES

   ;; To be precise, in a qualified type identifier only the type
   ;; identifier (not the module identifier) should be fontified. This
   ;; however turns out to look ugly and also distracts the eye, so I
   ;; have decided to fontify the entire type designator.

   ;; types in type declarations
   
   (list (concat "\\(\\w+\\)[* \t]*=[ \t]*"
                 (regexp-opt '("ARRAY" "RECORD" "POINTER" "PROCEDURE") 
                             'words))
         1 font-lock-type-face)

   (list "^[ \t]*\\(?:\\<TYPE\\>[ \t]*\\)?\\([[:alpha:]_]\\w*\\)[* \t]*=[ \t]*\\([[:alpha:]_][[:word:].]*\\)[ \t]*;"
         (list "\\(\\w+\\)[* \t]*=[ \t]*\\([[:word:].]+\\)"
               '(obn-type-point-init)
               '(end-of-line)
               '(1 font-lock-type-face) 
               '(2 font-lock-type-face)))

   (list "^[ \t]*\\(?:\\<TYPE\\>[ \t]*\\)?\\([[:alpha:]_]\\w*\\)[* \t]*=[ \t]*$"
         (list "\\(\\w+\\)[* \t]*="
               '(obn-type-point-init)
               '(end-of-line)
               '(1 font-lock-type-face)))

   ;; Types in WITH guards
   '(":[ \t]*\\([[:word:].]+\\)[ \t]+\\<DO\\>" 1 font-lock-type-face)

   ;; Protect first statement after `:' in case statements from being
   ;; fontified as a type (slow since it will inspect more or less
   ;; every row with a colon). The second row in the statement:
   ;;
   ;; CASE a OF
   ;;    b: c;
   ;; END
   ;;
   ;; illustrates the problem.
   (list "^[ \t]*\\(?:CASE\\>.+?\\<OF\\>[ \t]*\\)?\\(?:|[ \t]*\\)?.+?:[ \t]*[[:word:].]+"
    (list ":[ \t]*\\([[:word:].]+\\)"
               '(if (save-excursion
                      (and (progn (backward-word 1) 
                                  (not (looking-at obn-pre-decl-proc-re)))
                           (let ((case-fold-search nil))
                             (and (obn-re-search-backward obn-indent-re nil t)
                                  (looking-at "|\\|CASE")))))
                    (beginning-of-line)
                  (end-of-line))
               '(end-of-line)
               '(1 'default)))

   '("\\<POINTER[ \t]+TO\\>[ \t]+\\([[:word:].]+\\)" 1 font-lock-type-face)
   '("\\<ARRAY\\>.+?\\<OF[ \t]+\\([[:word:].]+\\)" 1 font-lock-type-face)
   '("\\<RECORD[ \t]*([ \t]*\\([[:word:].]+\\)" 1 font-lock-type-face)
   '("\\(\\<IS\\>\\|:\\)[ \t]*\\([[:word:].]+\\)" 2 font-lock-type-face)
   '("\\<\\(?:MIN\\|MAX\\|SIZE\\)[ \t]*(\\([[:word:].]+\\)"
     1 font-lock-type-face)
   ;; type guards
   '("\\([[:word:].]+\\)[ \t]*)[ \t]*[.^[]" 1 font-lock-type-face)
   '("\\](\\([[:word:].]+\\)" 1 font-lock-type-face)

   ;; CONSTANT DECLARATIONS

   (list "^[ \t]*\\(\\<CONST[ \t]+\\)?[[:alpha:]]\\w*[* \t]*="
         (list "\\(\\w+\\)[* \t]*="
               '(beginning-of-line)
               '(end-of-line)
               '(1 font-lock-variable-name-face)))

   ;; VARIABLE/PARAMETER/FIELD DECLARATIONS

   (list "^[ \t[:word:].,()*-=^]+:[ \t[:alpha:]_]"
         (list ;; "\\(\\w+\\)[-* \t]*\\(,\\|:[ \t]*\\(ARRAY\\>.+?\\(;\\|$\\)\\)?\\)"
          "\\(\\w+\\)[-* \t]*\\(,\\|:\\([ \t]*ARRAY\\>.+?\\(\\<OF\\>\\|$\\)\\)?\\)"
          '(let ((case-fold-search nil))
             (if (or (nth 4 (parse-partial-sexp (point-at-bol) 
                                                (point)))
                     (looking-at "[ \t[:word:].]+?\\<DO\\>")
                     (save-excursion 
                       (obn-re-search-backward obn-indent-re nil t)
                       (looking-at "|\\|CASE\\|WITH")))
                 (end-of-line)
               (beginning-of-line)))
          '(end-of-line)
          '(1 font-lock-variable-name-face)))
   
   ;; identifier list spanning several lines
   (list "\\<[[:alpha:]_]\\w*[-* \t]*,[ \t]*\\($\\|(\\*\\)"
         (list "\\(\\w+\\)[-* \t]*,"
               '(let ((case-fold-search nil))
                  (if (and (save-excursion
                             (and (obn-re-search-backward obn-indent-re nil t)
                                  (not (looking-at "|\\|CASE"))))
                           (save-excursion
                             (and (obn-re-search-forward "[:;]" nil t)
                                  (eq (char-before) ?\:)
                                  (not (eq (char-after) ?\=)))))
                      (beginning-of-line)
                    (end-of-line)))
               '(end-of-line)
               '(1 font-lock-variable-name-face))))

  "Additional rules for level three font lock.")

(defvar obn-font-lock-keywords-4-diff
  (list 
   (list "\\(\\w+\\)[ \t]*[*-][ \t]*[=:]" 1 font-lock-warning-face 'append)
   (list obn-font-lock-exp-proc-heading-1-re 1 font-lock-warning-face 'append)
   (list obn-font-lock-exp-proc-heading-2-re 1 font-lock-warning-face 'append))
   "additional rules for level four font lock")


(defconst obn-font-lock-keywords-2
  (append obn-font-lock-keywords-2-diff ;order significant!
          obn-font-lock-keywords-1)
  "rules for level two font lock")

(defconst obn-font-lock-keywords-3
  (append obn-font-lock-keywords-2
          obn-font-lock-keywords-3-diff)
  "rules for level three font lock")

(defconst obn-font-lock-keywords-4
  (append obn-font-lock-keywords-3
          obn-font-lock-keywords-4-diff)
  "rules for level four font lock")
      
(defconst obn-font-lock-keywords
  '(obn-font-lock-keywords-1
    obn-font-lock-keywords-2
    obn-font-lock-keywords-3
    obn-font-lock-keywords-4)
  "see `font-lock-keywords'")


;; --- Indentation ---

(defconst obn-decl-clause-re
  (concat "^[ \t]*\\(" (regexp-opt '("CONST" "IMPORT" "TYPE" "VAR") 'words)
          "\\|" obn-forward-decl-re "\\)"))

(defconst obn-block-begin-re
  (concat "^[ \t]*"
          (regexp-opt 
           '("CASE" "FOR" "IF" "LOOP" "REPEAT" "WHILE" "WITH") 
           'words)
          "\\|.*\\<RECORD\\>"))

(defconst obn-begin-re "^[ \t]*BEGIN\\>")

(defconst obn-block-outdent-re 
  (concat "^[ \t]*\\(|\\|" (regexp-opt '("ELSE" "ELSIF") 'words) "\\)"))

(defconst obn-block-end-re 
  (concat "^[ \t]*\\(END[ \t]*\\(;\\|$\\|(\\*\\)\\|UNTIL\\>\\)") 'words)

(defconst obn-body-end-re "^[ \t]*END[ \t]+\\w")

(defconst obn-operator-re
  (concat "\\([=#+*/&<>-]\\|"
          (regexp-opt '("DIV" "IN" "IS" "MOD" "OR") 'words)
          "\\)"))

(defconst obn-indentation-categories
  (list 
   (list obn-decl-clause-re "$" 'decl-clause)
   (list obn-module-proc-heading-re ".*\\<END[ \t]+\\w" 'module-proc)
   (list obn-begin-re ".*\\<END[ \t]+\\w" 'begin)
   (list obn-block-begin-re ".*\\<\\(END\\|UNTIL\\)\\>" 'block-begin)
   (list obn-body-end-re "$" 'body-end)
   (list obn-block-outdent-re ".*\\<END\\>" 'block-outdent)
   (list obn-block-end-re "$" 'block-end))
  "A line belong to the category if it is matched by the first
regexp and not matched by the second regexp and the second regexp
does not match a (part of a) string or comment.")

(defconst obn-indentation-table
  (let ((table (make-hash-table :test 'equal)))
    (puthash '(begin . block-begin) 1 table)
    (puthash '(begin . other) 1 table)
    (puthash '(block-begin . block-begin) 1 table)
    (puthash '(block-begin . other) 1 table)
    (puthash '(block-outdent . block-begin) 1 table)
    (puthash '(block-outdent . other) 1 table)
    (puthash '(decl-clause . block-begin) 1 table) ; RECORD
    (puthash '(decl-clause . block-end) 1 table)   ; end of RECORD
    (puthash '(decl-clause . other) 1 table)
    (puthash '(module-proc . decl-clause) 1 table)
    (puthash '(module-proc . module-proc) 1 table)
    (puthash '(module-proc . other) 1 table)        ; empty line

    (puthash '(body-end . begin) -1 table)
    (puthash '(decl-clause . begin) -1 table)
    (puthash '(block-end . block-end) -1 table)
    (puthash '(other . block-end) -1 table)
    (puthash '(block-end . block-outdent) -1 table)
    (puthash '(other . block-outdent) -1 table)
    (puthash '(block-end . body-end) -1 table)
    (puthash '(body-end . body-end) -1 table)
    (puthash '(other . body-end) -1 table)
    (puthash '(block-end . decl-clause) -1 table)
    (puthash '(other . decl-clause) -1 table)
    (puthash '(other . module-proc) -1 table)
    (puthash '(block-end . module-proc) -1 table)

    (puthash '(block-end . begin) -2 table) ; end of RECORD
    (puthash '(other . begin) -2 table)
    table))

(defun obn-indent-factor (reference-category current-category)
  "Return indentation factor from `obn-indentation-table'."
  (save-excursion
    (gethash (cons reference-category current-category)
             obn-indentation-table
             0)))                         ; value if not found


(defun obn-indent-category (line-number current-line-p)
  "Return indentation category from `obn-indentation-categories'.
If CURRENT-LINE-P is non-nil the line is regarded as the line to
be indented."
  (save-excursion
    (goto-line line-number)
    (let ((case-fold-search nil)
          (ls obn-indentation-categories)
          (result nil))
      (while (and (not (null ls)) (null result))
        (if (and (looking-at (nth 0 (car ls)))
                 (or current-line-p
                     (not (looking-at (nth 1 (car ls))))
                     (nth 4 (save-excursion
                              (parse-partial-sexp (point-at-bol) 
                                                  (match-end 0))))))
            (setq result (nth 2 (car ls))))
        (setq ls (cdr ls)))
      (if (null result)
          'other
        result))))


(defun obn-column (position)
  (save-excursion
    (goto-char position)
    (current-column)))


(defun obn-indentation (line)
  (save-excursion 
    (goto-line line)
    (current-indentation)))


(defun obn-previous-unbalanced-paren ()
  (nth 1 (parse-partial-sexp (point-min) (point))))


(defun obn-backward-to-code-line ()
  "Move backward to nearest line containing code. Moves backwards
to the start of the nearest line that is not a blank line (a line
that contains only spaces and tabs) and not a line that starts
with or continues a comment. If there is no such line point is
moved to the beginning of the buffer."
  (forward-line -1)
  (let ((case-fold-search nil)
        (regex "^[ \t]*\\($\\|(\\*\\)")
        (parse-result (parse-partial-sexp (point-min) (point))))
    (while (and (not (bobp))
                (or (nth 4 parse-result) (looking-at regex)))
      (when (nth 4 parse-result)
        (goto-char (nth 8 parse-result))
        (beginning-of-line))
      (while (and (not (bobp)) (looking-at regex))
        (forward-line -1))
      (setq parse-result (parse-partial-sexp (point-min) (point))))))


(defun obn-line-number-at-pos ()
  (1+ (count-lines (point-min) (point))))


(defun obn-continued-balanced-line-p ()
  (let ((case-fold-search nil)
        (re (concat "^.*\\(" 
                    "\\(" obn-operator-re "\\|,\\)[ \t]*\\($\\|(\\*\\)"
                    "\\|\n[ \t]*\\(" obn-operator-re "\\|,\\)"
                    "\\|PROCEDURE[ \t]*(.+?)[ \t]*\\($\\|(\\*\\)\\)")))
    (save-excursion
      (unless (= (obn-line-number-at-pos) 1)
        (obn-backward-to-code-line)
        (and (looking-at re)
             (null (nth 1 (parse-partial-sexp (point) (match-end 0))))
             (null (nth 8 (parse-partial-sexp 
                           (point-at-bol) (match-end 0)))))))))


(defun obn-continued-line-p ()
  (or (obn-continued-balanced-line-p)
      (not (null (obn-previous-unbalanced-paren)))))


(defun obn-block-comment-line-p ()
  (save-excursion
    (beginning-of-line)
    (or (nth 4 (parse-partial-sexp (point-min) (point)))
        (and (looking-at "^[ \t]*(\\*")
             (nth 4 (parse-partial-sexp (point) (point-at-eol)))))))


(defun obn-reference-line ()
  "Return line number of previous non-continued line. Comments
and empty lines are ignored."
  (save-excursion
    (obn-backward-to-code-line)
    (let ((unbalanced-paren (obn-previous-unbalanced-paren)))
      (when (not (null unbalanced-paren))
        (goto-char unbalanced-paren)
        (beginning-of-line)))
    (while (and (not (bobp)) (obn-continued-line-p))
      (obn-backward-to-code-line))
    (obn-line-number-at-pos)))


(defun obn-proc-heading-beneath ()
  "Return point of start of procedure heading beneath. If the
first non-empty line below point is a procedure heading then
return its starting point, else return nil."
  (let ((case-fold-search nil))
    (save-excursion
      (forward-line)
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line))
      (when (looking-at obn-proc-heading-re)
        (back-to-indentation)
        (point)))))


(defun obn-case-or-with-above ()
  (save-excursion
    (beginning-of-line)
    (when (and (obn-re-search-backward 
                (concat "^[ \t]*\\(" obn-reserved-words-re "\\||\\).*") nil t)
               (save-match-data
                 (string-match "\\(CASE\\|WITH\\)" 
                               (match-string-no-properties 1)))
               (save-match-data
                 (not (string-match "\\<END\\>" 
                                    (match-string-no-properties 0)))))
      (match-beginning 1))))


(defun obn-proper-indentation ()
  "Returns the proper indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (or (bobp) (looking-at "^[ \t]*MODULE\\>"))
        0
      (let* ((ref-line (obn-reference-line))
             (ref-category (obn-indent-category ref-line nil))
             (current-category (obn-indent-category 
				(obn-line-number-at-pos) t))
             (unbalanced-paren (obn-previous-unbalanced-paren))
             (proc-heading-beneath (obn-proc-heading-beneath))
             (case-or-with-above (obn-case-or-with-above)))
        
        (cond 
         ;; Continued line with unbalanced parentheses
         ((not (null unbalanced-paren))
          (+ (obn-column unbalanced-paren) 1))
         
         ;; Continued line with balanced parentheses
         ((obn-continued-balanced-line-p)
          (if (or (eq ref-category 'block-begin)
                  (eq ref-category 'block-outdent)
                  (eq ref-category 'module-proc))
              (+ (obn-indentation ref-line) (* 2 oberon-indent-level))
            (+ (obn-indentation ref-line) oberon-indent-level)))
         
         ;; Single line comment before a procedure (special case)
         ((and (looking-at "^[ \t]*(\\*") proc-heading-beneath)
          (obn-column proc-heading-beneath))
         
         ;; First CASE/WITH clause are indented by two blanks (special case)
         (case-or-with-above
          (cond ((looking-at "^[ \t]*\\(|\\|\\<\\(ELSE\\|END\\)\\>\\)")
                 (obn-column case-or-with-above))
                ((looking-at "[[:word:].,'\" \t]+:[^=]")
                 (+ (obn-column case-or-with-above) 2))
                (t (+ (obn-column case-or-with-above) 
                      oberon-indent-level))))
         
         ;; The "normal" case
         (t (+ (obn-indentation ref-line)
               (* (obn-indent-factor ref-category current-category)
                  oberon-indent-level))))))))
  

(defun oberon-indent-line ()
  (interactive)
  (let ((offset (- (current-column) (current-indentation))))
    (unless (obn-block-comment-line-p)
      (indent-line-to (obn-proper-indentation))
      (if (> offset 0) (forward-char offset)))))


;; --- Upcase Conversion ---

(defvar oberon-mode-abbrev-table nil
  "abbreviation table in use in Oberon mode buffers")

(define-abbrev-table 'oberon-mode-abbrev-table 
  (mapcar `(lambda (x) 
             (list (downcase x) x ,(when oberon-auto-indent-flag
                                     '(quote indent-according-to-mode))))
          (append obn-reserved-words obn-basic-types obn-pre-decl-const)))

(defun obn-pre-abbrev-expand-hook ()
  "Inhibit upcase conversion under certain circumstances.
Inhibits expansion of the word left of point if

   * word is not lowercase or 
   * word is inside a string or comment or

See also `pre-abbrev-expand-hook'."
  (let ((case-fold-search nil)
        (word (save-excursion
                (let ((p (point)))      ;...nilTHEN... => ...NIL THEN...
                  (backward-word 1)     ;      ^                 ^
                  (re-search-forward "\\w+" p)
                  (match-string-no-properties 0)))))
    (when (null local-abbrev-table) 
      (setq local-abbrev-table oberon-mode-abbrev-table))
    (when (or (not (string= word (downcase word)))
              (save-excursion           ;..."of"... => ..."of";...
                (backward-word 1)       ;       ^              ^
                (nth 8 (parse-partial-sexp (point-min) (point)))))
      (setq local-abbrev-table nil))))


;; --- Filling ---

(defun obn-end-of-comment (nesting)
  "Return point at next comment end.  NESTING is the current
comment nesting level (zero if outside a comment).  Returns nil
if NESTING is less than zero or if no end of comment is found."
  (save-excursion
    (when (>= nesting 0)
      (condition-case err
          (progn
            (when (= nesting 0)
              (re-search-forward "(\\*")
              (setq nesting 1))
            (while (and (> nesting 0) (re-search-forward "(\\*\\|\\*)"))
              (if (string-match (match-string-no-properties 0) "(*")
                  (setq nesting (1+ nesting))
                (setq nesting (1- nesting))))
            (point))
        (search-failed nil)))))


(defun obn-trailing-comment-p (parse-status)
  (when (nth 4 parse-status)
    (save-excursion
      (goto-char (nth 8 parse-status))
      (skip-chars-backward " \t")
      (not (bolp)))))


(defun oberon-fill-comment-paragraph (arg)
  "If inside a comment, fill the current comment paragraph, else
do nothing. Always returns t."
  (interactive)
  (let* ((parse-res (parse-partial-sexp (point-min) (point)))
         (nesting (nth 4 parse-res))
         (trailing-comment (obn-trailing-comment-p parse-res)))
    (when nesting 
      (save-restriction
        (narrow-to-region 
         (save-excursion (goto-char (nth 8 parse-res)) (point-at-bol))
         (obn-end-of-comment nesting))
        (when trailing-comment
          (save-excursion 
            (goto-char (nth 8 parse-res))
            (split-line)
            (split-line)))
        (let ((fill-paragraph-function nil)
              (left-margin (save-excursion (goto-char (nth 8 parse-res)) 
                                           (current-column))))
          (fill-paragraph arg))
        (when trailing-comment
          (save-excursion
            (goto-char (nth 8 parse-res))
            (looking-at "\\s-+")
            (replace-match ""))))))
  t)

        
;; --- Imenu ---

(defvar obn-imenu-generic-expression 
  (list
   (list
    nil 
    (concat "^ \\{0," (number-to-string oberon-indent-level) "\\}"
            "\\<PROCEDURE\\s-*\\(\\((.*?)\\s-*\\)?\\w+[ \t]*\\*?\\)")
    1))
  "Regular expression matching top level procedures (used by imenu)")


;; --- Interactive Utility Functions ---

(defun oberon-trim-enumeration (start-value)
  "Assign consecutive integers to (enumeration) constants in region.
The first constant gets the value START-VALUE."
  (interactive 
   (list (string-to-number
          (read-string "Enumeration start value (default 0): " nil nil "0"))))
  (let ((start (region-beginning))
        (end (make-marker))
        (value start-value))
    (set-marker end (region-end))
    (goto-char start)
    (while (obn-re-search-forward "\\([ \t]*\\)=[ \t]*.*?;" end t)
      (replace-match (concat "\\1=\\1" (int-to-string value) ";"))
      (setq value (1+ value)))))


(defun oberon-insert-module-skeleton ()
  "Insert module declaration at point.  Uses the name of the
buffer sans extension as the module name."
  (interactive)
  (let ((m (file-name-sans-extension (buffer-name))))
    (insert (concat "MODULE " m ";"))
    (indent-according-to-mode)
    (newline 4)
    (insert (concat "END " m "."))
    (indent-according-to-mode)
    (forward-line -2)
    (indent-according-to-mode)))


(defun oberon-insert-type-skeleton (name &optional base-type)
  "Insert type skeleton declarations for pointer and record type
at point."
  (interactive "sType name: \nsBase type name (default none): ")
  (let ((suffix oberon-record-type-suffix))
    (insert (concat name " = POINTER TO " name suffix ";"))
    (indent-according-to-mode)
    (newline)
    (insert (concat name suffix " = RECORD"))
    (unless (string-equal base-type "")
      (insert (format " (%s)" base-type)))
    (indent-according-to-mode)
    (newline)
    (insert "END;")
    (beginning-of-line)
    (indent-according-to-mode)
    (split-line)
    (indent-according-to-mode)))


(defun oberon-insert-procedure-skeleton (name &optional reciever-type)
  "Insert a procedure skeleton at point."
  (interactive "sProcedure name: \nsReciever type name (default none): ")
  (let ((reciever "self"))
    (insert "PROCEDURE ")
    (when (not (string-equal reciever-type "")) 
      (if (save-excursion              ;pointer or reference reciever?
            (obn-re-search-backward 
             (concat "\\<" reciever-type "\\>\\s-*=\\s-*RECORD\\>") nil t))
          (insert (concat "(VAR " reciever ": " reciever-type ") "))
        (insert (concat "(" reciever ": " reciever-type ") "))))
    (insert (concat name ";"))
    (indent-according-to-mode)
    (newline)
    (insert "BEGIN")
    (indent-according-to-mode)
    (newline)
    (insert (concat "END " name ";"))
    (indent-according-to-mode)
    (forward-line -2)
    (end-of-line)
    (backward-char)))


;; --- Main Function ---

;;;###autoload
(defun oberon-mode ()
  "Major mode for editing Oberon/Oberon-2 program texts.

Key bindings:

\\{oberon-mode-map}"
  (interactive)
  (kill-all-local-variables)

  ;; Mode Map
  (use-local-map oberon-mode-map)

  ;; Syntax Table And Parsing
  (set-syntax-table oberon-mode-syntax-table)

  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'oberon-indent-line)
  
  ;; Font Lock
  (setq font-lock-defaults (list obn-font-lock-keywords))

  ;; Abbreviation Table
  (setq local-abbrev-table oberon-mode-abbrev-table)
  (set (make-local-variable 'pre-abbrev-expand-hook)
       'obn-pre-abbrev-expand-hook)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression obn-imenu-generic-expression)

  ;; Comments
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+ *")
  (set (make-local-variable 'comment-end-skip) " *\\*+)")

  ;; Filling
  (set (make-local-variable 'fill-paragraph-function)
       'oberon-fill-comment-paragraph)

  (setq major-mode 'oberon-mode)
  (setq mode-name "Oberon")
  (run-hooks 'oberon-mode-hook))


(provide 'oberon)

;;; oberon.el ends here
