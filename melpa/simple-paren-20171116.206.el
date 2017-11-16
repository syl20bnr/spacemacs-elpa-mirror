;;; simple-paren.el --- Insert paired delimiter, wrap -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Version: 20171116.206
;; Copyright (C) 2016-2017  Andreas Röhler, Steve Purcell

;; Author: Andreas Röhler, Steve Purcell

;; URL: https://github.com/andreas-roehler/simple-paren

;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;; Keywords: convenience

;;; Commentary:
;; Commands inserting paired delimiters.  These are are easy to extend
;; by just providing the delimiting charakters as shown below with
;; math white square brackets

;; (defun simple-paren-mathematical-white-square-bracket (arg)
;;   "Insert MATHEMATICAL LEFT/RIGHT WHITE SQUARE BRACKETs

;; With \\[universal-argument] insert whitespaces literatim
;; With active region, wrap around.
;; With numerical ARG 2 honor padding"
;;   (interactive "*P")
;;   (simple-paren--intern ?⟦  ?⟧ arg))
;;
;; Or even shorter:
;; (simple-paren-define mathematical-white-square-bracketwhitespace ?⟦  ?⟧)

;; Examples, cursor as pipe-symbol:

;; (defun foo1 |	==> (defun foo1 ()

;; |interactive		==> (interactive)

;; int|eractive		==> (interactive)

;; with active region until end of word
;; int|eractive		==> int(eractive)

;; With C-u keep padding
;; | foo		==> ( foo )

;; Insertions are not electric, thus a mnemonic key is recommended:

;; (global-set-key [(super ?\()] 'simple-paren-parentize)
;; (global-set-key [(super ?{)] 'simple-paren-brace)
;; (global-set-key [(super ?\[)] 'simple-paren-bracket)
;; (global-set-key [(super ?')] 'simple-paren-singlequote)
;; (global-set-key [(super ?\")] 'simple-paren-doublequote)
;; (global-set-key [(super ?<)] 'simple-paren-lesser-then)
;; (global-set-key [(super ?>)] 'simple-paren-greater-then)

;;

;;; Code:

(defvar simple-paren-skip-chars "^, \t\r\n\f"
  "Skip chars backward not mentioned here.")

;; (setq simple-paren-skip-chars "^, \t\r\n\f")

(defun simple-paren--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\"."
  (pcase erg
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    ;; '(leftrightsinglequote 8216 8217)
    ;; '(leftrightdoublequote 8220 8221)
    (8216 8217)
    (8220 8221)
    (_ erg)))

(defvar simple-paren-braced-newline (list 'js-mode))

(defun simple-paren--intern (left-char right-char &optional arg)
  (let ((padding (eq 2 (prefix-numeric-value arg)))
	(no-wrap (eq 4 (prefix-numeric-value arg)))
	end erg)
    (if no-wrap
	(progn
	  (insert left-char)
	  (insert right-char))
      (if (region-active-p)
	  (progn
	    (setq end (copy-marker (region-end)))
	    (goto-char (region-beginning)))
	(when (setq erg (member (char-after) (list ?>  ?\) ?\]  ?} 8217 8221)))
	  (forward-char -1) )
	(unless (or (eobp) (eolp)(member (char-after) (list 32 9)))
	  (skip-chars-backward simple-paren-skip-chars)))
      (insert left-char)
      (if (region-active-p)
	  (goto-char end)
	(when (and padding (looking-at "\\( \\)?[^ \n]+"))
	  ;; travel symbols after point
	  (skip-chars-forward " "))
	(skip-chars-forward (char-to-string left-char))
	(skip-chars-forward simple-paren-skip-chars)
	;; (forward-sexp)
	(when (and padding (match-string-no-properties 1))
	  (insert (match-string-no-properties 1))))
      (insert right-char)
      (forward-char -1)
      (when (and (eq (char-after) ?})(member major-mode simple-paren-braced-newline))
	(newline 2)
	(indent-according-to-mode)
	(forward-char 1)
	(insert ?\;)
	(forward-line -1)
	(indent-according-to-mode)))))

(defmacro simple-paren-define (name code1 code2)
  "Define an insertion function with NAME, using char codes CODE1 and CODE2."
  (let ((func-name (intern (concat "simple-paren-" (symbol-name name))))
        (docstring (concat "With \\[universal-argument] insert " (symbol-name name) "s literatim.

With active region, wrap around.
With numerical ARG 2 honor padding")))
    `(defun ,func-name (&optional arg)
       ,docstring
       (interactive "*P")
       (simple-paren--intern ,code1 ,code2 arg))))

;; Commands
(simple-paren-define brace 123 125)
(simple-paren-define bracket 91 93)
(simple-paren-define lesser 60 62)
(simple-paren-define greater-than 62 60)
(simple-paren-define leftrightsinglequote 8216 8217)
(simple-paren-define leftrightdoublequote 8220 8221)
(simple-paren-define parentize 40 41)
(simple-paren-define acute-accent 180 180)
(simple-paren-define backslash 92 92)
(simple-paren-define backtick 96 96)
(simple-paren-define colon 58 58)
(simple-paren-define cross 43 43)
(simple-paren-define dollar 36 36)
(simple-paren-define doublequote 34 34)
(simple-paren-define equalize 61 61)
(simple-paren-define escape 92 92)
(simple-paren-define grave-accent 96 96)
(simple-paren-define hash 35 35)
(simple-paren-define hyphen 45 45)
(simple-paren-define singlequote 39 39)
(simple-paren-define slash 47 47)
(simple-paren-define star 42 42)
(simple-paren-define tild 126 126)
(simple-paren-define underscore 95 95)
(simple-paren-define whitespace 32 32)


(provide 'simple-paren)
;;; simple-paren.el ends here
