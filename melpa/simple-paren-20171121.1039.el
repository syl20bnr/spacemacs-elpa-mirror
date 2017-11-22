;;; simple-paren.el --- Insert paired delimiter, wrap -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Version: 20171121.1039

;; Copyright (C) 2016-2017  Andreas Röhler, Steve Purcell
;; See also http://www.unicode.org/L2/L2013/13046-BidiBrackets.txt

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
(simple-paren-define lesser-than 60 62)
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
(simple-paren-define parenthesis #x0028 #x0029)
(simple-paren-define square-bracket #x005b #x005d)
(simple-paren-define curly-bracket #x007b #x007d)
(simple-paren-define tibetan-mark-gug-rtags-gyon #x0f3a #x0f3b)
(simple-paren-define tibetan-mark-ang-khang-gyon #x0f3c #x0f3d)
(simple-paren-define ogham-feather-mark #x169b #x169c)
(simple-paren-define square-bracket-with-quill #x2045 #x2046)
(simple-paren-define superscript-parenthesis #x207d #x207e)
(simple-paren-define subscript-parenthesis #x208d #x208e)
(simple-paren-define pointing-angle-bracket #x2329 #x232a)
(simple-paren-define medium-parenthesis-ornament #x2768 #x2769)
(simple-paren-define medium-flattened-parenthesis-ornament #x276a #x276b)
(simple-paren-define medium-pointing-angle-bracket-ornament #x276c #x276d)
(simple-paren-define heavy-pointing-angle-quotation-mark-ornament #x276e #x276f)
(simple-paren-define heavy-pointing-angle-bracket-ornament #x2770 #x2771)
(simple-paren-define light-tortoise-shell-bracket-ornament #x2772 #x2773)
(simple-paren-define medium-curly-bracket-ornament #x2774 #x2775)
(simple-paren-define s-shaped-bag-delimiter #x27c5 #x27c6)
(simple-paren-define mathematical-white-square-bracket #x27e6 #x27e7)
(simple-paren-define mathematical-angle-bracket #x27e8 #x27e9)
(simple-paren-define mathematical-double-angle-bracket #x27ea #x27eb)
(simple-paren-define mathematical-white-tortoise-shell-bracket #x27ec #x27ed)
(simple-paren-define mathematical-flattened-parenthesis #x27ee #x27ef)
(simple-paren-define white-curly-bracket #x2983 #x2984)
(simple-paren-define white-parenthesis #x2985 #x2986)
(simple-paren-define z-notation-image-bracket #x2987 #x2988)
(simple-paren-define z-notation-binding-bracket #x2989 #x298a)
(simple-paren-define square-bracket-with-underbar #x298b #x298c)
(simple-paren-define square-bracket-with-tick-in-top-corner #x298d #x2990)
(simple-paren-define square-bracket-with-tick-in-bottom-corner #x298f #x298e)
(simple-paren-define angle-bracket-with-dot #x2991 #x2992)
(simple-paren-define arc-less-than-bracket #x2993 #x2994)
(simple-paren-define double-arc-greater-than-bracket #x2995 #x2996)
(simple-paren-define black-tortoise-shell-bracket #x2997 #x2998)
(simple-paren-define wiggly-fence #x29d8 #x29d9)
(simple-paren-define double-wiggly-fence #x29da #x29db)
(simple-paren-define pointing-curved-angle-bracket #x29fc #x29fd)
(simple-paren-define top-half-bracket #x2e22 #x2e23)
(simple-paren-define bottom-half-bracket #x2e24 #x2e25)
(simple-paren-define sideways-u-bracket #x2e26 #x2e27)
(simple-paren-define double-parenthesis #x2e28 #x2e29)
(simple-paren-define angle-bracket #x3008 #x3009)
(simple-paren-define double-angle-bracket #x300a #x300b)
(simple-paren-define corner-bracket #x300c #x300d)
(simple-paren-define white-corner-bracket #x300e #x300f)
(simple-paren-define black-lenticular-bracket #x3010 #x3011)
(simple-paren-define tortoise-shell-bracket #x3014 #x3015)
(simple-paren-define white-lenticular-bracket #x3016 #x3017)
(simple-paren-define white-tortoise-shell-bracket #x3018 #x3019)
(simple-paren-define white-square-bracket #x301a #x301b)
(simple-paren-define small-parenthesis #xfe59 #xfe5a)
(simple-paren-define small-curly-bracket #xfe5b #xfe5c)
(simple-paren-define small-tortoise-shell-bracket #xfe5d #xfe5e)
(simple-paren-define fullwidth-parenthesis #xff08 #xff09)
(simple-paren-define fullwidth-square-bracket #xff3b #xff3d)
(simple-paren-define fullwidth-curly-bracket #xff5b #xff5d)
(simple-paren-define fullwidth-white-parenthesis #xff5f #xff60)
(simple-paren-define halfwidth-corner-bracket #xff62 #xff63)


(provide 'simple-paren)
;;; simple-paren.el ends here
