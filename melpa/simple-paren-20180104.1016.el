;;; simple-paren.el --- Insert paired delimiter, wrap -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Version: 20180104.1016

;; Copyright (C) 2016-2018  Andreas Röhler, Steve Purcell
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
;; (global-set-key [(super ?<)] 'simple-paren-lesser-than)
;; (global-set-key [(super ?>)] 'simple-paren-greater-than)

;;

;;; Code:
(defvar simple-paren-paired-delimiter-chars
  (list
   ?‘ ?’
   ?` ?'
   ?< ?>
   ?> ?<
   ?\( ?\)
   ?\) ?\(
   ?\] ?\[
   ?\[ ?\]
   ?} ?{
   ?{ ?}
   ?\〈 ?\〉
   ?\⦑ ?\⦒
   ?\⦓ ?\⦔
   ?\【 ?\】
   ?\⦗ ?\⦘
   ?\⸤ ?\⸥
   ?\「 ?\」
   ?\《 ?\》
   ?\⦕ ?\⦖
   ?\⸨ ?\⸩
   ?\⧚ ?\⧛
   ?\｛ ?\｝
   ?\（ ?\）
   ?\［ ?\］
   ?\｟ ?\｠
   ?\｢ ?\｣
   ?\❰ ?\❱
   ?\❮ ?\❯
   ?\“ ?\”
   ?\‘ ?\’
   ?\❲ ?\❳
   ?\⟨ ?\⟩
   ?\⟪ ?\⟫
   ?\⟮ ?\⟯
   ?\⟦ ?\⟧
   ?\⟬ ?\⟭
   ?\❴ ?\❵
   ?\❪ ?\❫
   ?\❨ ?\❩
   ?\❬ ?\❭
   ?\᚛ ?\᚜
   ?\〈 ?\〉
   ?\⧼ ?\⧽
   ?\⟅ ?\⟆
   ?\⸦ ?\⸧
   ?\﹛ ?\﹜
   ?\﹙ ?\﹚
   ?\﹝ ?\﹞
   ?\⁅ ?\⁆
   ?\⦏ ?\⦎
   ?\⦍ ?\⦐
   ?\⦋ ?\⦌
   ?\₍ ?\₎
   ?\⁽ ?\⁾
   ?\༼ ?\༽
   ?\༺ ?\༻
   ?\⸢ ?\⸣
   ?\〔 ?\〕
   ?\『 ?\』
   ?\⦃ ?\⦄
   ?\〖 ?\〗
   ?\⦅ ?\⦆
   ?\〚 ?\〛
   ?\〘 ?\〙
   ?\⧘ ?\⧙
   ?\⦉ ?\⦊
   ?\⦇ ?\⦈))


(defvar simple-paren-skip-chars "^, \t\r\n\f"
  "Skip chars backward not mentioned here.")

(dolist (ele simple-paren-paired-delimiter-chars)
  (unless (string-match (regexp-quote (char-to-string ele)) simple-paren-skip-chars)
    (setq simple-paren-skip-chars
	  (concat simple-paren-skip-chars (char-to-string ele)))))

;; (setq simple-paren-skip-chars "^, \t\r\n\f")

(defun simple-paren--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\"."
  (pcase erg
    (?‘ ?’)
    (?` ?')
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    (?\〈 ?\〉)
    (?\⦑ ?\⦒)
    (?\⦓ ?\⦔)
    (?\【 ?\】)
    (?\⦗ ?\⦘)
    (?\⸤ ?\⸥)
    (?\「 ?\」)
    (?\《 ?\》)
    (?\⦕ ?\⦖)
    (?\⸨ ?\⸩)
    (?\⧚ ?\⧛)
    (?\｛ ?\｝)
    (?\（ ?\）)
    (?\［ ?\］)
    (?\｟ ?\｠)
    (?\｢ ?\｣)
    (?\❰ ?\❱)
    (?\❮ ?\❯)
    (?\“ ?\”)
    (?\‘ ?\’)
    (?\❲ ?\❳)
    (?\⟨ ?\⟩)
    (?\⟪ ?\⟫)
    (?\⟮ ?\⟯)
    (?\⟦ ?\⟧)
    (?\⟬ ?\⟭)
    (?\❴ ?\❵)
    (?\❪ ?\❫)
    (?\❨ ?\❩)
    (?\❬ ?\❭)
    (?\᚛ ?\᚜)
    (?\〈 ?\〉)
    (?\⧼ ?\⧽)
    (?\⟅ ?\⟆)
    (?\⸦ ?\⸧)
    (?\﹛ ?\﹜)
    (?\﹙ ?\﹚)
    (?\﹝ ?\﹞)
    (?\⁅ ?\⁆)
    (?\⦏ ?\⦎)
    (?\⦍ ?\⦐)
    (?\⦋ ?\⦌)
    (?\₍ ?\₎)
    (?\⁽ ?\⁾)
    (?\༼ ?\༽)
    (?\༺ ?\༻)
    (?\⸢ ?\⸣)
    (?\〔 ?\〕)
    (?\『 ?\』)
    (?\⦃ ?\⦄)
    (?\〖 ?\〗)
    (?\⦅ ?\⦆)
    (?\〚 ?\〛)
    (?\〘 ?\〙)
    (?\⧘ ?\⧙)
    (?\⦉ ?\⦊)
    (?\⦇ ?\⦈)
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
	;; (when (setq erg (member (char-after) (list ?>  ?\) ?\]  ?} 8217 8221)))
	;;   (forward-char -1) )
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

(simple-paren-define acute-accent ?\´ ?\´)
(simple-paren-define angle-bracket ?\〈 ?\〉)
(simple-paren-define angle-bracket-with-dot ?\⦑ ?\⦒)
(simple-paren-define arc-less-than-bracket ?\⦓ ?\⦔)
(simple-paren-define backslash ?\\ ?\\)
(simple-paren-define backtick ?\` ?\`)
(simple-paren-define question-mark ?\? ?\?)
(simple-paren-define exclamation-mark ?\! ?\!)
(simple-paren-define semicolon ?\; ?\;)
(simple-paren-define black-lenticular-bracket ?\【 ?\】)
(simple-paren-define black-tortoise-shell-bracket ?\⦗ ?\⦘)
(simple-paren-define bottom-half-bracket ?\⸤ ?\⸥)
(simple-paren-define brace ?\{ ?\})
(simple-paren-define bracket ?\[ ?\])
(simple-paren-define colon ?\: ?\:)
(simple-paren-define comma ?\, ?\,)
(simple-paren-define corner-bracket ?\「 ?\」)
(simple-paren-define cross ?\+ ?\+)
(simple-paren-define curly-bracket ?\{ ?\})
(simple-paren-define dollar ?\$ ?\$)
(simple-paren-define dot ?\. ?\.)
(simple-paren-define double-angle-bracket ?\《 ?\》)
(simple-paren-define double-arc-greater-than-bracket ?\⦕ ?\⦖)
(simple-paren-define double-parenthesis ?\⸨ ?\⸩)
(simple-paren-define double-wiggly-fence ?\⧚ ?\⧛)
(simple-paren-define doublequote ?\" ?\")
(simple-paren-define equalize ?\= ?\=)
(simple-paren-define escape ?\\ ?\\)
(simple-paren-define fullwidth-curly-bracket ?\｛ ?\｝)
(simple-paren-define fullwidth-parenthesis ?\（ ?\）)
(simple-paren-define fullwidth-square-bracket ?\［ ?\］)
(simple-paren-define fullwidth-white-parenthesis ?\｟ ?\｠)
(simple-paren-define grave-accent ?\` ?\`)
(simple-paren-define greater-than ?\> ?\<)
(simple-paren-define halfwidth-corner-bracket ?\｢ ?\｣)
(simple-paren-define hash ?\# ?\#)
(simple-paren-define heavy-pointing-angle-bracket-ornament ?\❰ ?\❱)
(simple-paren-define heavy-pointing-angle-quotation-mark-ornament ?\❮ ?\❯)
(simple-paren-define hyphen ?\- ?\-)
(simple-paren-define leftrightdoublequote ?\“ ?\”)
(simple-paren-define leftrightsinglequote ?\‘ ?\’)
(simple-paren-define lesser-than ?\< ?\>)
(simple-paren-define light-tortoise-shell-bracket-ornament ?\❲ ?\❳)
(simple-paren-define mathematical-angle-bracket ?\⟨ ?\⟩)
(simple-paren-define mathematical-double-angle-bracket ?\⟪ ?\⟫)
(simple-paren-define mathematical-flattened-parenthesis ?\⟮ ?\⟯)
(simple-paren-define mathematical-white-square-bracket ?\⟦ ?\⟧)
(simple-paren-define mathematical-white-tortoise-shell-bracket ?\⟬ ?\⟭)
(simple-paren-define medium-curly-bracket-ornament ?\❴ ?\❵)
(simple-paren-define medium-flattened-parenthesis-ornament ?\❪ ?\❫)
(simple-paren-define medium-parenthesis-ornament ?\❨ ?\❩)
(simple-paren-define medium-pointing-angle-bracket-ornament ?\❬ ?\❭)
(simple-paren-define ogham-feather-mark ?\᚛ ?\᚜)
(simple-paren-define parenthesis ?\( ?\))
(simple-paren-define parentize ?\( ?\))
(simple-paren-define period ?\. ?\.)
(simple-paren-define point ?\. ?\.)
(simple-paren-define pointing-angle-bracket ?\〈 ?\〉)
(simple-paren-define pointing-curved-angle-bracket ?\⧼ ?\⧽)
(simple-paren-define s-shaped-bag-delimiter ?\⟅ ?\⟆)
(simple-paren-define semicolon ?\; ?\;)
(simple-paren-define sideways-u-bracket ?\⸦ ?\⸧)
(simple-paren-define singlequote ?\' ?\')
(simple-paren-define slash ?\/ ?\/)
(simple-paren-define small-curly-bracket ?\﹛ ?\﹜)
(simple-paren-define small-parenthesis ?\﹙ ?\﹚)
(simple-paren-define small-tortoise-shell-bracket ?\﹝ ?\﹞)
(simple-paren-define square-bracket ?\[ ?\])
(simple-paren-define square-bracket-with-quill ?\⁅ ?\⁆)
(simple-paren-define square-bracket-with-tick-in-bottom-corner ?\⦏ ?\⦎)
(simple-paren-define square-bracket-with-tick-in-top-corner ?\⦍ ?\⦐)
(simple-paren-define square-bracket-with-underbar ?\⦋ ?\⦌)
(simple-paren-define star ?\* ?\*)
(simple-paren-define subscript-parenthesis ?\₍ ?\₎)
(simple-paren-define superscript-parenthesis ?\⁽ ?\⁾)
(simple-paren-define tibetan-mark-ang-khang-gyon ?\༼ ?\༽)
(simple-paren-define tibetan-mark-gug-rtags-gyon ?\༺ ?\༻)
(simple-paren-define tild ?\~ ?\~)
(simple-paren-define top-half-bracket ?\⸢ ?\⸣)
(simple-paren-define tortoise-shell-bracket ?\〔 ?\〕)
(simple-paren-define underscore ?\_ ?\_)
(simple-paren-define white-corner-bracket ?\『 ?\』)
(simple-paren-define white-curly-bracket ?\⦃ ?\⦄)
(simple-paren-define white-lenticular-bracket ?\〖 ?\〗)
(simple-paren-define white-parenthesis ?\⦅ ?\⦆)
(simple-paren-define white-square-bracket ?\〚 ?\〛)
(simple-paren-define white-tortoise-shell-bracket ?\〘 ?\〙)
(simple-paren-define whitespace ?\  ?\ )
(simple-paren-define wiggly-fence ?\⧘ ?\⧙)
(simple-paren-define z-notation-binding-bracket ?\⦉ ?\⦊)
(simple-paren-define z-notation-image-bracket ?\⦇ ?\⦈)
;; Commands

(defvar simple-paren-mode-map nil)
(defconst simple-paren-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(super ?\")] 'simple-paren-doublequote)
    (define-key map [(super ?\#)] 'simple-paren-hash)
    (define-key map [(super ?\$)] 'simple-paren-dollar)
    (define-key map [(super ?\')] 'simple-paren-singlequote)
    (define-key map [(super ?\()] 'simple-paren-parentize)
    (define-key map [(super ?\*)] 'simple-paren-star)
    (define-key map [(super ?\+)] 'simple-paren-cross)
    (define-key map [(super ?\-)] 'simple-paren-hyphen)
    (define-key map [(super ?\/)] 'simple-paren-slash)
    (define-key map [(super ?\:)] 'simple-paren-colon)
    (define-key map [(super ?\<)] 'simple-paren-lesser-than)
    (define-key map [(super ?\=)] 'simple-paren-equalize)
    (define-key map [(super ?\>)] 'simple-paren-greater-than)
    (define-key map [(super ?\[)] 'simple-paren-bracket)
    (define-key map [(super ?\\)] 'simple-paren-backslash)
    (define-key map [(super ?\\)] 'simple-paren-escape)
    (define-key map [(super ?\_)] 'simple-paren-underscore)
    (define-key map [(super ?\`)] 'simple-paren-backtick)
    (define-key map [(super ?\`)] 'simple-paren-grave-accent)
    (define-key map [(super ?\;)] 'simple-paren-semicolon)
    (define-key map [(super ?\.)] 'simple-paren-dot)
    (define-key map [(super ?\,)] 'simple-paren-comma)
    (define-key map [(super ?\{)] 'simple-paren-brace)
    (define-key map [(super ?\~)] 'simple-paren-tild)
    (define-key map [(super ?\?)] 'simple-paren-question-mark)
    (define-key map [(super ?\!)] 'simple-paren-exclamation-mark)
    (define-key map [(super ?\´)] 'simple-paren-acute-accent)
    map))

(define-minor-mode simple-paren-mode
  "Commands inserting paired delimiters.

Provide characters used in Math resp. Logic"

  nil " SP" simple-paren-mode-map)

(provide 'simple-paren)
;;; simple-paren.el ends here
