Commands inserting paired delimiters.  These are are easy to extend
by just providing the delimiting charakters as shown below with
math white square brackets

(defun simple-paren-mathematical-white-square-bracket (arg)
  "Insert MATHEMATICAL LEFT/RIGHT WHITE SQUARE BRACKETs

With \\[universal-argument] insert whitespaces literatim
With active region, wrap around.
With numerical ARG insert the delimiter arg times"
  (interactive "*P")
  (simple-paren--intern ?⟦  ?⟧ arg))

Or even shorter:
(simple-paren-define mathematical-white-square-bracketwhitespace ?⟦  ?⟧)

Examples, cursor as pipe-symbol:

(defun foo1 |	==> (defun foo1 ()

|interactive		==> (interactive)

int|eractive		==> (interactive)

with active region until end of word
int|eractive		==> int(eractive)

With ‘simple-paren-honor-padding-p’ set to ‘t’ --the default--
honor padding:
| foo		==> ( foo )

Insertions are not electric, thus a mnemonic key is recommended:

(global-set-key [(super ?\()] 'simple-paren-parentize)
(global-set-key [(super ?{)] 'simple-paren-brace)
(global-set-key [(super ?\[)] 'simple-paren-bracket)
(global-set-key [(super ?')] 'simple-paren-singlequote)
(global-set-key [(super ?\")] 'simple-paren-doublequote)
(global-set-key [(super ?<)] 'simple-paren-lesser-than)
(global-set-key [(super ?>)] 'simple-paren-greater-than)
