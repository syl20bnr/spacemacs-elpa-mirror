Parsing Expression Grammars (PEG) are a formalism in the spirit of
Context Free Grammars (CFG) with some simplifications which makes
the implementation of PEGs as recursive descent parser particularly
simple and easy to understand [Ford, Baker].

This file implements a macro `peg-parse' which parses the current
buffer according to a PEG.  E.g. we can match integers with a PEG
like this:

 (peg-parse (number   sign digit (* digit))
            (sign     (or "+" "-" ""))
            (digit    [0-9]))

In contrast to regexps, PEGs allow us to define recursive "rules".
A "grammar" is a list of rules.  A rule is written as (NAME PEX...)
E.g. (sign (or "+" "-" "")) is a rule with the name "sign".  The
syntax for PEX (Parsing Expression) is a follows:

Description		Lisp		Traditional, as in Ford's paper
Sequence		(and e1 e2)	e1 e2
Prioritized Choice   (or e1 e2)	e1 / e2
Not-predicate	(not e)		!e
And-predicate	(if e)		&e
Any character	(any)		.
Literal string	"abc"		"abc"
Character C		(char c)	'c'
Zero-or-more		(* e)		e*
One-or-more		(+ e)		e+
Optional		(opt e)		e?
Character range	(range a b)	[a-b]
Character set	[a-b "+*" ?x]	[a-b+*x]  ; note: [] is a elisp vector
Character classes    [ascii cntrl]
Beginning-of-Buffer  (bob)
End-of-Buffer        (eob)
Beginning-of-Line    (bol)
End-of-Line		(eol)
Beginning-of-Word    (bow)
End-of-Word		(eow)
Beginning-of-Symbol  (bos)
End-of-Symbol	(eos)
Syntax-Class		(syntax-class NAME)

`peg-parse' also supports parsing actions, i.e. Lisp snippets which
are executed when a pex matches.  This can be used to construct
syntax trees or for similar tasks.  Actions are written as

 (action FORM)          ; evaluate FORM
 `(VAR... -- FORM...)   ; stack action

Actions don't consume input, but are executed at the point of
match.  A "stack action" takes VARs from the "value stack" and
pushes the result of evaluating FORMs to that stack.  See
`peg-ex-parse-int' for an example.

Derived Operators:

The following operators are implemented as combinations of
primitive expressions:

(substring E)  ; match E and push the substring for the matched region
(region E)     ; match E and push the corresponding start and end positions
(replace E RPL); match E and replace the matched region with RPL.
(list E)       ; match E and push a list out of the items that E produces.

Regexp equivalents:

Here a some examples for regexps and how those could be written as pex.
[Most are taken from rx.el]

"^[a-z]*"
(and (bol) (* [a-z]))

"\n[^ \t]"
(and "\n" (not [" \t"]) (any))

"\\*\\*\\* EOOH \\*\\*\\*\n"
"*** EOOH ***\n"

"\\<\\(catch\\|finally\\)\\>[^_]"
(and (bow) (or "catch" "finally") (eow) (not "_") (any))

"[ \t\n]*:\\([^:]+\\|$\\)"
(and (* [" \t\n"]) ":" (or (+ (not ":") (any)) (eol)))

"^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
(and (bol)
     "content-transfer-encoding:"
     (* (opt "\n") ["\t "])
     "quoted-printable"
     (* (opt "\n") ["\t "]))

"\\$[I]d: [^ ]+ \\([^ ]+\\) "
(and "$Id: " (+ (not " ") (any)) " " (+ (not " ") (any)) " ")

"^;;\\s-*\n\\|^\n"
(or (and (bol) ";;" (* (syntax-class whitespace)) "\n")
    (and (bol) "\n"))

"\\\\\\\\\\[\\w+"
(and "\\\\[" (+ (syntax-class word)))

Search forward for ";;; Examples" for other examples.

References:

[Ford] Bryan Ford. Parsing Expression Grammars: a Recognition-Based
Syntactic Foundation. In POPL'04: Proceedings of the 31st ACM
SIGPLAN-SIGACT symposium on Principles of Programming Languages,
pages 111-122, New York, NY, USA, 2004. ACM Press.
http://pdos.csail.mit.edu/~baford/packrat/

[Baker] Baker, Henry G. "Pragmatic Parsing in Common Lisp".  ACM Lisp
Pointers 4(2), April--June 1991, pp. 3--15.
http://home.pipeline.com/~hbaker1/Prag-Parse.html

Roman Redziejowski does good PEG related research
http://www.romanredz.se/pubs.htm
