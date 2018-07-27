Edit regions in separate buffers based on `edit-indirect' especially for latex.
In the indirect buffer, latex special expressions like \ref{...} $...$ are translated to [number].
When commiting the edit, the translated [number] are backed to original expressions \ref{...} $...$.
Such translation is useful when english grammar cheker, for example ginger,
detect errors for latex special expressions like \ref{...} $...$.

Translation example


Equation (\ref{eq:aa}) and (\ref{eq:bb}) shows an results.
X shows an results\cite{AAa,Bb}.
$x=234$
Prolog is logical languages.
\verb|<Opinions>|
hello
\scheme|(setq q 2)|
hello
\lstinline$category$
xxxxxxxxxxxxx
\footnote{[1234]}
\footnote{\url{http://example.com/}}


is translated in the edit buffer


Equation ([26530]) and ([98699]) shows an results.
X shows an results[96852].
[11593]
Prolog is logical languages.
[26630]
hello
[39322]
hello
[99858]
xxxxxxxxxxxx
[21335]
[16739]


Then, we can deal the translated text with english grammar checker ( ginger grammarly etc ).
After the edit, we can commit the translated text to the original buffer(C-c C-c).
When commit, the translated expressions [number] back to the original expressions automatically.

Usually, many technical terms, for example 'Prolog' in the abobe Latex code,
are dealed as miss-spell in many english grammar checker.
Such words also can be translated by resisting the translation dictonary

(edit-indirect-region-latex-ht-resister "Prolog")

Then

"Prolog is logical languages."

is tranlslated to

"[78137] is logical languages."
