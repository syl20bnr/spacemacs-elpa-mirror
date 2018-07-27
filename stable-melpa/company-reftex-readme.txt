To use, add `company-reftex-labels' and `company-reftex-citations' to `company-backends' in the
buffers where you want it activated.  The major mode must be derived from `latex-mode', and
`reftex-mode' must be switched on for them to work.

- `company-reftex-labels' will trigger inside forms like \ref{}, \eqref{}, \auroref{}, etc.
- `company-reftex-citations' will trigger inside cite{}.

These backends collect data from RefTeX, which is very powerful.  Citations from external bibtex
files should be found automatically.  In multi-file documents, make sure `TeX-master' is set
appropriately.
