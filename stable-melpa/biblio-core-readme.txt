A framework for browsing bibliographic search results.  This is the core
package; for user interfaces, see any of `biblio-crossref', `biblio-dblp',
`biblio-doi', and `biblio-dissemin', and the more general `biblio' package.

(require 'bibtex)
(require 'browse-url)
(require 'hl-line)
(require 'ido)
(require 'json)
(require 'url-queue)

(require 'dash)
(require 'let-alist)
(require 'seq)
