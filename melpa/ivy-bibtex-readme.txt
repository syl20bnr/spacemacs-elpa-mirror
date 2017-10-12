A BibTeX bibliography manager based on Ivy and the
bibtex-completion backend.  If you are familiar with helm-bibtex,
this is the ivy version.

News:
- 10/10/2017: We now have support for ~@string~ constants.
- 10/02/2017: Use date field if year is not defined.
- 09/29/2017: If there is a BibTeX entry, citation macro, or
  org-bibtex entry at point, the corresponding publication will be
  pre-selected in helm-bibtex and ivy-bibtex giving quick access to
  PDFs and other functions.

See NEWS.org for old news.

Key features:
- Quick access to your bibliography from within Emacs
- Tightly integrated workflows
- Provides instant search results as you type
- Powerful search expressions
- Open the PDFs, URLs, or DOIs associated with an entry
- Insert LaTeX cite commands, Ebib links, or Pandoc citations,
  BibTeX entries, or plain text references at point, attach PDFs to
  emails
- Attach notes to publications

Install:

  Put this file in a directory included in your load path or
  install ivy-bibtex from MELPA (preferred).  Then add the
  following in your Emacs startup file:

    (require 'ivy-bibtex)

  Alternatively, you can use autoload:

    (autoload 'ivy-bibtex "ivy-bibtex" "" t)

  Requirements are parsebib, swiper, s, dash, and f.  The easiest way
  to install these packages is through MELPA.

  Let ivy-bibtex know where it can find your bibliography by
  setting the variable `bibtex-completion-bibliography'.  See the
  manual for more details:

    https://github.com/tmalsburg/helm-bibtex/blob/master/README.ivy-bibtex.org

Usage:

   Do M-x ivy-bibtex and start typing a search query when prompted.
