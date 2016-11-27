A bibliography manager for Emacs, based on Helm and the
bibtex-completion backend.

News:
- 11/24/2016: Added support for bare relative paths to PDF
  files.  Concatenates the path in the `file' field to all paths
  in `bibtex-completion-library-path'.
- 11/24/2016: Added citation function for APA-style citations in org
  files.  See `bibtex-completion-format-citation-org-apa-link-to-PDF'.
- 11/18/2016: Added support for bibliographies in org-bibtex
  format.  See docstring of `bibtex-completion-bibliography'.
- 11/10/2016: Layout of search results can now be customized.
- 04/18/2016: Improved support for Mendely/Jabref/Zotero way of
  referencing PDFs.
- 04/06/2016: Generic functions are factored out into a backend for
  use with other completion frameworks like ivy.
- 04/02/2016: Added support for biblio.el which is useful for
  importing BibTeX from CrossRef and other sources.  See new
  fallback options and the section "Importing BibTeX from CrossRef"
  on the GitHub page.

See NEWS.org for old news.

Key features:
- Quick access to your bibliography from within Emacs
- Powerful search capabilities
- Provides instant search results as you type
- Tightly integrated with LaTeX authoring, emails, Org mode, etc.
- Open the PDFs, URLs, or DOIs associated with an entry
- Insert LaTeX cite commands, Ebib links, or Pandoc citations,
  BibTeX entries, or plain text references at point, attach PDFs to
  emails
- Support for note taking
- Quick access to online bibliographic databases such as Pubmed,
  arXiv, Google Scholar, Library of Congress, etc.
- Imports BibTeX entries from CrossRef and other sources.

See the github page for details:

   https://github.com/tmalsburg/helm-bibtex
