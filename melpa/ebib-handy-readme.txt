* README                                                             :README:
** Introduce
ebib-handy is a ebib tool, which can let ebib become a cite chooser.
[[./snapshots/ebib-handy.gif]]

** Install
1. Config melpa: http://melpa.org/#/getting-started
2. M-x package-install RET ebib-handy RET

** Configure
#+BEGIN_EXAMPLE
(require 'ebib-handy)
(ebib-handy-enable)

(setq ebib-extra-fields
      '((BibTeX "keywords" "abstract" "timestamp"
                "file"  "url" "crossref" "annote" "doi")
        (biblatex "keywords" "abstract" "timestamp"
                  "file"  "url" "crossref" "annote" "doi")))
#+END_EXAMPLE

** Usage
#+BEGIN_EXAMPLE
(global-set-key "\C-c b" 'ebib-handy)
#+END_EXAMPLE

You can open "example/thesis.org" then type 'C-c b'.
