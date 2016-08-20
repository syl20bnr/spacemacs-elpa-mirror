This program searches the web for BibTeX entries matching the given
Author end/or Title.  It displays then a selection buffer, that permits
to examine and choose entries to add to the bibliography file, or to
insert into the current buffer.

The function is called through "M-x bibretrieve".  Then it prompts for
author and title.  For an advanced use, that permits to select which
backend to use, call it with "C-u M-x bibretrieve".

The configuration is done with the variable bibretrieve-backends, that
is an alist with pairs containing the backend to use and the timeout
for it. See the README file for the list of supported backends.

To create a new backend define a new function
"bibretrieve-BACKEND-create-url" that takes as input author and title
and returns a url that, when retrieved, gives some bibtex entries.
The function should be defined in "bibretrieve-base.el".
It is then necessary to advise bibretrieve of the new backend,
adding it to the list "bibretrieve-installed-backends".

The url is retrieved via mm-url.  You may want to customize the
variable mm-url-use-external and mm-url-program.

Acknowledgments: This program has been inspired by bibsnarf.  The
functions that create the urls for most backends are taken from
there.  This program uses the library mm-url.  This programs also uses
lot of function of RefTeX.  The selection process is entirely based on
reftex-sel.  Many function have also been adapted from there.
