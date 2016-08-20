This module allows a user to look up synonyms for a word in
a web-accessible thesaurus.

This code started with a basic version posted without license at
http://alexhenning.github.com/blog/2010/11/01/synonym.el/

I standardized the naming, re-factored, wrote some documentation,
introduced the dependency on dropdown-list.el and x-popup-menu, added
a license, introduced some error handling, and polished it.

Right now it depends on a web service from Big Huge Labs. It
is not tied to that service, and could be expanded to use other
services, and to even dynamically choose which service to access.

To use, first go to http://words.bighugelabs.com/ and register (no
cost) to get an API key. Then, put thesaurus.el in your emacs load
path and modify your .emacs to do this:

  (require 'thesaurus)
  (setq thesaurus-bhl-api-key "XXXXXXXXXXXX")  ;; from registration

   -or-

  (require 'thesaurus)
  (thesaurus-set-bhl-api-key-from-file "~/BigHugeLabs.apikey.txt")

Optionally, set a key binding:
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

This module currently relies on a BigHugeLabs thesaurus service. The
service is currently free, and has a limit of 10,000 lookups per
day. If the service changes, or becomes unavailable, or if anyone
exceeds the limit, it shouldn't be difficult to expand this module to
support other online thesaurus services. Wolfram Alpha is one
possible option; theirs is a free API. Wordnik has a free synonyms API.

eg:
http://api.wordnik.com//v4/word.json/awry/relatedWords?relationshipTypes=synonym

I think Bing has one. Probably there are others. This module would need
to be modified to support one of those.

If you want to proxy the URL calls, then use this:
  (setq url-proxy-services (list (cons "http" "proxyHost:proxyPort")))
