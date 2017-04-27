This module adds support for magit links in org buffers. The following links
are supported:
- magit:/path/to/repo::commit@<hash>
- magit:/path/to/repo::status
- magit:/path/to/repo::log

Of course those links can be stored as usual with `org-store-link' from the
corresponding magit buffers. By default the path to the repo is abbreviated
with `abbreviate-file-name', just like org-mode does. See
`directory-abbrev-alist' for configuring its behavior. Alternately, you can
customize `org-magit-filename-transformer' and provide your own
transformer function.

When exporting those links, the variable `org-magit-known-public-providers'
is used to generate meaningful links. This assumes there exists a public
http server that is able to expose those objects.

Certain settings can be configured directly at the repository level
if needed. For example

$ git config org-magit.remote upstream

In this case, html links will point to the "upstream" webserver, instead of
the default "origin". URL templates can also be stored in the
repository. For example

$ git config org-magit.log http://myserver/plop.git/history
