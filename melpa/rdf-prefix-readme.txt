The intention of this package is to simplify a common task in the
work of RDF developers: remembering and looking up URI prefixes.

The `rdf-prefix-insert' command is used to look up- and insert URIs
based on a prefix.

RDF prefixes are mapped to URIs by the alist `rdf-prefix-alist'. New
prefixes can be defined by adding them to `rdf-prefix-alist', on the
form (PREFIX . URI), where PREFIX and URI are both strings.

The prefix list is sorted by popularity, and is pulled regularly from
http://prefix.cc/.
