This is an addition to the standard org-mode exporters.  The package
extends the (X)HTML exporter to produce EPUB files.  It eliminates
all inline CSS and JavaScript to accomplish this.  This exporter
will also tie the XHTML DTD to XHTML 1.1, a concrete DTD specifier
that was not supported by ox-html previously.

The main part is the generation of the table of contents in machine
readable form, as well as the spine, which defines the order in
which files are presented.  A lesser part is the inclusion of
various metadata properties, among them authorship and rights.
