A major mode for creating and editing stand-off markup, also called
external markup.  It is written for use in the field of digital
humanities and the manual annotation of training data for
named-entity recognition.

After switching a buffer's major mode to standoff-mode, it gets
read-only. We then call it the source document, which the external
markup refers to. There are commands for creating 1) markup
elements, 2) relations between such elements that take the form of
RDF-like directed graphs; there will soon be commands for creating
3) attributes of markup elements and 4) writing free text comments
anchored on these elements. Markup elements refer to portions of
the source document by character offsets. Annotations can be dumped
as elisp-expressions to a file. Other persistent layers can be
plugged provided that they implement the API for backends defined
in `standoff-api.el'.

Usage:

Add to your Emacs config:

 (add-to-list 'load-path "/path/to/standoff-mode-directory")
 (autoload 'standoff-mode "standoff-mode.el"
   "Mode for creating and editing stand-off markup, aka external markup" t)
 ;; auto-load standoff-mode for files ending with .TEI-P5.xml:
 (add-to-list 'auto-mode-alist '("\\.TEI-P5.xml$" . standoff-mode))
