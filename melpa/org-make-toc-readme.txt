This package makes it easy to have one or more customizable tables of contents in Org files that can be
updated manually, or automatically when the file is saved.  Links to headings are created
compatible with GitHub's Org renderer.

Installation

Install the packages `dash' and `s'.  Then put this file in your `load-path', and put this in
your init file:

(require 'org-make-toc)

Usage

1.  Make a heading in the Org file where you want the table of contents, and give it the Org
property "TOC" with the value "this".

2.  Run the command `org-make-toc'.

Advanced

A document may contain multiple tables of contents.  Tables of contents can be customized by
setting the "TOC" property of headings to these values:

+  "all": Include all headings in the file, except ignored headings.
+  "children": Include only child headings of this ToC.
+  "siblings": Include only sibling headings of this ToC.
+  "ignore": Omit a heading from the TOC.
+  "ignore-children" or "0": Omit a heading's child headings from the TOC.
+  a number "N": Include child headings no more than "N" levels deep in the TOC.

Automatically update on save

To automatically update a file's TOC when the file is saved, use the command
"add-file-local-variable" to add "org-make-toc" to the Org file's "before-save-hook".

Or, you may activate it in all Org buffers like this:

  (add-hook 'org-mode-hook #'org-make-toc-mode)
