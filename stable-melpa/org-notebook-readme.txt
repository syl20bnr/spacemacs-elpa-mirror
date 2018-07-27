The main function is org-notebook-insert-image
Bind this to a convenient key combination

There is also org-notebook-new-notebook
This creates a directory with the name of the
notebook you provide, and then creates
a notebook.org file and an img directory in it.
It populates the notebook.org file with org
headers, asking you for a title only since
the author, email, and language are extracted
automatically.

For customization there is:

org-notebook-drawing-program
By default this is set to kolourpaint
This determines what image-drawing program
will be launched when org-notebook-insert-image
is called.

org-notebook-image-type
By default this is set to png
This determines the default filetype that
the drawn diagrams will be saved and linked.

org-notebook-language
By default this is set to en
This determines the language org header that
will be inserted when org-notebook-new-notebook
is called.

org-notebook-image-width
By default this is set to 600
This determines the image width org header that
will be inserted when org-notebook-new-notebook
is called. The header determines the width that
the images will be displayed in in org-mode.

org-notebook-headers
By default this is an empty list
This is a list of cons where the first element
of each con is the header name (eg. LATEX_CLASS,
HTML_HEAD, etc) and the second element of each
con is the value of the header. These will be
inserted into the notebook org file when
org-notebook-new-notebook is called.
