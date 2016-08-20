This is a library for computing context menus based on text
properties and overlays. The intended use is to have tools that
annotate source code and others that use these annotations, without
requiring a direct coupling between them, but maintaining
discoverability.

Major modes that wish to use this library should first define an
appropriate value for `prop-menu-item-functions'. Then, they should
bind `prop-menu-by-completing-read' to an appropriate
key. Optionally, a mouse pop-up can be added by binding
`prop-menu-show-menu' to a mouse event.
