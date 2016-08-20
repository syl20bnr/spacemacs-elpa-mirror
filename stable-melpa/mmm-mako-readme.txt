Parts of this code was borrowed from mmm-myghty mode. (Thanks Ben
Bangert and Michael Abraham Shulman)

Bugs -
mmm classes with :back delimeters of "$" (such as Mako's ## and %)
will carry the :back match over to the next line when <return> is
used to end the line (probably because the original "$" marker is
moved to the next line and mmm-mode doesn't automatically update
the match).
