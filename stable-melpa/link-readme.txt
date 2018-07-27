This file contains functions for using links in buffers.  A link is
a part of the buffer marked with a special face, beeing
hightlighted while the mouse points to it and beeing activated when
pressing return or clicking the button2.

Which each link a function and some data are associated.  Upon
clicking the function is called with the data as only
argument. Both the function and the data are stored in text
properties.

link-create-link       - insert a new link for the text in the given range
link-initialize-keymap - install the keybinding for selecting links
