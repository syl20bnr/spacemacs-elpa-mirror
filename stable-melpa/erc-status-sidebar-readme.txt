This package is provides a hexchat-like status bar for joined
channels in ERC.  It relies on the `erc-track' module, and displays
all the same information erc-track does in the mode line, but in an
alternative format.

Credit to sidebar.el (https://github.com/sebastiencs/sidebar.el)
and outline-toc.el (https://github.com/abingham/outline-toc.el),
from which all the sidebar window management ideas were lifted.

# Setup

To open the ERC status sidebar in the current frame:

M-x erc-status-sidebar-open

Ensure the `erc-track' module is active (a member of
`erc-modules'). This is the default.

To close the sidebar on the current frame:

M-x erc-status-sidebar-close

Use a prefix argument to close the sidebar on all frames.

To kill the sidebar buffer and close the sidebar on all frames:

M-x erc-status-sidebar-kill
