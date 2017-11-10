erc-scrolltoplace provides an erc module called 'scrolltoplace' which
tries to keep as many messages visible as possible in ERC while never moving
point.

This module allows you to see the last-viewed point in your erc buffers while
still seeing newer messages as they come in.

Add the following to your init to enable erc-scrolltoplace:
(require 'erc-scrolltoplace)
(add-to-list 'erc-modules 'scrolltoplace)
(erc-update-modules)
