This package is a port of vim-textobj-anyblock. It gives text objects for the
closest block of those defined in the evil-anyblock-blocks alist. By default
it includes (), {}, [], <>, '', "", ``, and “”. This is convenient for
operating on the closest block without having to choose between typing
something like i{ or i<. This package allows for the list of blocks to be
changed. They can be more complicated regexps. A simple expand-region like
functionality is also provided when in visual mode, though this is not a
primary focus of the plugin and does not exist in vim-textobj-anyblock. Also,
in the case that the point is not inside of a block, anyblock will seek
forward to the next block.

The required version of evil is based on the last change I could find to
evil-select-paren, but the newest version of evil is probably preferable.

For more information see the README in the github repo.
