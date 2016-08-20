The function `hide-region-hide' hides the region. You can hide many
different regions and they will be "marked" by two configurable
strings (so that you know where the hidden text is).

The hidden regions is pushed on a kind of hide-region \"ring".

The function `hide-region-unhide' "unhides" one region, starting
with the last one you hid.

The best is to try it out. Test on the following:

Test region 1
Test region 2
Test region 3

It can be useful to bind the commands to mnemonic keys, e.g.:
(global-set-key (kbd "C-c h r") 'hide-region-hide)
(global-set-key (kbd "C-c h u") 'hide-region-unhide)

Version history

Version 1.0.2

* Added defface for text properties.

* Minor tweaks.


Version 1.0.1

* Seems that the getting-stuck problem have disappeared since Emacs
21.3 was released, so no need anymore for the extra movement
commands.

* Added the intangible property to the overlays because that seemed
to remove a minor getting-stuck problem (the overlay "ate" one
keystroke) when navigating around an overlay. Adding the intangible
property makes it impossible to navigate into the overlay.

* Added custom option to propertize the overlay markers for greater
visibility.

* Minor code cleanup


Bugs

Probably many, but none that I know of. Comments and suggestions
are welcome!
