Commentary:

Bitcoin donations gratefully accepted: 13NyoPq3iusGsCtHNRT9xfA9jsqPjYtyyE

Extensions to emacs buffer-selection library (bs.el)
This extension allows you to bind keys to buffer selection configurations (using `bs-ext-config-keys'),
and optionally displays the configuration names and associated keybindings in the header line of the
*buffer-selection* buffer.
It also creates a new config called "regexp". When the "/" key is pressed the user is prompted for a regular
expression and any buffers with matching names are added to the "regexp" config.

The following new keybindings are defined:

/        : prompt user for regular expression, place matching buffers in "regexp" config, and change to that config
<left>   : select previous config using `bs-ext-select-previous-configuration'
<right>  : select next config using `bs-ext-select-next-configuration'
x        : kill buffer on current line using `bs-delete'

Also if you have color-moccur installed you can use M-O to find regexp matches in marked buffers.


Installation:

Put bs-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add
(you don't need to do this for ~/.emacs.d - it's added by default).

Add the following to your ~/.emacs startup file.

(require 'bs-ext)
