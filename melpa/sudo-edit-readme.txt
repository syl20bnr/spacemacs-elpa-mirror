This file provides several utility functions for opening buffers
as root using 'sudo'.  They are:

sudo-edit
sudo-edit-current-file

Suggested keybinding:
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

Installation

To use this mode, put the following in your init.el:
(require 'sudo-edit)
