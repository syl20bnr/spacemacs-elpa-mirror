ryo-modal provides a convenient way of defining modal keybindings in Emacs.
The primary way of binding keys is using `ryo-modal-key' and `ryo-modal-keys'.
Both of these functions provide useful keyword arguments.
`ryo-modal-mode' is used to toggle the modal editing environment.
ryo-modal does not come with any predefined bindings!

If you want bindings that only should be active when the region is active,
please have a look at `selected-minor-mode' (https://github.com/Kungsgeten/selected.el)
