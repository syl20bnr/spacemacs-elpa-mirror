A tool like vimdiff for Emacs

vdiff compares two or three buffers on the basis of the output from the diff
tool. The buffers are kept synchronized so that as you move through one of
the buffers the top of the active buffer aligns with the corresponding top of
the other buffer(s). This is similar to how ediff works, but in ediff you use
a third "control buffer" to move through the diffed buffers. The key
difference is that in vdiff you are meant to actively edit one of the buffers
and the display will update automatically for the other buffer. Similar to
ediff, vdiff provides commands to "send" and "receive" hunks from one buffer
to the other as well as commands to traverse the diff hunks, which are useful
if you are trying to merge changes. In contrast to ediff, vdiff also provides
folding capabilities to fold sections of the buffers that don't contain
changes. This folding occurs automatically. Finally, you are encouraged to
bind a key to `vdiff-hydra/body', which will use hydra.el (in ELPA) to create
a convenient transient keymap containing most of the useful vdiff commands.

This functionality is all inspired by (but not equivalent to) the vimdiff
tool from vim.

See https://github.com/justbur/emacs-vdiff for more information
