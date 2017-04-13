Manage multiple shell/terminal buffers for each project.  For
example, to develop for Linux kernel, I usually use one shell
buffer to configure and build kernel, one shell buffer to run some
git command not supported by magit, one shell buffer to run qemu
for built kernel, one shell buffer to ssh into guest system to
test.  Different set of commands is used by the shell in each
buffer, so each shell should have different command history
configuration, and for some shell, I may need different setup.  And
I have several projects to work on.  In addition to project
specific shell buffers, I want some global shell buffers, so that I
can use them whichever project I am working on.  Project shells is
an Emacs program to let my life easier via helping me to manage all
these shell/terminal buffers.

The ssh support code is based on Ian Eure's nssh.  Thanks Ian!
