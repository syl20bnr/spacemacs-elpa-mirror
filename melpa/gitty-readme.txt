gitty is a very small extension to `vc-mode' to make common git
commands easier to access than in the normal environment. The idea
is that you do most interaction with git using the command line or
magit, but some often-used commands are accessible easily.

C-x v b is the main new command. It prompts for an existing branch
with tab completion enabled and checks that out. If the entered
branch does not exist, it prompts if it should be created. This
makes switching between branches for quick feature branches much
easier than with `vc-mode'.

gitty also provides quick keys to stash save and stash pop in
the current repository, and to see the current `status'.

For more information, see the docstring for the command `gitty-mode`.

Note, though, that gitty does override some `vc-mode' keybindings
that are less useful for git.
