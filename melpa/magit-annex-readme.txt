Magit-annex adds a few git-annex operations to the Magit interface.
Annex commands are available under the annex popup menu, which is
bound to "@".  This key was chosen as a leading key mostly to be
consistent with John Wiegley's git-annex.el (which provides a Dired
interface to git-annex) [1].

Adding files:
  @a   Add a file to the annex.
  @A   Add all untracked and modified files to the annex.

Managing file content:
  @fu   Unlock files.
  @fl   Lock files.
  @fg   Get files.
  @fd   Drop files.
  @fc   Copy files.
  @fm   Move files.

  @u    Browse unused files.
  @l    List annex files.

Updating:
  @m   Run `git annex merge'.
  @y   Run `git annex sync'.

In the unused buffer
  l    Show log for commits touching a file
  RET  Open a file
  k    Drop files
  s    Add files back to the index

When Magit-annex is installed from MELPA, no additional setup is
needed.  The annex popup menu will be added under the main Magit
popup menu (and loading of Magit-annex will be deferred until the
first time the annex popup is called).

To use Magit-annex from the source repository, put

  (require 'magit-annex)

in your initialization file.


[1] https://github.com/jwiegley/git-annex-el
