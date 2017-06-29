Magit-tbdiff provides a Magit interface to git-tbdiff [1,2], a Git
extension for comparing two versions of a topic branch.

There are three commands for calling git-tbdiff:

  * `magit-tbdiff-ranges' is the most generic of the three
    commands.  It reads two ranges that represent the two series to
    be compared.

  * `magit-tbdiff-revs' reads two revisions.  From these (say, "A"
    and "B"), it constructs the two series as B..A and A..B.

  * `magit-tbdiff-revs-with-base' is like the previous command, but
    it also reads a base revision, constructing the range as
    <base>..A and <base>..B.

These commands are available in the popup `magit-tbdiff-popup',
which in turn is available in the Magit diff popup, bound by
default to "i" (for "interdiff").  So, with the default
keybindings, you can invoke the tbdiff popup with "di".

When Magit-tbdiff is installed from MELPA, no additional setup is
needed (aside installing git-tbdiff so that Git can find it [1]).
The tbdiff popup will be added under the Magit diff popup, and
Magit-tbdiff will be loaded the first time that the tbdiff popup is
invoked.

[1] https://github.com/trast/tbdiff
[2] https://public-inbox.org/git/87ip2pfs19.fsf@linux-k42r.v.cablecom.net/
