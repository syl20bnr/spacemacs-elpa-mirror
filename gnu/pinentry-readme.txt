This package allows GnuPG passphrase to be prompted through the
minibuffer instead of graphical dialog.

To use, add allow-emacs-pinentry to ~/.gnupg/gpg-agent.conf, and
start the server with M-x pinentry-start.

The actual communication path between the relevant components is
as follows:

  gpg --> gpg-agent --> pinentry --> Emacs

where pinentry and Emacs communicate through a Unix domain socket
created at:

  ${TMPDIR-/tmp}/emacs$(id -u)/pinentry

under the same directory which server.el uses.  The protocol is a
subset of the Pinentry Assuan protocol described in (info
"(pinentry) Protocol").

NOTE: As of June 2015, this feature requires newer versions of
GnuPG (2.1.5+) and Pinentry (not yet released, possibly 0.9.5+).
For details, see the discussion on gnupg-devel mailing list:
<https://lists.gnupg.org/pipermail/gnupg-devel/2015-May/029875.html>.