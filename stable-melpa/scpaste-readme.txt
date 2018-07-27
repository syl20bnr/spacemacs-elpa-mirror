This will place an HTML copy of a buffer on the web on a server
that the user has shell access on.

It's similar in purpose to services such as http://paste.lisp.org
or http://rafb.net, but it's much simpler since it assumes the user
has an account on a publicly-accessible HTTP server. It uses `scp'
as its transport and uses Emacs' font-lock as its syntax
highlighter instead of relying on a third-party syntax highlighter
for which individual language support must be added one-by-one.

Install

Add Marmalade as a package source, and then run M-x package-install
scpaste.

Set `scpaste-http-destination' and `scpaste-scp-destination' to
appropriate values, and add this to your Emacs config:

(setq scpaste-http-destination "http://p.hagelb.org"
      scpaste-scp-destination "p.hagelb.org:p.hagelb.org")

If you have a different keyfile, you can set that, too:
(setq scpaste-scp-pubkey "~/.ssh/my_keyfile.pub")

If you use a non-standard ssh port, you can specify it by setting
`scpaste-scp-port'.

If you need to use alternative scp and ssh programs, you can set
`scpaste-scp' and `scpaste-ssh'. For example, scpaste works with the Putty
suite on Windows if you set these to pscp and plink, respectively.

Optionally you can set the displayed name for the footer and where
it should link to:
(setq scpaste-user-name "Technomancy"
      scpaste-user-address "http://technomancy.us/")

Usage

M-x scpaste, enter a name, and press return. The name will be
incorporated into the URL by escaping it and adding it to the end
of `scpaste-http-destination'. The URL for the pasted file will be
pushed onto the kill ring.

You can autogenerate a splash page that gets uploaded as index.html
in `scpaste-http-destination' by invoking M-x scpaste-index. This
will upload an explanation as well as a listing of existing
pastes. If a paste's filename includes "private" it will be skipped.

You probably want to set up SSH keys for your destination to avoid
having to enter your password once for each paste. Also be sure the
key of the host referenced in `scpaste-scp-destination' is in your
known hosts file--scpaste will not prompt you to add it but will
simply hang.
