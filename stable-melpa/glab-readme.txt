A minuscule client for the Gitlab API.

Initial configuration:

  $ git config gitlab.user <username>
  $ emacs ~/.authinfo.gpg
  # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
  machine .gitlab.com login <login> password <token>

Usage examples:

Get details about a project:

  (glab-get "/projects/tarsius%2Fglab")

List names of all projects owned by you:

  (--keep (cdr (assq 'name it))
          (let ((glab-unpaginate t))
            (glab-get "/projects")))

This library just provides the basic verbs.  Instead of wrapping
every resource, I recommend http://doc.gitlab.com/ce/api.  Due to
the lack of doc-strings, I also recommend having a quick look at
the source, which is quite trivial.

If you like this, then you might also like `ghub.el'; a minuscule
client for the Github API.  See https://github.com/tarsius/ghub.

If you don't like this, then you might instead like `gitlab.el`;
a big client for the Gitlab API.  Which, somewhat surprisingly,
you can get from https://github.com/nlamirault/emacs-gitlab.
