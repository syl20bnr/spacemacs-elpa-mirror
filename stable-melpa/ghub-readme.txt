A minuscule client for the Github API.

This library just provides the HTTP methods.
See https://developer.github.com/v3 for valid requests.

Initial configuration
---------------------

  $ git config --global github.user <username>
  $ emacs ~/.authinfo.gpg
  # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
  machine api.github.com login <login> password <token>

To acquire a token, go to https://github.com/settings/tokens.  Note
that currently the same token is shared by all Emacs packages that
use `ghub.el'.

Usage examples
--------------

Getting details about a repository:

  (ghub-get "/repos/tarsius/ghub")

Listing names of all repositories of a user:

  (--keep (cdr (assq 'name it))
          (let ((ghub-unpaginate t))
            (ghub-get "/users/tarsius/repos")))

Making an unauthenticated request:

  (let ((ghub-authenticate nil))
    (ghub-get "/orgs/magit/repos"))

Making a request using basic authentication:

  (let ((ghub-authenticate 'basic))
    (ghub-get "/orgs/magit/repos"))

Github Enterprise support
-------------------------

Initial configuration:

  $ git config --global github.gh.example.com.user employee
  $ emacs ~/.authinfo.gpg
  # -*- epa-file-encrypt-to: ("employee@example.com") -*-
  machine gh.example.com login employee password <token>

Making a request:

  (let ((ghub-base-url "https://gh.example.com"))
    (ghub-get "/users/employee/repos"))
