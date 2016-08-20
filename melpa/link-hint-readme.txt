This packages gives commands for operating on visible links with avy. It is
inspired by link-hinting from vim-like browsers and browser plugins such as
pentadactyl. For example, `link-hint-open-link' will use avy to select and
open a link in the current buffer. A link can be a text, shr, mu4e or org
(htmlize) url. Mu4e attachments and mailto addresses, help mode links, and
info mode links are also considered to be links. The user can set
`link-hint-ignore-types' to can change what is considered a link. Commands
are also provided for copying links to the kill ring (and optionally the
clipboard and/or primary) and for opening multiple urls at once like with
pentadactyl's "g;".

For more information see the README in the github repo.
