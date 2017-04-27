To paste the current buffer to KDE's pastebin service, use the
command `paste-kde-buffer'.  To paste a region, `paste-kde-region'.

paste-kde will lookup the language using the buffer's major mode
and a table in `paste-kde-langs'.  If there isn't a match,
paste-kde will paste the code as Text.  If you use a prefix before
running the paste command, you'll be asked about the language.

Each paste is private by default.  This means that the url will
contain a hash in the end of it and the paste will not be listed on
the paste list.  Even though the paste is private, paste-kde uses
http to send the paste, so an adversary can eavesdrop on your
comunication with the server.

After pasting the code, the paste's url will be open using
`browser-url' and the server is instructed to delete the paste
after one week.  You can customize these and other settings with
M-x customize-group RET paste-kde RET.

It's worth mentioning that the table of languages and modes are not
necessarily correct as it was generated programmatic.  If something
is wrong, please patch it.
