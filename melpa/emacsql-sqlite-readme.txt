During package installation EmacSQL will attempt to compile a
custom native binary for communicating with a SQLite database. If
this fails (a C compiler is not available), it will attempt to
download, with permission, a pre-built binary when the first
database connection is attempted.
