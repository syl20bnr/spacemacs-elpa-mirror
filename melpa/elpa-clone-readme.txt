[![Build Status](https://travis-ci.org/dochang/elpa-clone.svg?branch=master)](https://travis-ci.org/dochang/elpa-clone)

Mirror an ELPA archive into a directory.

Prerequisites:

  - Emacs 24.4 or later
  - cl-lib

Installation:

To install `elpa-clone' from git repository, clone the repo, then add the
repo dir into `load-path'.

Usage:

To clone an ELPA archive `http://host/elpa' into `/path/to/elpa', invoke
`elpa-clone':

    (elpa-clone "http://host/elpa" "/path/to/elpa")

`elpa-clone' can also be invoked via `M-x'.

You can customize download interval via `elpa-clone-download-interval'.  But
note that the *real* interval is `(max elpa-clone-download-interval 5)'.

Note:

`elpa-clone' will **NOT** overwrite existing packages but will clean
outdated packages before downloading new packages.  If a package file is
broken, remove the file and call `elpa-clone' again.

License:

GPLv3
