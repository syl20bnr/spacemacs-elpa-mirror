Support for creation and update of file headers.

Some of this code and commentary were originally written by Lynn
Slater as file `header.el'.  Drew Adams updated it and maintains it
as `header2.el'.  The original is here:
`http://www.emacswiki.org/download/OriginalHeaderEl'.

Commands (interactive functions) defined here:

  `make-header', `make-revision', `make-divider',
  `make-box-comment', `make-box-comment-region',
  `update-file-header'.

Other functions defined here:

  `auto-make-header', `auto-update-file-header',
  `delete-and-forget-line', `header-AFS', `header-author',
  `header-blank', `header-code', `header-commentary',
  `header-compatibility', `header-copyright',
  `header-creation-date', `header-date-string',
  `header-description', `header-doc-url',`header-end-line',
  `header-eof', `header-file-name', `header-free-software',
  `header-history', `header-keywords', `header-lib-requires',
  `header-maintainer', `header-mode-line',
  `header-modification-author', `header-modification-date',
  `header-multiline', `header-pkg-requires',
  `header-prefix-string', `header-rcs-id', `header-rcs-log',
  `header-sccs', `header-shell', `header-status', `header-title',
  `header-toc', `header-update-count', `header-url',
  `header-version', `headerable-file-p', `make-box-comment',
  `make-divider', `make-revision', `nonempty-comment-end',
  `nonempty-comment-start', `register-file-header-action',
  `section-comment-start', `true-mode-name', `uniquify-list',
  `update-file-name', `update-last-modified-date',
  `update-last-modifier', `update-lib-requires',
  `update-write-count'.

User options (variables) defined here:

  `header-copyright-notice', `header-date-format',
  `header-history-label', `header-max',
  `make-box-comment-region-replace-prefix-flag',
  `make-header-hook'.

Other variables defined here:

  `file-header-update-alist', `header-auto-update-enabled',
  `header-multiline', `header-prefix-string', `return-to'.


To have Emacs update file headers automatically whenever you save a
file, put this in your init file (~/.emacs):

  (autoload 'auto-update-file-header "header2")
  (add-hook 'write-file-hooks 'auto-update-file-header)

To have Emacs add a file header whenever you create a new file in
some mode, put this in your init file (~/.emacs):

  (autoload 'auto-make-header "header2")
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header)
  ...



From the original header.el text by Lynn Slater:

    This file is particularly useful with the file-declarations
    package also by Lynn Slater.  Read the first 20% of this file
    to learn how to customize.

    From: eddie.mit.edu!think!ames!indetech.com!lrs (Lynn Slater)
    To: info-gnu-emacs@prep.ai.mit.edu
    Subject: Automatic header creation and maintenance
    Date: Wed, 1 Nov 89 09:33 PST

    Enclosed is code to automatically create and maintain file
    headers.  This code is cleaner and mush more easily customized
    than any of my previous header postings.

    New in this release are customizations that allow headers to be
    created and maintained from the command line.  This is good for
    projects with some vi die-hards or when headers are being added
    in mass for the first time.

    Example:
       cd $EMACS/lisp
       headers -make *.el

    I have found file headers to be very valuable in project
    development.  I always know who has been where and how many
    times they were there.  Most often, I also know what they did.
    The update count and last modified date are very useful in
    determining the proper version of a file to use.  I have often
    thought that it would be easier to integrate patches from
    individuals to gnu tools such as gcc and g++ if I knew for
    certain what version of a particular file they were working
    from.  If all had headers, I would see the update count and
    date in the "diff -c" output and would be able to find or
    recreate the file to patch accordingly.

    In this message are three files:
      header.el - Emacs header functions and instructions
      headers.1  - Man page for command line headers useage
      headers    - Shell script for command-line headers.

Text by Lynn Slater, updated as needed:

    Mode-specific headers:
    ---------------------
     Not all headers need look alike.  Suppose that you have a unix script mode
     and want it to have a shell specifier line that all other headers do not
     have.  To do this, Place the following line in a hook called when the
     mode is invoked or in the code that establishes the mode:

        (add-hook 'make-header-hook 'header-shell nil t)

     The header building blocks are sensitive to the different comment
     characters in different modes.

    Mode specific update actions:
    ----------------------------
     Suppose something needs to be automatically maintained only in certain
     modes.  An example is the .TH macro in man pages.  You can create mode-
     specific update actions by placing code like the following in the
     mode creation function of the mode hook.

       (register-file-header-action
         "^\.TH[ \t]+[^\" \t]+[ \t]+[^\" \t]+[ \t]+\"\\([^\"]*\\)\""
        'update-last-modified-date-macro)

    Define individual header elements.  These are the building blocks
    used to construct a site specific header.  You may add your own
    functions either in this file or in your `.emacs' file.  The
    variable `make-header-hook' specifies the functions that will
    actually be called.

Note on change-control systems:

 If you use `header2.el' in a change-control system, such as RCS,
 you might need to leave it checked out.  This is because any
 change-control keywords in the file will be expanded during
 check-in.  Normally, you will want those keywords to be inserted
 in file headers unexpanded.
