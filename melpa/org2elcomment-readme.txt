                            _______________

                             ORG2ELCOMMENT

                              Junpeng Qiu
                            _______________


Table of Contents
_________________

1 Overview
2 Usage
3 Customization


Convert `org-mode' file to Elisp comments.


1 Overview
==========

  This simple package is mainly used for Elisp package writers. After
  you've written the `README.org' for your package, you can use
  `org2elcomment' to convert the org file to Elisp comments in the
  corresponding source code file.


2 Usage
=======

  Make sure your source code file has `;;; Commentary:' and `;;; Code:'
  lines. The generated comments will be put between these two lines. If
  you use `auto-insert', it will take care of generating a standard file
  header that contains these two lines in your source code.

  In your Org file, invoke `org2elcomment', select the source code file,
  and done! Now take a look at your source code file, you can see your
  Org file has been converted to the comments in your source code file.


3 Customization
===============

  Behind the scenes, this package uses `org-export-as' function and the
  default backend is `ascii'. You can change to whatever backend that
  your org-mode export engine supports, such as `md' (for markdown):
  ,----
  | (setq org2elcomment-backend 'md)
  `----
