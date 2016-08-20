This is my rifle.  There are many like it, but this one is mine.
My rifle is my best friend. It is my life.  I must master it as I
must master my life.

What does my rifle do?  It searches rapidly through my Org files,
quickly bringing me the information I need to defeat the enemy.

This package is a continuation of the fantastic
org-search-goto/org-search-goto-ml packages, now with Helm
support. It searches both headings and contents of entries in Org
buffers, and it displays entries that match all search terms,
whether the terms appear in the heading, the contents, or both.
Matching portions of entries' contents are displayed with
surrounding context to make it easy to acquire your target.

Entries are fontified by default to match the appearance of an Org
buffer, and optionally the entire path can be displayed for each
entry, rather than just its own heading.

Installation

MELPA

If you installed from MELPA, your rifle is ready.  Just run one of
the commands below.

Manual

Install the Helm, dash.el, f.el, and s.el packages.  Then require
this package in your init file:

(require 'helm-org-rifle)

Usage

Run one of the rifle commands, type some words, and results will be
displayed, grouped by buffer.  Hit "RET" to show the selected
entry, or <C-return> to show it in an indirect buffer.

Commands:
+ `helm-org-rifle': Shows results from all open Org buffers
+ `helm-org-rifle-current-buffer': Shows results from current buffer
+ `helm-org-rifle-directories': Shows results from selected directories; with prefix, recursively
+ `helm-org-rifle-files': Shows results from selected files

Tips

+ Show results from certain buffers by typing the name of the
  buffer (usually the filename).
+ Show headings with certain todo keywords by typing the keyword,
  e.g. =TODO= or =DONE=.
+ Show headings with certain priorities by typing, e.g. =#A= or
  =[#A]=.
+ Show headings with certain tags by searching for, e.g. =:tag1:=.
+ Exclude results with a =!=, e.g. =pepperoni !anchovies=.
+ Show entries in an indirect buffer by selecting that action from
  the Helm actions list, or by pressing =<C-return>=.
+ You can customize the =helm-org-rifle= group if you like.
