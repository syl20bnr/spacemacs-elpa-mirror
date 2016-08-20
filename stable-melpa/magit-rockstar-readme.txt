This package provides two commands which manipulate author and
committer dates.  You could use it to make yourself look like
a rockstar programmer who hammers out commits at one commit per
minute.  But the real purpose is to recover from heavy
re-arrangements of commits, that have causes the existing author
and committer dates to become meaningless.

I add these commands to the appropriate popups like this:

   (magit-define-popup-action 'magit-rebase-popup
     ?R "Rockstar" 'magit-rockstar)

   (magit-define-popup-action 'magit-commit-popup
     ?n "Reshelve" 'magit-reshelve)

Also included are tools that are either only useful for people
working on Magit itself and/or that aren't ready to be added to
Magit yet.  These tools might change at any time, without prior
notice or way to appeal.  This is a staging ground.  It's okay
if things ain't perfect, or if they only do what *I currently*
need but not what you (or I) think they should (eventually) be
doing instead.

Currently my init file also contains this:

   (magit-define-popup-action 'magit-fetch-popup
     ?P "Pull request" 'magit-branch-pull-request)

To use the "uncommit-extend" feature add this:

   (magit-define-popup-action 'magit-revert-popup
     ?e "Revert & edit HEAD" 'magit-uncommit-extend)
