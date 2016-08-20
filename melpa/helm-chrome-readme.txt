Helm interface for Chrome bookmarks.

Warning: Multiple bookmarks with the same name will be overridden.
This restriction is for better performance.  If we use Bookmark IDs with
candidate-transformer, then the speed would be quite slow.

It's also possible to scan through urls of the bookmarks.
To do so one need to customize helm-chrome-use-urls variable
for the helm-chrome group or just set it's value in config file:
(setq helm-chrome-use-urls t).
Then reload bookmarks using function helm-chrome-reload-bookmarks.

Warning: On a big number of bookmark it may be quite slow.
