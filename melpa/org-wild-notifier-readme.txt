This package provides notification functions for org-agenda.

To perform a one-time check use `org-wild-notifier-check'
function.
To enable timer-based notifications please use
`org-wild-notifier-mode'.
Notification times can be customized either globally (for all org
entries) through `org-wild-notifier-alert-time' variable or on per
org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
in turn is customizable via
`org-wild-notifier-alert-times-property' variable.
By default you get notifications about TODO events only.  To
customize that behavior please use
`org-wild-notifier-keyword-whitelist' variable.  In contrary, if
you don't want to receive notifications regarding certain events,
you can use `org-wild-notifier-keyword-blacklist' variable.
