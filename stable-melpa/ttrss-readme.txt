Introduction

ttrss.el provides an interface to the Tiny Tiny RSS API. It targets
API level 5, and has been developed and tested against version
1.7.5. As such, the versioning on this file tracks that of ttrss
itself.  You can read more about Tiny Tiny RSS at
<http://tt-rss.org/redmine/projects/tt-rss/wiki>.

Scope

The current version of ttrss.el covers all methods as documented at
<http://tt-rss.org/redmine/projects/tt-rss/wiki/JsonApiReference>.
It purposefully doesn't offer any additional higher level methods.

Installation

(add-to-list 'load-path "/path/to/parent/directory")
(require 'ttrss)

Usage

(setq ttrss-address "http://example.com/ttrss/api/"
      ttrss-user "user"
      ttrss-password "pass")

(let ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
  (if (ttrss-logged-in-p ttrss-address ttrss-sid)
      (with-current-buffer (switch-to-buffer "*ttrss reader*")
	(erase-buffer)
	(message "Logged in to %s as %s with password %s\n"
		 ttrss-address
		 ttrss-user
		 ttrss-password)
	(message "Server running version %s and API level %d\n"
		 (ttrss-get-version ttrss-address ttrss-sid)
		 (ttrss-get-api-level ttrss-address ttrss-sid))
	(message "There are %s unread articles in %d feeds"
		 (ttrss-get-unread ttrss-address ttrss-sid)
		 (length (ttrss-get-feeds ttrss-address ttrss-sid :unread_only t)))
	(insert "The grand <b>*ttrss* reader!</b>\n\n---\n\n")
	(mapcar (lambda (pl) (insert (format "%s\n%s\n\n%s\n---\n"
					     (plist-get pl :title)
					     (plist-get pl :link)
					     (plist-get pl :content))))
		(ttrss-get-headlines ttrss-address ttrss-sid :feed_id -3 :limit 3 :show_content t))
	(html2text))
    (message "Login failed")))

Design

I was motivated to write this library to support a new backend for
Gnus, 'nnttrss.el'. Its design is kept as functional as possible.
That means that it makes no use of global state, amongst other
things.

TODO

* Interactive versions of the methods employing global state:
  You'll note that there are, in fact, a number of variables defined
  at the top-level, namely 'ttrss-address', 'ttrss-user',
  'ttrss-password', 'ttrss-session-id', 'ttrss-api-level', and
  'ttrss-server-version'. These are defined in anticipation of the
  next release, in which I plan to introduce interactive counterparts
  of most methods that do employ global state.

* variable 'nnttrss-debug':
  - Trace methods in the mini-buffer
  - Keep a log buffer with requests and responses data structures
