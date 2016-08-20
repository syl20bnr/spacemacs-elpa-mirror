;;; ttrss.el --- Tiny Tiny RSS elisp bindings

;; Copyright (C) 2013 Pedro Silva

;; Author: Pedro Silva <psilva+git@pedrosilva.pt>
;; Created: 01 April 2013
;; Version: 1.7.5
;; Package-Version: 20130409.1049
;; Keywords: news, local
;; Homepage: https://github.com/pedros/ttrss.el
;; Package-Requires: ((emacs "23.1"))

;; This file is not part of GNU Emacs.

;; ttrss is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ttrss is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ttrss.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Introduction

;; ttrss.el provides an interface to the Tiny Tiny RSS API. It targets
;; API level 5, and has been developed and tested against version
;; 1.7.5. As such, the versioning on this file tracks that of ttrss
;; itself.  You can read more about Tiny Tiny RSS at
;; <http://tt-rss.org/redmine/projects/tt-rss/wiki>.

;; Scope

;; The current version of ttrss.el covers all methods as documented at
;; <http://tt-rss.org/redmine/projects/tt-rss/wiki/JsonApiReference>.
;; It purposefully doesn't offer any additional higher level methods.

;; Installation

;; (add-to-list 'load-path "/path/to/parent/directory")
;; (require 'ttrss)

;; Usage

;; (setq ttrss-address "http://example.com/ttrss/api/"
;;       ttrss-user "user"
;;       ttrss-password "pass")

;; (let ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
;;   (if (ttrss-logged-in-p ttrss-address ttrss-sid)
;;       (with-current-buffer (switch-to-buffer "*ttrss reader*")
;; 	(erase-buffer)
;; 	(message "Logged in to %s as %s with password %s\n"
;; 		 ttrss-address
;; 		 ttrss-user
;; 		 ttrss-password)
;; 	(message "Server running version %s and API level %d\n"
;; 		 (ttrss-get-version ttrss-address ttrss-sid)
;; 		 (ttrss-get-api-level ttrss-address ttrss-sid))
;; 	(message "There are %s unread articles in %d feeds"
;; 		 (ttrss-get-unread ttrss-address ttrss-sid)
;; 		 (length (ttrss-get-feeds ttrss-address ttrss-sid :unread_only t)))
;; 	(insert "The grand <b>*ttrss* reader!</b>\n\n---\n\n")
;; 	(mapcar (lambda (pl) (insert (format "%s\n%s\n\n%s\n---\n"
;; 					     (plist-get pl :title)
;; 					     (plist-get pl :link)
;; 					     (plist-get pl :content))))
;; 		(ttrss-get-headlines ttrss-address ttrss-sid :feed_id -3 :limit 3 :show_content t))
;; 	(html2text))
;;     (message "Login failed")))

;; Design

;; I was motivated to write this library to support a new backend for
;; Gnus, 'nnttrss.el'. Its design is kept as functional as possible.
;; That means that it makes no use of global state, amongst other
;; things.

;; TODO

;; * Interactive versions of the methods employing global state:
;;   You'll note that there are, in fact, a number of variables defined
;;   at the top-level, namely 'ttrss-address', 'ttrss-user',
;;   'ttrss-password', 'ttrss-sid', 'ttrss-api-level', and
;;   'ttrss-server-version'. These are defined in anticipation of the
;;   next release, in which I plan to introduce interactive counterparts
;;   of most methods that do employ global state.

;; * variable 'nnttrss-debug':
;;   - Trace methods in the mini-buffer
;;   - Keep a log buffer with requests and responses data structures

;;; Change Log:

;; Version 0.0.1: First public release of this package.


;;; Code:

(eval-and-compile
  (autoload 'url-retrieve "url")
  (autoload 'url-retrieve-synchronously "url")
  (autoload 'json-read "json")
  (autoload 'json-encode "json"))

(defgroup ttrss nil
  "Tiny Tiny RSS elisp bindings"
  :group 'external
  :group 'communication
  :link '(url-link :tag "Tiny Tiny RSS"
		   "http://tt-rss.org/redmine/projects/tt-rss/wiki"))

(defcustom ttrss-address "http://localhost"
  "Address of the tt-rss server."
  :type 'string
  :group 'ttrss
  :link '(info-link "(url)http/https"))

(defcustom ttrss-user ""
  "Username to use for authentication to the tt-rss server."
  :type 'string
  :group 'ttrss)

(defcustom ttrss-password ""
  "Password to use for authentication to the tt-rss server."
  :type 'string
  :group 'ttrss)

(defvar ttrss-sid nil
  "Current session id, if any, set after successful login.")

(defvar ttrss-api-level nil
  "API version level, increased with each API functionality change.")

(defvar ttrss-server-version nil
  "Server version number.")


;;; Utilities

(defun ttrss-post-request (address property &rest content)
  "Post to ADDRESS and possibly retrieve PROPERTY from the response to CONTENT.
CONTENT must be a data structure that `json-encode' knows how to
encode as a JSON object.

Returns the JSON response as a property list (or, optionally, the
PROPERTY in the property list) if the response status is 0, nil
otherwise."
  (let ((url-request-method "POST")
	(url-request-data (json-encode content)))
    (with-current-buffer (url-retrieve-synchronously address)
      (ttrss-parse-request nil property))))

(defun ttrss-post-request-asynchronously (address &rest content)
  "Asynchronously post to ADDRESS the key-value pairs CONTENT.
Like 'ttrss-post-request', but perform request asynchronously
with 'ttrss-parse-request' as a callback."
  (let ((url-request-method "POST")
	(url-request-data (json-encode content)))
    (url-retrieve address 'ttrss-parse-request)))

(defun ttrss-parse-request (url-status &rest property)
  "Parse a url response buffer with URL-STATUS.
Return a property list of the response, or, optionally, the
PROPERTY value of said property list."
  (let ((json-object-type 'plist)
	(json-array-type 'list)
	(json-false nil))
    (goto-char (point-min))
    (search-forward-regexp "\n\n")
    (let* ((response (json-read))
	   (ttrss-status (plist-get response :status))
	   (content (plist-get response :content)))
      (if (= ttrss-status 1)
	  (user-error "API status error: %s"
		      (mapconcat 'downcase
				 (split-string (plist-get content :error) "_")
				 " "))
	(let ((value (or (plist-get content (car property)) content)))
	  (message "API status OK")
	  value)))))


;;; Session management

(defun ttrss-login (address user password)
  "Login to the server at ADDRESS using USER and PASSWORD.
Returns a session id string."
  (ttrss-post-request address
		      :session_id
		      :op "login"
		      :user user
		      :password password))

(defun ttrss-logout (address sid)
  "Logout of the server at ADDRESS using SID.
Returns a status string (typically 'OK')."
  (ttrss-post-request address
		      :status
		      :op "logout"
		      :sid sid))

(defun ttrss-logged-in-p (address sid)
  "Return t if there is a valid session at ADDRESS with SID."
  (ttrss-post-request address
		      :status
		      :op "isLoggedIn"
		      :sid sid))


;;; Server statistics

(defun ttrss-get-api-level (address sid)
  "Return the API level at ADDRESS using SID."
  (ttrss-post-request address
		      :level
		      :op "getApiLevel"
		      :sid sid))

(defun ttrss-get-version (address sid)
  "Return the server version at ADDRESS using SID."
  (ttrss-post-request address
		      :version
		      :op "getVersion"
		      :sid sid))

(defun ttrss-get-config (address sid &optional property)
  "Return configuration property list of server at ADDRESS using SID.
Optionally, retrieve PROPERTY only.  The property list members are:

'icons_dir'
     Path to icons on the server filesystem.

'icons_url'
     Path to icons when requesting them over http.

'daemon_is_running'
     Whether update daemon is running.

'num_feeds'
     Amount of subscribed feeds."
  (ttrss-post-request address
		      property
		      :op "getConfig"
		      :sid sid))

(defun ttrss-get-pref (address sid preference)
  "Return value, at ADDRESS using SID, given by PREFERENCE.
PREFERENCE must be one of the following strings:

'allow_duplicate_posts'
    Allow duplicate posts.

'auto_assign_labels'
    Assign articles to labels automatically.

'blacklisted_tags'
    Blacklisted tags.

'cdm_auto_catchup'
    Automatically mark articles as read.

'cdm_expanded'
    Automatically expand articles in combined mode.

'combined_display_mode'
    Combined feed display.

'confirm_feed_catchup'
    Confirm marking feed as read.

'default_article_limit'
    Amount of articles to display at once.

'default_update_interval'
    Default interval between feed updates.

'digest_catchup'
    Mark articles in e-mail digest as read.

'digest_enable'
    Enable e-mail digest.

'digest_preferred_time'
    Try to send digests around specified time.

'enable_api_access'
    Enable external API.

'enable_feed_cats'
    Enable feed categories.

'feeds_sort_by_unread'
    Sort feeds by unread articles count.

'fresh_article_max_age'
    Maximum age of fresh articles (in hours).

'hide_read_feeds'
    Hide feeds with no unread articles.

'hide_read_shows_special'
    Show special feeds when hiding read feeds.

'long_date_format'
    Long date format.

'on_catchup_show_next_feed'
    On catchup show next feed.

'purge_old_days'
    Purge articles after this number of days.

'purge_unread_articles'
    Purge unread articles.

'reverse_headlines'
    Reverse headline order (oldest first).

'short_date_format'
    Short date format.

'show_content_preview'
    Show content preview in headlines list.

'sort_headlines_by_feed_date'
    Sort headlines by feed date.

'ssl_cert_serial'
    Login with an SSL certificate.

'strip_images'
    Do not embed images in articles.

'strip_unsafe_tags'
    Strip unsafe tags from articles.

'user_css_theme'
    Select theme.

'user_stylesheet'
    Customize stylesheet.

'user_timezone'
    User timezone.

'vfeed_group_by_feed'
    Group headlines in virtual feeds."
  (ttrss-post-request address
		      :value
		      :op "getPref"
		      :sid sid
		      :pref_name preference))

(defun ttrss-get-unread (address sid)
  "Return number of unread articles at ADDRESS using SID."
  (ttrss-post-request address
		      :unread
		      :op "getUnread"
		      :sid sid))


;;; Feed listings

(defun ttrss-get-counters (address sid &optional output-mode)
  "Return list of property lists of counters at ADDRESS using SID.
OUTPUT-MODE is a string of what kind of counter to return:
feed (\"f\"), category (\"c\"), label (\"l\"), or tag (\"t\").
The default is \"flc\".  The property list members are:

'counter'
    Number of articles in the feed.

'id'
    Feed ID.

'has_img'
    Feed has a thumbnail image available (optional).

'updated'
    Timestamp of last update in \"HH:MM\" format (optional).

'kind'
    Type of feed (optional)."
  (ttrss-post-request address
		      nil
		      :op "getCounters"
		      :sid sid))

(defun ttrss-get-feeds (address sid &rest params)
  "Return a list of property lists of feeds at ADDRESS using SID.
PARAMS is any number of the following property-value pairs:

'cat_id'
    Return feeds under category CAT_ID (integer).

'unread_only'
    Only return feeds which have unread articles (boolean).

'limit'
    Limit amount of feeds returned to this value (integer).

'offset'
    Skip this amount of feeds first (integer).

'include_nested'
    Include child categories (boolean).

The property list's members are:

'last_updated'
    Timestamp of last update in unix epoch format.

'cat_id'
    Category ID.

'order_id'
    Sort index of the feed, if any.

'feed_url'
    URL of subscription.

'unread'
    Number of unread articles in feed.

'title'
    Title of feed.

'id'
    Feed ID.

'has_icon'
    Feed has a thumbnail image available.

The following feed IDs are special:

 0: Uncategorized
-1: Virtual (starred, published, archived, fresh)
-2: Labels
-3: All feeds excluding virtual feeds and labels
-4: All feeds."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getFeeds"
	 :sid sid
	 params))

(defun ttrss-get-categories (address sid &rest params)
  "Return list of category plists at ADDRESS using SID.
PARAMS is any number of the following key-value pairs:

'unread_only'
    Only return categories which have unread articles (boolean).

'enable_nested'
    Nested mode: flatten everything into topmost categories (boolean).

'include_empty'
    Include empty categories (boolean)."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getCategories"
	 :sid sid
	 params))

(defun ttrss-get-labels (address sid &optional article-id)
  "Return a list of label property lists at ADDRESS using SID.
Optionally, checks whether ARTICLE-ID has been set to any of the
labels.  The property list's members are:

'id'
    Label ID.  Note that this is an internal database ID of the label
    (feed ID = -11 - label ID).

'caption'
    Description text.

'fg_color'
    Foreground color of label in web interface.

'bg_color'
    Background color of label in web interface.

'checked'
    Set to t if ARTICLE-ID has this label."
  (ttrss-post-request address
		      nil
		      :op "getLabels"
		      :sid sid))


;;; Feed manipulation

(defun ttrss-update-feed (address sid feed-id)
  "Ask server at ADDRESS using SID to update FEED-ID.
This operation is not performed in the background by the server,
so it might take considerable time and, potentially, be aborted
by the HTTP server.  Therefore, this function performs the
request asynchronously.  A message is printed in the mini-buffer
area when the request completes."
  (ttrss-post-request-asynchronously address
				     :op "updateFeed"
				     :sid sid
				     :feed_id feed-id))

(defun ttrss-set-article-label (address sid article-ids label-id
					&optional assign)
  "Update, at ADDRESS using SID, ARTICLE-IDS with LABEL-ID.
Assign labels if ASSIGN is t, remove otherwise.  Returns number
of articles updated."
  (ttrss-post-request address
		      :updated
		      :op "setArticleLabel"
		      :sid sid
		      :article-ids (mapconcat (lambda (i) (format "%d" i))
					      article-ids
					      ",")
		      :label_id label-id
		      :assign assign))


;;; Article listings

(defun ttrss-get-headlines (address sid &rest params)
  "Return a list of headline plists at ADDRESS using SID.
PARAMS is any number of the following key-value pairs:

'feed_id'
    Only output articles for this feed (integer).
    The following feed IDs are special:

     0: Archived
    -1: Starred
    -2: Published
    -3: Fresh
    -4: All articles
    less than -10: Labels

'limit'
    Limits the amount of returned articles (integer).

'skip'
    Skip this amount of feeds first (integer).

'filter'
     Currently unused (string).

'is_cat'
    Requested feed_id is a category (boolean).

'show_excerpt'
    Include article excerpt in the output (boolean).

'show_content'
    Include full article text in the output (boolean).

'view_mode'
     All_articles, unread, adaptive, marked, updated (string).

'include_attachments'
    Include article attachments (boolean).

'since_id'
    Articles with id greater than since_id (integer).

'include_nested'
    Include articles from child categories (boolean).

'order_by'
    Override default sort order: 'date_reverse' or 'feed_dates' (string).

'search'
    Search query (string).

'search_mode'
    (string).

'match_on'
    (string)."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getHeadlines"
	 :sid sid
	 params))

(defun ttrss-get-article (address sid &rest article-ids)
  "Return article property list from ADDRESS using SID with ARTICLE-IDS.
Returns nil or an empty list if those IDs can't be found.  The
property list's members are:

'attachments'
    List of RSS enclosures.  Each enclosure is a
    property list with the following members:

    'content_type'
        MIME type of the attachment.

    'id'
        Attachment ID.

    'duration'
        ???.

    'content_url'
        URL of the attachment.

    'post_id'
        Parent article ID.

    'title'
        Title of the attachment.

'title'
      Article title.

'labels'
      Article labels.

'published'
      Whether article is in 'Published' virtual feed.

'link'
      URL of the article.

'content'
      HTML content of the article.

'id'
      Article ID.

'marked'
      Whether article is in 'Starred' virtual feed.

'unread'
      Whether article is in 'Fresh' virtual feed.

'comments'
      ...

'Author'
      Article author..

'updated'
      Timestamp of last update in unix epoch format..

'feed_id'
      Feed ID."
  (when article-ids
    (ttrss-post-request address
			nil
			:op "getArticle"
			:sid sid
			:article_id (mapconcat (lambda (i) (format "%d" i))
					       article-ids
					       ","))))


;;; Article manipulation

(defun ttrss-update-article (address sid article-ids &rest params)
  "Update, at ADDRESS using SID, ARTICLE-IDS based on PARAMS.
Returns number of articles updated.  PARAMS is any number of the
following key-value pairs:

'mode'
    0: false, 1: true, 2: toggle.

'field'
    0: starred, 1: published, 2: unread, 3: article.

'data'
    Optional data parameter when setting note field (string)."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "updateArticle"
	 :sid sid
	 :article_ids article-ids
	 params))



(defun ttrss-catchup-feed (address sid feed-id)
  "Catchup, at ADDRESS using SID, FEED-ID.
Returns a status string (typically 'OK')."
  (ttrss-post-request address
		      :status
		      :op "catchupFeed"
		      :sid sid
		      :feed_id feed-id))

(defun ttrss-share-to-published (address sid title url content)
  "Publish, at ADDRESS using SID, an article with TITLE,  URL, and CONTENT.
Returns a status string (typically 'OK')."
  (ttrss-post-request address
		      :status
		      :op "shareToPublished"
		      :sid sid
		      :title title
		      :url url
		      :content content))

(defun ttrss-subscribe-to-feed (address sid feed-url &rest params)
  "Subscribe, at ADDRESS using SID, to FEED-URL with PARAMS.
Returns 0 if the feed already exists, 1 if successfully added,
and nil otherwise.  PARAMS is any number of the following
key-value pairs:

'category-id'
    Category ID to place feed into (defaults to 0: Uncategorized)

'login'
    Username to use for basic HTTP authentication at FEED-URL.

'password'
    Password to use for basic HTTP authentication at FEED-URL."
  (plist-get (apply 'ttrss-post-request
		    address
		    :status
		    :op "subscribeToFeed"
		    :sid sid
		    :feed_url feed-url
		    params)
	     :code))

(defun ttrss-unsubscribe-feed (address sid feed-id)
  "Unsubscribe, at ADDRESS using SID, from FEED-ID."
  (ttrss-post-request address
		      :status
		      :op "unsubscribeFeed"
		      :sid sid
		      :feed_id feed-id))

(defun ttrss-get-feed-tree (address sid &optional include-empty)
  "Return, at ADDRESS using SID, full tree of categories and feeds.
Optionally INCLUDE-EMPTY categories."
  (ttrss-post-request address
		      nil
		      :op "getFeedTree"
		      :sid sid
		      :include_empty include-empty))

(provide 'ttrss)
;;; ttrss.el ends here
