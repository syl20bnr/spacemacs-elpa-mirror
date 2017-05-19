;;; ghub+.el --- a thick GitHub API client built on ghub  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, multimedia, tools
;; Homepage: https://github.com/vermiculus/ghub-plus
;; Package-Requires: ((emacs "25") (ghub "1.2") (apiwrap "0.1.2"))
;; Package-Version: 0.1.4
;; Package-X-Original-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides some sugar for `ghub'.  See package `apiwrap' for
;; generated function usage instructions.

;;; Code:

(require 'url)
(require 'ghub)
(require 'apiwrap)

(eval-and-compile
  (defun ghubp--make-link (alist)
    "Create a link from an ALIST of API endpoint properties."
    (format "https://developer.github.com/v3/%s" (alist-get 'link alist)))

  (defun ghubp--stringify-params (params)
    "Process PARAMS from textual data to Lisp structures."
    (mapcar (lambda (p)
              (let ((k (car p)) (v (cdr p)))
                (cons k (alist-get v '((t . "true") (nil . "false")) v))))
            params))

  (defun ghubp--remove-api-links (object &optional preserve-objects)
    "Remove everything in OBJECT that points back to `api.github.com'.

If PRESERVE-OBJECTS is non-nil, those objects will not be
stripped of references."
    ;; execution time overhead of 0.5%
    (let ((recurse (lambda (o) (ghubp--remove-api-links o preserve-objects))))
      (delq nil (if (and (consp object) (consp (car object)))
                    (mapcar recurse object)
                  (if (consp object)
                      (if (memq (car object) preserve-objects)
                          object
                        (unless (and (stringp (cdr object))
                                     (string-match-p (rx bos (+ alnum) "://api.github.com/")
                                                     (cdr object)))
                          (cons (car object)
                                (if (consp (cdr object))
                                    (mapcar recurse (cdr object))
                                  (cdr object))))))))))

  (defun ghubp--pre-process-params (params)
    (thread-first params
      (ghubp--stringify-params)))

  (defun ghubp--post-process (object &optional preserve-objects)
    (thread-first object
      (ghubp--remove-api-links preserve-objects)))

  (apiwrap-new-backend
   "GitHub" "ghubp"
   '((repo . "REPO is a repository alist of the form returned by `ghubp-get-user-repos'.")
     (org  . "ORG is an organization alist of the form returned by `ghubp-get-user-orgs'.")
     (thread . "THREAD is a thread"))
   :get #'ghub-get :put #'ghub-put :head #'ghub-head
   :post #'ghub-post :patch #'ghub-patch :delete #'ghub-delete

   :link #'ghubp--make-link
   :post-process #'ghubp--post-process
   :pre-process-params #'ghubp--pre-process-params))

;;; Utilities
(defmacro ghubp-unpaginate (&rest body)
  "Unpaginate API responses and execute BODY.
See `ghub-unpaginate'."
  `(let ((ghub-unpaginate t)) ,@body))

(defun ghubp-keep-only (structure object)
  "Keep a specific STRUCTURE in OBJECT.
See URL `http://emacs.stackexchange.com/a/31050/2264'."
  (declare (indent 1))
  (if (and (consp object) (consp (car object)) (consp (caar object)))
      (mapcar (apply-partially #'ghubp-keep-only structure) object)
    (mapcar (lambda (el)
              (if (consp el)
                  (cons (car el)
                        (ghubp-keep-only (cdr el) (alist-get (car el) object)))
                (cons el (alist-get el object))))
            structure)))

;;; Repositories
(defapiget-ghubp "/repos/:owner/:repo/collaborators"
  "List collaborators."
  "repos/collaborators/#list-collaborators"
  (repo) "/repos/:repo.owner.login/:repo.name/comments")

(defapiget-ghubp "/repos/:owner/:repo/comments"
  "List commit comments for a repository."
  "repos/comments/#list-commit-comments-for-a-repository"
  (repo) "/repos/:repo.owner.login/:repo.name/comments")

;;; Issues
(defapiget-ghubp "/issues"
  "List all issues assigned to the authenticated user across all
visible repositories including owned repositories, member
repositories, and organization repositories."
  "issues/#list-issues")

(defapiget-ghubp "/repos/:owner/:repo/issues/:number"
  "Get a single issue."
  "issues/#get-a-single-issue"
  (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number")

(defapiget-ghubp "/user/issues"
  "List all issues across owned and member repositories assigned
to the authenticated user."
  "issues/#list-issues")

(defapiget-ghubp "/orgs/:org/issues"
  "List all issues for a given organization assigned to the
authenticated user."
  "issues/#list-issues"
  (org) "/org/:org.login/issues")

(defapiget-ghubp "/repos/:owner/:repo/issues"
  "List issues for a repository."
  "issues/#list-issues-for-a-repository"
  (repo) "/repos/:repo.owner.login/:repo.name/issues")

(defapiget-ghubp "/repos/:owner/:repo"
  "List issues for a repository."
  "issues/#list-issues-for-a-repository"
  (repo) "/repos/:repo.owner.login/:repo.name")

(defapiget-ghubp "/repos/:owner/:repo/labels"
  "List labels for a repository"
  "issues/labels/#list-all-labels-for-this-repository"
  (repo) "/repos/:repo.owner.login/:repo.name/labels")

(defapiget-ghubp "/repos/:owner/:repo/commits/:ref/statuses"
  "List statuses for a specific ref"
  "repos/statuses/#list-statuses-for-a-specific-ref"
  (repo ref) "/repos/:repo.owner.login/:repo.name/commits/:ref/statuses")

(defapiget-ghubp "/repos/:owner/:repo/commits/:ref/status"
  "Get the combined status for a specific ref"
  "repos/statuses/#get-the-combined-status-for-a-specific-ref"
  (repo ref) "/repos/:repo.owner.login/:repo.name/commits/:ref/status")

(defapiget-ghubp "/user"
  "Return the currently authenticated user"
  "users/#get-the-authenticated-user")

(defapiget-ghubp "/user/repos"
  "Return repositories of the currently authenticated user"
  "issues/#list-issues-for-a-repository")

(defapiget-ghubp "/notifications"
  "List all notifications for the current user, grouped by repository"
  "activity/notifications/#list-your-notifications"
  :post-process (lambda (o) (ghubp--post-process o '(subject))))

(defapipost-ghubp "/repos/:owner/:repo/issues"
  "Create an issue.
Any user with pull access to a repository can create an issue."
  "issues/#create-an-issue"
  (repo) "/repos/:repo.owner.login/:repo.name/issues")

(defapipatch-ghubp "/repos/:owner/:repo/issues/:number"
  "Issue owners and users with push access can edit an issue."
  "issues/#edit-an-issue"
  (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number")

(defapipatch-ghubp "/notifications/threads/:id"
  ""
  "activity/notifications/#mark-a-thread-as-read"
  (thread) "/notifications/threads/:thread.id")

(defapipost-ghubp "/repos/:owner/:repo/forks"
  "Create a fork for the authenticated user."
  "repos/forks/#create-a-fork"
  (repo) "/repos/:repo.owner.login/:repo.name/forks")

(defapipost-ghubp "/repos/:owner/:repo/pulls"
  "Open a pull request."
  "pulls/#create-a-pull-request"
  (repo) "/repos/:repo.owner.login/:repo.name/pulls"
  :validate-data
  (lambda (o)
    (--all? (let ((v (alist-get it o)))
              (and v (stringp v) (< 0 (length v))))
            '(title head base))))

(defapipost-ghubp "/user/repos"
  "Create a fork for the authenticated user."
  "repos/forks/#create-a-fork")

(defapiget-ghubp "/notifications/threads/:id"
  "Adds Mlatest_comment_url-callback and Murl-callback to .subject"
  "activity/notifications/#view-a-single-thread"
  (thread) "/notifications/threads/:thread.id"
  :post-process (lambda (o) (ghubp--post-process o '(subject))))

(defapipost-ghubp "/repos/:owner/:repo/issues/:number/comments"
  "Post a comment to an issue"
  "issues/comments/#create-a-comment"
  (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number/comments")

(defapiget-ghubp "/repos/:owner/:repo/issues/:number/comments"
  "List comments on an issue"
  "issues/comments/#list-comments-on-an-issue"
  (repo issue) "/repos/:repo.owner.login/:repo.name/issues/:issue.number/comments")

(defun ghubp-url-parse (url)
  "Parse URL for its type and API callback.

A cons cell is returned.  The car is one of

 - `issue'
 - `pull-request'

and the cdr is a callback suitable for `ghub-get', etc."
  (let ((callback (url-filename (url-generic-parse-url url))))
    (cons
     (cond
      ((string-match-p (rx bol "/repos/" (+? any) "/" (+? any) "/issues/" (+ digit) eol)
                       callback)
       'issue)
      ((string-match-p (rx bol "/repos/" (+? any) "/" (+? any) "/pulls/" (+ digit) eol)
                       callback)
       'pull-request)
      (t 'unknown))
     callback)))

(provide 'ghub+)
;;; ghub+.el ends here
