;;; git-commit-insert-issue.el --- Get issues list when typing "Fixes #"

;; Copyright (C) 2015-2016 vindarel <ehvince@mailz.org>

;; Author: Vindarel
;; URL: https://gitlab.com/emacs-stuff/git-commit-insert-issue/
;; Package-Version: 0.3
;; Keywords: git, github, gitlab, bitbucket, commit, issues
;; Version: 0.2.0
;; Package-Requires: ((helm "0") (projectile "0") (s "0") (github-issues "0") (gitlab "0") (bitbucket "0"))
;; Summary: Get issues list when typeng "Fixes #" in a commit message. github, gitlab and bitbucket.

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library provides a minor mode and an interactive function to
;; fetch issues of your project when you type "Fixes #" in a commit
;; message. Github, Gitlab and Bitbucket.

;;; Code:

(require 'helm)
(require 'projectile)
(require 's)
(require 'github-issues)
(require 'bitbucket)
(require 'gitlab)

(defvar git-commit-insert-issue-github-keywords '("Fixes" "fixes" "fix" "fixed"
                                "close" "closes" "closed"
                                "resolve" "resolves" "resolved")
  "List of keywords that github accepts to close issues.")

(defvar git-commit-insert-issue-gitlab-keywords '("see" "for")
  "List of keywords to reference an issue with gitlab. Gitlab works on 'fixes' and the like, but isn't limited to them. We can reference any issue with no keyword.")

(defvar git-commit-insert-issue-bitbucket-keywords '("see" "for")
  "Similar to Gitlab, Bitbucket can reference issues with or without keywords, see: https://confluence.atlassian.com/bitbucket/resolve-issues-automatically-when-users-push-code-221451126.html")

;; (defvar git-commit-insert-issue-helm-source
(setq git-commit-insert-issue-helm-source
      '((name . "Select an issue")
        (candidates . git-commit-insert-issue-get-issues-github-or-gitlab-or-bitbucket-format)
        (action . (lambda (candidate)
                    candidate))))

(defun git-commit-insert-issue-helm ()
  (interactive)
  (helm :sources '(git-commit-insert-issue-helm-source))
)

(defun git-username ()
  (s-trim (shell-command-to-string "git config user.name")))

(defun git-commit-insert-issue-gitlab-issues ()
  "Return a list of the opened issues on gitlab."
  (or (gitlab--get-host)
      (error "We can't find your gitlab host. Did you set gitlab-[host, username, password] ?"))
  (when (s-blank? gitlab-token-id)
    (gitlab-login))
  (let* ((username (insert-issue--get-group))
         (project-name (projectile-project-name))
         (user-project (format "%s/%s" username project-name)))
    (gitlab-list-project-issues user-project nil nil '((state . "opened")))))

(defun git-commit-insert-issue-gitlab-issues-format ()
  "Get issues and return a list of strings formatted with '#id - title'"
  (--map (format "#%i - %s" (assoc-default 'iid it) (assoc-default 'title it))
                 (git-commit-insert-issue-gitlab-issues)))

(defun git-commit-insert-issue-github-issues (&optional username project-name)
  "Return a plist of github issues, raw from the api request."
  (github-api-repository-issues username project-name))

(defun git-commit-insert-issue-github-issues-format (&optional username project-name)
  "Get all the issues from the current project.
   Return a list of formatted strings: '#id - title'"
  (let* ((username (or username (insert-issue--get-group)))
         (project-name (or project-name (projectile-project-name)))
         (issues (git-commit-insert-issue-github-issues username project-name)))
    (if (string= (plist-get issues ':message) "Not Found")
          (error (concat "Nothing found with user " (git-username)))
      (progn
        ;;todo: watch for api rate limit.
        (setq git-commit-insert-issue-project-issues (--map
                              (format "#%i - %s" (plist-get it ':number) (plist-get it ':title))
                              issues))
        ))))

(defun git-commit-insert-issue-bitbucket-issues (&optional username project-name)
  "Return a list of bitbucket issues."
  (let* ((username (insert-issue--get-group))
          (project-name (projectile-project-name)))
          (bitbucket-issues-list-all username project-name)))

(defun git-commit-insert-issue-bitbucket-issues-format (&optional username project-name)
  "Get issues and return a list of strings formatted with '#id - title'"
  (--map (format "#%i - %s" (assoc-default 'id it) (assoc-default 'title it))
    (git-commit-insert-issue-bitbucket-issues username project-name)))

(defun git-commit-insert-issue-get-issues-github-or-gitlab-or-bitbucket-format ()
  "Get the list of issues, from github, gitlab or bitbucket."
  (cond ((string-equal "github.com" (insert-issue--get-server))
          (git-commit-insert-issue-github-issues-format))
    ((string-equal "bitbucket.org" (insert-issue--get-server))
      (git-commit-insert-issue-bitbucket-issues-format))
    ;; for every other choice it's gitlab atm, since github isn't self hosted it won't have other names.
    (git-commit-insert-issue-gitlab-issues-format)))

(defun git-commit-insert-issue--construct-regexp (kw)
  "From a list of words, constructs a regexp to match each one at
  a start of a line followed by a blank space:
  (\"fix\" \"close\") => \"fix |close \" "
  (let ((regexp (concat "^" (car kw) " ")))
    (concat regexp (mapconcat (lambda (it)
                                (concat "\\|" it " "))
                              (cdr kw)
                              ""))))

;;;###autoload
(defun git-commit-insert-issue-ask-issues ()
  "Ask for the issue to insert."
  (interactive)
  ;; This helm call doesn't work alone, but isn't actually needed.
  ;; (helm :sources '(issues-helm-source)))
  (let ((ido-separator "\n"))
    (insert (ido-completing-read "Choose the issue: "
                               (git-commit-insert-issue-get-issues-github-or-gitlab-or-bitbucket-format)))))

(defun git-commit-insert-issue-gitlab-insert ()
  "Choose and insert the issue id"
  (interactive)
  (let ((ido-separator "\n"))
    (insert (ido-completing-read "Gitlab issue ? " (git-commit-insert-issue-gitlab-issues-format)))))

(defun insert-issue--get-remotes ()
  "Get this repo's remote names"
  (s-split "\n" (s-trim (shell-command-to-string "git remote"))))

(defun insert-issue--get-first-remote ()
  "Get the first remote name found in git config. It should be the prefered one."
  (let* ((first-remote
          (with-temp-buffer
            (insert-file-contents (concat (projectile-project-root) ".git/config"))
            (if (search-forward "[remote \"")
                (progn
                  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
         (first-remote (car (cdr (s-split " " first-remote))))
         (first-remote (s-replace "\"" "" first-remote))
         (first-remote (s-chop-suffix "]" first-remote)))
    first-remote))

(defun insert-issue--get-remote-url ()
  "Get the url of the first remote" ;XXX: shall we ask if many remotes ?
  (shell-command-to-string (format "git config remote.%s.url"
                                   ;; (-first-item (insert-issue--get-remotes))))) ;; -first-item may not be the one we want.
                                   (insert-issue--get-first-remote))))

(defun insert-issue--get-server ()
  "Check the git host.
   From git@server.com:group/project.git or https://server.com/group/project, get server.com"
  (let* ((url (insert-issue--get-remote-url)) ;; git@gitlab.com:emacs-stuff/project-name.git
         ;; Dealing with different protocols: git@foo:bar or https://foo/bar
         ;; Could definitely be proper.
         (server-group-name (if (s-contains? "@" url)
                                (-first-item (cdr (s-split "@" url)))
                              (if (s-contains? "://" url)
                                  (-first-item (cdr (s-split "://" url)))))) ;; gitlab.com:emacs-stuff/project-name.git
         (server (if (s-contains? ":" server-group-name)
                     (car (s-split ":" server-group-name))
                   (if (s-contains? "/" server-group-name)
                       (car (s-split "/" server-group-name)))))
         )
  server))

(defun insert-issue--get-group ()
  "The remote group can be different than the author.
   From git@server.com:group/project.git, get group"
  ;; Again, dealing with git@ or https?://
  (let* ((url (insert-issue--get-remote-url)) ;; git@gitlab.com:emacs-stuff/project-name.git
         (server-group-name (if (s-contains? "@" url)
                                (-first-item (cdr (s-split "@" url)))
                              (car (cdr (s-split "://" url))))) ;; gitlab.com:emacs-stuff/project-name.git
         (group-project (if (s-contains? ":" server-group-name)
                            (cdr (s-split ":" server-group-name))
                          (cdr (s-split "/" server-group-name)))) ;; emacs-stuff/project-name.git
         (group (-first-item (s-split "/" (-first-item group-project)))) ;; emacs-stuff
         )
    group))

;;;###autoload
(define-minor-mode git-commit-insert-issue-mode
  "See the issues when typing 'Fixes #' in a commit message."
  :global nil
  :group 'git
  (if git-commit-insert-issue-mode
      (progn
        (define-key git-commit-mode-map "#"
          (lambda () (interactive)
             (if (looking-back
                  (git-commit-insert-issue--construct-regexp (append
                                                              git-commit-insert-issue-github-keywords
                                                              git-commit-insert-issue-gitlab-keywords)))
                 ;; (insert (git-commit-insert-issue-helm)) ;; broken helm
                 (insert (git-commit-insert-issue-ask-issues))
               (self-insert-command 1))))
        )
    (define-key git-commit-mode-map "#" (insert "#")) ;; works. Good ?
    ))

(provide 'git-commit-insert-issue)

;;; git-commit-insert-issue.el ends here
