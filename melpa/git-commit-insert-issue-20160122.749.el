;;; git-commit-insert-issue.el --- Get issues list when typing "Fixes #"

;; Copyright (C) 2015 vindarel <ehvince@mailz.org>

;; Author: Vindarel
;; URL: https://gitlab.com/emacs-stuff/git-commit-insert-issue/
;; Package-Version: 20160122.749
;; Keywords: git, commit, issues
;; Version: 0.1.0
;; Package-Requires: ((helm "0") (projectile "0") (s "0") (github-issues "0"))
;; Summary: Get issues list when typeng "Fixes #" in a commit message. github only atm.

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
;; message.

;;; Code:

(require 'helm)
(require 'projectile)
(require 's)
(require 'github-issues)

(defvar git-commit-insert-issue-helm-source
      '((name . "Select an issue")
        (candidates . issues-get-issues)
        (action . (lambda (candidate)
                    candidate))))

(defun git-commit-insert-issue-helm ()
  (interactive)
  (helm :sources '(git-commit-insert-issue-helm-source))
)

(defun git-username ()
  (s-trim (shell-command-to-string "git config user.name")))

;;;###autoload
(defun git-commit-insert-issue-get-issues (&optional username project-name)
  "Get all the issues from the current project.
   Return a list."
  (let* ((username (or username (git-username)))
         (project-name (or project-name (projectile-project-name)))
         (issues (github-api-repository-issues username project-name)))
    (if (string= (plist-get issues ':message) "Not Found")
        `(,(concat "Not found with user " (git-username)) )
      (progn
        ;;todo: watch for api rate limit.
        (setq git-commit-insert-issue-project-issues (--map
                              (format "#%i - %s" (plist-get it ':number) (plist-get it ':title))
                              issues))
        ))))

(defvar git-commit-insert-issue-github-keywords '("Fixes" "fixes" "fix" "fixed"
                                "close" "closes" "closed"
                                "resolve" "resolves" "resolved") "List of keywords that github accepts to close issues.")

(defun git-commit-insert-issue--construct-regexp (kw)
  "From a list of words, constructs a regexp to match each one at
  a start of a line followed by a blank space:
  (\"fix\" \"close\") => \"^fix |^close \" "
  (let ((regexp (concat "^" (car kw) " ")))
    (concat regexp (mapconcat (lambda (it) (concat "\\|^" it " "))
               (cdr kw)
               ""))))

(defun git-commit-insert-issue--ask-issues ()
  "Ask for the issue to insert."
  (interactive)
  ;; This helm call doesn't work alone, but isn't actually needed.
  ;; (helm :sources '(issues-helm-source)))
  (insert (ido-completing-read "Choose the issue: " (git-commit-insert-issue-get-issues))))

;;;###autoload
(define-minor-mode git-commit-insert-issue-mode
  "See the issues when typing 'Fixes #' in a commit message."
  :global nil
  :group 'git
  (if git-commit-insert-issue-mode
      (progn
        (define-key git-commit-mode-map "#"
          (lambda () (interactive)
            (setq git-commit-insert-issue-project-issues (git-commit-insert-issue-get-issues))
             (if (looking-back
                  (git-commit-insert-issue--construct-regexp git-commit-insert-issue-github-keywords))
                 (insert (git-commit-insert-issue-helm))
               (self-insert-command 1))))
        )
    (define-key git-commit-mode-map "#" (insert "#")) ;; works. Good ?
    ))

(provide 'git-commit-insert-issue)

;;; git-commit-insert-issue.el ends here
