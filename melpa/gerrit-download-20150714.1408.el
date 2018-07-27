;;; gerrit-download.el --- Show gerrit reviews in a diff buffer.

;; Copyright (C) 2013 Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; URL: https://github.com/chmouel/gerrit-download.el
;; Package-Version: 20150714.1408
;; Version: 0.2.1
;; Keywords: tools gerrit git
;; Package-Requires: ((emacs "24.0") (magit "2.1.0"))
;;
;;; Commentary:

;;; This is a mode that will download a review from gerrit using the
;;; `git-review' software and run show the diff for that changes.
;;;
;;; This is using magit and the `magit-repo-dirs` variable to download
;;; the change into.
;;;
;;; With gnus add a hook like this
;;; (add-hook 'gnus-startup-hook 'gerrit-download-insinuate-gnus)
;;; in your init file and use the 'v' key to have it automatically
;;; parse the email and show the diff.

;;; TODO:
;; magit interface key.

;;; License:

;; This file is NOT part of GNU Emacs.

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

(eval-when-compile
  (require 'gnus)
  (require 'gnus-art))

;;; Code:
(require 'magit)

;; User variables

(defvar gerrit-review-program "git-review"
  "Path to gerrit review.")

;; Internal variables

(defvar gerrit-project-cwd nil)


;; Functions

(defun gerrit-check-if-repo-modified ()
  "Check if current repo has been modified."
  (null (magit-git-items "status" "-z" "--porcelain")))

(defun gerrit-get-local-directory (project)
  "Get local project on filesystem from magit-repo-dirs."
  (mapconcat 'identity
             (mapcar (lambda (path)
                       (cond
                        ((file-exists-p (concat path "/" project))
                         (concat path "/" project))
                        ((file-exists-p (concat path "/" (file-name-nondirectory
                                                          project)))
                         (concat path "/" (file-name-nondirectory project)))
                        )) magit-repo-dirs) ""))

(defun gerrit-download-insinuate-gnus()
  "Hook Gerrit Download into Gnus."
  (define-key gnus-summary-mode-map "v" 'gerrit-download-gnus-from-email)
  (define-key gnus-article-mode-map "v" 'gerrit-download-gnus-from-email))

;;;###autoload
(defun gerrit-download-gnus-from-email()
  "Parse an email from jenkins in Gnus and get the project and change-id."
  (interactive)
  (let (change-id project)
  (gnus-with-article-buffer
      (article-goto-body)
      (while (re-search-forward "^Gerrit-Project:\s*\\([^ \t\n]+\\)" nil t)
        (setq project (match-string-no-properties 1)))
      (article-goto-body)
      (while (re-search-forward "^Gerrit-Change-Id:\s*\\([^ \t\n]+\\)" nil t)
        (setq change-id (match-string-no-properties 1)))
      (message "%s %s" project change-id)
      (if (and project change-id)
          (gerrit-download project change-id)))))

;;;###autoload
(defun gerrit-download (project review-id)
  (interactive
   (list (read-string "Project: ")
         (read-string "Review-ID: ")))
  (let* ((local-directory (gerrit-get-local-directory project))
         (default-directory local-directory)
         changes)
    (magit-with-toplevel
      (setq gerrit-project-cwd default-directory)
      (if (string= "" local-directory)
          (error "Cannot find %s in magit-repos-dir" project))
      (unless (gerrit-check-if-repo-modified)
        (error "%s has changes, not processing" project))
      (let ((proc (concat "git-review" review-id)))
        (message "Starting git-review...")
        (start-process proc "*git review*" gerrit-review-program "-v" "-d" review-id)
        (set-process-sentinel
         (get-process proc)
         #'(lambda (process event)
             (let ((default-directory gerrit-project-cwd))
               (message event)
               (if (string= event "finished\n")
                   (magit-show-commit "HEAD")
                 nil nil t)
               (error "Error while downloading review, check *git review* buffer."))))))))

;;; End gerrit-download.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gerrit-download)

;;; gerrit-download.el ends here
