;;; magit-rockstar.el --- commit like a rockstar  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((dash "2.13.0") (magit "2.12.0"))
;; Package-Version: 20171213.737
;; Homepage: https://github.com/tarsius/magit-rockstar
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides two commands which manipulate author and
;; committer dates.  You could use it to make yourself look like
;; a rockstar programmer who hammers out commits at one commit per
;; minute.  But the real purpose is to recover from heavy
;; re-arrangements of commits, that have causes the existing author
;; and committer dates to become meaningless.

;; I add these commands to the appropriate popups like this:
;;
;;    (magit-define-popup-action 'magit-rebase-popup
;;      ?R "Rockstar" 'magit-rockstar)
;;
;;    (magit-define-popup-action 'magit-commit-popup
;;      ?n "Reshelve" 'magit-reshelve)

;; Also included are tools that are either only useful for people
;; working on Magit itself and/or that aren't ready to be added to
;; Magit yet.  These tools might change at any time, without prior
;; notice or way to appeal.  This is a staging ground.  It's okay
;; if things ain't perfect, or if they only do what *I currently*
;; need but not what you (or I) think they should (eventually) be
;; doing instead.

;; To use the "uncommit-extend" feature add this:
;;
;;    (magit-define-popup-action 'magit-revert-popup
;;      ?e "Revert & edit HEAD" 'magit-uncommit-extend)

;;; Code:

(require 'dash)
(require 'magit)

;;;###autoload
(defun magit-rockstar (from &optional offset)
  "Attempt to make you look like a rockstar programmer.
Want to hammer out commits at one commit per minute?
With this function you can!"
  (interactive
   (list (magit-read-other-branch-or-commit "Rocking since" nil
                                            (magit-get-upstream-branch))
         (read-number "Offset: " 0)))
  (let* ((branch (magit-get-current-branch))
         (range (concat from ".." branch))
         (time (+ (truncate (float-time)) (* (or offset 0) 60)))
         (tz (car (process-lines "date" "+%z")))
         (format (format "%%s) \
export GIT_AUTHOR_DATE=\"%%s %s\"; \
export GIT_COMMITTER_DATE=\"%%s %s\";;" tz tz)))
    (setq time (- time (% time 60)))
    (magit-with-toplevel
      (magit-call-git "filter-branch" "-f" "--env-filter"
                      (format "case $GIT_COMMIT in %s\nesac"
                              (mapconcat
                               (lambda (commit)
                                 (format format commit (cl-decf time 60) time))
                               (magit-git-lines "rev-list" range) " "))
                      range "--")
      (magit-run-git "update-ref" "-d"
                     (concat "refs/original/refs/heads/" branch)))))

;;;###autoload
(defun magit-reshelve (date)
  "Change the author and committer dates of `HEAD' to DATE." 
  (interactive (list (read-string "Date or offset: "
                                  (car (process-lines "date" "+%FT%T%z")))))
  (let ((process-environment process-environment))
    (when (string-match "^[0-9]+$" date)
      (setq date (format "%s%s" (- (truncate (float-time)) (* date 60))
                         (car (process-lines "date" "+%z")))))
    (push (concat "GIT_COMMITTER_DATE=" date) process-environment)
    (magit-run-git "commit" "--amend" "--no-edit" (concat "--date=" date))))

;;;###autoload
(defun magit-debug-sections ()
  "Print information about the current Magit buffer's sections."
  (interactive)
  (magit-debug-sections-1 magit-root-section 0)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((next (or (next-single-property-change
                       (point) 'invisible)
                      (point-max))))
        (message "%4s-%4s %s" (point) next
                 (get-text-property (point) 'invisible))
        (goto-char next)))))

(defun magit-debug-sections-1 (section level)
  (message "%-4s %-10s [%4s %3s]-[%4s %3s]  (%4s %3s)"
           (make-string (1+ level) ?*)
           (magit-section-type section)
           (marker-position       (magit-section-start section))
           (marker-insertion-type (magit-section-start section))
           (marker-position       (magit-section-end section))
           (marker-insertion-type (magit-section-end section))
           (ignore-errors (marker-position       (magit-section-content section)))
           (ignore-errors (marker-insertion-type (magit-section-content section))))
  (--each (magit-section-children section)
    (magit-debug-sections-1 it (1+ level))))

;;;###autoload
(defun magit-uncommit-extend (&rest args)
  "Reverse the change at point in `HEAD'."
  (interactive)
  (let ((inhibit-magit-refresh t))
    (magit-reverse-in-index args))
  (magit-commit-extend))

;;; magit-rockstar.el ends soon
(provide 'magit-rockstar)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-rockstar.el ends here
