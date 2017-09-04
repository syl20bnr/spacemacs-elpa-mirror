;;; helm-notmuch.el --- Search emails with Notmuch and Helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/helm-notmuch
;; Package-Version: 20170903.2159
;; Keywords: mail
;; Version: 1.1
;; Package-Requires: ((helm "1.9.3") (notmuch "0.21"))

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

;; To use, type M-x helm-notmuch.  helm-notmuch will gets start to search when
;; length of your input is no less than 2.

;; News:
;; - 2017-09-04 v1.1 Fix a regexp bug and use `notmuch-command' instead of hardcode "notmuch"
;; - 2016-11-28 v1.0 Add two user options: `helm-notmuch-max-matches' and `helm-notmuch-match-incomplete-words'

;;; Code:

(require 'helm)
(require 'notmuch)

(defgroup helm-notmuch nil
  "Helm interface for notmuch."
  :group 'notmuch
  :link '(url-link :tag "Homepage" "https://github.com/xuchunyang/helm-notmuch"))

(defcustom helm-notmuch-max-matches 0
  "Maximum number of matches shown.
Notice that a setting of 0 means \"Show all matches\"."
  :group 'helm-notmuch
  :type '(choice (const :tag "Show all matches" 0)
                 (integer :tag "Maximum number of matches shown" 50)))

(defcustom helm-notmuch-match-incomplete-words nil
  "If non-nil, treat last word in query as incomplete.

If this variable is non-nil, include results with words for which
the last word of the input is a prefix. Note that this (slightly)
slows down searches."
  :group 'helm-notmuch
  :type 'boolean)

(defun helm-notmuch-collect-candidates ()
  (let* ((cmds (delq nil (list notmuch-command "search"
                               (and (> helm-notmuch-max-matches 0)
                                    (concat "--limit=" (number-to-string helm-notmuch-max-matches)))
                               helm-pattern)))
         (proc (apply 'start-process "helm-notmuch" helm-buffer cmds)))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (cond
          ((= 1 (process-exit-status process))
           (with-helm-buffer
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     "[notmuch process finished - (no results)]"))
             (force-mode-line-update)))
          ((string= "finished\n" event)
           (with-helm-buffer
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     (:eval (format "L%s" (helm-candidate-number-at-point)))
                     " "
                     (:eval (format
                             "[notmuch process finished - (%s results)]"
                             (helm-get-candidate-number)))))
             (force-mode-line-update)))))))))

(defconst helm-notmuch-thread-id-length (length "thread:0000000000000028"))

(defun helm-notmuch-candidates-formatter (candidates)
  (if (and (stringp (car candidates))
           (string-prefix-p "thread:" (car candidates)))
      ;; Remove leading thread-id
      ;; TODO: fold too long line...
      (mapcar (lambda (cand)
                (cons (substring cand (+ 2 helm-notmuch-thread-id-length))
                      (substring cand 0 helm-notmuch-thread-id-length)))
              candidates)
    candidates))

(defun helm-notmuch-maybe-match-incomplete (pattern)
  (if helm-notmuch-match-incomplete-words
      (if (string-match-p "[[:alnum:]]$" pattern)
          (concat pattern "*")
        pattern)
    pattern))

(defvar helm-source-notmuch
  (helm-build-async-source "Search email with notmuch"
    :candidates-process #'helm-notmuch-collect-candidates
    :candidate-transformer #'helm-notmuch-candidates-formatter
    :requires-pattern 2
    :pattern-transformer #'helm-notmuch-maybe-match-incomplete
    :nohighlight t
    :action '(("Show message in notmuch" . notmuch-show))))

;;;###autoload
(defun helm-notmuch ()
  (interactive)
  (helm :sources helm-source-notmuch
        :buffer "*helm notmuch*"))

(provide 'helm-notmuch)
;;; helm-notmuch.el ends here
