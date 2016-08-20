;;; helm-notmuch.el --- Search emails with Notmuch and Helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/helm-notmuch
;; Package-Version: 20160412.1206
;; Keywords: mail
;; Version: 0.02
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

;;; Code:

(require 'helm)
(require 'notmuch)

(defun helm-notmuch-collect-candidates ()
  (let ((proc (start-process "helm-notmuch" helm-buffer
                             "notmuch" "search" helm-pattern)))
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

(defvar helm-source-notmuch
  (helm-build-async-source "Search email with notmuch"
    :candidates-process #'helm-notmuch-collect-candidates
    :candidate-transformer #'helm-notmuch-candidates-formatter
    ;; TODO: What if I want to a single character? for example, '*' for matching
    ;; all emails. But notmuch (actually Xapian) has more serious limitation:
    ;; not working with CJK text, for example, I can't just use my last name '徐'
    ;; (or first name '春阳') to search emails which contain my name.
    :requires-pattern 2
    :nohighlight t
    :action '(("Show message in notmuch" . notmuch-show))))

;;;###autoload
(defun helm-notmuch ()
  (interactive)
  (helm :sources helm-source-notmuch
        :buffer "*helm notmuch*"))

(provide 'helm-notmuch)
;;; helm-notmuch.el ends here
