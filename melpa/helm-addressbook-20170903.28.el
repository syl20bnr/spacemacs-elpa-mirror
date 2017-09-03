;;; helm-addressbook.el --- Helm for addressbook bookmarks. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Version: 1.0
;; Package-Version: 20170903.28
;; Package-Requires: ((helm "2.8.2") (addressbook-bookmark "1.0") (cl-lib "0.5") (emacs "24.4"))
;; URL: https://github.com/emacs-helm/helm-addressbook

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
;; Needs https://github.com/thierryvolpiatto/addressbook-bookmark as dependency.

;;; Code:
(require 'cl-lib)
(require 'bookmark)
(require 'helm-bookmark)

(declare-function addressbook-bookmark-edit "ext:addressbook-bookmark.el" (bookmark))
(declare-function message-buffers "message.el")
(declare-function addressbook-set-mail-buffer-1 "ext:addressbook-bookmark.el"
                  (&optional bookmark-name append cc))
(declare-function addressbook-bookmark-set-1 "ext:addressbook-bookmark.el" (&optional contact))

(defcustom helm-addressbook-actions
  '(("Show Contact(s)"
     . (lambda (candidate)
         (let* ((contacts (helm-marked-candidates))
                (current-prefix-arg helm-current-prefix-arg))
           (bookmark-jump
            (helm-bookmark-get-bookmark-from-name (car contacts)))
           (helm-aif (cdr contacts)
               (let ((current-prefix-arg '(4)))
                 (cl-loop for bmk in it do
                          (bookmark-jump
                           (helm-bookmark-get-bookmark-from-name bmk))))))))
    ("Mail To" . helm-addressbook-send-mail-1)
    ("Mail Cc" . (lambda (_candidate)
                   (helm-addressbook-send-mail-1 nil 'cc)))
    ("Mail Bcc" . (lambda (_candidate)
                    (helm-addressbook-send-mail-1 nil 'bcc)))
    ("Edit Bookmark"
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (addressbook-bookmark-edit
            (assoc bmk bookmark-alist)))))
    ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
    ("Insert Email at point"
     . (lambda (candidate)
         (let* ((bmk   (helm-bookmark-get-bookmark-from-name
                        candidate))
                (mlist (split-string
                        (assoc-default
                         'email (assoc bmk bookmark-alist))
                        ", ")))
           (insert
            (if (> (length mlist) 1)
                (helm-comp-read
                 "Insert Mail Address: " mlist :must-match t)
                (car mlist))))))
    ("Show annotation"
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark-show-annotation bmk))))
    ("Edit annotation"
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark-edit-annotation bmk))))
    ("Show Google map"
     . (lambda (candidate)
         (let* ((bmk (helm-bookmark-get-bookmark-from-name
                      candidate))
                (full-bmk (assoc bmk bookmark-alist)))
           (addressbook-google-map full-bmk)))))
  "Actions for addressbook bookmarks."
  :group 'helm-bookmark
  :type '(alist :key-type string :value-type function))

;;; Addressbook.
;;
;;
(defun helm-addressbook--search-mail (pattern)
  "Search function to search PATTERN for helm-addressbook."
  (helm-awhile (next-single-property-change (point) 'email)
    (goto-char it)
    (end-of-line)
    (when (string-match pattern
                        (get-text-property
                         0 'email (buffer-substring
                                   (point-at-bol) (point-at-eol))))
      (cl-return
       (+ (point) (match-end 0))))))

(defun helm-addressbook--search-group (pattern)
  "Search function to search PATTERN for helm-addressbook."
  (helm-awhile (next-single-property-change (point) 'group)
    (goto-char it)
    (end-of-line)
    (when (string-match pattern
                        (get-text-property
                         0 'group (buffer-substring
                                   (point-at-bol) (point-at-eol))))
      (cl-return
       (+ (point) (match-end 0))))))

(defclass helm-addressbook-class (helm-source-in-buffer)
  ((init :initform (lambda ()
                     (require 'addressbook-bookmark)
                     (bookmark-maybe-load-default-file)
                     (helm-init-candidates-in-buffer
                         'global
                       (cl-loop for b in (helm-addressbook-setup-alist)
                                collect (propertize b
                                                    'email (bookmark-prop-get b 'email)
                                                    'group (bookmark-prop-get b 'group))))))
   (search :initform '(helm-addressbook--search-group
                       helm-addressbook--search-mail))
   (persistent-action :initform
                      (lambda (candidate)
                        (let ((bmk (helm-bookmark-get-bookmark-from-name
                                    candidate)))
                          (if (and (get-buffer-window addressbook-buffer-name 'visible)
                                   (string= bmk (with-current-buffer addressbook-buffer-name
                                                  (save-excursion
                                                    (search-forward "^Name: " nil t)
                                                    (car (addressbook-get-contact-data))))))
                              (kill-buffer addressbook-buffer-name)
                              (when (buffer-live-p (get-buffer addressbook-buffer-name))
                                (kill-buffer addressbook-buffer-name))
                              (bookmark--jump-via bmk 'switch-to-buffer)))))
   (persistent-help :initform "Show contact - Prefix with C-u to append")
   (mode-line :initform (list "Contact(s)" helm-mode-line-string))
   (filtered-candidate-transformer :initform
                                   '(helm-adaptive-sort
                                     helm-highlight-bookmark))
   (action :initform 'helm-addressbook-actions))

  "Helm class to build helm-addressbook source.")

(defun helm-addressbook-send-mail-1 (_candidate &optional cc)
  "Generic action to send mail from helm-addressbook.
Argument _CANDIDATE is unused and argument CC can be one of `cc' or
`bcc'."
  (let* ((contacts (helm-marked-candidates))
         (bookmark      (helm-bookmark-get-bookmark-from-name
                         (car contacts)))
         (append   (message-buffers)))
    (addressbook-set-mail-buffer-1 bookmark append cc)
    (helm-aif (cdr contacts)
        (cl-loop for bmk in it do
                 (addressbook-set-mail-buffer-1
                  (helm-bookmark-get-bookmark-from-name bmk) 'append cc)))))

(defun helm-addressbook-setup-alist ()
  "Specialized filter function for addressbook bookmarks."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-addressbook-p))

(defvar helm-source-addressbook
  (helm-make-source "Bookmark Addressbook" 'helm-addressbook-class)
  "Main source for helm-addressbook.")

(defvar helm-source-addressbook-set
  (helm-build-dummy-source "Addressbook add contact"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
                     helm-pattern)
                "Enter a contact name to record")))
    :action (lambda (candidate)
              (addressbook-bookmark-set-1 candidate)))
  "Source to add contacts from helm-addressbook.")

;;;###autoload
(defun helm-addressbook-bookmarks ()
  "Preconfigured helm for addressbook bookmarks.
Need addressbook-bookmark package as dependencie."
  (interactive)
  (helm :sources '(helm-source-addressbook
                   helm-source-addressbook-set)
        :prompt "Search Contact: "
        :buffer "*helm addressbook*"
        :default (list (thing-at-point 'symbol)
                       (buffer-name helm-current-buffer))))

(provide 'helm-addressbook)

;;; helm-addressbook.el ends here
