;;; glab.el --- minuscule client for the Gitlab API  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://gitlab.com/tarsius/glab
;; Keywords: tools
;; Package-Version: 20160808.539
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; A minuscule client for the Gitlab API.

;; Initial configuration:
;;
;;   $ git config gitlab.user <username>
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
;;   machine .gitlab.com login <login> password <token>

;; Usage examples:
;;
;; Get details about a project:
;;
;;   (glab-get "/projects/tarsius%2Fglab")
;;
;; List names of all projects owned by you:
;;
;;   (--keep (cdr (assq 'name it))
;;           (let ((glab-unpaginate t))
;;             (glab-get "/projects")))

;; This library just provides the basic verbs.  Instead of wrapping
;; every resource, I recommend http://doc.gitlab.com/ce/api.  Due to
;; the lack of doc-strings, I also recommend having a quick look at
;; the source, which is quite trivial.

;; If you like this, then you might also like `ghub.el'; a minuscule
;; client for the Github API.  See https://github.com/tarsius/ghub.

;; If you don't like this, then you might instead like `gitlab.el`;
;; a big client for the Gitlab API.  Which, somewhat surprisingly,
;; you can get from https://github.com/nlamirault/emacs-gitlab.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst glab--domain ".gitlab.com")
(defconst glab--root-endpoint "https://gitlab.com/api/v3")

(defvar glab-unpaginate nil)

(defun glab-get (resource &optional params data noerror)
  (glab--request "GET" resource params data noerror))

(defun glab-put (resource &optional params data noerror)
  (glab--request "PUT" resource params data noerror))

(defun glab-head (resource &optional params data noerror)
  (glab--request "HEAD" resource params data noerror))

(defun glab-post (resource &optional params data noerror)
  (glab--request "POST" resource params data noerror))

(defun glab-patch (resource &optional params data noerror)
  (glab--request "PATCH" resource params data noerror))

(defun glab-delete (resource &optional params data noerror)
  (glab--request "DELETE" resource params data noerror))

(define-error 'glab-error "Glab Error")
(define-error 'glab-http-error "HTTP Error")
(define-error 'glab-301 "Moved Permanently" 'glab-http-error)
(define-error 'glab-400 "Bad Request" 'glab-http-error)
(define-error 'glab-404 "Not Found" 'glab-http-error)
(define-error 'glab-422 "Unprocessable Entity" 'glab-http-error)

(defun glab--request (method resource &optional params data noerror)
  (let* ((p (and params (concat "?" (glab--url-encode-params params))))
         (d (and data   (json-encode-list data)))
         (url-request-extra-headers
          `(("Content-Type"  . "application/json")
            ("PRIVATE-TOKEN" . ,(glab--get-access-token))))
         (url-request-method method)
         (url-request-data d))
    (with-current-buffer
        (url-retrieve-synchronously (concat glab--root-endpoint resource p))
      (let (link body)
        (goto-char (point-min))
        (save-restriction
          (narrow-to-region (point) url-http-end-of-headers)
          (setq link (mail-fetch-field "X-Next-Page")))
        (when (equal link "")
          (setq link nil))
        (goto-char (1+ url-http-end-of-headers))
        (setq body (ignore-errors (glab--read-response)))
        (unless (or noerror (= (/ url-http-response-status 100) 2))
          (pcase url-http-response-status
            (301 (signal 'glab-301 (list method resource p d body)))
            (400 (signal 'glab-400 (list method resource p d body)))
            (404 (signal 'glab-404 (list method resource p d body)))
            (422 (signal 'glab-422 (list method resource p d body)))
            (_   (signal 'glab-http-error
                         (list url-http-response-status
                               method resource p d body)))))
        (if (and link glab-unpaginate)
            (nconc body
                   (glab--request method resource
                                  (cons (cons 'page link)
                                        (cl-delete 'page params :key #'car))
                                  data noerror))
          body)))))

(defun glab--read-response ()
  (unless (eobp)
    (let ((json-object-type 'alist)
          (json-array-type  'list)
          (json-key-type    'symbol)
          (json-false       nil)
          (json-null        nil))
      (json-read))))

(defun glab--url-encode-params (params)
  (mapconcat (pcase-lambda (`(,key . ,val))
               (concat (url-hexify-string (symbol-name key)) "="
                       (url-hexify-string val)))
             params "&"))

(defun glab--get-access-token ()
  (let ((secret
         (plist-get (car (auth-source-search
                          :max 1
                          :user (substring (shell-command-to-string
                                            "git config gitlab.user")
                                           0 -1)
                          :host glab--domain))
                    :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun glab-wait (resource)
  (with-local-quit
    (let ((for 0.5)
          (total 0))
      (while (not (ignore-errors (glab-get resource)))
        (setq for (truncate (* 2 for)))
        (setq total (+ total for))
        (when (= for 128)
          (signal 'glab-error
                  (format "Gitlab is taking to long to create %s" resource)))
        (message "Waiting for %s (%ss)..." resource total)
        (sit-for for)))))

;;; glab.el ends soon
(provide 'glab)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; glab.el ends here
