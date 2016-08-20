;;; helm-img-tiqav.el --- An helm-source for joking.

;; Description: An helm-source for joking.
;; Author: Sho Matsumoto <l3msh0_at_gmail.com>
;; Maintainer: l3msh0
;; Copyright (C) 2015 l3msh0 all rights reserved.
;; Created: :2015-12-20
;; Version: 0.0.1
;; Package-Version: 20151224.1522
;; Keywords: convenience
;; URL: https://github.com/l3msh0/helm-img
;; Package-Requires: ((helm-img "0.0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'helm-img)
(require 'url-util)

(defun helm-img-tiqav-search (query)
  (let* ((result-buffer (url-retrieve-synchronously (concat "http://api.tiqav.com/search.json?q=" (url-hexify-string query))))
         (body (helm-img-extract-body result-buffer)))
    (json-read-from-string body)))

(defun helm-img-tiqav-make-url (img-info size)
  (let ((id (cdr (assoc 'id img-info)))
        (ext (cdr (assoc 'ext img-info)))
        (suffix (cond ((eq size 'thumb) ".th")
                      ((eq size 'full) ""))))
    (concat "http://img.tiqav.com/" id suffix "." ext)))

(defun helm-img-tiqav-make-candidates (query)
  (let ((results (helm-img-tiqav-search query)))
    (mapcar (lambda (img-info)
              (list
               (cons 'thumb (helm-img-tiqav-make-url img-info 'thumb))
               (cons 'full (helm-img-tiqav-make-url img-info 'full))))
            (append results nil))))

(helm-img-define-source "tiqav" :candidates 'helm-img-tiqav-make-candidates)

(defun helm-img-tiqav (query)
  (interactive "Mquery: ")
  (let ((helm-img-query query))
    (helm
     :sources helm-img-source-tiqav
     :buffer "*helm image tiqav")))

(provide 'helm-img-tiqav)
;;; helm-img-tiqav.el ends here
