;;; helm-safari.el --- Browse your Safari bookmarks and history  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/helm-safari
;; Package-Version: 20160403.2024
;; Package-Requires: ((helm "1.9.1") (emacs "24"))
;; Version: 0.1
;; Keywords: tools

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

;; Browse your Safari bookmarks and history with Helm.

;; To use, type M-x helm-safari-bookmarks or M-x helm-safari-history

;; FIXME: `helm-safari-history' is slow and ugly, it takes me more than 15
;; seconds to be ready with 1166 history entries
;; TODO: Improve UI to show link along side name
;; TODO: Search (match) against link (not just name)
;; TODO: Improve performance, especially for `helm-safari-history'

;;; Code:

(require 'helm)
(require 'json)

(defgroup helm-safari nil
  "Helm interface for Safari Bookmarks and History."
  :group 'helm)

(defun helm-safari-list-to-alist (list)
  ;; '(1 2 3 4) ⇒ ((2 . 1) (4 . 3))
  (let* ((idx 0)
         res)
    (while (< idx (length list))
      (push (cons (nth (1+ idx) list)
                  (nth idx list))
            res)
      (setq idx (+ idx 2)))
    (nreverse res)))

(defun helm-safari-init (dir)
  (let* ((dir (expand-file-name dir))
         (files
          (delete (concat dir "..")
                  (delete (concat dir ".")
                          (directory-files dir 'full nil 'nosort))))
         name-url-alist)
    (with-temp-buffer
      (dolist (f files)
        (erase-buffer)
        (let ((cmd (format "plutil -convert json -o - -- %s" (shell-quote-argument f)))
              json)
          (when (zerop (call-process-shell-command cmd nil t))
            (setq json (json-read-from-string (buffer-string)))
            (push (cons (cdr (assoc 'Name json))
                        (cdr (assoc 'URL json)))
                  name-url-alist)))))
    name-url-alist))

(defvar helm-safari-bookmarks-alist nil)

(defvar helm-source-safari-bookmarks
  (helm-build-sync-source "Safari Bookmarks"
    :candidates 'helm-safari-bookmarks-alist
    :action '(("Browse Url" . browse-url))))

;;;###autoload
(defun helm-safari-bookmarks ()
  "Search Safari Bookmarks."
  (interactive)
  (unless helm-safari-bookmarks-alist
    (setq helm-safari-bookmarks-alist
          (helm-safari-init "~/Library/Caches/Metadata/Safari/Bookmarks/")))
  (helm :sources 'helm-source-safari-bookmarks
        :prompt "Find Bookmarks: "
        :buffer "*Helm Safari Bookmarks*"))

(defvar helm-source-safari-history
  (helm-build-in-buffer-source "Safari History"
    :init #'helm-safari-history-init
    :action (lambda (candidate)
              (browse-url (cdr (assoc 'URL (json-read-from-string candidate)))))))

(defun helm-safari-history-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (let* ((dir
            (expand-file-name "~/Library/Caches/Metadata/Safari/History/"))
           (files
            (delete (concat dir "..")
                    (delete (concat dir ".")
                            (directory-files dir 'full nil 'nosort)))))
      (dolist (f files)
        (call-process "plutil" nil t nil "-convert" "json" "-o" "-" "--" f)
        (insert "\n")))))

;;;###autoload
(defun helm-safari-history ()
  "Search Safari History."
  (interactive)
  (helm :sources 'helm-source-safari-history
        :prompt "Find History: "
        :buffer "*Helm Safari History*"))

(provide 'helm-safari)
;;; helm-safari.el ends here
