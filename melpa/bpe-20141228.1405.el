;;; bpe.el --- Blog from Org mode to Blogger -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013, 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/bpe
;; Package-Version: 20141228.1405
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: Blogger, blog

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; Configuration sample
;; (require 'bpe)
;; (setq ;; whether ask or not if you update your blog post by using bpe
;;       bpe:no-ask t
;;       ;; Set your language configuration if needed
;;       ;; default is your LANG environment.
;;       bpe:lang "ja_JP.UTF-8"
;;       ;; do not use --draft when updating(note your view count may disappear)
;;       bpe:use-real-post-when-updating t)
;;; Code:
(require 'cl-lib)
(require 'org)
(require 'ox-html nil 'noerror)

;; google blog
(defvar bpe:account   "your-user-account@gmail.com")
(defvar bpe:blog-name "blog-name")
(defvar bpe:multiple-blog-names nil
  "Set like below form if you want to use multiple blog name.
Use matched blog name that you specified directory when you push your
org file to Blogger.

For example:
'((\"blogname1\" . \"~/blogname1-directory\")
  (\"blogname2\" . \"~/blogname2-directory\"))")

(defvar bpe:lang      (shell-command-to-string "echo -n $LANG"))
(defvar bpe:update-by-default nil
  "If this value was non-nil, update article if there are same title's
article(s)")
(defvar bpe:use-real-post-when-updating nil
  "If non-nil do not attach --draft option when updating.")
(defvar bpe:no-ask nil
  "Attach --yes option when user update(delete old article) if this variable
was non-nil")

(defvar bpe:template "#+TITLE:
#+OPTIONS: toc:nil \\n:nil num:nil
#+TAGS:
#+AUTHOR: "
  "Template for `bpe:insert-template'")

(defvar bpe:tempfile "/tmp/bpe-minified.html")

(defvar bpe:command "google blogger")

(defun bpe:insert-template ()
  "Insert blog template"
  (interactive)
  (insert bpe:template))

(defun bpe:export-html ()
  (interactive)
  (org-html-export-to-html nil nil nil t))

(defun bpe:export-html-old-version ()
  (with-no-warnings
    (let ((version (string-to-number
                    (replace-regexp-in-string "\\." "" org-version))))
      (if (< version 794)
          (funcall 'org-export-as-html 23 nil nil t)
        (funcall 'org-export-as-html 23 nil nil nil 'string)))))

(defun bpe:get-option (title-or-tag)
  (interactive)
  (goto-char (point-min))
  (let ((option (cl-case title-or-tag
                  (:title "TITLE")
                  (:tag   "TAGS"))))
    (if (re-search-forward
         (format "^#.*%s: \\(.+\\)" option) nil t)
        (match-string 1))))

(defun bpe:format-title (raw-title)
  (let ((title (format "'%s'" raw-title))
        (name  (format "\"%s\"" (bpe:get-blog-name))))
    (bpe:format "--blog" name "--title" title)))

(defun bpe:get-blog-name ()
  ""
  (if bpe:multiple-blog-names
      (cl-loop for (blogname . directory) in bpe:multiple-blog-names
               if (string-match (concat "^" (file-truename directory))
                                buffer-file-name)
               do (cl-return blogname))
    bpe:blog-name))

(defun bpe:get-tags ()
  "Get tag(s) from current file."
  (let ((tags
         (condition-case _err
             (mapconcat 'identity
                        (split-string (bpe:get-option :tag) " ") ",")
           (error ""))))
    (if tags (format " --tags \"%s\" " tags) "")))

(defun bpe:get-draft-string ()
  (if (and current-prefix-arg
           bpe:use-real-post-when-updating)
      ""
    " --draft "))

;;;###autoload
(defun bpe:post-article (&optional update)
  "Post current file that converted html to your blog of Google Blogger.
 If you pushed C-u before execute this command, then post article after
delete same title's article."
  (interactive)
  (let* ((original-lang (getenv "LANG"))
         (title      (or (bpe:get-option :title)
                         (read-string "title here: ")))
         (blog-and-title (bpe:format-title title))
         (file-name
          (replace-regexp-in-string "\\.org$" ".html"
                                    (expand-file-name buffer-file-truename))))
    (setenv bpe:lang)
    (bpe:export-html)
    (shell-command
     (format "htmlminify -o %s %s" bpe:tempfile file-name))
    (bpe:post blog-and-title bpe:tempfile update)
    (delete-file file-name)
    (setenv original-lang)))

(defun bpe:post (blog-and-title file-name update)
  (let* ((delete  (bpe:get-delete-string blog-and-title))
         (post    (bpe:get-post-string   blog-and-title file-name))
         (command (if (bpe:update-required-p update)
                      (concat delete " && " post)
                    post)))
    (async-shell-command command "*bpe*")))

(defun bpe:get-post-string (blog-and-title content)
  (bpe:format (concat "LANG=" bpe:lang) bpe:command "post" (bpe:get-draft-string)
              "-u" bpe:account (bpe:get-tags) blog-and-title content))

(defun bpe:get-delete-string (blog-and-title)
  (bpe:format (concat "LANG=" bpe:lang) bpe:command "delete" blog-and-title
              (if bpe:no-ask "--yes" "")))

(defun bpe:format (&rest list)
  (mapconcat 'identity list " "))

(defun bpe:update-required-p (&optional force)
  (or bpe:update-by-default current-prefix-arg force))

(defun bpe:update-article ()
  (interactive)
  (bpe:post-article t))

(provide 'bpe)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; bpe.el ends here
