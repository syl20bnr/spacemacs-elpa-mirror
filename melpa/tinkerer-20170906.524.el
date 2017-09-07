;;; tinkerer.el --- Elisp wrapper for Tinkerer Blogging Engine.

;; Copyright (C) 2015, 2016, 2017 Yagnesh Raghava Yakkala

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: 19 Feb 2015
;; Version: 0.1
;; Package-Version: 20170906.524
;; Package-Requires: ((s "1.2.0"))
;; Keywords: Tinkerer, blog, wrapper
;; X-URL: https://github.com/yyr/tinkerer.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; put tinkerer.el file in emacs load-path and the following in your init.el
;; (require 'tinkerer)
;; (setq tinkerer-root-path "/your/blog/root/folder/") ; Final slash is important.

;;; Code:

(require 's)

(defgroup tinkerer nil
  "Customizations for tinkerer wrapper."
  :group 'applications)

(defcustom tinkerer-root-path nil
  "Tinkerer blog root folder path."
  :group 'tinkerer
  :type 'path)

(defcustom tinkerer-executable "tinker"
  "The location of the tinkerer executable."
  :group 'tinkerer
  :type 'string)

;;;###autoload
(defcustom tinkerer-prefix-key (kbd "C-c C-t")
 "Tinkerer keymap prefix."
 :group 'tinkerer
 :type 'string)

(defvar tinkerer-prefix-map
  (let ((map 'tinkerer-prefix-command))
    (define-key map "d" 'tinkerer-draft)
    (define-key map "D" 'tinkerer-preview-draft)
    (define-key map "P" 'tinkerer-page)
    (define-key map "p" 'tinkerer-post)
    (define-key map "b" 'tinkerer-build))
  "Tinkerer key map.")

(define-prefix-command
  'tinkerer-prefix-command
  'tinkerer-prefix-map "Tinkerer")

(global-set-key tinkerer-prefix-key 'tinkerer-prefix-command)

;;;
(defvar tinkerer--hist nil)

(defun tinkerer--construct-command (&rest args)
  "Build a safely-escaped command line to run tinkerer with ARGS."
  (mapconcat #'identity
             (append (list tinkerer-executable) (mapcar #'shell-quote-argument args))
             " "))

;;;###autoload
(defun tinkerer-build ()
  "Run tinkerer build command."
  (interactive)
  (let ((default-directory tinkerer-root-path))
    (async-shell-command (tinkerer--construct-command "-b"))))

;;;###autoload
(defun tinkerer-draft (title)
  "Read TITLE and create a draft with it."
  (interactive
   (list
    (funcall #'read-from-minibuffer
             "Title of the Draft: " nil nil nil tinkerer--hist "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (tinkerer--construct-command "-f" "-d" title))))))

;;;###autoload
(defun tinkerer-post (arg &optional title)
  "Read TITLE and create a post with it.
If prefix argument ARG provided move a draft and publish it."
  (interactive "p")
  (if (not (= arg 4))
      ;; new post
      (let ((title (if title
                       title
                     (funcall #'read-from-minibuffer
                              "Title of the  Post: " nil nil nil tinkerer--hist "new-draft")))
            (default-directory tinkerer-root-path))
        (find-file-other-window
         (s-trim (shell-command-to-string
                  (tinkerer--construct-command "-f" "-p" title)))))
    ;; move draft and post.
    (let* ((drafts-path (expand-file-name "drafts" tinkerer-root-path))
           (draft (ido-completing-read "Draft file: " (directory-files drafts-path)
                                       nil t nil tinkerer--hist)))
      (let ((default-directory tinkerer-root-path)
            (temp-buf "*temp-buf*"))
        (async-shell-command
         (tinkerer--construct-command "-f" "-p" (expand-file-name draft drafts-path)))))))

;;;###autoload
(defun tinkerer-page (title)
  "Read TITLE and create a page with it."
  (interactive
   (list
    (read-from-minibuffer
     "Title of the Page: "  nil nil nil tinkerer--hist "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (tinkerer--construct-command "-f" "--page" title))))))

;;;###autoload
(defun tinkerer-preview-draft ()
  "Build draft preview."
  (interactive)
  (let* ((drafts-path (expand-file-name "drafts" tinkerer-root-path))
         (draft (ido-completing-read "Draft file: " (directory-files drafts-path)
                                     nil t nil tinkerer--hist)))
    (let ((default-directory tinkerer-root-path))
      (async-shell-command
       (tinkerer--construct-command "--preview" (expand-file-name draft drafts-path))))))

;;;###autoload
(global-set-key tinkerer-prefix-key 'tinkerer-prefix-command)

(provide 'tinkerer)
;;; tinkerer.el ends here
