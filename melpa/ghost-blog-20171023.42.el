;;; ghost-blog.el --- A package to manage Ghost blog
;;

;; Author: Javier Aguirre <hello@javaguirre.net>
;; Maintainer: Javier Aguirre <hello@javaguirre.net>
;; Version: 0.1
;; Package-Version: 20171023.42
;; Package-Requires: ((markdown-mode "1.0"))
;; Created: 10 Feb 2016
;; Keywords: ghost, blog
;; URL: https://github.com/javaguirre/ghost-blog

;;; Commentary:

;; This is a package to manage Ghost blogs
;; through their Rest API.Ghost Rest API only permits
;; reading for now.This package has the option of reading
;; posts and opening them.Edit and create post is implemented
;; but won't work until the Ghost team opens that part of the API

;;; LICENSE

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;;
(require 'url)
(require 'json)
(require 'markdown-mode)
(eval-when-compile (require 'cl))

(defgroup ghost-blog nil
  "Customization group for `ghost-blog.el'."
  :group 'tools)

;; User options
(defcustom ghost-blog-url nil
  "Ghost blog api url."
  :group 'ghost-blog)
(defcustom ghost-blog-bearer-token nil
  "Ghost bearer token for authentication."
  :group 'ghost-blog)
(defcustom ghost-blog-post-list-limit 10
  "Ghost post list limit."
  :group 'ghost-blog)

(defvar ghost-blog-post-list-header-title "Ghost mode - Posts\n\n")
(defvar ghost-blog-post-endpoint "/posts/")
(defvar ghost-blog-buffer-post-name "ghost-post.md")

;; Metadata
(defvar ghost-blog-metadata-default-header-string
  "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post")
(defvar ghost-blog--metadata-prefix "---\n\n")
(defvar ghost-blog--metadata-suffix "\n---\n\n")
(defvar ghost-blog--metadata-field-separator ": ")

;; Fields
(defvar ghost-blog-default-metadata-fields
  '(title slug status image featured page language meta_title meta_description))
(defvar ghost-blog-required-metadata-fields
  '(title markdown))

;; Messages
(defvar
  ghost-blog--invalid-metadata-message
  "Error in metadata, you need to set the title")
(defvar ghost-blog--update-post-message "Post updated successfully!")
(defvar ghost-blog--create-post-message "Post created successfully!")
(defvar
  ghost-blog--persist-post-failed
  "Post persist failed, please check if your credentials are well set")

(defvar ghost-blog--date-format-string "%d-%m-%Y")
;;;###autoload
;;; Commands
(defun ghost-blog-new-post ()
  "Create new post template."
  (interactive)

  (ghost-blog--use-ghost-post-buffer
   ghost-blog-metadata-default-header-string))

(defun ghost-blog-save-new-post ()
  "Create new post."
  (interactive)

  (let* ((json-object-type 'hash-table)
	 (metadata (json-encode (ghost-blog--read-from-post-buffer)))
	 (payload (json-encode metadata)))
    (if (ghost-blog--is-metadata-valid metadata)
        (ghost-blog--connection
         (ghost-blog--get-post-list-endpoint)
         'ghost-blog--create-post-callback
         "POST"
         payload)
      (message ghost-blog--invalid-metadata-message))))

(defun ghost-blog-update-post ()
  "Update a post."
  (interactive)

  (let* ((json-object-type 'hash-table)
	 (metadata (ghost-blog--read-from-post-buffer))
	 (payload (json-encode metadata)))
    (if (ghost-blog--is-metadata-valid metadata)
	(ghost-blog--connection
	 (concat ghost-blog-post-endpoint (gethash "id" metadata))
	 'ghost-blog--update-post-callback
	 "PUT"
	 payload)
      (message ghost-blog--invalid-metadata-message))))

(defun ghost-blog-get-posts ()
  "Get posts from ghost."
  (interactive)
  (ghost-blog--connection ghost-blog-post-endpoint 'ghost-blog--get-posts-callback))

(defun ghost-blog--connection (endpoint callback &optional method data)
  "HTTP Connection with Ghost API using ENDPOINT, execute CALLBACK.  METHOD and DATA can be set."
  (let ((url-request-method (or method "GET"))
	(url-request-extra-headers
	 `(("Authorization" . ,ghost-blog-bearer-token))))
    (url-retrieve (concat ghost-blog-url endpoint) callback)))

;; Callbacks
(defun ghost-blog--create-post-callback (status)
  "Process post creation, receive HTTP response STATUS."
  (if (ghost-blog--is-request-successful)
      (message ghost-blog--create-post-message)
    (message ghost-blog--persist-post-failed)))

(defun ghost-blog--update-post-callback (status)
  "Process post update, receive HTTP response STATUS."
  (if (ghost-blog--is-request-successful)
      (message ghost-blog--update-post-message)
    (message ghost-blog--persist-post-failed)))

(defun ghost-blog--get-posts-callback (status)
  "Process post list callback, receive HTTP response STATUS."
  (ghost-blog--go-to-body)

  (let ((posts (ghost-blog--get-response-posts)))
    (define-button-type 'ghost-show-post-button
      'action 'ghost-blog--show-post-action
      'follow-link t
      'help-echo "Show post")

    (erase-buffer)

    (insert ghost-blog-post-list-header-title)

    (dotimes (i (length posts))

      (insert-text-button (format "%d %s - %s\n\n"
				  (gethash "id" (aref posts i))
				  (ghost-blog--format-date
				   (gethash "created_at" (aref posts i)))
				  (gethash "title" (aref posts i)))
			  :type 'ghost-show-post-button))))

(defun ghost-blog--get-post-callback (status)
  "Process post read callback, receive HTTP response STATUS."
  (ghost-blog--go-to-body)

  (let* ((posts (ghost-blog--get-response-posts))
	 (current-post (aref posts 0)))
    (ghost-blog--use-ghost-post-buffer
     (format "%s%s"
	     (ghost-blog--get-metadata-as-string current-post)
	     (gethash "markdown" current-post)))))

;; Metadata
(defun ghost-blog--get-metadata-as-string (post)
  "Get list of POST metadata as a string."
  (let ((metadata ghost-blog--metadata-prefix)
	current-value)
    (dolist (metadata-field ghost-blog-default-metadata-fields)
      (setq current-value (gethash (symbol-name metadata-field) post))

      (if (not (stringp current-value))
	  (setq current-value ""))

    (setq metadata
	(concat metadata
		(symbol-name metadata-field)
		ghost-blog--metadata-field-separator
		current-value
		"\n")))
    (setq metadata (concat metadata ghost-blog--metadata-suffix))
    metadata))

(defun ghost-blog--get-metadata-as-hash-table (metadata)
  "Get list of metadata as a hash table from a METADATA string."
  (let* ((items (split-string metadata "\n"))
	 (post (make-hash-table :test 'equal))
	 (current-item nil))
    (dolist (item items)
      (setq current-item (split-string item ": "))

      (if (and (> (length (car current-item)) 0)
	       (member (intern (car current-item)) ghost-blog-default-metadata-fields))
	  (puthash (car current-item) (cadr current-item) post)))
    post))

(defun ghost-blog--is-metadata-valid (metadata)
  "Validate METADATA."
  (let ((is-valid t))
    (dolist (required-metadata-field ghost-blog-required-metadata-fields)
      (unless (gethash (symbol-name required-metadata-field) metadata)
	(setq is-valid nil)))
    is-valid))

;; Utils
(defun ghost-blog--use-ghost-post-buffer (buffer-data)
  "Use ghost post buffer and insert BUFFER-DATA on It."
  (let ((post-buffer ghost-blog-buffer-post-name))
    (get-buffer-create post-buffer)
    (switch-to-buffer post-buffer)
    (erase-buffer)
    (insert buffer-data)
    (markdown-mode)))

(defun ghost-blog--read-from-post-buffer ()
  "Read from current post buffer and transform It to hash-table."
  (let* ((metadata-start (string-match ghost-blog--metadata-prefix (buffer-string)))
	 (metadata-end (string-match ghost-blog--metadata-suffix (buffer-string)))
	 (metadata
	  (substring
	   (buffer-string)
	   (+ metadata-start (length ghost-blog--metadata-prefix))
	   metadata-end))
	 (markdown
	  (substring
	   (buffer-string)
	   (+ metadata-end (length ghost-blog--metadata-suffix))
	   (- (point-max) 1)))
	 (post nil))
    (setq post (ghost-blog--get-metadata-as-hash-table metadata))
    (puthash "markdown" markdown post)
    post))

(defun ghost-blog--show-post-action (button)
  "Show a post by id from BUTTON."
  (let* ((id (car (split-string (button-label button))))
	 (endpoint (concat "/posts/" id)))
    (ghost-blog--connection endpoint 'ghost-blog--get-post-callback)))

(defun ghost-blog--get-http-status-code ()
  "Get the HTTP status of the current request."
  (switch-to-buffer (current-buffer))

  (let* ((http-status-length 3)
	 (start-point (re-search-forward "\\s-")))
	 (buffer-substring start-point (+ start-point http-status-length))))

(defun ghost-blog--go-to-body ()
  "Go to HTTP response body."
  (switch-to-buffer (current-buffer))
  (search-forward "\n\n")
  (delete-region (point-min) (point)))

(defun ghost-blog--get-response-posts ()
  "Get posts from HTTP response body."
  (let ((body (ghost-blog--get-response-body)))
    (gethash "posts" body)))

(defun ghost-blog--get-response-body ()
  "Get HTTP response body json decoded."
  (let ((json-object-type 'hash-table))
    (json-read-from-string (buffer-string))))

(defun ghost-blog--is-request-successful ()
  "Check if the request has a successful http status."
  (let ((ghost-blog--http-ok "200"))
   (= (ghost-blog--get-http-status-code) ghost-blog--http-ok)))

(defun ghost-blog--format-date (date)
  "Get friendlier date format from DATE."
  (format-time-string
   ghost-blog--date-format-string
   (date-to-time date)))

;; Endpoints

(defun ghost-blog--get-post-list-endpoint ()
  "Get the post list endpoint."
  (let ((limit (or ghost-blog-post-list-limit "")))
    (if limit (setq limit (format "?limit=%d" limit)))
    (concat ghost-blog-post-endpoint limit)))

(provide 'ghost-blog)
;;; ghost-blog.el ends here
