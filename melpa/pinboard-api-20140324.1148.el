;;; pinboard-api.el --- Rudimentary http://pinboard.in integration

;; Copyright (C) 2013
;; Danie Roux <danie@danieroux.com>

;; Author: Danie Roux <danie@danieroux.com>
;; URL: https://github.com/danieroux/pinboard-api-el
;; Package-Version: 20140324.1148
;; Keywords: pinboard, WWW
;; Version: 0.1

;; This file is NOT part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This allows adding URLs to http://pinboard.in

;;; Usage:

;; Set pinboard-api-token which can be found at: https://pinboard.in/settings/password

;; Suggested integration with w3m:

;; (defun w3m-pinboard-add-current-buffer ()
;;  (pinboard-add-interactively w3m-current-url w3m-current-title))

;;; Code:

;;; Interesting entry points:

(defun pinboard-add-interactively (url &optional description tags)
  "Interactively add the url to pinboard.in with optional details - will cause an error if it could not complete"
  (interactive)
  (let ((a-url (read-from-minibuffer "URL to add to Pinboard? " url))
        (a-description (read-from-minibuffer "title? " description))
        (a-tags (pinboard-gather-tags))
        (a-extended (read-from-minibuffer "description? " " ... "))
        )
    (pinboard-api-add a-url a-description a-tags a-extended)))

(defun pinboard-refresh-tags-cache ()
  "Refresh list of tags explicitly"
  (interactive)
  (with-temp-buffer
    (insert (pp (pinboard-api-tags-get)))
    (write-region nil nil pinboard-tags-cache-file-name)))

;;; Customisations:

(defgroup pinboard nil
  "pinboard.in integration"
  :group 'comm)

(defcustom pinboard-api-token "username:hexstring"
  "Your pinboard.in API token that can be found at: https://pinboard.in/settings/password"
  :group 'pinboard
  :type 'string)

(defcustom pinboard-tags-cache-file-name (locate-user-emacs-file "pinboard-tags-cache")
  "Where the pinboard.in cache file for tags is located")

(defcustom pinboard-completing-read-function 
  (if (fboundp 'ido-completing-read) 'ido-completing-read 'completing-read)
  "The function to use for choosing tags"
  :group 'pinboard
  :type 'function)

;;; API:

(defun pinboard-api-add (url &optional description tags extended)
  "Add the url to pinboard.in with optional details - will cause an error if it could not complete"
  (url-retrieve
   (pinboard-auth-request "posts/add" (pinboard-build-add-request url description tags extended))
   (lambda (status)
     (let* ((m-error (plist-get status :error))
            (full-response (car (pinboard-response (current-buffer))))
            ;; (result ((code . "done")))
            (response (cdr (assoc 'code (plist-get full-response 'result))))) 
       (when m-error
         (signal (car m-error) (cadr m-error)))
       (when (not (string-equal "done" response))
         (error "pinboard.in - could not complete adding %s because: %s" url response))))))

(defun pinboard-api-tags-get ()
  "Gets a full list of all user's tags - does not retain the count"
  (let* ((retrieved-buffer
          (url-retrieve-synchronously
           (pinboard-auth-request "tags/get")))
         (parsed-tree
          (pinboard-response retrieved-buffer)))
    (pinboard-parse-tags parsed-tree)))

;;; Supporting:

(defun pinboard-completing-read (&rest args)
  "Call the completing-read function defined through the variable pinboard-completing-read-function" 
  (apply pinboard-completing-read-function args))

(defun pinboard-gather-tags ()
  "Returns a string of tags - use C-j to break the ido loop"
  (interactive)
  (let* ((tag-list (pinboard--get-tags-from-cache-or-online))
         (selected-tags (delete-dups (pinboard--gather-tags tag-list))))
    (mapconcat 'identity selected-tags " ")))

(defun pinboard--load-tags-from-cache ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents pinboard-tags-cache-file-name)
    (read (current-buffer))))

(defun pinboard--get-tags-from-cache-or-online ()
  (when (not (file-exists-p pinboard-tags-cache-file-name))
    (pinboard-refresh-tags-cache))
  (pinboard--load-tags-from-cache))

(defun string-set-p (str)
  "Returns true if str is not-nil and not-empty"
  (when str (not (string-equal "" str))))

(defun pinboard--gather-tags (tag-list)
  "Recursively ask for a tag until an empty input is received (use C-j) for this"
  (interactive)
  (let ((tag (pinboard-completing-read "tag? " tag-list)))
    (if (string-set-p tag)
        (cons tag (pinboard--gather-tags tag-list))
      '())))

(defun pinboard-build-add-request (url &optional description tags extended)
  (when (not (string-set-p url)) (error "For this to be useful, we need a URL"))
  (concat "&url=" (url-hexify-string url)
	  (when (string-set-p description)
	    (concat "&description=" (url-hexify-string description)))
	  (when (string-set-p tags)
	    (concat "&tags=" (url-hexify-string tags)))
	  (when (string-set-p extended)
	    (concat "&extended=" (url-hexify-string extended)))
      ))

(defun pinboard-response (buffer)
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (xml-parse-region (point)
                            (point-max))))))

(defun pinboard-auth-request (call &optional parameters)
  ;; https://pinboard.in/settings/password
  (concat "https://api.pinboard.in/v1/" call "?auth_token=" pinboard-api-token parameters))

(defun pinboard-parse-tags (xml-parsed-tree)
  (let ((taglist
         (cdr (remove-if-not (lambda (x) (listp x))
                             (car xml-parsed-tree)))))
    (loop for (ignore (count-pair tag-pair)) in taglist
          collect (cdr tag-pair))))

(provide 'pinboard-api)

;;; pinboard-api.el ends here
