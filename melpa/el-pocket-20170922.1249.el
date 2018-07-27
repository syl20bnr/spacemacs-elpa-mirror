;;; el-pocket.el --- Read and write to Pocket (getpocket.com) -*- lexical-binding: t -*-
;; Author: Tod Davies <davies.t.o@gmail.com>
;; Created: 4 Aug 2014
;; Last Updated: 28 Jan 2015
;; Version: 0.2
;; Package-Version: 20170922.1249
;; Package-X-Original-Version: 20150202.1528
;; Url: http://github.com/pterygota/el-pocket
;; Keywords: emacs, pocket, bookmarks
;; Package-Requires: ((web "0.5.2") (emacs "24"))
;; License: GPL-3+
;; Installation/Setup/Usage

;;; Commentary:

;; Put this file in your load-path somewhere and require it.

;; Or use the MELPA repository to install it via package-install.

;; Now do an M-x el-pocket-authorize RET. The first time you do this
;; you will be directed to the oauth/request page, where you can click
;; on authorize. After authorizing, you may see a message that makes
;; it seem like it didn't work (e.g. broken images or redirect
;; failures). This is because we haven't yet set up proper
;; authorization.

;; Now, return to emacs and do M-x el-pocket-authorize RET again. This
;; time you should get an access token, and it will be saved to
;; ~/.el-pocket-auth.json.

;; Once this is done you should be able to use M-x el-pocket-add RET to add URLs.

;; Reading articles still neeed to be added. Maybe it could be
;; integrated using the Diffbot's Article Extraction API

;; Now you can add these lines to your init file for future use:

;;  (require 'el-pocket)
;;  (el-pocket-load-auth)

;;; History:

;; Changes from 0.1 to 0.2:
;; * Remove '*' from names.
;; * Create a customization group and add some doc strings.
;; * Use defvar instead of setq'ing undefined variables.
;; * Address other compilation warnings.

;;; Code:

(require 'json)
(require 'web)

;;various mouse-eared items
(defgroup el-pocket nil
  "Pocket"
  :prefix "el-pocket-"
  :group 'external)
(defcustom el-pocket-oauth-request-url "https://getpocket.com/v3/oauth/request"
  "URL to use for OAuth request.")
(defcustom el-pocket-oauth-authorize-url "https://getpocket.com/v3/oauth/authorize"
  "URL to use for OAuth authorization.")
(defvar el-pocket-request-token nil
  "Holds the request token")
(defvar el-pocket-access-token-and-username nil
  "Holds the current access token")
(defvar el-pocket-default-extra-headers (let ((extra-headers (make-hash-table :test 'equal)))
                                          (puthash 'Host "getpocket.com" extra-headers)
                                          (puthash 'Content-type "application/x-www-form-urlencoded; charset=UTF-8" extra-headers)
                                          (puthash 'X-Accept "application/json" extra-headers)
                                          extra-headers)
  "Default extra headers")

;;no use hiding this I suppose
(defcustom el-pocket-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key")

;;access-key and username stored here
(defcustom el-pocket-auth-file (expand-file-name "~/.el-pocket-auth.json")
  "JSON file to store the authorization.")

(defun el-pocket-load-auth ()
  (setq el-pocket-access-token-and-username
        (if (file-readable-p el-pocket-auth-file)
            (json-read-file el-pocket-auth-file))))

(defun el-pocket-save-auth ()
  (with-temp-file el-pocket-auth-file
    (insert (json-encode-alist el-pocket-access-token-and-username))))

(defun el-pocket-clear-auth ()
  (setq el-pocket-request-token nil)
  (setq el-pocket-access-token-and-username nil))

;; the authorization dance:
;; TODO - make a nice interface for this
;; TODO - maybe use the oauth or oauth2 package instead?
(defun el-pocket-authorize ()
  (interactive)
  (unless el-pocket-access-token-and-username
    (unless (el-pocket-load-auth)
      (if el-pocket-request-token
          (el-pocket-get-access-token)
        (el-pocket-get-request-token)))))

;; http post helper function
(defun el-pocket--post (url post-data-alist callback)
  "Post POST-DATA-ALIST to URL and then call the CALLBACK with data decoded as utf-8"
  (let ((post-data (make-hash-table :test 'equal)))
    (dolist (post-data-pair post-data-alist)
      (puthash (car post-data-pair) (cdr post-data-pair) post-data))
    (web-http-post
     (lambda (con header data)
       (unless (string= "200" (gethash 'status-code header))
         (error "error status :%s" (gethash 'status-code header)))
       (let ((data (decode-coding-string data 'utf-8)))
         (message "data received is:header=[%s],data = [%s]" header data)
         (funcall callback con header data)))
     :url url
     :data post-data
     :extra-headers el-pocket-default-extra-headers)))

;; once the request token is a-gotten,
;; and you've gone to the oauth/authorize page
;; and and done that, this will then get the
;; all-important access-token, huzzah!
(defun el-pocket-get-access-token ()
  "After authorizing, el-pocket-authorize again to call this and get an access-token."
  (el-pocket--post el-pocket-oauth-authorize-url
                   `((consumer_key . ,el-pocket-consumer-key)
                     (code . ,el-pocket-request-token))
                   (lambda (con header data)
                     (setq el-pocket-access-token-and-username
                           (json-read-from-string data))
                     (el-pocket-save-auth)
                     (display-message-or-buffer
                      "access a-gotten!"))))

;; we don't have a request token yet, so request
;; one, then send the user to oauth/authorize for
;; to authorize this shiz
(defun el-pocket-get-request-token ()
  "Request a request token, then direct the user to authorization URL"
  (el-pocket--post el-pocket-oauth-request-url
                   `((consumer_key . ,el-pocket-consumer-key)
                     (redirect_uri . "http://www.google.com" ))
                   (lambda (con header data)
                     (let* ((token (cdr (assoc 'code (json-read-from-string data))))
                            (url (concat "https://getpocket.com/auth/authorize?request_token=" token)))
                       (setq el-pocket-request-token token)
                       (kill-new url)
                       (display-message-or-buffer
                        (concat "authorize el-pocket at " url
                                " (copied to clipboard)\n"))
                       (browse-url url))
                     ;; (el-pocket-authorize)
                     )))

(defun el-pocket-access-granted-p ()
  "Do we have access yet?"
  el-pocket-access-token-and-username)

(defun el-pocket-access-not-granted ()
  (display-message-or-buffer
   "Do an M-x el-pocket-authorize to get access to pocket."))

;; skeleton function to test getting things from pocket
;; response is printed to *Messages*
;; TODO make this do useful things
(defun el-pocket-get ()
  "Gets things from your pocket."
  (if (el-pocket-access-granted-p)
      (el-pocket--post  "https://getpocket.com/v3/get"
                        `((consumer_key . ,el-pocket-consumer-key)
                          (access_token . ,(cdr (assoc 'access_token el-pocket-access-token-and-username)))
                          (count . "5")
                          (detailType . "simple"))
                        (lambda (con header data)
                          (json-read-from-string data)))
    (el-pocket-access-not-granted)))

;;oh my gosh
(defun el-pocket-add (url-to-add)
  "Add URL-TO-ADD to your pocket."
  (interactive
   (list
    (read-string "el-pocket url: ")))
  (if (el-pocket-access-granted-p)
      (el-pocket--post  "https://getpocket.com/v3/add"
                        `((consumer_key . ,el-pocket-consumer-key)
                          (access_token . ,(cdr (assoc 'access_token el-pocket-access-token-and-username)))
                          (url . ,url-to-add))
                        (lambda (con header data)
                          (json-read-from-string data)))
    (el-pocket-access-not-granted)))

(defun el-pocket-add-url (url &optional ignore)
  "Add URL to pocket."
  (interactive (browse-url-interactive-arg "URL: "))
  (el-pocket-add url))

(provide 'el-pocket)

;;; el-pocket.el ends here
