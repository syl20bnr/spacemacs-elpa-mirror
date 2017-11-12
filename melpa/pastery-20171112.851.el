;;; pastery.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.15
;; Package-Version: 20171112.851
;; Package-Requires: ((emacs "24.4") (request "0.2.0"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'subr-x)

(defgroup pastery nil
  "Publish to pastery.net."
  :group 'application)

(defconst pastery-version "0.1.15"
  "Pastery for emacs version.")

(defvar pastery-url "https://www.pastery.net/api/paste/"
  "Pastery url.")

(defvar pastery-emacs-user-agent "Mozilla/5.0 (Emacs) pastery"
  "User-agent saying that we are in emacs.")

(defvar pastery-response-parser 'json-read
  "What to use to parse the response.")

(defcustom pastery-api-key ""
  "User's api key. Prefer adding to your ~/.emacs file."
  :type 'string
  :group 'pastery)

(defcustom pastery-default-duration "1440"
  "Default duration."
  :type 'string
  :group 'pastery)

(defun pastery--format-paste-item (paste-item)
  "Format each paste key/value from the paste json."
  (let ((key (car paste-item))
        (value (cdr paste-item)))
    (concat (symbol-name key)
            ": "
            (if (numberp value)
                (number-to-string value)
              value))))

(defun pastery--format-paste (paste)
  (mapconcat 'pastery--format-paste-item paste "\n"))

(defun pastery--format-list (pastes)
  "Format a SEQUENCE of pastes."
  (mapconcat 'identity
             (mapcar (lambda (x) (pastery--format-paste x)) pastes)
             "\n\n"))

(defun pastery--region-substring (beg end)
  "Get the region if available."
  (if (and beg end)
      (buffer-substring-no-properties beg end)
    ""))

(defun pastery--get-content ()
  "Get the content of the pastery from buffer or region."
  (if (region-active-p)
      (pastery--region-substring (point) (mark))
    (pastery--region-substring (point-min) (point-max))))

(defun pastery--query-item (key value)
  "Creates a query item only if available."
  (if value
      (concat (symbol-name key) "=" value)
    ""))

(defun pastery--ask (prefix default-value)
  "Prompts to a user to ask."
  (concat prefix "(" default-value ")" ":"))

(defun pastery--info ()
  "Get information to about the diff to create it's configuration"
  (let* ((title (read-string "Title: "))
         (dur (read-string (pastery--ask "Duration" pastery-default-duration)))
         (lang (read-string "Language: "))
         (maxviews (read-string "Max views: "))
         (files (read-string "Files: "))
         ;; finish reading strings
         (paste-title (if (not (string-empty-p title))
                          `(title . ,title)
                        nil))
         (paste-duration (if (not (string-empty-p dur))
                             `(duration . ,dur)
                           `(duration . ,pastery-default-duration)))
         (paste-lang (if (not (string-empty-p lang))
                         `(language . ,lang)
                       nil))
         (paste-max-views (if (not (string-empty-p maxviews))
                              `(max-views . ,maxviews)
                            nil))
         (paste-files (if (not (string-empty-p files))
                          `(files . ,files)
                        nil)))
    (cl-remove-if (lambda (x) (eq x nil))
                  (list paste-title
                        paste-duration
                        paste-lang
                        paste-max-views
                        paste-files))))

(defun pastery--request (method request-url request-data err succ)
  (request request-url
           :type method
           :parser 'json-read
           :data request-data
           :error err
           :success succ))

(defun pastery--check-content (content)
  "Print to buffer empty message if the CONTENT is empty."
  (if (string= content "")
      "No 'pastes' available."
    content))

(defun pastery--write-to-buffer (paste-buffer-name title content)
  "Place the data to a buffer."
  (with-current-buffer (get-buffer-create paste-buffer-name)
    (progn
      (message title)
      (erase-buffer)
      (insert title "\n\n"
              (pastery--check-content content)
              "\n")
      (view-buffer (current-buffer))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun pastery-submit ()
  "Create a pastery from a region."
  (interactive)
  (let* ((request-data (pastery--get-content))
         (from-buffer (current-buffer))
         (user-info (pastery--info))
         (paste-info (mapconcat
                      (lambda (x) (pastery--query-item (car x) (cdr x)))
                      user-info "&"))
         (request-url (concat pastery-url
                              "?api_key=" pastery-api-key "&"
                              paste-info)))
    (pastery--request "POST" request-url request-data
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (let* ((paste-id (cdr (assoc 'id data)))
                                (paste-buffer-name (concat "*pastery-" paste-id "*")))
                           (pastery--write-to-buffer paste-buffer-name
                                                     (concat "Pastery " paste-id)
                                                     (pastery--format-paste data))))))
    (set-buffer from-buffer)
    t))

;;;###autoload
(defun pastery-list ()
  "List all my pastes."
  (interactive)
  (let ((request-url (concat pastery-url "?api_key=" pastery-api-key)))
    (pastery--request "GET" request-url nil
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (pastery--write-to-buffer "*pastery-list*"
                                                   "Pastery List"
                                                   (pastery--format-list (cdar data))))))
    (message "Fetching list...")))

;;;###autoload
(defun pastery-get (paste-id)
  "Get a paste by id."
  (interactive
   (list (read-string "Paste ID: ")))
  (let ((request-url (concat (concat pastery-url paste-id "/")
                             (concat "?" "api_key" "=" pastery-api-key)))
        (paste-buffer-name (concat "*pastery-" paste-id "*")))
    (pastery--request "GET" request-url nil
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (pastery--write-to-buffer paste-buffer-name
                                                   "Pastery"
                                                   (pastery--format-list (cdar data))))))
    (message (concat "Fetching paste " paste-id "..."))))

;;;###autoload
(defun pastery-delete (paste-id)
  "Delete a paste by id."
  (interactive
   (list (read-string "Paste ID: ")))
  (let ((request-url (concat (concat pastery-url paste-id "/")
                             (concat "?" "api_key" "=" pastery-api-key))))
    (pastery--request "DELETE" request-url nil
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "...done."))))
    (message (concat "Deleting paste " paste-id "..."))))


(provide 'pastery)
;;; pastery.el ends here
