;;; imgbb.el --- Simple image upload client for imgbb.com -*-coding: utf-8; -*-
;;
;; Copyright (C) 2018 craven@gmx.net
;;
;; Author: Peter <craven@gmx.net>
;; URL: https://github.com/ecraven/imgbb.el
;; Package-Version: 20180609.1649
;; Package-X-Original-Version: 20180518
;; Package-Requires: ((emacs "24") (request "0.3.0"))
;; Version: 0.1
;; Keywords: extensions
;; Created: 2018-05-18
;;
;;; License:
;;
;; Licensed under the GPLv3.
;;
;;; Commentary:
;;
;; Run M-x imgbb-upload and enter an image filename. The URL of the uploaded image is shown in the minibuffer and put into the kill ring.
;;
;;; Code:
(require 'request)
(require 'cl-lib)

(defvar imgbb-url
  "https://imgbb.com/json"
  "The URL of the imgbb service.")

(defvar imgbb-parameters
  '((type . "file")
    (action . "upload"))
  "Default parameters to send.")

;;;###autoload
(defun imgbb-upload (filename)
  "Upload FILENAME to imgbb.com, show the image url and put it into the kill ring."
  (interactive "fImage file: ")
  (request imgbb-url
   :params imgbb-parameters
   :files `(("source" . (,(file-name-nondirectory filename) :file ,filename)))
   :parser 'json-read
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
             (message "Error uploading image.")))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let ((url (assoc-default 'url (assoc-default 'image (assoc-default 'image data)))))
                 (message "%s" url)
                 (kill-new url)))))
  (message "Started upload of %S to imgbb.com." filename))

(provide 'imgbb)
;;; imgbb.el ends here
