;;; lodgeit.el --- Paste to a lodgeit powered pastebin

;; Copyright (C) 2015 Eric Larson

;; Author: Eric Larson <eric@ionrock.org>
;; Created: 9 Mar 2012
;; Version: 1.0
;; Package-Version: 20150312.1349
;; Keywords: pastebin lodgeit
;; X-URL: https://github.com/ionrock/lodgeit-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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

;; `lodgeit.el` allows you to easily submit regions to a lodgeit base
;; pastebin.
;;
;; Installation:
;;
;;    (require 'lodgeit)
;;
;; You can then configure the pastebin you want to use via the
;; `lodgeit-pastebin-base` variable.
;;
;;    (setq lodgeit-pastebin-base "http://paste.openstack.org/")
;;
;; Usage:
;;
;; Select some region and `M-x lodgeit-paste`. The major mode should
;; be used to properly highlight the paste on the server. The URL is
;; added to the kill ring for pasting as needed.
;;

;;; Code:
(require 'json)

;; TODO: Make this a defcustom
(defvar lodgeit-pastebin-base "http://paste.openstack.org")

(defun lodgeit-find-language ()
  "Try to find the language based on the major mode of the buffer."
  (car (split-string (symbol-name major-mode) "-mode")))

(defun lodgeit-paste-json-body (code)
  "Create a JSON based on the CODE for submitting to the pastebin."
  (json-encode `(,(lodgeit-find-language) ,code)))

(defun lodgeit-new-paste-handler (status)
  "A handler for the response STATUS."
  (search-forward "\n\n")  ;; move past the headers
  (let* ((data (buffer-substring (point) (buffer-end 1)))
	 (paste-id (assoc-default 'data (json-read-from-string data)))
	 (paste-url (format "%s/show/%s/" lodgeit-pastebin-base paste-id)))
    (message (format "Paste URL: %s" paste-url))
    (with-temp-buffer
      (insert paste-url)
      (kill-new (buffer-string)))))

(defun lodgeit-create-paste (message)
  "Create a new paste.
The MESSAGE must be a JSON encoded string."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data message)
	(paste-url (format "%s/json/?method=pastes.newPaste" lodgeit-pastebin-base)))
    (url-retrieve paste-url 'lodgeit-new-paste-handler)))

;;;###autoload
(defun lodgeit-paste (start end)
  "Paste the selected area to the lodgeit pastebin."
  (interactive "r")
  (let* ((content (buffer-substring (mark) (point)))
	 (message (lodgeit-paste-json-body content)))
    (lodgeit-create-paste message)))


(provide 'lodgeit)

;;; lodgeit.el ends here
