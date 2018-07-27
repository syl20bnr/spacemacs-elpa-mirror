;;; pushover.el --- Pushover API Access

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.2
;; Package-Version: 20170818.2103
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: notifications
;; URL: http://github.com/swflint/pushover.el
;; License: GNU General Public License version 3, or (at your option) any later version

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

;;; Commentary:
;;
;; This library provides a single function, pushover-send, which is
;; used to send notifications using the pushover service.

;;; Code:

(require 'cl-lib)

(defgroup pushover nil
  "Send notifications to the pushover service."
  :group 'communication)

(defcustom pushover-api-key "agd6sapvp5xcdmbuo1qdpc7mddscri"
  "Pushover API key."
  :type 'string
  :group 'pushover)

(defcustom pushover-user-key nil
  "Pushover user key."
  :type 'string
  :group 'pushover)

;;;### autoload
(cl-defun pushover-send (title message &key url url-title sound (html t) (priority 0) retry expire)
  "Send notification to the pushover service.

TITLE is the message title.
MESSAGE is the message itself.
URL is a URL to be shown.
URL-TITLE is the title of the given URL.
SOUND is the sound to be used.
HTML, default t, signals to Pushover that the message is in HTML.
PRIORITY is an integer from -2 to 2, with 0 (mid-priority) being the default.
RETRY is the number of seconds between retries for emergency (2) priority.
EXPIRE is number of seconds before an emergency priority message expires."
  (let ((url-request-method "POST")
        (url-request-data (concat (format "token=%s&user=%s&title=%s&message=%s&priority=%s&timestamp=%s"
                                          pushover-api-key
                                          pushover-user-key
                                          (url-encode-url title)
                                          (url-encode-url message)
                                          priority
                                          (current-time))
                                  (if html
                                      "&html=1"
                                    "")
                                  (if url
                                      (format "&url=%s" (url-encode-url url))
                                    "")
                                  (if url-title
                                      (format "&url_title=%s" (url-encode-url url-title))
                                    "")
                                  (if sound
                                      (format "&sound=%s" sound)
                                    "")
                                  (if retry
                                      (format "&retry=%s" retry)
                                    "")
                                  (if expire
                                      (format "&expire=%s" expire)
                                    ""))))
    (url-retrieve "https://api.pushover.net/1/messages.json" (lambda (status) (kill-buffer (current-buffer)) t))))

(when (fboundp 'alert-define-style)
  (alert-define-style 'pushover :title "Pushover Alerter"
                      :notifier (lambda (info)
                                  (let ((title (plist-get info :title))
                                        (message (plist-get info :message)))
                                    (pushover-send title message)))))

(provide 'pushover)

;;; pushover.el ends here
