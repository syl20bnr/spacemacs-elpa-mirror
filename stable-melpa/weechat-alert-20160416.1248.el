;;; weechat-alert.el --- Weechat notifier using alerts  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016 Andreas Klein
;;
;; Author: Andreas Klein <git@kungi.org>
;; Keywords: irc chat network weechat
;; Package-Version: 20160416.1248
;; URL: https://github.com/kungi/weechat-alert
;; Version: 0.0.1
;; Package-Requires: ((weechat "0.3.1") (cl-lib "0.5") (alert "1.2"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Have a look at the alert package documentation
;; https://github.com/jwiegley/alert. A basic setup for the alerting
;; system is shown there.
;;
;;; Code:

(require 'weechat)
(require 'cl-lib)
(require 'alert)

(defgroup weechat-alert nil
  "Weechat alert customization options"
  :group 'emacs)

(defcustom weechat-alert-category
  'chat
  "Symbol of the alert category for all weechat alerts.
   You can filter alerts based on this value."
  :type 'symbol
  :group 'weechat-alert)

(defun weechat-alert-send (title text)
  (alert text :title title :category weechat-alert-category))

(defun weechat-alert-handler (type &optional sender text _date buffer-ptr)
  (setq text (if text (weechat-strip-formatting text)))
  (setq sender (if sender (weechat-strip-formatting sender)))
  (cl-case type
    (:highlight
     (weechat-alert-send
      (format "Highlight from %s"
              sender)
      (format "in %s: %s"
              (weechat-buffer-name buffer-ptr)
              text)))
    (:query
     (weechat-alert-send
      (format "Query from %s"
              sender)
      (format "%s"
              text)))
    (:disconnect
     (weechat-alert-send
      "Disconnected from WeeChat"
      ""))))

(add-hook 'weechat-notification-handler-functions
          'weechat-alert-handler)

(provide 'weechat-alert)

;;; weechat-alert.el ends here
