;;; datetime-format.el --- Datetime functions

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 16 May 2016
;; Version: 0.0.1
;; Package-Version: 20160612.1715
;; Package-Requires: ()
;; Keywords: datetime calendar
;; Homepage: https://github.com/zonuexe/emacs-datetime

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; (datetime-format 'atom) ;=> "2015-01-12T02:01:11+09:00"
;; (datetime-format 'atom (current-time)) ;=> "2015-01-12T02:01:11+09:00"
;; (datetime-format 'atom 0) ;=> "1970-01-01T09:00:00+09:00"
;; (datetime-format 'atom 0 :timezone "UTC") ;=> "1970-01-01T00:00:00+00:00"
;; (datetime-format 'atom "2015-01-12 02:01:11") ;=> "2015-01-12T02:01:11+09:00"
;; (datetime-format 'atom "2015-01-12 02:01:11" :timezone "Europe/Moscow") ;=> "2015-01-12T01:01:11+03:00"
;; (datetime-format 'atom-utc "2015-01-12 02:01:11") ;=> "2015-01-11T17:01:11Z"
;; (datetime-format 'atom nil :timezone "America/New_York") ;=> "2016-05-18T13:05:41-04:00"

;;; Code:
;;(require 'timezone)

(defconst datetime-format--fmt-atom
  '(local . "%Y-%m-%dT%H:%M:%S%:z")
  "ATOM date construct format (local time).

RFC4287: The Atom Syndication Format \"3.3.  Date Constructs\"
URL `https://www.ietf.org/rfc/rfc4287'")

(defconst datetime-format--fmt-atom-utc
  '(utc . "%Y-%m-%dT%H:%M:%SZ")
  "ATOM date construct format (UTC).

RFC4287: The Atom Syndication Format \"3.3.  Date Constructs\"
URL `https://www.ietf.org/rfc/rfc4287'")

(defconst datetime-format--fmt-cookie
  '(local . "%A, %d-%b-%Y %H:%M:%S %Z")
  "Cookie date format.

RFC6265: HTTP State Management Mechanism \"5.1.1.  Dates\"
URL `https://tools.ietf.org/html/rfc6265#section-5.1.1'")

(defconst datetime-format--fmt-rfc-822
  '(local . "%a, %d %b %y %H:%M:%S %z")
  "RFC 822 date-time format.

RFC822: Standard for ARPA Internet Text Messages
\"5. Date and Time Specification\"
URL `https://www.w3.org/Protocols/rfc822/#z28'")

(defconst datetime-format--fmt-rfc-850
  '(local . "%A, %d-%b-%y %H:%M:%S %Z")
  "RFC 850 \"Date\" line format.

RFC850: Standard for Interchange of USENET Messages \"2.1.4  Date\"
URL `https://www.ietf.org/rfc/rfc0850'")

(defconst datetime-format--fmt-rfc-1036
  '(local . "%a, %d %b %y %H:%M:%S %z")
  "RFC 1036 \"Date\" line format.

RFC1036 Standard for Interchange of USENET Messages \"2.1.2.  Date\"
URL `https://www.ietf.org/rfc/rfc1036'")

(defconst datetime-format--fmt-rfc-1123
  '(local . "%a, %d %b %Y %H:%M:%S %z")
  "RFC 1123 Date and Time format.

RFC1123: Requirements for Internet Hosts -- Application and Support
\"5.2.14  RFC-822 Date and Time Specification\"
URL `https://www.ietf.org/rfc/rfc1123'")

(defconst datetime-format--fmt-rfc-2822
  '(local . "%a, %d %b %Y %H:%M:%S %z")
  "RFC 2822 Date and Time format.

RFC2822: Internet Message Format \"3.3. Date and Time Specification\"
URL `https://www.ietf.org/rfc/rfc2822.txt'")

(defconst datetime-format--fmt-rfc-3339
  '(local . "%Y-%m-%dT%H:%M:%S%:z")
  "RFC 3339 Timestamp format.

RFC3339: Date and Time on the Internet: Timestamps
URL `https://www.ietf.org/rfc/rfc3339.txt'")

(defconst datetime-format--fmt-rss
  '(local . "%a, %d %b %y %H:%M:%S %z")
  "RSS pubDate element format.

Really Simple Syndication 2.0
URL `https://validator.w3.org/feed/docs/rss2.html'")

(defconst datetime-format--fmt-w3c
  '(local . "%Y-%m-%dT%H:%M:%S%:z")
  "W3C `Complete date plus hours, minutes and seconds' format.

W3C Date and Time Formats
URL `https://www.w3.org/TR/NOTE-datetime'")


;;;###autoload
(defun datetime-format (sym-or-fmt &optional time &rest option)
  "Use SYM-OR-FMT to format the time TIME and OPTION plist.

OPTION plist expect :timezone.
See URL `https://en.wikipedia.org/wiki/List_of_tz_database_time_zones'"
  (let ((timezone (plist-get option :timezone))
        is-utc format name)
    (cond
     ((integerp time) (setq time (datetime-format--int-to-timestamp time)))
     ((stringp time)  (setq time (datetime-format--parse-time-with-timezone time timezone))))
    (cond
     ((stringp sym-or-fmt)
      (setq format sym-or-fmt)
      (setq is-utc (equal "UTC" timezone)))
     ((symbolp sym-or-fmt)
      (setq name (intern (concat "datetime-format--fmt-" (symbol-name sym-or-fmt))))
      (unless (boundp name)
        (error (format "`%s' is invalid time format name."
                       (symbol-name sym-or-fmt))))
      (when (eq 'utc (car (eval name)))
        (setq is-utc t)
        (setq timezone nil))
      (setq format (cdr (eval name))))
     (:else (error "Wrong type argument")))
    (if (or is-utc (null timezone))
        (format-time-string format time is-utc)
      (datetime-format--with-timezone format time timezone))))

(defun datetime-format--int-to-timestamp (int)
  "Convert INT to time stamp list.

See describe `current-time' function."
  (let* ((low (- (lsh 1 16) 1)) (high (lsh low 16)))
    (list (lsh (logand high int) -16) (logand low int) 0 0)))

(defun datetime-format-convert-timestamp-dwim (time &optional timezone)
  ""
  (cond
   ((integerp time) (datetime-format--int-to-timestamp time))
   ((stringp  time) (datetime-format--parse-time-with-timezone time timezone))
   (:else (error "Error time format"))))

(defun datetime-format--with-timezone (fmt time timezone)
  "Use FMT to format the time TIME in TIMEZONE.

TIME is specified as (HIGH LOW USEC PSEC), as returned by
`current-time' or `file-attributes'."
  (let ((real-time-zone (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" timezone)
          (format-time-string fmt time))
      (setenv "TZ" real-time-zone))))

(defun datetime-format--parse-time-with-timezone (time timezone)
  ""
  (let ((real-time-zone (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" timezone)
          (apply #'encode-time (parse-time-string time)))
      (setenv "TZ" real-time-zone))))

(provide 'datetime-format)
;;; datetime-format.el ends here
