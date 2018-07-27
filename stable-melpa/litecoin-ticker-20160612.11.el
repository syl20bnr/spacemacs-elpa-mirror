;;; litecoin-ticker.el --- litecoin price in modeline

;; Copyright (C) 2016 by Zhe Lei.
;;
;; Author: Zhe Lei
;; Version: 0.2
;; Package-Version: 20160612.11
;; Package-X-Original-Version: 20160513
;; Package-Requires: ((json "1.2"))
;;
;; This file is not part of GNU emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'json)

(defgroup litecoin-ticker nil
  "litecoin-ticker"
  :group 'comms)

(defconst litecoin-ticker-url
  "https://btc-e.com/api/2/ltc_usd/ticker")

(defvar litecoin-ticker-mode-line nil)

(defvar litecoin-ticker-timer nil
  "litecoin ticker timer to update the price")

(defcustom litecoin-ticker-timer-interval 20
  "Timer interval to update the price"
  :type 'number
  :group 'litecoin-ticker)

(defcustom litecoin-ticker-symbol "$"
  "Price symbol to show in modeline, default $"
  :type 'string
  :group 'litecoin-ticker)

(defcustom litecoin-price-higher-than nil
  "litecoin price higher than the many price, displayed on mode-line
if nil, always displayed"
  :type 'number
  :group 'litecoin-ticker)

(defun litecoin-ticker-info ()
  "Retrieve the lastest litecoin price from `litecoin-ticker-url'"
  (let (info price)
    (with-current-buffer
	(url-retrieve-synchronously litecoin-ticker-url t)
      (setq info (car (json-read))))
    (setq price (assoc-default 'last info))
    (if litecoin-price-higher-than
	(if (>= price litecoin-price-higher-than)
	    (setq litecoin-ticker-mode-line
		  (format " %s%s" litecoin-ticker-symbol price))
	  (setq litecoin-ticker-mode-line nil))
      (setq litecoin-ticker-mode-line
	    (format " %s%s" litecoin-ticker-symbol price)))
    price))

;;;###autoload
(define-minor-mode litecoin-ticker-mode
  "Minor mode to display the lastest litecoin price"
  :init-value nil
  :global t
  :lighter litecoin-ticker-mode-line
  (if litecoin-ticker-mode
      (setq litecoin-ticker-timer
	    (run-at-time "0 sec" litecoin-ticker-timer-interval #'litecoin-ticker-info))
    (cancel-timer litecoin-ticker-timer)
    (setq litecoin-ticker-timer nil)))

(provide 'litecoin-ticker)
;;; litecoin-ticker.el ends here
