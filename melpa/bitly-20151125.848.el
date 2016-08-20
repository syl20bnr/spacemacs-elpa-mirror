;;; bitly.el --- Shorten URLs using the bitly.com shortener service

;; Copyright (C) 2013  Jorgen Schaefer <forcer@forcix.cx>

;; Version: 1.0
;; Package-Version: 20151125.848
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/bitly-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple mode to shorten URLs from Emacs.

;; Use (bitly-shorten URL) from an Emacs Lisp program, or
;; M-x bitly-url-at-point to replace the URL at point (or the region)
;; with a shortened version.

;; To use, go to https://bitly.com/a/oauth_apps to generate your personal
;; API access token. Then customize `bitly-access-token' and set it to
;; the token you just got.

;;; Code:

(require 'thingatpt)
(require 'json)

(defgroup bitly nil
  "The bitly URL shortening service."
  :prefix "bitly-"
  :group 'applications)

(defcustom bitly-access-token nil
  "The OAuth access token for bitly.

Get your personal token here: https://bitly.com/a/oauth_apps"
  :type 'string
  :group 'bitly)

(defvar bitly-base-url "https://api-ssl.bitly.com")

(defun bitly-shorten (long-url)
  "Return a shortened URL for LONG-URL."
  (let* ((url (format "%s/v3/shorten?access_token=%s&longUrl=%s"
                      bitly-base-url
                      (url-hexify-string bitly-access-token)
                      (url-hexify-string long-url)))
         (json-buffer (url-retrieve-synchronously url))
         (response (with-current-buffer json-buffer
                     (goto-char (point-min))
                     (search-forward "\n\n" nil t)
                     (json-read)))
         (status-code (cdr (assq 'status_code response))))
    (if (equal status-code 200)
        (cdr (assq 'url
                   (cdr (assq 'data response))))
      (error "Error %s calling bitly: %s"
             status-code
             (cdr (assq 'status_txt response))))))

;;;###autoload
(defun bitly-url-at-point ()
  "Replace the URL at point with a shortened one.

With an active region, use the region contents as an URL."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning)
                          (region-end))
                  (thing-at-point-bounds-of-url-at-point)))
        url)
    (when (not bounds)
      (error "No URL at point"))
    (setq url (bitly-shorten (buffer-substring-no-properties (car bounds)
                                                             (cdr bounds))))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert url))))

(provide 'bitly)
;;; bitly.el ends here
