;;; urlenc.el --- URL encoding/decoding utility for Emacs.

;; Copyright (C) 2012  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: url
;; Package-Version: 20140116.1456
;; URL: https://github.com/buzztaiki/urlenc-el

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

;;; Commentary:

;; 

;;; Code:

(require 'url-util) 

(defcustom urlenc:default-coding-system 'utf-8
  "Default encode/decode coding-system for `urlenc'."
  :group 'urlenc
  :type 'coding-system)

(defun urlenc:decode-string (url cs)
  "Decode URL with coding system CS."
  (decode-coding-string (url-unhex-string (string-to-unibyte url)) cs))

(defun urlenc:encode-string (url cs)
  "Encode URL with coding system CS."
  (url-hexify-string (encode-coding-string url cs)))

(defun urlenc:replace-region (start end func cs)
  (let ((url (buffer-substring start end))
	(marker (point-marker)))
    (goto-char start)
    (delete-region start end)
    (insert (funcall func url cs))
    (goto-char marker)))

(defun urlenc:read-cs ()
  (let ((def urlenc:default-coding-system))
    (read-coding-system (format "coding-system(default: %s): " def) def)))

(defun urlenc:insert-read ()
  (list (read-string "url: ")
	(urlenc:read-cs)))

(defun urlenc:region-read ()
  (list (region-beginning) (region-end)
	(urlenc:read-cs)))

;;;###autoload
(defun urlenc:decode-region (start end cs)
  "Decode region between START and END as url with coding system CS."
  (interactive (urlenc:region-read))
  (urlenc:replace-region start end 'urlenc:decode-string cs))

;;;###autoload
(defun urlenc:encode-region (start end cs)
  "Encode region between START and END as url with coding system CS."
  (interactive (urlenc:region-read))
  (urlenc:replace-region start end 'urlenc:encode-string cs))

;;;###autoload
(defun urlenc:decode-insert (url cs)
  "Insert decoded URL into current position with coding system CS."
  (interactive (urlenc:insert-read))
  (insert (urlenc:decode-string url cs)))

;;;###autoload
(defun urlenc:encode-insert (url cs)
  "Insert encoded URL into current position with coding system CS."
  (interactive (urlenc:insert-read))
    (insert (urlenc:encode-string url cs)))

(provide 'urlenc)
;;; urlenc.el ends here
