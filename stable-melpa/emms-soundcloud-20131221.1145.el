;;; emms-soundcloud.el --- EMMS source for Soundcloud audio sharing platform

;; Copyright (C) 2013 Ozan Sener

;; Author: Ozan Sener <ozan@ozansener.com>
;; Keywords: emms, soundcloud
;; Package-Version: 20131221.1145
;; URL: http://github.com/osener/emms-soundcloud
;; Version: 0.1
;; Package-Requires: ((emms "20131016") (json "1.2"))

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

;; After you've configured EMMS, load this file and give a Soundcloud set URL
;; to the relevant EMMS source commands.

;; Example usage:

;;    M-x emms-play-soundcloud-set RET
;;    http://soundcloud.com/devolverdigital/sets/hotline-miami-official
;;    RET

;;; Code:

(require 'url)
(require 'json)
(require 'emms)
(require 'emms-browser)
(require 'emms-source-file)
(require 'emms-source-playlist)

(defvar client-id "fbb79007b6072bbf5c6c1e75cb880ca2"
  "Key for the Soundcloud API.")

(defvar emms-soundcloud-api-base-url
  "http://api.soundcloud.com/"
  "URL for API calls.")

(defun emms-soundcloud-get-and-parse-json (url)
  "Parse JSON from given URL and return it as alist."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

(defcustom emms-soundcloud-display-header t
  "Non-nil means we display artwork and title for the set in the playlist."
  :type 'boolean
  :group 'emms-soundcloud)

(defun emms-soundcloud-resolve (url)
  "Make the proper API call for the given Soundcloud page URL and parse it."
  (let ((request-url (concat emms-soundcloud-api-base-url "resolve.json?"
                             "url=" (url-hexify-string url)
                             "&client_id=" client-id)))
    (emms-soundcloud-get-and-parse-json request-url)))

(defun emms-soundcloud-convert-track (track)
  "Read Soundcloud TRACK info into an emms-track."
  (let* ((stream-url (concat (cdr (assoc 'stream_url track))
                             "?client_id=" client-id))
         (emms-track (emms-track 'url stream-url))
         (title (concat (if emms-soundcloud-display-header "  " "")
                        (cdr (assoc 'title track))))
         (duration (/ (cdr (assoc 'duration track)) 1000)))
    (emms-track-set emms-track 'info-title title)
    (emms-track-set emms-track 'info-playing-time duration)
    emms-track))

(defun emms-soundcloud-insert-artwork (url)
  "Download the given artwork URL and insert it into the active playlist."
  (let* ((filename (substring (elt (url-generic-parse-url url) 6) 1))
         (directory (concat temporary-file-directory
                            (file-name-as-directory "soundcloud-artwork")))
         (path (concat directory filename)))
    (make-directory (concat temporary-file-directory
                            (file-name-as-directory "soundcloud-artwork")) t)
    (when (not (file-exists-p path))
      (url-copy-file url path))
    (emms-browser-insert-cover path)))

;;;###autoload (autoload 'emms-play-soundcloud-set "emms-soundcloud" nil t)
;;;###autoload (autoload 'emms-add-soundcloud-set "emms-soundcloud" nil t)
(define-emms-source soundcloud-set (url)
  "An EMMS source for Soundcloud sets (aka playlists)."
  (interactive (list (read-string "Soundcloud set URL: ")))
  (let* ((result (emms-soundcloud-resolve url))
         (title (cdr (assoc 'title result)))
         (artwork-url (cdr (assoc 'artwork_url result)))
         (tracks (cdr (assoc 'tracks result)))
         (emms-tracks (mapcar #'emms-soundcloud-convert-track tracks)))
    (when emms-soundcloud-display-header
      (emms-soundcloud-insert-artwork artwork-url)
      (insert " " (propertize title 'face 'emms-browser-album-face)
              "\n"))
    (mapc #'emms-playlist-insert-track emms-tracks)))

(provide 'emms-soundcloud)

;;; emms-soundcloud.el ends here
