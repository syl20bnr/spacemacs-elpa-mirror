;;; kodi-remote.el --- functions to remote control a kodi instance

;; Copyright (C) 2015 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>
;; URL: http://github.com/spiderbit/kodi-remote.el
;; Package-Version: 20160712.1652
;; Package-Requires: ((request "0.2.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Emacs Remote Control functions for Kodi
;; including a function to play directly videos from youtube and
;; other sites see youtube-dl for supported sites.
;;

;;; Code:

(require 'request)
(require 'json)


(defvar kodi-host-name "localhost:9090")


(defun kodi-json-url ()
  "Function to create the full json-url of the kodi-instance."
  (concat "http://" kodi-host-name "/jsonrpc")
  )


;;;###autoload
(defun kodi-remote-play-pause ()
  "Toggle playing of the audio stream in kodi.
for controlling the video player you need to change the playerid.
I think the playerid for video is 1."
  (interactive "p")
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode '(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . "Player.PlayPause")
			("params" . (
				     ("playerid" . 1)
				     )
			 )))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))


;;;###autoload
(defun kodi-remote-music ()
  "Start musik playing in kodi in party mode."
  (interactive)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode '(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . "Player.Open")
			("params" . (
				     (
				      "item" .
				      (
				       ("partymode" . "music")
				       )
				      )
				     )
			 )))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  )


(defun kodi-remote-playlist-previous ()
  "Previous song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "previous"))

(defun kodi-remote-playlist-next ()
  "Next song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "next"))

(defun kodi-remote-playlist-goto (pos)
  "Function to set the POS of kodi musik player."
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)("jsonrpc" . "2.0")
			("method" . "Player.GoTo")
			("params" . (("playerid" . 0)
				     ("to" . ,pos)))))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  )


(defun kodi-remote-play-url (url)
  "Plays either direct links to video files or plugin play command URLs."
  (interactive "surl: ")
  (let* ((json (json-encode `(("id" . 1)("jsonrpc" . "2.0")("method" . "Player.Open")("params" . (("item" .  (("file" . ,url)))))))))
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Clear")("params" . (("playlistid" . 1)))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)
    (request
     (kodi-json-url)
     :type "POST"
     :data json
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)))


;;;###autoload
(defun kodi-remote-play-video-url (video-url)
  "Sends urls from videos like youtube to kodi.
it depends on having youtube-dl installed because that was the only way
I got it to run.  Using quvi to get the url or dircectly sending a play
command to the plugin did both not work.
could be used for other sites, too.  whatever youtube-dl supports.
Argument VIDEO-URL A Url from a youtube video."
  (interactive "surl: ")
  (let ((url
	 (substring
	  (shell-command-to-string
	   (concat "youtube-dl -f best -g " video-url)) 0 -1)))
    (kodi-remote-play-url url)))



;;; Some code where I tried to get appending working:

;; (defun kodi-remote-append-url (url)
;;   "appends video urls to the video queue, either pure urls to video files
;; or plugin command urls"
;;   (interactive "surl: ")
;;   (setq json (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Add")("params" . (("playlistid" . 1)("item" .  (("file" . "url"))))))))
;;   (setq json-with-url (replace-regexp-in-string "url" url json))
;;   (request
;;    (kodi-json-url)
;;    :type "POST"
;;    :data json-with-url
;;    :headers '(("Content-Type" . "application/json"))
;;    :parser 'json-read)
;;   )



;; (defun kodi-remote-append-video-url (video-url)
;;   "sends urls from videos like youtube to kodi.
;; it depends on having youtube-dl installed because that was the only way
;; I got it to run. Using quvi to get the url or dircectly sending a play
;; command to the plugin did both not work.
;; could be used for other sites, too. whatever youtube-dl supports."
;;   (interactive "surl: ")
;;   (let ((url
;; 	 (substring
;; 	  (shell-command-to-string
;; 	   (concat "youtube-dl -f best -g " video-url)) 0 -1)))
;;     (kodi-remote-append-url url)))


;;; Code for appending ends here


(provide 'kodi-remote)
;;; kodi-remote.el ends here
