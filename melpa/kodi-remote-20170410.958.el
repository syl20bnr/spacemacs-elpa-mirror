;;; kodi-remote.el --- Remote Control for Kodi

;; Copyright (C) 2015-2016 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>
;; URL: http://github.com/spiderbit/kodi-remote.el
;; Package-Version: 20170410.958
;; Package-Requires: ((request "0.2.0")(let-alist "1.0.4")(json "1.4")(elnode "20140203.1506"))
;; Keywords: kodi tools convinience

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

;; A major to remote control kodi instances.
;; First specify the hostname/ip of your kodi webserver:
;; (setq kodi-host-name "my-htpc:8080")
;; Then start the Remote with the command:
;; 'kodi-remote-keyboard'
;; Alternativly start the Series Overview with the command:
;; 'kodi-remote-series'
;; Also open kodi Video Playlist with the command:
;; 'kodi-remote-playlist'


;;; Code:

(require 'json)
(require 'request)
(require 'let-alist)
(require 'subr-x)
(require 'elnode)

(defvar kodi-host-name "localhost:8080")
(defvar kodi-active-player -1)
(defvar kodi-active-window nil)
(defvar kodi-fullscreen nil)
(defvar kodi-volume nil)
(defvar kodi-properties nil)
(defvar kodi-request-running nil)
(defvar kodi-unseen-visible nil)
(defvar kodi-selected-show nil)
(defvar-local kodi-remote-refresh-function nil
  "The name of the function used to redraw a buffer.")
(defvar kodi-network-interface nil
  "network interface connecting to kodi.")
(defvar kodi-elnode-directory nil
  "the temp dir to use for serving streams to kodi.")

(defun kodi-client-ip ()
  "Function to create the local client ip address."
  (let* ((ip-vector
	  (seq-take
	   (assoc-default
	    kodi-network-interface
	    (network-interface-list))
	   4)))
    (mapconcat 'number-to-string ip-vector ".")))

(defun kodi-json-url ()
  "Function to create the full json-url of the kodi-instance."
  (concat "http://" kodi-host-name "/jsonrpc"))

(defun kodi-remote-post (method params)
  "Function to send post requests to the kodi instance.
Argument METHOD kodi json api argument.
Argument PARAMS kodi json api argument."
  (let* ((request-data
	  `(("id" . 0)
	   ("jsonrpc" . "2.0")
	   ("method" . ,method))))
    (if (equal params nil) ()
    	(setq request-data
	      (append request-data params)))
    ;; (print request-data)
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode request-data)
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)
    ))

(defun kodi-remote-get (method params)
  "Function to send get requests to the kodi instance.
Argument METHOD kodi json api argument.
Argument PARAMS kodi json api argument."
  (setq kodi-request-running t)
  (let* ((request-data
	  `(("id" . 0)
	    ("jsonrpc" . "2.0")
	    ("method" . ,method))))
    (if (equal params nil) ()
      (setq request-data
	    (append request-data params)))
    ;; (print request-data)
    (request
     (kodi-json-url)
     :data (json-encode request-data)
     :headers '(("Content-Type" . "application/json"))
     :success (cl-function (lambda (&key data &allow-other-keys)
			     (when data
			       ;; (print data)
			       (setq kodi-properties (let-alist (json-read-from-string data)
						       .result))
			       ;; (message kodi-properties)
			       ;; (print (aref (let-alist kodi-properties .episodedetails) 0))
			       (setq kodi-request-running nil)
			       )))
     :error (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
			   (message "Got error: %S" error-thrown)))
     ;; :complete (lambda (&rest _) (message "Finished!"))
     :parser 'buffer-string)))

;;;###autoload
(defun kodi-remote-music ()
  "Start musik playing in kodi in party mode."
  (interactive)
  (let* ((params '(("params" . (("item" . (("partymode" . "music"))))))))
    (kodi-remote-post "Player.Open" params)))

;;;###autoload
(defun kodi-remote-play-pause ()
  "Toggle play/pause of active player."
  (interactive)
  (kodi-remote-get-active-player-id)
  (let* ((params
	  `(("params" . (("playerid" . ,kodi-active-player))))))
    (kodi-remote-post "Player.PlayPause" params)))

;;;###autoload
(defun kodi-remote-stop ()
  "Stopps active player."
  (interactive)
  (kodi-remote-get-active-player-id)
  (sit-for 0.01)
  (let* ((params
	  `(("params" . (("playerid" . ,kodi-active-player))))))
    (kodi-remote-post "Player.Stop" params)))

(defun kodi-remote-player-seek (direction)
  "Seek active player.
Argument DIRECTION which direction and how big of step to seek."
  (kodi-remote-get-active-player-id)
  (let* ((params
	  `(("params" . (("playerid" . ,kodi-active-player)
			 ("value" . ,direction))))))
    (kodi-remote-post "Player.Seek" params)))

(defun kodi-remote-play-database-id (id)
  "Play series in database with given ID."
  (let* ((params
	  `(("params" . (("item" . (("episodeid" . ,id))))))))
    (kodi-remote-post "Player.Open" params)))

(defun kodi-remote-play-playlist-item (position)
  "Play series in playlist position with given ID."
  (let* ((params
	  `(("params" . (("item" . (("playlistid" . 1)
				    ("position" . ,position))))))))
    (kodi-remote-post "Player.Open" params)))

;;;###autoload
(defun kodi-remote-toggle-fullscreen ()
  "Toggle Fullscreen."
  (interactive)
  (kodi-remote-get-active-player-id)
  (let* ((params
	  `(("params" . (("fullscreen" . "toggle"))))))
    (kodi-remote-post "Gui.SetFullScreen" params)))

;;;###autoload
(defun kodi-remote-set-volume (offset)
  "Change volume recording to OFFSET."
  (interactive)
  (kodi-remote-get-volume)
  (let* ((vol (+ kodi-volume offset)))
    (let* ((params
	    `(("params" . (("volume" . ,vol))))))
      (kodi-remote-post "Application.SetVolume" params)))
    (kodi-remote-sit-for-done))

(defun kodi-remote-input (input)
  "Function to send post INPUT json requests."
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . ,input)
			))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))

(defun kodi-remote-input-execute-action (action)
  "Function to send post ACTION json requests."
  (let* ((params
	  `(("params" . (("action" . ,action))))))
    (kodi-remote-post "Input.ExecuteAction" params)))

;; todo: need to compare to other active windows (like musik) for actions.
;;;###autoload
(defun kodi-remote-input-left ()
  "Move left in menu or seek small backward."
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.01)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "smallbackward")
    (kodi-remote-input "Input.Left")))

;;;###autoload
(defun kodi-remote-input-right ()
  "Move right in menu or seek small forward."
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.01)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "smallforward")
    (kodi-remote-input "Input.Right")))

;;;###autoload
(defun kodi-remote-input-up ()
  "Move up in menu or seek big forward."
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.01)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "bigforward")
    (kodi-remote-input "Input.Up")))

;;;###autoload
(defun kodi-remote-input-down ()
  "Move down in menu or seek big backward."
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.01)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "bigbackward")
    (kodi-remote-input "Input.Down")))

;;;###autoload
(defun kodi-remote-input-back ()
  "Move back menu."
  (interactive)
  (kodi-remote-input "Input.Back"))

;;;###autoload
(defun kodi-remote-input-delete ()
  "Delete selected file."
  (interactive)
  (kodi-remote-input-execute-action "delete"))

;;;###autoload
(defun kodi-remote-input-context-menu ()
  "Activate context menu."
  (interactive)
  (kodi-remote-input "Input.ContextMenu"))

;;;###autoload
(defun kodi-remote-input-home ()
  "Switch to the home screen."
  (interactive)
  (kodi-remote-input "Input.Home"))

;;;###autoload
(defun kodi-remote-input-enter ()
  "Select active item."
  (interactive)
  (kodi-remote-input-execute-action "select"))

;;;###autoload
(defun kodi-remote-playlist-previous ()
  "Previous song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "previous"))

;;;###autoload
(defun kodi-remote-playlist-next ()
  "Next song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "next"))

(defun kodi-remote-get-volume ()
  "Poll current volume."
  (let* ((params
	  '(("params" . (("properties" . ("volume")))))))
    (kodi-remote-get "Application.GetProperties" params))
  (kodi-remote-sit-for-done)
  (setq kodi-volume (let-alist kodi-properties .volume)))

(defun kodi-remote-get-series-episodes (&optional show-id)
  "Poll availible series episodes.
Optional argument SHOW-ID limits to a specific show."
  (let* ((params
	      `(("params" . (( "tvshowid" . ,show-id )
			     ))
		)
	      ))
    (kodi-remote-get "VideoLibrary.GetEpisodes" params)))

(defun kodi-remote-get-unwatched-series-episodes (&optional show-id)
  "Poll unwatches episodes from show.
Optional argument SHOW-ID limits to a specific show."
  ;; (setq show-id nil)
  (if (integerp show-id)
      (let* ((params
	      `(("params" . (( "tvshowid" . ,show-id )
			     ("filter" .
			      (("field" . "playcount")
			       ("operator" . "lessthan")
			       ("value" . "1" ))
			      )
			     ("properties" .
			      ["title" "episode"])
			     ))
		)
	      ))
	(kodi-remote-get "VideoLibrary.GetEpisodes" params))
    (let* ((params
	    '(("params" . (("properties" .
			    ["title" "watchedepisodes" "episode"])
			   ("filter" .
			    (("field" . "playcount")
			     ("operator" . "lessthan")
			     ("value" . "1" ))
			    ))
	       )
	      )))
      (kodi-remote-get "VideoLibrary.GetEpisodes" params))))

(defun kodi-remote-get-show-list ()
  "Poll unwatched show."
  (let* ((params
  	  '(("params" . (("properties" .
			  ["title" "watchedepisodes" "episode"]))))))
  (kodi-remote-get "VideoLibrary.GetTVShows" params)))

(defun kodi-remote-playlist-add-episode ()
  "Add episode to playlist."
  (interactive)
  (let* ((params `(("params" .
		    (("playlistid" . 1)
		     ("item" .
		      (("episodeid" . ,(tabulated-list-get-id)))))))))
    (kodi-remote-post "Playlist.Add" params)))

(defun kodi-remote-playlist-play ()
  "Add episode to playlist."
  (interactive)
  (let* ((params `(("params" .
		    (("item" . (("playlistid" . 1))))))))
    (kodi-remote-post "Player.Open" params)))

(defun kodi-remote-playlist-clear ()
  "Add episode to playlist."
  (interactive)
  (let* ((params `(("params" .
		    (("playlistid" . 1))))))
    (kodi-remote-post "Playlist.Clear" params)))

;; ;; (setq elnode-webserver-docroot "~/webroot")
;; ;; (setq elnode-send-file-program (locate-file "cat" exec-path))
;; (setq elnode-send-file-program "cat")
;; (defconst kodi-elnode-handler
;;    (elnode-webserver-handler-maker "/tmp/webroot/")
;;    )


(defun kodi-remote-playlist-add-url (url)
  "Add item/video to playlist."
  (interactive "sUrl: ")
  (let* ((params `(("params" . (("playlistid" . 1)
				("item" . (("file" . ,url))))))))
    (kodi-remote-post "Playlist.Add" params)))


(defun kodi-remote-playlist-remove ()
  "Remove item/video from playlist."
  (interactive)
  (let* ((params `(("params" . (("playlistid" . 1)
				("position" . ,(tabulated-list-get-id)))))))
    (kodi-remote-post "Playlist.Remove" params)))


(defun kodi-remote-playlist-get ()
  "Requests playlist items."
  (let* ((params
  	  '(("params" . (("properties" . ["duration" "runtime" "title"])
			 ("playlistid" . 1))))))
    (kodi-remote-get "Playlist.GetItems" params)))

(defun kodi-remote-playlist-move-up ()
  "Moves item/video up in the playlist."
  (interactive)
  (let* ((position1 (tabulated-list-get-id))
	 (position2 (1- position1)))
    (if (>= position2 0)
	(let* ((params `(("params"
			  . (("playlistid" . 1)
			     ("position1" . ,position1)
			     ("position2" . ,position2))))))
	  (kodi-remote-post "Playlist.Swap" params))
      ;; (kodi-remote-playlist-draw)
      )))

(defun kodi-remote-playlist-move-down ()
  "Moves item/video up in the playlist."
  (interactive)
  (let* ((position1 (tabulated-list-get-id))
	 (position2 (1+ position1)))
    (if (< position2 (length (cdr ( assoc 'items kodi-properties))))
	(let* ((params `(("params"
			  . (("playlistid" . 1)
			     ("position1" . ,position1)
			     ("position2" . ,position2))))))
	  (kodi-remote-post "Playlist.Swap" params))
      ;; (kodi-remote-playlist-draw)
      )))

;; (defun kodi-remote-playlists-get ()
;;   "Requests playlist items."
;;   (let* ((params nil
;;   	  ))
;;     (kodi-remote-get "Playlist.GetPlaylists" params)))

(defun kodi-remote-video-scan ()
  "Update availible/new videos."
  (kodi-remote-post "VideoLibrary.Scan" nil))

(defun kodi-remote-get-episode-details (id)
  "Poll details of a episode.
Argument ID kodi series database identifier."
  (let* ((params
	  `(("params" . (("episodeid" . ,id)
			 ("properties" . ("playcount")))))))
    (kodi-remote-get "VideoLibrary.GetEpisodeDetails" params))
  (sit-for 0.02))

(defun kodi-remote-get-active-window ()
  "Update currently active window."
  (let* ((params
	  '(("params" . (("properties" . ("currentwindow")))))))
    (kodi-remote-get "Gui.GetProperties" params))
  (sit-for 0.1)
  (setq kodi-active-window (let-alist kodi-properties .currentwindow.label)))

(defun kodi-remote-get-active-player-id ()
  "Update currently active player."
  (kodi-remote-get "Player.GetActivePlayers" nil)
  (kodi-remote-sit-for-done)
  (setq kodi-active-player ( let-alist (elt kodi-properties 0) .playerid)))

;;;###autoload
(defun kodi-remote-volume-decrease ()
  "Decrease the volume of kodi."
  (interactive)
  (kodi-remote-set-volume -10))

;;;###autoload
(defun kodi-remote-volume-increase ()
  "Increase the volume of kodi."
  (interactive)
  (kodi-remote-set-volume 10))

;;;###autoload
(defun kodi-remote-is-fullscreen ()
  "Update fullscreen status."
  (let* ((params
	  '(("params" . (("properties" . ("fullscreen")))))))
    (kodi-remote-get "Gui.GetProperties" params))
  (let-alist kodi-properties .fullscreen))

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
   :parser 'json-read))

;;;###autoload
(defun kodi-remote-play-url (url)
  "Plays either direct links to video files or plugin play command URLs."
  (interactive "surl: ")
  (let* ((json (json-encode `(("id" . 1)("jsonrpc" . "2.0")("method" . "Player.Open")
			      ("params" . (("item" .  (("file" . ,url)))))))))
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Clear")
			  ("params" . (("playlistid" . 1)))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)
    (request
     (kodi-json-url)
     :type "POST"
     :data json
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)))

;FIXME: use quvi instead of youtube-dl
;;;###autoload
(defun kodi-remote-play-video-url (video-url)
  "Sends urls from videos like youtube to kodi.
Could be used for other sites, too.  whatever youtube-dl
supports.  Argument VIDEO-URL A Url from a youtube video."
  (interactive "surl: ")
  (let* ((response
	 (shell-command-to-string
	  (concat "youtube-dl -f best -g -e " video-url))))
    (let* ((url (nth 1 (split-string response "\n")))
	   (title (nth 0 (split-string response "\n"))))
      (kodi-remote-play-url url))))

;FIXME: use quvi instead of youtube-dl
;;;###autoload
(defun kodi-remote-append-video-url (video-url)
  "Appends urls from videos like youtube to kodi playlists.
Could be used for other sites, too.  whatever youtube-dl
supports.  Argument VIDEO-URL A Url from a youtube video."
  (interactive "surl: ")
  (let* ((response
	 (shell-command-to-string
	  (concat "youtube-dl -f best -g -e " video-url))))
    (let* ((url (nth 1 (split-string response "\n")))
	   (title (nth 0 (split-string response "\n"))))
      (if kodi-network-interface
	  (kodi-remote-playlist-add-url-pls url title)
	(kodi-remote-playlist-add-url url)))))


(defun kodi-setup-elnode ()
  "starts elnode deamon and sets up everything if not done already"
  ;; (print (elnode-ports))
  (if (not (member 8028 (elnode-ports)))
      (progn
	(setq kodi-elnode-directory (make-temp-file "kodi-" t))
	(setq elnode-send-file-program "cat")
	(defconst kodi-elnode-handler
	  (elnode-webserver-handler-maker kodi-elnode-directory))
	(elnode-start kodi-elnode-handler :port 8028 :host "*"))))

(defun kodi-remote-playlist-add-url-pls (url &optional label)
  "Add item/video to playlist."
  ;; (interactive "sUrl: ")
  (kodi-setup-elnode)
  (with-temp-file (expand-file-name
		   (format  "%s.pls" label) kodi-elnode-directory)
    (insert (string-join `("[playlist]"
			   ,(concat "File1=" url "")
			   ,(concat "Title1=" label "")
			   "NumberOfEntries=1\n"
			   ) "\n")))
  (let* ((stream-url (format "http://%s:8028/%s.pls"
			     (kodi-client-ip) label))
  	 (params `(("params" . (("playlistid" . 1)
				("item" . (("file" . ,stream-url))))))))
    (kodi-remote-post "Playlist.Add" params))
  (kodi-remote-sit-for-done)
  ;; (let* ((url2 (concat "http://" kodi-remote-local-host
  ;; 			 ":8028/"))
  ;; 	   (params `(("params" . (("directory" . ,url2))))))
  ;;   (kodi-remote-post "VideoLibrary.Scan" params))
  ;; (kodi-remote-sit-for-done)
  )


(defun kodi-remote-playlist-add-url-strm (url &optional label)
  "Add item/video to playlist."
  ;; (interactive "sUrl: ")
  (kodi-setup-elnode)
  ;; (print kodi-elnode-directory)
  (with-temp-file (expand-file-name
		   (format "%s.strm" label)
		   kodi-elnode-directory)
    (insert url))
  ;; (with-temp-file (expand-file-name
  ;; 		       "youtube-video.nfo" full-dir)
  ;; 	(insert (format "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n<movie>\n<title>%s</title>\n</movie>" label)))
  (let* ((stream-url (format "http://%s:8028/%s.strm"
			     kodi-remote-local-host label))
	 (params `(("params" . (("playlistid" . 1)
				("item" . (("file" . ,stream-url))))))))
    (kodi-remote-post "Playlist.Add" params))
  (kodi-remote-sit-for-done)
  ;; (let* ((url2 (concat "http://" kodi-remote-local-host
  ;; 			 ":8028/youtube-titel-321/"))
  ;; 	   (params `(("params" . (("directory" . ,url2))))))
  ;;   (kodi-remote-post "VideoLibrary.Scan" params))
  ;; (kodi-remote-sit-for-done)
)


(defvar kodi-remote-keyboard-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "m") 'kodi-remote-music)
    (define-key map (kbd "SPC") 'kodi-remote-play-pause)
    (define-key map (kbd "<left>") 'kodi-remote-input-left)
    (define-key map (kbd "<right>") 'kodi-remote-input-right)
    (define-key map (kbd "<up>") 'kodi-remote-input-up)
    (define-key map (kbd "<down>") 'kodi-remote-input-down)
    (define-key map (kbd "<backspace>") 'kodi-remote-input-back)
    (define-key map (kbd "<return>") 'kodi-remote-input-enter)
    (define-key map (kbd "x") 'kodi-remote-stop)
    (define-key map (kbd "<delete>") 'kodi-remote-input-delete)
    ;; start dvorak specific keybindings
    (define-key map (kbd "h") 'kodi-remote-input-left)
    (define-key map (kbd "n") 'kodi-remote-input-right)
    (define-key map (kbd "c") 'kodi-remote-input-up)
    (define-key map (kbd "t") 'kodi-remote-input-down)
    ;; end dvorak specific keybindings
    (define-key map (kbd "=") 'kodi-remote-volume-increase)
    (define-key map (kbd "+") 'kodi-remote-volume-increase)
    (define-key map (kbd "-") 'kodi-remote-volume-decrease)
    (define-key map (kbd "<tab>") 'kodi-remote-toggle-fullscreen)
    map)
  "Keymap for `kodi-remote-keyboard-mode'.")

(define-derived-mode kodi-remote-keyboard-mode special-mode "kodi-remote-keyboard"
  "Major mode for remote controlling kodi instance with keyboard commands
Key bindings:
\\{kodi-remote-keyboard-mode-map}"
  (setq cursor-type nil))

;;;###autoload
(defun kodi-remote-keyboard ()
  "Open a `kodi-remote-keyboard-mode' buffer."
  (interactive)
  (let* ((name "*kodi-remote-keyboard*")
         (buffer (get-buffer-create name)))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t) )
          (erase-buffer)
          (kodi-remote-keyboard-mode)
          (insert (concat "Kodi Remote:\n"
                          (substitute-command-keys
                           "\\{kodi-remote-keyboard-mode-map}"))))
        (switch-to-buffer-other-window buffer)))))

(defun kodi-remote-episode-toggle-visibility ()
  "Toggle visability of watched episodes."
  (interactive)
  (setq kodi-unseen-visible(not kodi-unseen-visible))
  (kodi-remote-draw-episodes))

(defun kodi-remote-series-toggle-visibility ()
  "Toggle visability of watched series."
  (interactive)
  (setq kodi-unseen-visible(not kodi-unseen-visible))
  (kodi-remote-draw-shows))

(defun kodi-remote-delete ()
  "Deletes episode."
  (interactive)
  (let* ((params
	  `(("params" . (("episodeid" . ,(tabulated-list-get-id)))))))
    (kodi-remote-post "VideoLibrary.RemoveEpisode" params)))

(defun kodi-remote-series-clean ()
  "Cleans video library."
  (interactive)
  (let* ((params nil))
    (kodi-remote-post "VideoLibrary.Clean" params)))

(defun kodi-remote-playlist-clean ()
  "Cleans video library."
  (interactive)
  (let* ((params nil))
    (kodi-remote-post "Playlist.Clean" params)))

(defun kodi-remote-series-scan ()
  "Scans kodi for new video content."
  (interactive)
  (let* ((params nil))
    (kodi-remote-post "VideoLibrary.Scan" params)))

(defun sbit-action (obj)
  "Helper method for series start buttons.
Argument OBJ the button obj."
  (kodi-remote-play-database-id
   (button-get obj 'id)))

(defun sbit-action-playlist (obj)
  "Helper method for series start buttons.
Argument OBJ the button obj."
  (kodi-remote-play-playlist-item
   (button-get obj 'id)))

(defun spiderbit-get-name (episode)
  "Return the name of a EPISODE."
  (decode-coding-string (cdr (assoc 'label episode)) 'utf-8) )

(defun spiderbit-get-id (episode)
  "Return the id of a EPISODE."
  (cdr (assoc 'episodeid episode)))

(defun spiderbit-get-show-id (show)
  "Return the id of a SHOW."
  (cdr (assoc 'tvshowid show)))

(defun kodi-show-get-number-of-unwatched (show)
  "Return number of unwatched episodes from a SHOW."
  (- (cdr (assoc 'episode show))
     (cdr (assoc 'watchedepisodes show))))

(defun kodi-remote-sit-for-done ()
  "Sits till the last json request is done."
  (while kodi-request-running (sit-for 0.05)))

(defun kodi-remote-series-episodes-wrapper (button)
  "Set the selected show and then displays episodes.
Argument BUTTON contains the show-id"
  (unless (equal button nil)
    (setq kodi-selected-show (button-get button 'id)))
  (kodi-remote-series-episodes))

(defun kodi-generate-series-entry (item)
  "Generate tabulated-list entry for kodi media buffers.
Argument ITEM the media data form kodi"
  (let* ((number-of-episodes (kodi-show-get-number-of-unwatched item))
	 (episodeid (assoc-default 'episodeid item))
	 (label (assoc-default 'label item))
	 (tvshowid (assoc-default 'tvshowid item)))
    (when (or (> number-of-episodes 0) kodi-unseen-visible)
      (list episodeid
	    (vector `(,label
		      action kodi-remote-series-episodes-wrapper
		      id ,tvshowid)
		    `(,(number-to-string number-of-episodes)
		      action kodi-remote-draw-episodes
		      id ,tvshowid))))))

(defun kodi-remote-draw ()
  "Draw the buffer with new contents via `kodi-refresh-function'."
  (with-silent-modifications
    (funcall kodi-remote-refresh-function ;; transmission-torrent-id
)))

;;;###autoload
(defun kodi-remote-draw-episodes (&optional _arg _noconfirm)
  "Draw a list of episodes of all or a specific show.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-video-scan)
  (if kodi-unseen-visible
      (kodi-remote-get-series-episodes kodi-selected-show)
      (kodi-remote-get-unwatched-series-episodes kodi-selected-show))
  (kodi-remote-sit-for-done)
  (let* ((entries '()))
    (dolist (episode (append (let-alist kodi-properties .episodes) nil))
      (push (list (spiderbit-get-id episode)
		  (vector `(,(spiderbit-get-name episode)
			    action sbit-action
			    id ,(spiderbit-get-id episode))))
	    entries))
    (setq tabulated-list-entries entries)
    ;; (print tabulated-list-entries)
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;;###autoload
(defun kodi-remote-draw-shows (&optional _arg _noconfirm)
  "Draw a list of shows.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-video-scan)
  (kodi-remote-sit-for-done)
  (kodi-remote-get-show-list)
  (kodi-remote-sit-for-done)
  (setq tabulated-list-entries
  	(remove nil
  		(mapcar 'kodi-generate-series-entry
  			(let-alist kodi-properties .tvshows))))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun kodi-remote-playlist-draw (&optional _arg _noconfirm)
  "Draw the current playlist.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-playlist-get)
  (kodi-remote-sit-for-done)
  (let* ((items (cdr (assoc 'items kodi-properties)))
  	 (id 0))
    (setq tabulated-list-entries
	  (mapcar
	   (lambda (item)
	     (let* ((title (or ;;(cdr (assoc 'title item))
			    (string-remove-suffix ".pls"
						  (spiderbit-get-name item))
			    "None"))
		    (entry (list `,id (vector `(,title
						action sbit-action-playlist
						id ,id)))))
	       (setq id (1+ id))
	       entry))
	   items)))
  (tabulated-list-init-header)
  (tabulated-list-print))


;;;###autoload
;; (defun kodi-remote-playlists-draw (&optional _arg _noconfirm)
;;   "Draw the list of playlists.
;; Optional argument _ARG revert excepts this param.
;; Optional argument _NOCONFIRM revert excepts this param."
;;   (interactive)
;;   (kodi-remote-playlists-get)
;;   (kodi-remote-sit-for-done)
;;   (setq tabulated-list-entries '())
;;   (dolist (item (append (let-alist kodi-properties .items) nil))
;;     (push (list (spiderbit-get-id item)
;; 		(vector `(,(spiderbit-get-name item)
;; 			  id ,(spiderbit-get-show-id item))))
;; 	  tabulated-list-entries))
;;   (tabulated-list-init-header)
;;   (tabulated-list-print))

(defvar kodi-remote-playlist-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    ;; (define-key map (kbd "n") 'kodi-remote-playlist-next)
    (define-key map (kbd "d") 'kodi-remote-playlist-remove)
    (define-key map (kbd "a") 'kodi-remote-playlist-add-url)
    (define-key map (kbd "p") 'kodi-remote-playlist-play)
    (define-key map (kbd "c") 'kodi-remote-playlist-clear)
    map)
  "Keymap for `kodi-remote-playlist-mode'.")

(defvar kodi-remote-series-episodes-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    (define-key map (kbd "l") 'kodi-remote-episode-toggle-visibility)
    (define-key map (kbd "d") 'kodi-remote-delete)
    (define-key map (kbd "a") 'kodi-remote-playlist-add-episode)
    map)
  "Keymap for `kodi-remote-playlist-mode'.")

(defvar kodi-remote-series-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    (define-key map (kbd "g") 'kodi-remote-draw-shows)
    (define-key map (kbd "c") 'kodi-remote-series-clean)
    (define-key map (kbd "s") 'kodi-remote-series-scan)
    (define-key map (kbd "l") 'kodi-remote-series-toggle-visibility)
    map)
  "Keymap for `kodi-remote-series-mode'.")

(define-derived-mode kodi-remote-series-mode tabulated-list-mode "kodi-remote-series"
  "Major Mode for kodi series.
Key bindings:
\\{kodi-remote-series-mode-map}"
  (setq tabulated-list-format
        `[("Name" 30 t)
          ("unseen" 10 t)])
  ;; (transmission-tabulated-list-format)
  ;; (setq kodi-refresh-function #'kodi-remote-draw-shows)
  ;; (setq kodi-refresh-function nil)
  (setq-local revert-buffer-function #'kodi-remote-draw-shows)
  ;; (add-hook 'post-command-hook #'transmission-timer-check nil t)
  ;; (add-hook 'before-revert-hook #'transmission-tabulated-list-format nil t)
  )

(define-derived-mode kodi-remote-series-episodes-mode tabulated-list-mode "kodi-remote-series-episodes"
  "Major Mode for kodi series.
Key bindings:
\\{kodi-remote-series-episodes-mode-map}"
  (setq tabulated-list-format
        `[("Episodes" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-draw-episodes))

(define-derived-mode kodi-remote-playlist-mode tabulated-list-mode "kodi-remote-playlist"
  "Major Mode for kodi playlists.
Key bindings:
\\{kodi-remote-playlist-mode-map}"
  (setq tabulated-list-format
        `[("Entry" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-playlist-draw))

;; (define-derived-mode kodi-remote-playlists-mode tabulated-list-mode "kodi-remote-playlists"
;;   "Major Mode for kodi playlists.
;; Key bindings:
;; \\{kodi-remote-playlists-mode-map}"
;;   (setq tabulated-list-format
;;         `[("Playlists" 30 t)])
;;   (setq-local revert-buffer-function #'kodi-remote-playlists-draw))

;;;###autoload
(defun kodi-remote-series-episodes ()
  "Open a `kodi-remote-series-episodes-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-series-episodes-mode))

;;;###autoload
(defun kodi-remote-series ()
  "Open a `kodi-remote-series-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-series-mode))

;;;###autoload
(defun kodi-remote-playlist ()
  "Open a `kodi-remote-playlist-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-playlist-mode))

;;;###autoload
;; (defun kodi-remote-playlists ()
;;   "Open a `kodi-remote-playlists-mode' buffer."
;;   (interactive)
;;   (kodi-remote-context kodi-remote-playlists-mode))

(defmacro kodi-remote-context (mode)
  "Switch to a context buffer of major mode MODE."
  (cl-assert (string-suffix-p "-mode" (symbol-name mode)))
  (let ((name (make-symbol "name")))
    `(let (;; (id (or transmission-torrent-id
           ;;         (cdr (assq 'id (tabulated-list-get-id)))))
           (,name ,(format "*%s*" (string-remove-suffix "-mode" (symbol-name mode)))))
       (if nil (user-error "No media selected")
         (let ((buffer (or (get-buffer ,name)
                           (generate-new-buffer ,name))))
           (with-current-buffer buffer
             (unless (eq major-mode ',mode)
	       (funcall #',mode))
	     (if t ;; (and old-id (eq old-id id))
		 (revert-buffer)
	       ;; (setq transmission-torrent-id id)
	       (kodi-remote-draw)
	       ;; (goto-char (point-min))
	       ))
           (pop-to-buffer-same-window buffer))))))

(provide 'kodi-remote)
;;; kodi-remote.el ends here

;; (easy-menu-define kodi-remote-mode-menu kodi-remote-mode-map
;;   "Menu used in `kodi-remote-mode' buffers."
;;   '("Kodi"
;;     ["Series" kodi-remote-series]
;;     ["Add Media" kodi-remote-add
;;      :help "Add Media to the playlist"]
;;     "--"
;;     ;; ["View Series" kodi-remote-series]
;;     ;; ["View playlist" kodi-remote-playlist]
;;     ;; ["View Movies" kodi-remote-movies]
;;     ;; ["View Music" kodi-remote-music]
;;     "--"
;;     ["Refresh" revert-buffer]
;;     ;; ["Quit" kodi-quit]
;;     ))
