;;; kodi-remote.el --- Remote Control for Kodi

;; Copyright (C) 2015-2016 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>
;; URL: http://github.com/spiderbit/kodi-remote.el
;; Package-Version: 20171008.2226
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
;; Then open the Remote with the command:
;; 'kodi-remote-keyboard'
;; Alternativly open the Movies, Series or Music Overview with the command:
;; 'kodi-remote-movies'
;; 'kodi-remote-series'
;; 'kodi-remote-music'
;; Also open kodi Video Playlist with the command:
;; 'kodi-remote-playlist'
;; OPTIONAL: setup settings for deleting files (over tramp)
;; (setq kodi-dangerous-options t)
;; (setq kodi-access-host "my-htpc")
;; if you don't use ssh to access your kodi server / nas:
;; (setq kodi-access-method "smb/ftp/adb...")


;;; Code:

(require 'json)
(require 'request)
(require 'let-alist)
(require 'subr-x)
(require 'elnode)
(defcustom kodi-host-name "localhost:8080"
  "Host to access Kodi remote control."
  :type 'string
  :group 'kodi-remote)
(defcustom kodi-dangerous-options nil
  "Option to give access for destructive operations."
  :type 'boolean
  :group 'kodi-remote)
(defcustom kodi-access-host nil
  "Host to access media over tramp."
  :type 'string
  :group 'kodi-remote)
(defcustom kodi-access-method "ssh"
  "Method to access media over tramp."
  :type 'string
  :group 'kodi-remote)
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
  "Network interface connecting to kodi.")
(defvar kodi-elnode-directory nil
  "The temp dir to use for serving streams to kodi.")

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
  (let* ((default-directory "~")
	 (request-data
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
     :parser 'json-read))
  (kodi-remote-sit-for-done))

(defun kodi-remote-get (method params)
  "Function to send get requests to the kodi instance.
Argument METHOD kodi json api argument.
Argument PARAMS kodi json api argument."
  (setq kodi-request-running t)
  (let* ((default-directory "~")
	 (request-data
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
     :parser 'buffer-string))
  (kodi-remote-sit-for-done))

;;;###autoload
(defun kodi-remote-start-music-party ()
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

(defun kodi-remote-play-database-id (field-name id)
  "Play series in database with given ID."
  (let* ((params
	  `(("params" . (("item" . ((,field-name . ,id))))))))
    (kodi-remote-post "Player.Open" params)))

(defun kodi-remote-play-playlist-item (position)
  "Play series in playlist POSITION with given ID."
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
      (kodi-remote-post "Application.SetVolume" params))))

(defun kodi-remote-input (input)
  "Function to send post INPUT json requests."
  (let ((default-directory "~"))
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode `(("id" . 1)
			  ("jsonrpc" . "2.0")
			  ("method" . ,input)
			  ))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read))
  (wait-for-done))

(defun kodi-remote-input-execute-action (action)
  "Function to send post ACTION json requests."
  (let* ((params
	  `(("params" . (("action" . ,action))))))
    (kodi-remote-post "Input.ExecuteAction" params)))

(defun kodi-remote-input-direct (seek input)
  "Move horrizontal or seek.
Depending on current window move horizontal in menu (INPUT)
 or SEEK big forward/backward."
  (kodi-remote-get-active-window)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek seek)
    (kodi-remote-input input)))

;; todo: need to compare to other active windows (like musik) for actions.
;;;###autoload
(defun kodi-remote-input-left ()
  "Move left in menu or seek small backward."
  (interactive)
  (kodi-remote-input-direct "smallbackward" "Input.Left"))

;;;###autoload
(defun kodi-remote-input-right ()
  "Move right in menu or seek small forward."
  (interactive)
  (kodi-remote-input-direct "smallforward" "Input.Right"))

;;;###autoload
(defun kodi-remote-input-up ()
  "Move up in menu or seek big forward."
  (interactive)
  (kodi-remote-input-direct "bigforward" "Input.Up"))

;;;###autoload
(defun kodi-remote-input-down ()
  "Move down in menu or seek big backward."
  (interactive)
  (kodi-remote-input-direct "bigbackward" "Input.Down"))

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
  (setq kodi-volume (let-alist kodi-properties .volume)))

(defun kodi-remote-get-songs (&optional id)
  "Poll list of songs.
Optional argument ID limits to a specific artist."
  (let* ((params
	  `(("params" . (("filter" .
			  (("artistid". ,id))))))))
    (kodi-remote-get "AudioLibrary.GetSongs" params)))

(defun kodi-remote-get-series-episodes (&optional show-id filter-watched)
  "Poll unwatches episodes from show.
Optional argument SHOW-ID limits to a specific show.
Optional argument FILTER-WATCHED filters watched episodes."
  (let* ((filter '("filter" . (("field" . "playcount")
			       ("operator" . "lessthan")
			       ("value" . "1" ))))
	 (pre-params (if (integerp show-id)
			 `(("tvshowid" . ,show-id)
			   ("properties" . ["title" "episode"]))
		       `(("properties" .
			   ["title" "watchedepisodes" "episode"]))
		       ))
	 (params (list (append '("params") pre-params
			       (if filter-watched `(,filter))))))
    (kodi-remote-get "VideoLibrary.GetEpisodes" params)))

(defun kodi-remote-get-show-list ()
  "Poll unwatched show."
  (let* ((params
  	  '(("params" . (("properties" .
			  ["title" "watchedepisodes" "episode"]))))))
  (kodi-remote-get "VideoLibrary.GetTVShows" params)))

(defun kodi-remote-get-artist-list ()
  "Poll music artists."
  (let* ((params
  	  '(("params" . (("properties" .
			  ["genre"]))))))
  (kodi-remote-get "AudioLibrary.GetArtists" params)))

;;;###autoload
(defun kodi-remote-playlist-add-episode ()
  "Add episode to playlist."
  (interactive)
  (let* ((params `(("params" .
		    (("playlistid" . 1)
		     ("item" .
		      (("episodeid" . ,(tabulated-list-get-id)))))))))
    (kodi-remote-post "Playlist.Add" params)))

;;;###autoload
(defun kodi-remote-playlist-play ()
  "Add episode to playlist."
  (interactive)
  (let* ((params `(("params" .
		    (("item" . (("playlistid" . 1))))))))
    (kodi-remote-post "Player.Open" params)))

;;;###autoload
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
  "Add item/video to playlist.
Argument URL the file url to the media."
  (interactive "sUrl: ")
  (let* ((params `(("params" . (("playlistid" . 1)
				("item" . (("file" . ,url))))))))
    (kodi-remote-post "Playlist.Add" params)))

;;;###autoload
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

(defun kodi-remote-playlist-swap (direction)
  "Move item/video up in the playlist.
Argument DIRECTION can be up or down."
  (if-let ((position1 (tabulated-list-get-id))
	   (difference '(("up" . -1) ("down" . 1)))
	   (position2 (+ position1 (assoc-default direction difference)))
	   (max (length tabulated-list-entries))
	   (positions (list position1 position2))
	   (eval (cons 'and (mapcar (lambda (x)(<= 0 x max)) positions)))
	   (params `(("params"
		      . (("playlistid" . 1)
			 ("position1" . ,position1)
			 ("position2" . ,position2))))))
      (kodi-remote-post "Playlist.Swap" params)
    ;; (kodi-remote-playlist-draw)
    ))

;;;###autoload
(defun kodi-remote-playlist-move-up ()
  "Move item/video up in the playlist."
  (interactive)
  (kodi-remote-playlist-swap "up"))

;;;###autoload
(defun kodi-remote-playlist-move-down ()
  "Move item/video up in the playlist."
  (interactive)
  (kodi-remote-playlist-swap "down"))

;; (defun kodi-remote-playlists-get ()
;;   "Requests playlist items."
;;   (let* ((params nil
;;   	  ))
;;     (kodi-remote-get "Playlist.GetPlaylists" params)))

(defun kodi-remote-video-scan ()
  "Update availible/new videos."
  (kodi-remote-post "VideoLibrary.Scan" nil))

(defun kodi-remote-audio-scan ()
  "Update availible/new videos."
  (kodi-remote-post "AudioLibrary.Scan" nil))

(defun kodi-remote-get-episode-details (id)
  "Poll details of a episode.
Argument ID kodi series database identifier."
  (let* ((params
	  `(("params" . (("episodeid" . ,id)
			 ("properties" . ("playcount")))))))
    (kodi-remote-get "VideoLibrary.GetEpisodeDetails" params)))

(defun kodi-remote-get-active-window ()
  "Update currently active window."
  (let* ((params
	  '(("params" . (("properties" . ("currentwindow")))))))
    (kodi-remote-get "Gui.GetProperties" params))
  (setq kodi-active-window (let-alist kodi-properties .currentwindow.label)))

(defun kodi-remote-get-active-player-id ()
  "Update currently active player."
  (kodi-remote-get "Player.GetActivePlayers" nil)
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
  (let* ((default-directory "~")
	 (json (json-encode `(("id" . 1)("jsonrpc" . "2.0")("method" . "Player.Open")
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
	  (concat "youtube-dl --no-warnings -f best -g -e " video-url))))
    (let* ((url (nth 1 (split-string response "\n")))
	   (title (nth 0 (split-string response "\n"))))
      (kodi-remote-play-url url))))

;FIXME: use quvi instead of youtube-dl
;;;###autoload
(defun kodi-remote-append-video-url (video-url)
  "Append urls from videos like youtube to kodi playlists.
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
  "Start elnode deamon and set up everything if not done already."
  ;; (print (elnode-ports))
  (if (not (member 8028 (elnode-ports)))
      (progn
	(setq kodi-elnode-directory (make-temp-file "kodi-" t))
	(setq elnode-send-file-program "cat")
	(defconst kodi-elnode-handler
	  (elnode-webserver-handler-maker kodi-elnode-directory))
	(elnode-start kodi-elnode-handler :port 8028 :host "*"))))

(defun kodi-remote-playlist-add-url-pls (url &optional label)
  "Add item/video to playlist.
Argument URL the video url.
Optional argument LABEL a custom label for the file."
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
  ;; (let* ((url2 (concat "http://" kodi-remote-local-host
  ;; 			 ":8028/"))
  ;; 	   (params `(("params" . (("directory" . ,url2))))))
  ;;   (kodi-remote-post "VideoLibrary.Scan" params))
  )


(defun kodi-remote-playlist-add-url-strm (url &optional label)
  "Add item/video to playlist.
Argument URL the url to the media.
Optional argument LABEL cutom name of the entry."
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
  ;; (let* ((url2 (concat "http://" kodi-remote-local-host
  ;; 			 ":8028/youtube-titel-321/"))
  ;; 	   (params `(("params" . (("directory" . ,url2))))))
  ;;   (kodi-remote-post "VideoLibrary.Scan" params))
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

(defun kodi-remote-delete-multiple (ids)
  "Deletes all entries with id in ids list"
  (mapcar 'kodi-remote-delete-entry ids))

(defun kodi-remote-delete-entry (id)
  "Deletes episode over tramp.
For it to work ‘kodi-dangerous-options’ must be set to t
and ‘kodi-access-host’ must be set to the hostname of your kodi-file host."
  (let* ((default-directory "~"))
    (if (and kodi-dangerous-options (boundp 'kodi-access-host))
	(progn
	  (let* ((params
		  `(("params" .
		     (("episodeid" . ,id)
		      ("properties" . ("file")))))))
	    (kodi-remote-get "VideoLibrary.GetEpisodeDetails" params))
	  (let* ((default-directory
		   (concat "/" kodi-access-method
			   ":" kodi-access-host ":/"))
		 (file-name
		  (substring
		   (decode-coding-string
		    (let-alist
			kodi-properties .episodedetails.file)
		    'utf-8)
		   1)))
	    (if (file-writable-p file-name )
		(delete-file file-name)))
	  (let* ((params
		  `(("params" . (("episodeid" .
				  ,id ))))))
	    (kodi-remote-post "VideoLibrary.RemoveEpisode" params))))))

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

(defun sbit-action (field-name obj)
  "Helper method for series start buttons.
Argument OBJ the button obj."
  (kodi-remote-play-database-id
   field-name (button-get obj 'id)))

(defun sbit-action-playlist (obj)
  "Helper method for playlist start buttons.
Argument OBJ the button obj."
  (kodi-remote-play-playlist-item
   (button-get obj 'id)))

(defun spiderbit-get-name (episode)
  "Return the name of a EPISODE."
  (decode-coding-string (cdr (assoc 'label episode)) 'utf-8) )

(defun kodi-show-get-number-of-unwatched (show)
  "Return number of unwatched episodes from a SHOW."
  (- (cdr (assoc 'episode show))
     (cdr (assoc 'watchedepisodes show))))

(defun kodi-remote-sit-for-done ()
  "Sits till the last json request is done."
  (let* ((waiting-time 0))
    (while (and kodi-request-running (< waiting-time 10.0))
      (sit-for 0.1)
      (setq waiting-time (+ 0.1 waiting-time)))))

(defun kodi-remote-series-episodes-wrapper (button)
  "Set the selected show and then displays episodes.
Argument BUTTON contains the show-id"
  (unless (equal button nil)
    (setq kodi-selected-show (button-get button 'id)))
  (kodi-remote-series-episodes))

(defun kodi-remote-songs-wrapper (button)
  "Set the selected artist and then displays songs.
Argument BUTTON contains the artist-id"
  (unless (equal button nil)
    (setq kodi-selected-artist (button-get button 'id)))
  (kodi-remote-songs))

(defun kodi-generate-entry (action id subitems item)
  "Generate tabulated-list entry for kodi media buffers.
Argument ACTION button action.
Argument ID button/entry id.
Argument SUBITEMS sets entry as category/tag with child entries.
Argument ITEM the media data from kodi"
  (let* ((number-of-nodes
	  (if subitems
	      (kodi-show-get-number-of-unwatched item) 5)) ; not abstracted yet
	 (subitemid (assoc-default id item))
	 (label (decode-coding-string (assoc-default 'label item) 'utf-8))
	 (itemid (assoc-default id item)))
    (when (or (> number-of-nodes 0) kodi-unseen-visible)
      (let* ((button1 `(,label
			action ,action
			id ,subitemid))
	     (button2 `(,(number-to-string number-of-nodes)
			action ,action
			id ,subitemid)))
	(list subitemid
	      (if subitems
		  (vector button1 button2)(vector button1)))))))

(defun kodi-remote-draw ()
  "Draw the buffer with new contents via `kodi-refresh-function'."
  (with-silent-modifications
    (funcall kodi-remote-refresh-function ;; transmission-torrent-id
)))

(defun kodi-draw-tab-list (action parent id data-name subitems)
  "Creates and draws a media overview buffer.
Argument ACTION button action.
Argument PARENT sets entry as category/tag with child entries.
Argument ID button/entry id.
Argument DATA-NAME name of the data we want to show.
Argument ITEM the media data from kodi"
  (setq tabulated-list-entries
  	(remove nil (mapcar
		     (apply-partially
		      'kodi-generate-entry
		      (if parent action (apply-partially
					 'sbit-action action))
		      id subitems)
		     (assoc-default data-name kodi-properties))))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun kodi-remote-draw-movies (&optional _arg _noconfirm)
  "Draw a list of movies.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-video-scan)
  (kodi-remote-get-movies (not kodi-unseen-visible))
  (kodi-draw-tab-list 'movieid nil 'movieid 'movies nil))

;;;###autoload
(defun kodi-remote-draw-episodes (&optional _arg _noconfirm)
  "Draw a list of episodes of all or a specific show.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-video-scan)
  (kodi-remote-get-series-episodes
   kodi-selected-show (not kodi-unseen-visible))
  (kodi-draw-tab-list 'episodeid nil 'episodeid 'episodes nil))

;;;###autoload
(defun kodi-remote-draw-songs (&optional _arg _noconfirm)
  "Draw a list of songs of all or a specific artist.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-get-songs kodi-selected-artist)
  (kodi-draw-tab-list 'songid nil 'songid 'songs nil))

;;;###autoload
(defun kodi-remote-draw-shows (&optional _arg _noconfirm)
  "Draw a list of shows.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-video-scan)
  (kodi-remote-get-show-list)
  (kodi-draw-tab-list 'kodi-remote-series-episodes-wrapper t
		      'tvshowid 'tvshows t))

;;;###autoload
(defun kodi-remote-draw-music (&optional _arg _noconfirm)
  "Draw a list of music.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-get-artist-list)
  (kodi-draw-tab-list 'kodi-remote-songs-wrapper t
		      'artistid 'artists nil))

;;;###autoload
(defun kodi-remote-playlist-draw (&optional _arg _noconfirm)
  "Draw the current playlist.
Optional argument _ARG revert excepts this param.
Optional argument _NOCONFIRM revert excepts this param."
  (interactive)
  (kodi-remote-playlist-get)
  (let* ((items (cdr (assoc 'items kodi-properties)))
  	 (id 0))
    (setq tabulated-list-entries
	  (mapcar (lambda (item)
	     (let* ((title (or ;;(cdr (assoc 'title item))
			    (string-remove-suffix ".pls"
						  (spiderbit-get-name item))
			    "None"))
		    (entry (list `,id (vector `(,title
						action sbit-action-playlist
						id ,id)))))
	       (setq id (1+ id)) entry)) items)))
  (tabulated-list-init-header)
  (tabulated-list-print))

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

(defvar kodi-remote-songs-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    ;; (define-key map (kbd "l") 'kodi-remote-episode-toggle-visibility)
    ;; (define-key map (kbd "d") 'kodi-remote-delete)
    ;; (define-key map (kbd "a") 'kodi-remote-playlist-add-episode)
    map)
  "Keymap for `kodi-remote-songs-mode'.")

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

(defvar kodi-remote-music-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    (define-key map (kbd "g") 'kodi-remote-draw-music)
    ;; (define-key map (kbd "c") 'kodi-remote-series-clean)
    ;; (define-key map (kbd "s") 'kodi-remote-series-scan)
    ;; (define-key map (kbd "l") 'kodi-remote-series-toggle-visibility)
    map)
  "Keymap for `kodi-remote-music-mode'.")


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

(define-derived-mode kodi-remote-songs-mode tabulated-list-mode "kodi-remote-songs"
  "Major Mode for kodi songs.
Key bindings:
\\{kodi-remote-songs-mode-map}"
  (setq tabulated-list-format
        `[("Songs" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-draw-songs))

(define-derived-mode kodi-remote-music-mode tabulated-list-mode "kodi-remote-music"
  "Major Mode for kodi music.
Key bindings:
\\{kodi-remote-music-mode-map}"
  (setq tabulated-list-format
        `[("Artists" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-draw-music))


(define-derived-mode kodi-remote-playlist-mode tabulated-list-mode "kodi-remote-playlist"
  "Major Mode for kodi playlists.
Key bindings:
\\{kodi-remote-playlist-mode-map}"
  (setq tabulated-list-format
        `[("Entry" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-playlist-draw))

;;;###autoload
(defun kodi-remote-series-episodes ()
  "Open a `kodi-remote-series-episodes-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-series-episodes-mode))

;;;###autoload
(defun kodi-remote-songs ()
  "Open a `kodi-remote-songs-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-songs-mode))

;;;###autoload
(defun kodi-remote-series ()
  "Open a `kodi-remote-series-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-series-mode))

;;;###autoload
(defun kodi-remote-music ()
  "Open a `kodi-remote-music-mode' buffer."
  (interactive)()
  (kodi-remote-context kodi-remote-music-mode))

;;;###autoload
(defun kodi-remote-playlist ()
  "Open a `kodi-remote-playlist-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-playlist-mode))

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

(defun kodi-remote-movies-toggle-visibility ()
  "Toggle visability of watched movies."
  (interactive)
  (setq kodi-unseen-visible(not kodi-unseen-visible))
  (kodi-remote-draw-movies))

(defun spiderbit-get-movie-id (movie)
  "Return the id of a Movie."
  (cdr (assoc 'movieid movie)))

(defun kodi-remote-get-movies (&optional filter-watched)
  "Poll unwatches movies.
Optional argument FILTER-WATCHED filters watched episodes."
  (let* ((filter '("filter" . (("field" . "playcount")
			       ("operator" . "lessthan")
			       ("value" . "1" ))))
	 (pre-params `(("properties" .
                        ["title" "file"])))
	 (params (list (append '("params") pre-params
			       (if filter-watched `(,filter))))))
    (kodi-remote-get "VideoLibrary.GetMovies" params)))

(defvar kodi-remote-movies-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "k") 'kodi-remote-keyboard)
    (define-key map (kbd "l") 'kodi-remote-movies-toggle-visibility)
    ;; (define-key map (kbd "d") 'kodi-remote-delete)
    ;; (define-key map (kbd "a") 'kodi-remote-playlist-add-episode)
    map)
  "Keymap for `kodi-remote-movies-mode'.")


(define-derived-mode kodi-remote-movies-mode tabulated-list-mode "kodi-remote-movies"
  "Major Mode for kodi movies.
Key bindings:
\\{kodi-remote-movies-mode-map}"
  (setq tabulated-list-format
        `[("Movies" 30 t)])
  (setq-local revert-buffer-function #'kodi-remote-draw-movies))

;;;###autoload
(defun kodi-remote-movies ()
  "Open a `kodi-remote-movies-mode' buffer."
  (interactive)
  (kodi-remote-context kodi-remote-movies-mode))


(defun kodi-files-do (action)
  "Apply ACTION to files in `kodi-media-mode' buffers."
  ;; (cl-assert (memq action kodi-file-symbols))
  (let* ((id nil)
         (prop 'tabulated-list-id)
         (region (use-region-p))
         (beg (and region (region-beginning)))
         (end (and region (region-end)))
         (indices
          (if (null region)
	      (list (tabulated-list-get-id))
	    (mapcar (lambda (id) id)
		    (kodi-text-property-all beg end prop)))))
    (if (and indices)
        (let ((arguments (list :ids action indices)))
	  (if (equal :delete action)
	      (kodi-remote-delete-multiple indices)))
      (user-error "No entries selected or at point"))))

(defun kodi-remote-delete ()
  "Deletes entry(s) at point or in region."
  (interactive)
  (kodi-files-do :delete))

(defun kodi-text-property-all (beg end prop)
  "Return a list of non-nil values of a text property PROP between BEG and END.
If none are found, return nil."
  (let (res pos)
    (save-excursion
      (goto-char beg)
      (while (> end (point))
        (push (get-text-property (point) prop) res)
        (setq pos (text-property-not-all (point) end prop (car-safe res)))
        (goto-char (or pos end))))
    (nreverse (delq nil res))))


;; (define-derived-mode kodi-remote-playlists-mode tabulated-list-mode "kodi-remote-playlists"
;;   "Major Mode for kodi playlists.
;; Key bindings:
;; \\{kodi-remote-playlists-mode-map}"
;;   (setq tabulated-list-format
;;         `[("Playlists" 30 t)])
;;   (setq-local revert-buffer-function #'kodi-remote-playlists-draw))

;;;###autoload
;; (defun kodi-remote-playlists ()
;;   "Open a `kodi-remote-playlists-mode' buffer."
;;   (interactive)
;;   (kodi-remote-context kodi-remote-playlists-mode))

;;;###autoload
;; (defun kodi-remote-playlists-draw (&optional _arg _noconfirm)
;;   "Draw the list of playlists.
;; Optional argument _ARG revert excepts this param.
;; Optional argument _NOCONFIRM revert excepts this param."
;;   (interactive)
;;   (kodi-remote-playlists-get)
;;   (setq tabulated-list-entries '())
;;   (dolist (item (append (let-alist kodi-properties .items) nil))
;;     (push (list (spiderbit-get-id item)
;; 		(vector `(,(spiderbit-get-name item)
;; 			  id ,(spiderbit-get-show-id item))))
;; 	  tabulated-list-entries))
;;   (tabulated-list-init-header)
;;   (tabulated-list-print))



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
