;;; helm-rhythmbox.el --- control Rhythmbox's play queue via Helm -*-lexical-binding: t-*-

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/helm-rhythmbox
;; Package-Version: 20160524.1158
;; Version: 0.1
;; Package-Requires: ((helm "1.5.0") (cl-lib "0.5"))
;; Created: Mar 13 2015

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

;; Control Rhythmbox's play queue via Helm.
;;
;; Start by calling `helm-rhythmbox' to browse the library retrieved from a
;; running Rhythmbox instance via D-Bus.  The default action is to play the
;; selected song.  There is also the option to enqueue the selected song, this
;; also works for a selection.
;;
;; The whole library is retrieved on first use and can be reloaded
;; with `helm-rhythmbox-reload-library'.
;;
;; Note that candidates formatted with `helm-rhythmbox-candidate-format' are
;; cached, so to see the effect of changing `helm-rhythmbox-candidate-format'
;; (after already having loaded the library) requires reloading the library to
;; reset the cache.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'cl-macs)
(require 'helm)


(defvar helm-rhythmbox-candidate-format
  #'helm-rhythmbox-candidate-default-format
  "The function to format a candidate.
Will get a `helm-rhythmbox-song' struct as input and must output
a string.  Defaults to `helm-rhythmbox-candidate-format'.")

(defvar helm-rhythmbox--cache nil
  "Store a list of cached candidates for `helm-rhythmbox'.
A candidate is of the format (DISPLAY . SONG), where SONG is a
`helm-rhythmbox-song' structure, and DISPLAY is the result
of (funcall helm-rhythmbox-candidate-format SONG).")

(cl-defstruct (helm-rhythmbox-song
               (:constructor helm-rhythmbox-song-new
                             (artist album title uri)))
  "Structure representing a song retrieved from Rhythmbox.
Slots:

`artist'
     The artist of the song.

`album'
     The album containing the song.

`title'
     The title of the song.

`uri'
     The URI of the song, used by Rhythmbox as identifier."
  artist album title uri)

(defun helm-rhythmbox-candidate-default-format (song)
  "Default candidate format function for `helm-rhythmbox'.
Formats the SONG as \"ARTIST - ALBUM - TITLE\"."
  (format "%s - %s - %s"
          (helm-rhythmbox-song-artist song)
          (helm-rhythmbox-song-album song)
          (helm-rhythmbox-song-title song)))

(defun helm-rhythmbox-song-from-dbus-item (dbus-item)
  "Make a `helm-rhythmbox-song' from DBUS-ITEM.
DBUS-ITEM is a song retrieved via D-Bus."
  (helm-rhythmbox-song-new
   (cl-caadr  (assoc "Artist" dbus-item))
   (cl-caadr  (assoc "Album" dbus-item))
   (cl-caadr  (assoc "DisplayName" dbus-item))
   (cl-caaadr (assoc "URLs" dbus-item))))

(defun helm-rhythmbox-load-callback (dbus-items)
  "Callback for `helm-rhythmbox-load-library'.
Will populate `helm-rhythmbox--cache' with DBUS-ITEMS using
`helm-rhythmbox-song-from-dbus-item' and
`helm-rhythmbox-candidate-format'."
  (setq helm-rhythmbox--cache
        (mapcar (lambda (dbus-item)
                  (let ((song (helm-rhythmbox-song-from-dbus-item dbus-item)))
                    (cons (funcall helm-rhythmbox-candidate-format song)
                          song)))
                dbus-items)))

(defun helm-rhythmbox-load-library ()
  "Load the Rhythmbox library via D-Bus."
  (let* ((service "org.gnome.Rhythmbox3")
         (path "/org/gnome/UPnP/MediaServer2/Library/all")
         (interface "org.gnome.UPnP.MediaContainer2")
         (nb-songs (dbus-get-property
                    :session service path interface "ChildCount")))
    (if (not nb-songs)
        (error "Couldn't connect to Rhythmbox")
      (dbus-call-method-asynchronously
       :session service path interface "ListChildren" #'helm-rhythmbox-load-callback
       0 nb-songs '("*")))))

(defun helm-rhythmbox-reload-library ()
  "Reload the Rhythmbox library."
  (interactive)
  (helm-rhythmbox-load-library))

(defun helm-rhythmbox-play-song (song)
  "Let Rhythmbox play the given SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (dbus-call-method :session service path interface
                      "OpenUri" (helm-rhythmbox-song-uri song))))

(defun helm-rhythmbox-enqueue-song (_)
  "Let Rhythmbox enqueue the marked songs."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dolist (song (helm-marked-candidates))
      (dbus-call-method :session service path interface
                        "AddToQueue" (helm-rhythmbox-song-uri song)))))

(defun helm-rhythmbox-playpause-song ()
  "Play/pause the selected song."
  (interactive)
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (dbus-call-method :session service path interface
                      "PlayPause")))

(defvar helm-source-rhythmbox-track-search
  (helm-build-sync-source "Rhythmbox"
    :candidates 'helm-rhythmbox--cache
    :action  '(("Play song" . helm-rhythmbox-play-song)
               ("Enqueue song" . helm-rhythmbox-enqueue-song))
    :init (lambda () (unless helm-rhythmbox--cache
                  (helm-rhythmbox-load-library))))
  "Helm source for searching Rhythmbox tracks.")

;;;###autoload
(defun helm-rhythmbox ()
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive)
  (helm :sources '(helm-source-rhythmbox-track-search)
        :buffer "*helm-rhythmbox*"))


(provide 'helm-rhythmbox)
;;; helm-rhythmbox.el ends here
