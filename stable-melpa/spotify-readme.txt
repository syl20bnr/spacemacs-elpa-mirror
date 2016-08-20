Play, pause, skip songs in the Spotify app from Emacs.

(global-set-key (kbd "s-<pause>") #'spotify-playpause)
(global-set-key (kbd "s-M-<pause>") #'spotify-next)

On a system supporting freedesktop.org's D-Bus you can enable song
notifications in the minibuffer.

(spotify-enable-song-notifications)
