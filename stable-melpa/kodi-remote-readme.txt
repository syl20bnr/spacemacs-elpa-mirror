A major to remote control kodi instances.
First specify the hostname/ip of your kodi webserver:
(setq kodi-host-name "my-htpc:8080")
Then open the Remote with the command:
'kodi-remote-keyboard'
Alternativly open the Media buffers with the commands:
'kodi-remote-movies'
'kodi-remote-series'
'kodi-remote-music'
'kodi-remote-playlists'
Also open the current kodi Video Playlist with the command:
'kodi-remote-playlist'
OPTIONAL: setup settings for deleting files (over tramp)
(setq kodi-dangerous-options t)
(setq kodi-access-host "my-htpc")
if you don't use ssh to access your kodi server / nas:
(setq kodi-access-method "smb/ftp/adb...")
