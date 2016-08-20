;;; ampc.el --- Asynchronous Music Player Controller -*- lexical-binding: t -*-

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Christopher Schmidt <christopher@ch.ristopher.com>
;; Maintainer: Christopher Schmidt <christopher@ch.ristopher.com>
;; Version: 0.2
;; Created: 2011-12-06
;; Keywords: ampc, mpc, mpd
;; Compatibility: GNU Emacs: 24.x

;; This file is part of ampc.

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
;;; * description
;; ampc is a controller for the Music Player Daemon (http://mpd.wikia.com/).

;;; ** installation
;; If you use GNU ELPA, install ampc via M-x package-list-packages RET or
;; (package-install 'ampc).  Otherwise, grab the files in this repository and
;; put the Emacs Lisp ones somewhere in your load-path or add the directory the
;; files are in to it, e.g.:
;;
;; (add-to-list 'load-path "~/.emacs.d/ampc")
;; (autoload 'ampc "ampc" nil t)
;;
;; Byte-compile ampc (M-x byte-compile-file RET /path/to/ampc.el RET) to improve
;; its performance!

;;; *** tagger
;; ampc is not only a frontend to MPD but also a full-blown audio file tagger.
;; To use this feature you have to build the backend application, `ampc_tagger',
;; which in turn uses TagLib (http://taglib.github.com/), a dual-licended
;; (LGPL/MPL) audio meta-data library written in C++.  TagLib has no
;; dependencies on its own.
;;
;; To build `ampc_tagger', locate ampc_tagger.cpp.  The file can be found in the
;; directory in which this file, ampc.el, is located.  Compile the file and
;; either customize `ampc-tagger-executable' to point to the binary file or move
;; the executable in a suitable directory so Emacs finds it via consulting
;; `exec-path'.
;;
;; g++ -O2 ampc_tagger.cpp -oampc_tagger -ltag && sudo cp ampc_tagger /usr/local/bin && rm ampc_tagger
;;
;; You have to customize `ampc-tagger-music-directories' in order to use the
;; tagger.  This variable should be a list of directories in which your music
;; files are located.  Usually this list should have only one entry, the value
;; of your mpd.conf's `music_directory'.
;;
;; If `ampc-tagger-backup-directory' is non-nil, the tagger saved copies of all
;; files that are about to be modified to this directory.  Emacs's regular
;; numeric backup filename syntax is used for the backup file names.  By default
;; `ampc-tagger-backup-directory' is set to "~/.emacs.d/ampc-backups/".

;;; ** usage
;; To invoke ampc call the command `ampc', e.g. via M-x ampc RET.  The first
;; argument to `ampc' is the host, the second is the port.  Both values default
;; to nil.  If nil, ampc will use the value specified in `ampc-default-server',
;; by default localhost:6600.  To make ampc use the full frame rather than the
;; selected window for its window setup, customise `ampc-use-full-frame' to a
;; non-nil value.
;;
;; ampc offers three independent views which expose different parts of the user
;; interface.  The current playlist view, the default view at startup, may be
;; accessed using the `J' key (that is `S-j').  The playlist view may be
;; accessed using the `K' key.  The outputs view may be accessed by pressing
;; `L'.

;;; *** current playlist view
;; The playlist view looks like this:
;;
;; .........................
;; . 1      . 3  . 4  . 5  .
;; ..........    .    .    .
;; . 2      .    .    .    .
;; .        .    .    .    .
;; .        .    .    .    .
;; .        ................
;; .        . 6            .
;; .        .              .
;; .........................
;;
;; Window one exposes basic information about the daemon, such as the current
;; state (stop/play/pause), the song currently playing or the volume.
;;
;; All windows, except the status window, contain a tabular list of items.  Each
;; item may be selected/marked.  There may be multiple selections.
;;
;; To mark an entry, move the point to the entry and press `m' (ampc-mark).  To
;; unmark an entry, press `u' (ampc-unmark).  To unmark all entries, press `U'
;; (ampc-unmark-all).  To toggle marks, press `t' (ampc-toggle-marks).  Pressing
;; `<down-mouse-1>' with the mouse mouse cursor on a list entry will move point
;; to the entry and toggle the mark.  To navigate to the next entry, press `n'
;; (ampc-next-line).  Analogous, pressing `p' (ampc-previous-line) moves the
;; point to the previous entry.
;;
;; Window two shows the current playlist.  The song that is currently played by
;; the daemon, if any, is highlighted.  To delete the selected songs from the
;; playlist, press `d' (ampc-delete).  Pressing `<down-mouse-3>' will move the
;; point to the entry under cursor and delete it from the playlist.  To move the
;; selected songs up, press `<up>' (ampc-up).  Analogous, press `<down>'
;; (ampc-down) to move the selected songs down.  Pressing `<return>'
;; (ampc-play-this) or `<down-mouse-2>' will play the song at point/cursor.
;;
;; Windows three to five are tag browsers.  You use them to narrow the song
;; database to certain songs.  Think of tag browsers as filters, analogous to
;; piping `grep' outputs through additional `grep' filters.  The property of the
;; songs that is filtered is displayed in the header line of the window.
;;
;; Window six shows the songs that match the filters defined by windows three to
;; five.  To add the selected song to the playlist, press `a' (ampc-add).
;; Pressing `<down-mouse-3>' will move the point to the entry under the cursor
;; and execute `ampc-add'.  These key bindings works in tag browsers as well.
;; Calling `ampc-add' in a tag browser adds all songs filtered up to the
;; selected browser to the playlist.
;;
;; The tag browsers of the current playlist view (accessed via `J') are `Genre'
;; (window 3), `Artist' (window 4) and `Album' (window 5).  The key `M' may be
;; used to fire up a slightly modified current playlist view.  There is no
;; difference to the default current playlist view other than that the tag
;; browsers filter to `Genre' (window 3), `Album' (window 4) and `Artist'
;; (window 5).  Metaphorically speaking, the order of the `grep' filters defined
;; by the tag browsers is different.

;;; *** playlist view
;; The playlist view resembles the current playlist view.  The window, which
;; exposes the playlist content, is replaced by three windows, vertically
;; arragned, though.  The top one still shows the current playlist.  The bottom
;; one shows a list of stored playlists.  The middle window exposes the content
;; of the selected (stored) playlist.  All commands that used to work in the
;; current playlist view and modify the current playlist now modify the selected
;; (stored) playlist unless the point is within the current playlist buffer.
;; The list of stored playlists is the only view in ampc that may have only one
;; marked entry.
;;
;; To queue a playlist, press `l' (ampc-load) or `<down-mouse-2>'.  To delete a
;; playlist, press `d' (ampc-delete-playlist) or `<down-mouse-3>'.  The command
;; `ampc-rename-playlist', bound to `r', can be used to rename a playlist.
;;
;; Again, the key `<' may be used to setup a playlist view with a different
;; order of tag browsers.

;;; *** outputs view
;; The outputs view contains a single list which shows the configured outputs of
;; MPD.  To toggle the enabled property of the selected outputs, press `a'
;; (ampc-toggle-output-enabled) or `<mouse-3>'.

;;; ** tagger
;; To start the tagging subsystem, press `I' (ampc-tagger).  This key binding
;; works in every buffer associated with ampc.  First, the command tries to
;; determine which files you want to tag.  The files are collected using either
;; the selected entries within the current buffer, the file associated with the
;; entry at point, or, if both sources did not provide any files, the audio file
;; that is currently played by MPD.  Next, the tagger view is created.  On the
;; right there is the buffer that contain the tag data.  Each line in this
;; buffer represents a tag with a value.  Tag and value are separated by a
;; colon.  Valid tags are "Title", "Artist", "Album", "Comment", "Genre", "Year"
;; and "Track".  The value can be an arbitrary string.  Whitespaces in front and
;; at the end of the value are ignored.  If the value is "<keep>", the tag line
;; is ignored.
;;
;; To save the specified tag values back to the files, press `C-c C-c'
;; (ampc-tagger-save).  To exit the tagger and restore the previous window
;; configuration, press `C-c C-q'.  `C-u C-c C-c' saved the tags and exits the
;; tagger.  Only tags that are actually specified within the tagger buffer
;; written back to the file.  Other tags will not be touched by ampc.  For
;; example, to clear the "Commentary" tag, you need to specify the line
;;
;; Commentary:
;;
;; In the tagger buffer.  Omitting this line will make the tagger not touch the
;; "Commentary" tag at all.
;;
;; On the right there is the files list buffer.  The selection of this buffer
;; specifies which files the command `ampc-tag-save' will write to.  If no file
;; is selected, the file at point in the file list buffer is used.
;;
;; To reset the values of the tags specified in the tagger buffer to the common
;; values of all selected files specified by the selection of the files list
;; buffer, press `C-c C-r' (ampc-tagger-reset).  With a prefix argument,
;; `ampc-tagger-reset' restores missing tags as well.
;;
;; You can use tab-completion within the tagger buffer for both tags and tag
;; values.
;;
;; You can also use the tagging subsystem on its own without a running ampc
;; instance.  To start the tagger, call `ampc-tag-files'.  This function accepts
;; one argument, a list of absolute file names which are the files to tag.  ampc
;; provides a minor mode for dired, `ampc-tagger-dired-mode'.  If this mode is
;; enabled within a dired buffer, pressing `C-c C-t' (ampc-tagger-dired) will
;; start the tagger on the current selection.
;;
;; The following ampc-specific hooks are run during tagger usage:
;;
;; `ampc-tagger-grab-hook': Run by the tagger before grabbing tags of a file.
;; Each function is called with one argument, the file name.
;;
;; `ampc-tagger-grabbed-hook': Run by the tagger after grabbing tags of a file.
;; Each function is called with one argument, the file name.
;;
;; `ampc-tagger-store-hook': Run by the tagger before writing tags back to a
;; file.  Each function is called with two arguments, FOUND-CHANGED and DATA.
;; FOUND-CHANGED is non-nil if the tags that are about to be written differ from
;; the ones in the file.  DATA is a cons.  The car specifies the full file name
;; of the file that is about to be written to, the cdr is an alist that
;; specifies the tags that are about to be (over-)written.  The car of each
;; entry in this list is a symbol specifying the tag (one of the ones in
;; `ampc-tagger-tags'), the cdr a string specifying the value.  The cdr of DATA
;; may be modified.  If FOUND-CHANGED is nil and the cdr of DATA is not modified
;; throughout the hook is run, the file is not touched.
;; `ampc-tagger-stored-hook' is still run, though.
;;
;; `ampc-tagger-stored-hook': Run by the tagger after writing tags back to a
;; file.  Each function is called with two arguments, FOUND-CHANGED and DATA.
;; These are the same arguments that were already passed to
;; `ampc-tagger-store-hook'.  The car of DATA, the file name, may be modified.
;;
;; These hooks can be used to handle vc locking and unlocking of files.  For
;; renaming files according to their (new) tag values, ampc provides the
;; function `ampc-tagger-rename-artist-title' which may be added to
;; `ampc-tagger-stored-hook'.  The new file name generated by this function is
;; "Artist"_-_"Title"."extension".  Characters within "Artist" and "Title" that
;; are not alphanumeric are substituted with underscores.

;;; ** global keys
;; Aside from `J', `M', `K', `<' and `L', which may be used to select different
;; views, and `I' which starts the tagger, ampc defines the following global
;; keys.  These binding are available in every buffer associated with ampc:
;;
;; `k' (ampc-toggle-play): Toggle play state.  If MPD does not play a song,
;; start playing the song at point if the current buffer is the playlist buffer,
;; otherwise start at the beginning of the playlist.  With numeric prefix
;; argument 4, stop player rather than pause if applicable.
;;
;; `l' (ampc-next): Play next song.
;; `j' (ampc-previous): Play previous song
;;
;; `c' (ampc-clear): Clear playlist.
;; `s' (ampc-shuffle): Shuffle playlist.
;;
;; `S' (ampc-store): Store playlist.
;; `O' (ampc-load): Load selected playlist into the current playlist.
;; `R' (ampc-rename-playlist): Rename selected playlist.
;; `D' (ampc-delete-playlist): Delete selected playlist.
;;
;; `y' (ampc-increase-volume): Increase volume.
;; `M-y' (ampc-decrease-volume): Decrease volume.
;; `C-M-y' (ampc-set-volume): Set volume.
;; `h' (ampc-increase-crossfade): Increase crossfade.
;; `M-h' (ampc-decrease-crossfade): Decrease crossfade.
;; `C-M-h' (ampc-set-crossfade): Set crossfade.
;;
;; `e' (ampc-toggle-repeat): Toggle repeat state.
;; `r' (ampc-toggle-random): Toggle random state.
;; `f' (ampc-toggle-consume): Toggle consume state.
;;
;; `P' (ampc-goto-current-song): Select the current playlist window and move
;; point to the current song.
;; `G' (ampc-mini): Select song to play via `completing-read'.
;;
;; `T' (ampc-trigger-update): Trigger a database update.
;; `Z' (ampc-suspend): Suspend ampc.
;; `q' (ampc-quit): Quit ampc.
;;
;; The keymap of ampc is designed to fit the QWERTY United States keyboard
;; layout.  If you use another keyboard layout, feel free to modify
;; `ampc-mode-map'.  For example, I use a regular QWERTZ German keyboard
;; (layout), so I modify `ampc-mode-map' in my init.el like this:
;;
;; (eval-after-load 'ampc
;;   '(flet ((substitute-ampc-key
;;            (from to)
;;            (define-key ampc-mode-map to (lookup-key ampc-mode-map from))
;;            (define-key ampc-mode-map from nil)))
;;      (substitute-ampc-key (kbd "z") (kbd "Z"))
;;      (substitute-ampc-key (kbd "y") (kbd "z"))
;;      (substitute-ampc-key (kbd "M-y") (kbd "M-z"))
;;      (substitute-ampc-key (kbd "C-M-y") (kbd "C-M-z"))
;;      (substitute-ampc-key (kbd "<") (kbd ";"))))
;;
;; If ampc is suspended, you can still use every interactive command that does
;; not directly operate on or with the user interace of ampc.  For example it is
;; perfectly fine to call `ampc-increase-volume' or `ampc-toggle-play' via M-x
;; RET.  Especially the commands `ampc-status' and `ampc-mini' are predesignated
;; to be bound in the global keymap and called when ampc is suspended.
;; `ampc-status' messages the information that is displayed by the status window
;; of ampc.  `ampc-mini' lets you select a song to play via `completing-read'.
;; To start ampc suspended, call `ampc' with the third argument being non-nil.
;; To check whether ampc is connected to the daemon and/or suspended, call
;; `ampc-is-on-p' or `ampc-suspended-p'.
;;
;; (global-set-key (kbd "<f7>")
;;                 (lambda ()
;;                   (interactive)
;;                   (unless (ampc-on-p)
;;                     (ampc nil nil t))
;;                   (ampc-status)))
;; (global-set-key (kbd "<f8>")
;;                 (lambda ()
;;                   (interactive)
;;                   (unless (ampc-on-p)
;;                     (ampc nil nil t))
;;                   (ampc-mini)))

;;; Code:
;;; * code
(eval-when-compile
  (require 'cl))
(require 'network-stream)
(require 'avl-tree)

;;; ** declarations
(defgroup ampc ()
  "Asynchronous client for the Music Player Daemon."
  :prefix "ampc-"
  :group 'multimedia
  :group 'applications)

;;; *** customs
(defcustom ampc-debug nil
  "Non-nil means log outgoing communication between ampc and MPD.
If the value is neither t nor nil, also log incoming data."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Outgoing" t)
                 (const :tag "Incoming and outgoing" full)))

(defcustom ampc-use-full-frame nil
  "If non-nil, ampc will use the entire Emacs screen."
  :type 'boolean)

(defcustom ampc-truncate-lines t
  "If non-nil, truncate lines in ampc buffers."
  :type 'boolean)

(defcustom ampc-default-server '("localhost" . 6600)
  "The MPD server to connect to if the arguments to `ampc' are nil.
This variable is a cons cell, with the car specifying the
hostname and the cdr specifying the port.  Both values can be
nil, which will make ampc query the user for values on each
invocation."
  :type '(cons (choice :tag "Hostname"
                       (string)
                       (const :tag "Ask" nil))
               (choice :tag "Port"
                       (string)
                       (integer)
                       (const :tag "Ask" nil))))

(defcustom ampc-synchronous-commands '(t status currentsong play)
  "List of MPD commands that should be executed synchronously.
Executing commands that print lots of output synchronously will
result in massive performance improvements of ampc.  If the car
of this list is t, execute all commands synchronously other
than the ones specified by the rest of the list."
  :type '(repeat symbol))

(defcustom ampc-status-tags nil
  "List of additional tags of the current song that are added to
the internal status of ampc and thus are passed to the functions
in `ampc-status-changed-hook'.  Each element may be a string that
specifies a tag that is returned by MPD's `currentsong'
command."
  :type '(list symbol))

(defcustom ampc-volume-step 5
  "Default step of `ampc-increase-volume' and
`ampc-decrease-volume' for changing the volume."
  :type 'integer)

(defcustom ampc-crossfade-step 5
  "Default step of `ampc-increase-crossfade' and
`ampc-decrease-crossfade' for changing the crossfade."
  :type 'integer)

(defcustom ampc-tag-transform-funcs '(("Time" . ampc-transform-time)
                                      ("Track" . ampc-transform-track))
  "Alist of tag treatment functions.
The car, a string, of each entry specifies the MPD tag, the cdr a
function which transforms the tag to the value that should be
used by ampc.  The function is called with one string argument,
the tag value, and should return the treated value."
  :type '(alist :key-type string :value-type function))

(defcustom ampc-tagger-music-directories nil
  "List of base directories in which your music files are located.
Usually this list should have only one entry, the value of your
mpd.conf's `music_directory'"
  :type '(list directory))

(defcustom ampc-tagger-executable "ampc_tagger"
  "The name or full path to ampc's tagger executable."
  :type 'string)

(defcustom ampc-tagger-backup-directory
  (file-name-directory (locate-user-emacs-file "ampc-backups/"))
  "The directory in which the tagger copies files before modifying.
If nil, disable backups."
  :type '(choice (const :tag "Disable backups" nil)
                 (directory :tag "Directory")))

;;; **** hooks
(defcustom ampc-before-startup-hook nil
  "A hook run before startup.
This hook is called as the first thing when ampc is started."
  :type 'hook)

(defcustom ampc-connected-hook nil
  "A hook run after ampc connected to MPD."
  :type 'hook)

(defcustom ampc-suspend-hook nil
  "A hook run when suspending ampc."
  :type 'hook)

(defcustom ampc-quit-hook nil
  "A hook run when exiting ampc."
  :type 'hook)

(defcustom ampc-status-changed-hook nil
  "A hook run whenever the status of the daemon (that is volatile
properties such as volume or current song) changes.  The hook is
run with one arg, an alist that contains the new status.  The car
of each entry is a symbol, the cdr is a string.  Valid keys are:

    volume
    repeat
    random
    consume
    xfade
    state
    song
    Artist
    Title

and the keys in `ampc-status-tags'.  Not all keys may be present
all the time!"
  :type 'hook)

(defcustom ampc-tagger-grab-hook nil
  "Hook run by the tagger before grabbing tags of a file.
Each function is called with one argument, the file name."
  :type 'hook)
(defcustom ampc-tagger-grabbed-hook nil
  "Hook run by the tagger after grabbing tags of a file.
Each function is called with one argument, the file name."
  :type 'hook)

(defcustom ampc-tagger-store-hook nil
  "Hook run by the tagger before writing tags back to a file.
Each function is called with two arguments, FOUND-CHANGED and
DATA.  FOUND-CHANGED is non-nil if the tags that are about to be
written differ from the ones in the file.  DATA is a cons.  The
car specifies the full file name of the file that is about to be
written to, the cdr is an alist that specifies the tags that are
about to be (over-)written.  The car of each entry in this list
is a symbol specifying the tag (one of the ones in
`ampc-tagger-tags'), the cdr a string specifying the value.  The
cdr of DATA may be modified.  If FOUND-CHANGED is nil and the cdr
of DATA is not modified throughout the hook is run, the file is
not touched.  `ampc-tagger-stored-hook' is still run, though."
  :type 'hook)
(defcustom ampc-tagger-stored-hook nil
  "Hook run by the tagger after writing tags back to a file.
Each function is called with two arguments, FOUND-CHANGED and
DATA.  These are the same arguments that were already passed to
`ampc-tagger-store-hook'.  The car of DATA, the file name, may be
modified."
  :type 'hook)

;;; *** faces
(defface ampc-mark-face '((t (:inherit font-lock-constant-face)))
  "Face of the mark.")
(defface ampc-marked-face '((t (:inherit warning)))
  "Face of marked entries.")
(defface ampc-unmarked-face '((t (:inerhit default)))
  "Face of unmarked entries.")
(defface ampc-current-song-mark-face '((t (:inherit region)))
  "Face of mark of the current song.")
(defface ampc-current-song-marked-face '((t (:inherit region)))
  "Face of the current song if marked.")

(defface ampc-tagger-tag-face '((t (:inherit font-lock-constant-face)))
  "Face of tags within the tagger.")
(defface ampc-tagger-keyword-face '((t (:inherit font-lock-keyword-face)))
  "Face of tags within the tagger.")

;;; *** internal variables
(defvar ampc-views
  (let* ((songs '(1.0 song :properties (("Track" :title "#" :width 4)
                                        ("Title" :min 15 :max 40)
                                        ("Time" :width 6)
                                        ("Artist" :min 15 :max 40)
                                        ("Album" :min 15 :max 40))))
         (rs_a `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1 :select t)
                          (0.33 tag :tag "Artist" :id 2)
                          (1.0 tag :tag "Album" :id 3))
                     ,songs))
         (rs_b `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1 :select t)
                          (0.33 tag :tag "Album" :id 2)
                          (1.0 tag :tag "Artist" :id 3))
                     ,songs))
         (pl-prop '(:properties (("Title" :min 15 :max 40)
                                 ("Artist" :min 15 :max 40)
                                 ("Album" :min 15 :max 40)
                                 ("Time" :width 6)))))
    `((tagger
       horizontal
       (0.65 files-list
             :properties ((filename :shrink t :title "File" :min 20 :max 40)
                          ("Title" :min 15 :max 40)
                          ("Artist" :min 15 :max 40)
                          ("Album" :min 15 :max 40)
                          ("Genre" :min 15 :max 40)
                          ("Year" :width 5)
                          ("Track" :title "#" :width 4)
                          ("Comment" :min 15 :max 40))
             :dedicated nil)
       (1.0 tagger))
      ("Current playlist view (Genre|Artist|Album)"
       ,(kbd "J")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist ,@pl-prop))
       ,rs_a)
      ("Current playlist view (Genre|Album|Artist)"
       ,(kbd "M")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist ,@pl-prop))
       ,rs_b)
      ("Playlist view (Genre|Artist|Album)"
       ,(kbd "K")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.4 current-playlist ,@pl-prop)
                 (0.4 playlist ,@pl-prop)
                 (1.0 playlists)))
       ,rs_a)
      ("Playlist view (Genre|Album|Artist)"
       ,(kbd "<")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.4 current-playlist ,@pl-prop)
                 (0.4 playlist ,@pl-prop)
                 (1.0 playlists)))
       ,rs_b)
      ("Outputs view"
       ,(kbd "L")
       outputs :properties (("outputname" :title "Name" :min 10 :max 30)
                            ("outputenabled" :title "Enabled" :width 9))))))

(defvar ampc-connection nil)
(defvar ampc-host nil)
(defvar ampc-port nil)
(defvar ampc-outstanding-commands nil)

(defvar ampc-no-implicit-next-dispatch nil)
(defvar ampc-working-timer nil)
(defvar ampc-yield nil)
(defvar ampc-yield-redisplay nil)

(defvar ampc-windows nil)
(defvar ampc-all-buffers nil)

(defvar ampc-type nil)
(make-variable-buffer-local 'ampc-type)
(defvar ampc-dirty nil)
(make-variable-buffer-local 'ampc-dirty)

(defvar ampc-internal-db nil)
(defvar ampc-status nil)

(defvar ampc-tagger-previous-configuration nil)
(defvar ampc-tagger-version-verified nil)
(defvar ampc-tagger-completion-all-files nil)
(defvar ampc-tagger-genres nil)

(defconst ampc-tagger-version "0.1")
(defconst ampc-tagger-tags '(Title Artist Album Comment Genre Year Track))

;;; *** mode maps
(defvar ampc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "k") 'ampc-toggle-play)
    (define-key map (kbd "l") 'ampc-next)
    (define-key map (kbd "j") 'ampc-previous)
    (define-key map (kbd "c") 'ampc-clear)
    (define-key map (kbd "s") 'ampc-shuffle)
    (define-key map (kbd "S") 'ampc-store)
    (define-key map (kbd "O") 'ampc-load)
    (define-key map (kbd "R") 'ampc-rename-playlist)
    (define-key map (kbd "D") 'ampc-delete-playlist)
    (define-key map (kbd "y") 'ampc-increase-volume)
    (define-key map (kbd "M-y") 'ampc-decrease-volume)
    (define-key map (kbd "C-M-y") 'ampc-set-volume)
    (define-key map (kbd "h") 'ampc-increase-crossfade)
    (define-key map (kbd "M-h") 'ampc-decrease-crossfade)
    (define-key map (kbd "C-M-h") 'ampc-set-crossfade)
    (define-key map (kbd "e") 'ampc-toggle-repeat)
    (define-key map (kbd "r") 'ampc-toggle-random)
    (define-key map (kbd "f") 'ampc-toggle-consume)
    (define-key map (kbd "P") 'ampc-goto-current-song)
    (define-key map (kbd "G") 'ampc-mini)
    (define-key map (kbd "q") 'ampc-quit)
    (define-key map (kbd "z") 'ampc-suspend)
    (define-key map (kbd "T") 'ampc-trigger-update)
    (define-key map (kbd "I") 'ampc-tagger)
    (loop for view in ampc-views
          do (when (stringp (car view))
               (define-key map (cadr view)
                 `(lambda ()
                    (interactive)
                    (ampc-change-view ',view)))))
    map))

(defvar ampc-item-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "m") 'ampc-mark)
    (define-key map (kbd "u") 'ampc-unmark)
    (define-key map (kbd "U") 'ampc-unmark-all)
    (define-key map (kbd "n") 'ampc-next-line)
    (define-key map (kbd "p") 'ampc-previous-line)
    (define-key map (kbd "<down-mouse-1>") 'ampc-mouse-toggle-mark)
    (define-key map (kbd "<mouse-1>") 'ampc-mouse-align-point)
    (define-key map [remap next-line] 'ampc-next-line)
    (define-key map [remap previous-line] 'ampc-previous-line)
    (define-key map [remap tab-to-tab-stop] 'ampc-move-to-tab)
    map))

(defvar ampc-current-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<return>") 'ampc-play-this)
    (define-key map (kbd "<down-mouse-2>") 'ampc-mouse-play-this)
    (define-key map (kbd "<mouse-2>") 'ampc-mouse-align-point)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "d") 'ampc-delete)
    (define-key map (kbd "<up>") 'ampc-up)
    (define-key map (kbd "<down>") 'ampc-down)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-playlists-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "l") 'ampc-load)
    (define-key map (kbd "r") 'ampc-rename-playlist)
    (define-key map (kbd "d") 'ampc-delete-playlist)
    (define-key map (kbd "<down-mouse-2>") 'ampc-mouse-load)
    (define-key map (kbd "<mouse-2>") 'ampc-mouse-align-point)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete-playlist)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-tag-song-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-add)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-add)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-outputs-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-toggle-output-enabled)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-toggle-output-enabled)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-files-list-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "C-c C-q") 'ampc-tagger-quit)
    (define-key map (kbd "C-c C-c") 'ampc-tagger-save)
    (define-key map (kbd "C-c C-r") 'ampc-tagger-reset)
    (define-key map [remap ampc-tagger] nil)
    (define-key map [remap ampc-quit] 'ampc-tagger-quit)
    (loop for view in ampc-views
          do (when (stringp (car view))
               (define-key map (cadr view) nil)))
    map))

(defvar ampc-tagger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") 'ampc-tagger-quit)
    (define-key map (kbd "C-c C-c") 'ampc-tagger-save)
    (define-key map (kbd "C-c C-r") 'ampc-tagger-reset)
    (define-key map (kbd "<tab>") 'ampc-tagger-completion-at-point)
    map))

(defvar ampc-tagger-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ampc-tagger-dired)
    map))

;;; **** menu
(easy-menu-define nil ampc-mode-map nil
  `("ampc"
    ("Change view" ,@(loop for view in ampc-views
                           when (stringp (car view))
                           collect (vector (car view)
                                           `(lambda ()
                                              (interactive)
                                              (ampc-change-view ',view)))
                           end))
    ["Run tagger" ampc-tagger]
    "--"
    ["Play" ampc-toggle-play
     :visible (and ampc-status
                   (not (equal (cdr (assq 'state ampc-status)) "play")))]
    ["Pause" ampc-toggle-play
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Stop" (lambda () (interactive) (ampc-toggle-play 4))
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Next" ampc-next]
    ["Previous" ampc-previous]
    "--"
    ["Clear playlist" ampc-clear]
    ["Shuffle playlist" ampc-shuffle]
    ["Store playlist" ampc-store]
    ["Queue Playlist" ampc-load :visible (ampc-playlist)]
    ["Rename Playlist" ampc-rename-playlist :visible (ampc-playlist)]
    ["Delete Playlist" ampc-delete-playlist :visible (ampc-playlist)]
    "--"
    ["Increase volume" ampc-increase-volume]
    ["Decrease volume" ampc-decrease-volume]
    ["Set volume" ampc-set-volume]
    ["Increase crossfade" ampc-increase-crossfade]
    ["Decrease crossfade" ampc-decrease-crossfade]
    ["Set crossfade" ampc-set-crossfade]
    ["Toggle repeat" ampc-toggle-repeat
     :style toggle
     :selected (equal (cdr (assq 'repeat ampc-status)) "1")]
    ["Toggle random" ampc-toggle-random
     :style toggle
     :selected (equal (cdr (assq 'random ampc-status)) "1")]
    ["Toggle consume" ampc-toggle-consume
     :style toggle
     :selected (equal (cdr (assq 'consume ampc-status)) "1")]
    "--"
    ["Trigger update" ampc-trigger-update]
    ["Suspend" ampc-suspend]
    ["Quit" ampc-quit]))

(easy-menu-define ampc-selection-menu ampc-item-mode-map
  "Selection menu for ampc"
  '("ampc Mark"
    ["Add to playlist" ampc-add
     :visible (not (eq (car ampc-type) 'outputs))]
    ["Toggle enabled" ampc-toggle-output-enabled
     :visible (eq (car ampc-type) 'outputs)]
    "--"
    ["Next line" ampc-next-line]
    ["Previous line" ampc-previous-line]
    ["Mark" ampc-mark]
    ["Unmark" ampc-unmark]
    ["Unmark all" ampc-unmark-all]
    ["Toggle marks" ampc-toggle-marks
     :visible (not (eq (car ampc-type) 'playlists))]))

(defvar ampc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item
     "mpc/prev" 'ampc-previous 'previous map
     :help "Previous")
    (tool-bar-local-item
     "mpc/play" 'ampc-toggle-play 'play map
     :help "Play"
     :visible '(and ampc-status
                    (not (equal (cdr (assq 'state ampc-status)) "play"))))
    (tool-bar-local-item
     "mpc/pause" 'ampc-toggle-play 'pause map
     :help "Pause"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/stop" (lambda () (interactive) (ampc-toggle-play 4)) 'stop map
     :help "Stop"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/next" 'ampc-next 'next map
     :help "Next")
    map))

;;; ** code
;;; *** macros
(defmacro ampc-with-buffer (type &rest body)
  (declare (indent 1) (debug t))
  `(let* ((type- ,type)
          (w (if (windowp type-)
                 type-
               (loop for w in (ampc-normalize-windows)
                     thereis (when (with-current-buffer
                                       (window-buffer w)
                                     (etypecase type-
                                       (symbol (eq (car ampc-type) type-))
                                       (cons (equal ampc-type type-))))
                               w)))))
     (when w
       (with-selected-window w
         (with-current-buffer (window-buffer w)
           (let ((inhibit-read-only t))
             ,@(if (eq (car body) 'no-se)
                   (cdr body)
                 `((save-excursion
                     (goto-char (point-min))
                     ,@body)))))))))

(defmacro ampc-fill-skeleton (tag &rest body)
  (declare (indent 1) (debug t))
  `(let ((tag- ,tag)
         (data-buffer (current-buffer)))
     (ampc-with-buffer tag-
       no-se
       (unless (eq ampc-dirty 'keep-dirty)
         (let ((old-point-data (get-text-property (point) 'cmp-data))
               (old-window-start-offset
                (1- (count-lines (window-start) (point)))))
           (put-text-property (point-min) (point-max) 'not-updated t)
           (when (eq ampc-dirty 'erase)
             (put-text-property (point-min) (point-max) 'data nil))
           (goto-char (point-min))
           ,@body
           (goto-char (point-min))
           (loop until (eobp)
                 do (if (get-text-property (point) 'not-updated)
                        (kill-line 1)
                      (add-text-properties (+ (point) 2)
                                           (progn (forward-line nil)
                                                  (1- (point)))
                                           '(mouse-face highlight))))
           (remove-text-properties (point-min) (point-max) '(not-updated))
           (goto-char (point-min))
           (when old-point-data
             (loop until (eobp)
                   do (when (equal (get-text-property (point) 'cmp-data)
                                   old-point-data)
                        (set-window-start
                         nil
                         (save-excursion
                           (forward-line (- old-window-start-offset))
                           (point))
                         t)
                        (return))
                   (forward-line)
                   finally do (goto-char (point-min)))))
         (let ((effective-height (- (window-height)
                                    (if mode-line-format 1 0)
                                    (if header-line-format 1 0))))
           (when (< (- (1- (line-number-at-pos (point-max)))
                       (line-number-at-pos (window-start)))
                    effective-height)
             (set-window-start nil
                               (save-excursion
                                 (goto-char (point-max))
                                 (forward-line (- (1+ effective-height)))
                                 (point))
                               t)))
         (ampc-align-point)
         (ampc-set-dirty nil)))))

(defmacro ampc-with-selection (arg &rest body)
  (declare (indent 1) (debug t))
  `(let ((arg- ,arg))
     (if (or (and (not arg-)
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward-regexp "^* " nil t)))
             (and arg- (symbolp arg-)))
         (loop initially do (goto-char (point-min))
               finally do (ampc-align-point)
               while (search-forward-regexp "^* " nil t)
               for index from 0
               do (save-excursion
                    ,@body))
       (setf arg- (prefix-numeric-value arg-))
       (ampc-align-point)
       (loop until (eobp)
             for index from 0 to (1- (abs arg-))
             do (save-excursion
                  ,@body)
             until (if (< arg- 0) (ampc-previous-line) (ampc-next-line))))))

(defmacro ampc-iterate-source (data-buffer delimiter bindings &rest body)
  (declare (indent 3) (debug t))
  (when (memq (intern delimiter) bindings)
    (callf2 delq (intern delimiter) bindings)
    (push (list (intern delimiter)
                '(buffer-substring (point) (line-end-position)))
          bindings))
  `(,@(if data-buffer `(with-current-buffer ,data-buffer) '(progn))
    (when (search-forward-regexp
           ,(concat "^" (regexp-quote delimiter) ": ")
           nil t)
      (loop with next
            do (save-restriction
                 (setf next (ampc-narrow-entry
                             ,(concat "^" (regexp-quote delimiter) ": ")))
                 (let ,(loop for binding in bindings
                             if (consp binding)
                             collect binding
                             else
                             collect `(,binding (ampc-extract
                                                 (ampc-extract-regexp
                                                  ,(symbol-name binding))))
                             end)
                   ,@body))
            while next
            do (goto-char next)))))

(defmacro ampc-iterate-source-output (delimiter bindings pad-data &rest body)
  (declare (indent 2) (debug t))
  `(let ((output-buffer (current-buffer))
         (tags (loop for (tag . props) in
                     (plist-get (cdr ampc-type) :properties)
                     collect (cons tag (ampc-extract-regexp tag)))))
     (ampc-iterate-source
         data-buffer ,delimiter ,bindings
       (let ((pad-data ,pad-data))
         (with-current-buffer output-buffer
           (ampc-insert (ampc-pad pad-data) ,@body))))))

(defmacro ampc-extract-regexp (tag)
  (if (stringp tag)
      (concat "^" (regexp-quote tag) ": \\(.*\\)$")
    `(concat "^" (regexp-quote ,tag) ": \\(.*\\)$")))

(defmacro ampc-tagger-log (&rest what)
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create "*Tagger Log*")
     (ampc-tagger-log-mode)
     (save-excursion
       (goto-char (point-max))
       (let ((inhibit-read-only t)
             (what (concat ,@what)))
         (when ampc-debug
           (message "ampc: %s" what))
         (insert what)))))

;;; *** modes
(define-derived-mode ampc-outputs-mode ampc-item-mode "ampc-o")

(define-derived-mode ampc-tag-song-mode ampc-item-mode "ampc-ts")

(define-derived-mode ampc-current-playlist-mode ampc-playlist-mode "ampc-cpl"
  (ampc-highlight-current-song-mode))

(define-derived-mode ampc-playlist-mode ampc-item-mode "ampc-pl")

(define-derived-mode ampc-playlists-mode ampc-item-mode "ampc-pls")

(define-derived-mode ampc-files-list-mode ampc-item-mode "ampc-files-list")

(define-derived-mode ampc-tagger-mode nil "ampc-tagger"
  (set (make-local-variable 'tool-bar-map) ampc-tool-bar-map)
  (set (make-local-variable 'tab-stop-list)
       (list (+ (loop for tag in ampc-tagger-tags
                      maximize (length (symbol-name tag)))
                2)))
  (set (make-local-variable 'completion-at-point-functions)
       '(ampc-tagger-complete-tag ampc-tagger-complete-value))
  (setf truncate-lines ampc-truncate-lines
        font-lock-defaults
        `(((,(concat "^\\([ \t]*\\(?:"
                     (mapconcat 'symbol-name ampc-tagger-tags "\\|")
                     "\\)[ \t]*:\\)"
                     "\\(\\(?:[ \t]*"
                     "\\(?:"
                     (mapconcat 'identity ampc-tagger-genres "\\|") "\\|<keep>"
                     "\\)"
                     "[ \t]*$\\)?\\)")
            (1 'ampc-tagger-tag-face)
            (2 'ampc-tagger-keyword-face)))
          t)))

(define-derived-mode ampc-tagger-log-mode nil "ampc-tagger-log")

(define-derived-mode ampc-item-mode ampc-mode "ampc-item"
  (setf font-lock-defaults '((("^\\(\\*\\)\\(.*\\)$"
                               (1 'ampc-mark-face)
                               (2 'ampc-marked-face))
                              ("" 0 'ampc-unmarked-face))
                             t)))

(define-derived-mode ampc-mode special-mode "ampc"
  (buffer-disable-undo)
  (set (make-local-variable 'tool-bar-map) ampc-tool-bar-map)
  (setf truncate-lines ampc-truncate-lines
        mode-line-modified "--"))

(define-minor-mode ampc-highlight-current-song-mode ""
  nil
  nil
  nil
  (funcall (if ampc-highlight-current-song-mode
               'font-lock-add-keywords
             'font-lock-remove-keywords)
           nil
           '((ampc-find-current-song
              (1 'ampc-current-song-mark-face)
              (2 'ampc-current-song-marked-face)))))

;;;###autoload
(define-minor-mode ampc-tagger-dired-mode
  "Minor mode that adds a audio file meta data tagging key binding to dired."
  nil
  " ampc-tagger"
  nil
  (assert (derived-mode-p 'dired-mode)))

;;; *** internal functions
(defun ampc-tagger-report (args status)
  (unless (zerop status)
    (let ((message (format (concat "ampc_tagger (%s %s) returned with a "
                                   "non-zero exit status (%s)")
                           ampc-tagger-executable
                           (mapconcat 'identity args " ")
                           status)))
      (ampc-tagger-log message "\n")
      (error message))))

(defun ampc-tagger-call (&rest args)
  (ampc-tagger-report
   args
   (apply 'call-process ampc-tagger-executable nil t nil args)))

(defun ampc-int-insert-cmp (p1 p2)
  (cond ((< p1 p2) 'insert)
        ((eq p1 p2) 'overwrite)
        (t (- p1 p2))))

(defun ampc-normalize-windows ()
  (setf ampc-windows
        (loop for (window . buffer) in ampc-windows
              collect (cons (if (and (window-live-p window)
                                     (eq (window-buffer window) buffer))
                                window
                              (get-buffer-window buffer))
                            buffer)))
  (delq nil (mapcar 'car ampc-windows)))

(defun ampc-restore-window-configuration ()
  (let ((windows
          (sort (delq nil
                      (mapcar (lambda (w)
                                (when (eq (window-frame w)
                                          (selected-frame))
                                  w))
                              (ampc-normalize-windows)))
                (lambda (w1 w2)
                  (loop for w in (window-list nil nil (frame-first-window))
                        do (when (eq w w1)
                             (return t))
                        (when (eq w w2)
                          (return nil)))))))
    (when windows
      (setf (window-dedicated-p (car windows)) nil)
      (loop for w in (cdr windows)
            do (delete-window w)))))

(defun ampc-tagger-tags-modified (tags new-tags)
  (loop with found-changed
        for (tag . value) in new-tags
        for prop = (assq tag tags)
        do (unless (equal (cdr prop) value)
             (setf (cdr prop) value
                   found-changed t))
        finally return found-changed))

(defun ampc-change-view (view)
  (if (equal ampc-outstanding-commands '((idle nil)))
      (ampc-configure-frame (cddr view))
    (message "ampc is busy, cannot change window layout")))

(defun ampc-quote (string)
  (concat "\"" (replace-regexp-in-string "\"" "\\\"" string) "\""))

(defun ampc-in-ampc-p (&optional or-in-tagger)
  (or (when (ampc-on-p)
        ampc-type)
      (when or-in-tagger
        (memq (car ampc-type) '(files-list tagger)))))

(defun ampc-add-impl (&optional data)
  (ampc-on-files (lambda (file)
                   (if (ampc-playlist)
                       (ampc-send-command 'playlistadd
                                          '(:keep-prev t)
                                          (ampc-quote (ampc-playlist))
                                          file)
                     (ampc-send-command 'add '(:keep-prev t) (ampc-quote file)))
                   data)))

(defun ampc-on-files (func &optional data)
  (cond ((null data)
         (loop for d in (get-text-property (line-end-position) 'data)
               do (ampc-on-files func d)))
        ((avl-tree-p data)
         (avl-tree-mapc (lambda (e) (ampc-on-files func (cdr e))) data))
        ((stringp data)
         (funcall func data))
        (t
         (loop for d in (reverse data)
               do (ampc-on-files func (cdr (assoc "file" d)))))))

(defun ampc-skip (N)
  (ampc-send-command
   'play
   `(:callback ,(lambda ()
                  (ampc-send-command 'status '(:front t))))
   (lambda ()
     (let ((song (cdr (assq 'song ampc-status)))
           (playlist-length (cdr (assq 'playlistlength ampc-status))))
       (unless (and song playlist-length)
         (throw 'skip nil))
       (max 0 (min (+ (string-to-number song) N)
                   (1- (string-to-number playlist-length))))))))

(defun* ampc-find-current-song
    (limit &aux (point (point)) (song (cdr (assq 'song ampc-status))))
  (when (and song
             (<= (1- (line-number-at-pos (point)))
                 (setf song (string-to-number song)))
             (>= (1- (line-number-at-pos limit)) song))
    (goto-char (point-min))
    (forward-line song)
    (save-restriction
      (narrow-to-region (max point (point)) (min limit (line-end-position)))
      (search-forward-regexp "\\(?1:\\(\\`\\*\\)?\\)\\(?2:.*\\)$"))))

(defun ampc-set-volume-impl (arg &optional func)
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ampc-send-command
   'setvol
   `(:callback ,(lambda ()
                  (ampc-send-command 'status '(:front t))))
   (lambda ()
     (unless ampc-status
       (throw 'skip nil))
     (max (min (if func
                   (funcall func
                            (string-to-number
                             (cdr (assq 'volume ampc-status)))
                            (or arg ampc-volume-step))
                 arg)
               100)
          0))))

(defun ampc-set-crossfade-impl (arg &optional func)
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ampc-send-command
   'crossfade
   `(:callback ,(lambda ()
                  (ampc-send-command 'status '(:front t))))
   (lambda ()
     (unless ampc-status
       (throw 'skip nil))
     (max (if func
              (funcall func
                       (string-to-number
                        (cdr (assq 'xfade ampc-status)))
                       (or arg ampc-crossfade-step))
            arg)
          0))))

(defun* ampc-tagger-make-backup (file)
  (unless ampc-tagger-backup-directory
    (return-from ampc-tagger-make-backup))
  (when (functionp ampc-tagger-backup-directory)
    (funcall ampc-tagger-backup-directory file)
    (return-from ampc-tagger-make-backup))
  (unless (file-directory-p ampc-tagger-backup-directory)
    (make-directory ampc-tagger-backup-directory t))
  (let* ((real-file
          (loop with real-file = file
                for target = (file-symlink-p real-file)
                while target
                do (setf real-file (expand-file-name
                                    target (file-name-directory real-file)))
                finally return real-file))
         (target
          (loop with base = (file-name-nondirectory real-file)
                for i from 1
                for file = (expand-file-name
                            (concat base ".~"
                                    (int-to-string i)
                                    "~")
                            ampc-tagger-backup-directory)
                while (file-exists-p file)
                finally return file)))
    (ampc-tagger-log "\tBackup file: " (abbreviate-file-name target) "\n")
    (copy-file real-file target nil t)))

(defun* ampc-move (N &aux with-marks entries-to-move (up (< N 0)))
  (save-excursion
    (goto-char (point-min))
    (loop while (search-forward-regexp "^* " nil t)
          do (push (point) entries-to-move)))
  (if entries-to-move
      (setf with-marks t)
    (push (point) entries-to-move))
  (when (save-excursion
          (loop with max = (1- (count-lines (point-min) (point-max)))
                for p in entries-to-move
                do (goto-char p)
                for line = (+ (1- (line-number-at-pos)) N)
                always (and (>= line 0) (<= line max))))
    (when up
      (setf entries-to-move (nreverse entries-to-move)))
    (when with-marks
      (ampc-unmark-all))
    (loop for p in entries-to-move
          do  (goto-char p)
          for line = (1- (line-number-at-pos))
          do (if (and (not (eq (car ampc-type) 'current-playlist))
                      (ampc-playlist))
                 (ampc-send-command 'playlistmove
                                    '(:keep-prev t)
                                    (ampc-quote (ampc-playlist))
                                    line
                                    (+ line N))
               (ampc-send-command 'move '(:keep-prev t) line (+ line N))))
    (if with-marks
        (loop for p in (nreverse entries-to-move)
              do (goto-char p)
              (forward-line N)
              (save-excursion
                (ampc-mark-impl t 1))
              (ampc-align-point))
      (forward-line N)
      (ampc-align-point))))

(defun ampc-toggle-state (state arg)
  (when (or arg ampc-status)
    (ampc-send-command
     state
     nil
     (cond ((null arg)
            (if (equal (cdr (assq state ampc-status)) "1")
                0
              1))
           ((> (prefix-numeric-value arg) 0) 1)
           (t 0)))))

(defun ampc-playlist (&optional at-point)
  (ampc-with-buffer 'playlists
    (if (and (not at-point)
             (search-forward-regexp "^* \\(.*\\)$" nil t))
        (let ((result (match-string 1)))
          (set-text-properties 0 (length result) nil result)
          result)
      (unless (eobp)
        (buffer-substring-no-properties
         (+ (line-beginning-position) 2)
         (line-end-position))))))

(defun* ampc-mark-impl (select N &aux result (inhibit-read-only t))
  (when (eq (car ampc-type) 'playlists)
    (assert (or (not select) (null N) (eq N 1)))
    (ampc-with-buffer 'playlists
      (loop while (search-forward-regexp "^\\* " nil t)
            do (replace-match "  " nil nil))))
  (loop repeat (or N 1)
        until (eobp)
        do (move-beginning-of-line nil)
        (delete-char 1)
        (insert (if select "*" " "))
        (setf result (ampc-next-line nil)))
  (ampc-post-mark-change-update)
  result)

(defun ampc-post-mark-change-update ()
  (ecase (car ampc-type)
    ((current-playlist playlist outputs))
    (playlists
     (ampc-update-playlist))
    ((song tag)
     (loop
      for w in
      (loop for w on (ampc-normalize-windows)
            thereis (when (or (eq (car w) (selected-window))
                              (and (eq (car ampc-type) 'tag)
                                   (eq (with-current-buffer
                                           (window-buffer (car w))
                                         (car ampc-type))
                                       'song)))
                      (cdr w)))
      do (with-current-buffer (window-buffer w)
           (when (memq (car ampc-type) '(song tag))
             (ampc-set-dirty t))))
     (ampc-fill-tag-song))
    (files-list
     (ampc-tagger-update))))

(defun* ampc-tagger-get-values (tag all-files &aux result)
  (ampc-with-buffer 'files-list
    no-se
    (save-excursion
      (macrolet
          ((add-file
            ()
            `(let ((value (cdr (assq tag (get-text-property (point) 'data)))))
               (unless (member value result)
                 (push value result)))))
        (if all-files
            (loop until (eobp)
                  initially do (goto-char (point-min))
                  (ampc-align-point)
                  do (add-file)
                  until (ampc-next-line))
          (ampc-with-selection nil
            (add-file))))))
  result)

(defun ampc-tagger-update ()
  (ampc-with-buffer 'tagger
    (loop
     while (search-forward-regexp (concat "^[ \t]*\\("
                                          (mapconcat 'symbol-name
                                                     ampc-tagger-tags
                                                     "\\|")
                                          "\\)[ \t]*:"
                                          "[ \t]*\\(<keep>[ \t]*?\\)"
                                          "\\(?:\n\\)?$")
                                  nil
                                  t)
     for tag = (intern (match-string 1))
     do (when (memq tag ampc-tagger-tags)
          (let ((values (save-match-data (ampc-tagger-get-values tag nil))))
            (when (eq (length values) 1)
              (replace-match (car values) nil t nil 2)))))))

(defun ampc-tagger-complete-tag ()
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (unless (search-backward-regexp "^.*:" nil t)
        (when (search-backward-regexp "\\(^\\|[ \t]\\).*" nil t)
          (when (looking-at "[ \t]")
            (forward-char 1))
          (list (point)
                (search-forward-regexp ":\\|$")
                (mapcar (lambda (tag) (concat (symbol-name tag) ":"))
                        ampc-tagger-tags)))))))

(defun* ampc-tagger-complete-value (&aux tag)
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (save-excursion
        (unless (search-backward-regexp (concat "^[ \t]*\\("
                                                (mapconcat 'symbol-name
                                                           ampc-tagger-tags
                                                           "\\|")
                                                "\\)[ \t]*:")
                                        nil t)
          (return-from ampc-tagger-complete-tag))
        (setf tag (intern (match-string 1))))
      (save-excursion
        (search-backward-regexp "[: \t]")
        (forward-char 1)
        (list (point)
              (search-forward-regexp "[ \t]\\|$")
              (let ((values (cons "<keep>" (ampc-tagger-get-values
                                            tag
                                            ampc-tagger-completion-all-files))))
                (when (eq tag 'Genre)
                  (loop for g in ampc-tagger-genres
                        do (unless (member g values)
                             (push g values))))
                values))))))

(defun ampc-align-point ()
  (unless (eobp)
    (move-beginning-of-line nil)
    (forward-char 2)
    (re-search-forward " *" nil t)))

(defun* ampc-pad (tabs &optional dont-honour-item-mode)
  (loop with new-tab-stop-list
        with offset-dec = (if (and (not dont-honour-item-mode)
                                   (derived-mode-p 'ampc-item-mode))
                              2
                            0)
        for tab in tabs
        for offset-cell on (if (derived-mode-p 'ampc-item-mode)
                               tab-stop-list
                             (cons 0 tab-stop-list))
        for offset = (car offset-cell)
        for props in (or (plist-get (cdr ampc-type) :properties)
                         '(nil . nil))
        by (lambda (cell) (or (cdr cell) '(nil . nil)))
        do (decf offset offset-dec)
        with first = t
        with current-offset = 0
        when (<= current-offset offset)
        do (when (and (not first) (eq (- offset current-offset) 0))
             (incf offset))
        and concat (make-string (- offset current-offset) ? ) into result
        and do (setf current-offset offset)
        else
        concat " " into result
        and do (incf current-offset)
        end
        do (unless tab
             (setf tab ""))
        (when (and (plist-get (cdr props) :shrink)
                   (cadr offset-cell)
                   (>= (+ current-offset (length tab) 1) (- (cadr offset-cell)
                                                            offset-dec)))
          (setf tab (concat (substring tab 0 (max (- (cadr offset-cell)
                                                     offset-dec
                                                     current-offset
                                                     4)
                                                  3))
                            "...")))
        concat tab into result
        do (push (+ current-offset offset-dec) new-tab-stop-list)
        (incf current-offset (length tab))
        (setf first nil)
        finally return
        (if (equal (callf nreverse new-tab-stop-list) tab-stop-list)
            result
          (propertize result 'tab-stop-list new-tab-stop-list))))

(defun ampc-update-header ()
  (when (or (memq (car ampc-type) '(tag playlists))
            (plist-get (cdr ampc-type) :properties))
    (setf header-line-format
          (concat
           (make-string (floor (fringe-columns 'left t)) ? )
           (ecase (car ampc-type)
             (tag
              (concat "  " (plist-get (cdr ampc-type) :tag)))
             (playlists
              "  Playlists")
             (t
              (ampc-pad (loop for (name . props) in
                              (plist-get (cdr ampc-type) :properties)
                              collect (or (plist-get props :title) name))
                        t)))))))

(defun ampc-set-dirty (tag-or-dirty &optional dirty)
  (if (or (null tag-or-dirty) (memq tag-or-dirty '(t erase keep-dirty)))
      (setf ampc-dirty tag-or-dirty)
    (loop for w in (ampc-normalize-windows)
          do (with-current-buffer (window-buffer w)
               (when (eq (car ampc-type) tag-or-dirty)
                 (ampc-set-dirty dirty))))))

(defun ampc-update ()
  (if ampc-status
      (loop for w in (ampc-normalize-windows)
            do (with-current-buffer (window-buffer w)
                 (when (and ampc-dirty (not (eq ampc-dirty 'keep-dirty)))
                   (ecase (car ampc-type)
                     (outputs
                      (ampc-send-command 'outputs))
                     (playlist
                      (ampc-update-playlist))
                     ((tag song)
                      (if (assoc (ampc-tags) ampc-internal-db)
                          (ampc-fill-tag-song)
                        (push (cons (ampc-tags) nil) ampc-internal-db)
                        (ampc-set-dirty 'tag 'keep-dirty)
                        (ampc-set-dirty 'song 'keep-dirty)
                        (ampc-send-command 'listallinfo)))
                     (status
                      (ampc-send-command 'status)
                      (ampc-send-command 'currentsong))
                     (playlists
                      (ampc-send-command 'listplaylists))
                     (current-playlist
                      (ampc-send-command 'playlistinfo))))))
    (ampc-send-command 'status)
    (ampc-send-command 'currentsong)))

(defun ampc-update-playlist ()
  (ampc-with-buffer 'playlists
    (if (search-forward-regexp "^\\* " nil t)
        (ampc-send-command 'listplaylistinfo
                           nil
                           (get-text-property (point) 'data))
      (ampc-with-buffer 'playlist
        (erase-buffer)
        (ampc-set-dirty nil)))))

(defun ampc-send-command-impl (command)
  (when ampc-debug
    (message "ampc: -> %s" command))
  (when (ampc-on-p)
    (process-send-string ampc-connection (concat command "\n"))))

(defun* ampc-send-command (command &optional props &rest args)
  (destructuring-bind (&key (front nil) (keep-prev nil) (full-remove nil)
                            (remove-other nil) &allow-other-keys
                            &aux idle)
      props
    (when (and (not keep-prev)
               (eq (caar ampc-outstanding-commands) command)
               (equal (cddar ampc-outstanding-commands) args))
      (return-from ampc-send-command))
    (unless ampc-working-timer
      (setf ampc-yield 0
            ampc-working-timer (run-at-time nil 0.1 'ampc-yield)))
    (when (equal (caar ampc-outstanding-commands) 'idle)
      (pop ampc-outstanding-commands)
      (setf idle t))
    (when (and (not keep-prev) (cdr ampc-outstanding-commands))
      (setf (cdr ampc-outstanding-commands)
            (loop for other-cmd in (cdr ampc-outstanding-commands)
                  unless (and (memq (car other-cmd) (list command remove-other))
                              (or (not full-remove)
                                  (progn
                                    (assert (null remove-other))
                                    (equal (cddr other-cmd) args))))
                  collect other-cmd
                  end)))
    (setf command (apply 'list command props args))
    (if front
        (push command ampc-outstanding-commands)
      (setf ampc-outstanding-commands
            (nconc ampc-outstanding-commands
                   (list command))))
    (when idle
      (push '(noidle nil) ampc-outstanding-commands)
      (ampc-send-command-impl "noidle"))))

(defun ampc-send-next-command ()
  (loop while ampc-outstanding-commands
        for command =
        (loop for command = (car ampc-outstanding-commands)
              for command-id = (replace-regexp-in-string
                                "^.*?-" ""
                                (symbol-name (car command)))
              thereis
              (catch 'skip
                (ampc-send-command-impl
                 (concat command-id
                         (loop for a in (cddr command)
                               concat " "
                               do (when (functionp a)
                                    (callf funcall a))
                               concat (etypecase a
                                        (integer (number-to-string a))
                                        (string a)))))
                (let ((callback (plist-get (cadar ampc-outstanding-commands)
                                           :callback))
                      (old-head (pop ampc-outstanding-commands)))
                  (when callback (funcall callback))
                  (push old-head ampc-outstanding-commands))
                command-id)
              do (pop ampc-outstanding-commands)
              while ampc-outstanding-commands)
        while command
        while (let ((member (memq (intern command) ampc-synchronous-commands)))
                (if member
                    (not (eq (car ampc-synchronous-commands) t))
                  (eq (car ampc-synchronous-commands) t)))
        do (loop with head = ampc-outstanding-commands
                 with ampc-no-implicit-next-dispatch = t
                 with ampc-yield-redisplay = t
                 while (ampc-on-p)
                 while (eq head ampc-outstanding-commands)
                 do (accept-process-output ampc-connection 0 100)))
  (unless ampc-outstanding-commands
    (when ampc-working-timer
        (cancel-timer ampc-working-timer)
        (setf ampc-yield nil
              ampc-working-timer nil)
        (ampc-fill-status))
    (setf ampc-outstanding-commands '((idle nil)))
    (ampc-send-command-impl "idle")))

(defun ampc-tree< (a b)
  (string< (car a) (car b)))

(defun ampc-create-tree ()
  (avl-tree-create 'ampc-tree<))

(defsubst ampc-extract (regexp)
  (goto-char (point-min))
  (when (search-forward-regexp regexp nil t)
    (match-string 1)))

(defsubst ampc-clean-tag (tag value)
  (if value
      (let ((func (cdr (assoc tag ampc-tag-transform-funcs))))
        (if func
            (funcall func value)
          value))
    (unless (equal tag "Track")
      "[Not Specified]")))

(defun ampc-insert (element data &optional cmp cmp-data)
  (goto-char (point-min))
  (unless cmp-data
    (setf cmp-data data))
  (let ((action
         (if (functionp cmp)
             (loop until (eobp)
                   for tp = (get-text-property (+ (point) 2) 'cmp-data)
                   thereis (let ((r (funcall cmp cmp-data tp)))
                             (if (symbolp r)
                                 r
                               (forward-line r)
                               nil))
                   finally return 'insert)
           (loop with stringp-cmp-data = (stringp cmp-data)
                 with min = 1
                 with max = (1+ (count-lines (point-min) (point-max)))
                 with at-min = t
                 do (when (< (- max min) 20)
                      (unless at-min
                        (forward-line (- min max)))
                      (return (loop repeat (- max min)
                                    for tp = (get-text-property (+ (point) 2)
                                                                'cmp-data)
                                    thereis
                                    (if (equal tp cmp-data)
                                        'update
                                      (unless (if stringp-cmp-data
                                                  (string< tp cmp-data)
                                                (string<
                                                 (buffer-substring-no-properties
                                                  (+ (point) 2)
                                                  (line-end-position))
                                                 element))
                                        'insert))
                                    do (forward-line)
                                    finally return 'insert)))
                 do (forward-line (funcall (if at-min '+ '-) (/ (- max min) 2)))
                 for tp = (get-text-property (+ (point) 2) 'cmp-data)
                 thereis (when (equal tp cmp-data) 'update)
                 do (if (setf at-min (if stringp-cmp-data
                                         (string< tp cmp-data)
                                       (string< (buffer-substring-no-properties
                                                 (+ (point) 2)
                                                 (line-end-position))
                                                element)))
                        (incf min (floor (/ (- max min) 2.0)))
                      (decf max (floor (/ (- max min) 2.0))))
                 finally return 'insert))))
    (ecase action
      (insert
       (insert (propertize (concat "  " element "\n")
                           'data (if (eq cmp t) (list data) data)
                           'cmp-data cmp-data)))
      ((update overwrite)
       (remove-text-properties (point) (1+ (point)) '(not-updated))
       (when (or (eq ampc-dirty 'erase) (eq action 'overwrite))
         (let ((origin (point)))
           (forward-char 2)
           (kill-line 1)
           (insert element "\n")
           (goto-char origin)))
       (let ((next (1+ (line-end-position))))
         (put-text-property (point) next 'cmp-data cmp-data)
         (put-text-property
          (point) next
          'data (cond ((eq cmp t)
                       (let ((rest (get-text-property (point) 'data)))
                         (if (memq data rest)
                             rest
                           (cons data rest))))
                      (t data))))
       (eq (char-after) ?*)))))

(defun ampc-fill-tag (trees)
  (put-text-property (point-min) (point-max) 'data nil)
  (loop with new-trees
        for tree in trees
        do (when tree
             (avl-tree-mapc
              (lambda (e)
                (when (ampc-insert (car e) (cdr e) t (car e))
                  (push (cdr e) new-trees)))
              tree))
        finally return new-trees))

(defun ampc-fill-song (trees)
  (loop
   for songs in trees
   do (loop for song in songs
            do (ampc-insert
                (ampc-pad
                 (loop for (p . v) in (plist-get (cdr ampc-type) :properties)
                       collect (cdr (assoc p song))))
                `((,song))))))

(defsubst ampc-narrow-entry (delimiter-regexp)
  (let ((result))
    (narrow-to-region
     (line-beginning-position)
     (or (save-excursion
           (goto-char (line-end-position))
           (when (search-forward-regexp delimiter-regexp nil t)
             (setf result (point))
             (1- (line-beginning-position))))
         (point-max)))
    result))

(defun ampc-fill-playlist ()
  (ampc-fill-skeleton 'playlist
    (let ((index 0))
      (ampc-iterate-source-output "file" (file)
        (loop for (tag . tag-regexp) in tags
              collect (ampc-clean-tag tag (ampc-extract tag-regexp)))
        `(("file" . ,file)
          (index . ,(1- (incf index))))
        'ampc-int-insert-cmp
        index))))

(defun ampc-fill-outputs ()
  (ampc-fill-skeleton 'outputs
    (ampc-iterate-source-output "outputid" (outputid outputenabled)
      (loop for (tag . tag-regexp) in tags
            collect (ampc-clean-tag tag (ampc-extract tag-regexp)))
      `(("outputid" . ,outputid)
        ("outputenabled" . ,outputenabled)))))

(defun* ampc-mini-impl (&aux songs)
  (ampc-iterate-source
      nil
      "file"
      (Title
       Artist
       (Pos (string-to-number (ampc-extract (ampc-extract-regexp "Pos")))))
    (let ((entry (cons (concat Title
                               (when Artist
                                 (concat " - " Artist)))
                       Pos)))
      (loop with mentry = (cons (car entry) (cdr entry))
            for index from 2
            while (assoc (car mentry) songs)
            do (setf (car mentry) (concat (car entry)
                                          " (" (int-to-string index) ")"))
            finally do (push mentry songs))))
  (unless songs
    (message "No song in the playlist")
    (return-from ampc-mini-impl))
  (let ((song (assoc (let ((inhibit-quit t))
                       (prog1
                           (with-local-quit
                             (completing-read "Song to play: " songs nil t))
                         (setf quit-flag nil)))
                     songs)))
    (when song
      (ampc-play-this (cdr song)))))

(defun ampc-fill-current-playlist ()
  (ampc-fill-skeleton 'current-playlist
    (ampc-iterate-source-output
        "file"
        (file (pos (string-to-number (ampc-extract
                                      (ampc-extract-regexp "Pos")))))
      (loop for (tag . tag-regexp) in tags
            collect (ampc-clean-tag tag (ampc-extract tag-regexp)))
      `(("file" . ,file)
        ("Pos" . ,pos))
      'ampc-int-insert-cmp
      pos)))

(defun ampc-fill-playlists ()
  (ampc-fill-skeleton 'playlists
    (with-current-buffer data-buffer
      (loop while (search-forward-regexp "^playlist: \\(.*\\)$" nil t)
            for playlist = (match-string 1)
            do (ampc-with-buffer 'playlists
                 (ampc-insert playlist playlist)))))
  (ampc-set-dirty 'playlist t)
  (ampc-update))

(defun ampc-yield ()
  (incf ampc-yield)
  (ampc-fill-status)
  (when ampc-yield-redisplay
    (redisplay t)))

(defun ampc-fill-status ()
  (ampc-with-buffer 'status
    (erase-buffer)
    (funcall (or (plist-get (cadr ampc-type) :filler)
                 (lambda (_)
                   (insert (ampc-status t) "\n")))
             ampc-status)
    (ampc-set-dirty nil)))

(defun ampc-fill-tag-song ()
  (loop
   with trees = (list (cdr (assoc (ampc-tags) ampc-internal-db)))
   for type in '(tag song)
   do
   (loop
    for w in (ampc-normalize-windows)
    do
    (with-current-buffer (window-buffer w)
      (when (eq (car ampc-type) type)
        (if ampc-dirty
            (if (and (not trees) (not (eq ampc-dirty 'keep-dirty)))
                (progn
                  (let ((inhibit-read-only t))
                    (erase-buffer))
                  (ampc-set-dirty nil))
              (ampc-fill-skeleton w
                (if (eq type 'tag)
                    (setf trees (ampc-fill-tag trees))
                  (ampc-fill-song trees))))
          (setf trees nil)
          (save-excursion
            (goto-char (point-min))
            (loop while (search-forward-regexp "^* " nil t)
                  do (callf append trees
                       (get-text-property (point) 'data))))))))))

(defun ampc-transform-track (track)
  (when (eq (length track) 1)
    (setf track (concat "0" track)))
  track)

(defun* ampc-transform-time (data &aux (time (string-to-number data)))
  (concat (number-to-string (/ time 60))
          ":"
          (when (< (% time 60) 10)
            "0")
          (number-to-string (% time 60))))

(defun ampc-handle-idle ()
  (loop until (eobp)
        for subsystem = (buffer-substring (point) (line-end-position))
        do (when (string-match "^changed: \\(.*\\)$" subsystem)
             (case (intern (match-string 1 subsystem))
               (database
                (setf ampc-internal-db (list (cons (ampc-tags) nil)))
                (ampc-set-dirty 'tag 'keep-dirty)
                (ampc-set-dirty 'song 'keep-dirty)
                (ampc-send-command 'listallinfo))
               (output
                (ampc-set-dirty 'outputs t))
               ((player options mixer)
                (setf ampc-status nil)
                (ampc-set-dirty 'status t))
               (stored_playlist
                (ampc-set-dirty 'playlists t))
               (playlist
                (ampc-set-dirty 'current-playlist t)
                (ampc-set-dirty 'status t))))
        (forward-line))
  (ampc-update))

(defun ampc-handle-setup (status)
  (unless (and (string-match "^ MPD \\(.+\\)\\.\\(.+\\)\\.\\(.+\\)$"
                             status)
               (let ((version-a (string-to-number (match-string 1 status)))
                     (version-b (string-to-number (match-string 2 status)))
                     ;; (version-c (string-to-number (match-string 2 status)))
                     )
                 (or (> version-a 0)
                     (>= version-b 15))))
    (error (concat "Your version of MPD is not supported.  "
                   "ampc supports MPD protocol version 0.15.0 "
                   "and later"))))

(defun ampc-fill-internal-db (running)
  (loop with tree = (assoc (ampc-tags) ampc-internal-db)
        with tags =
        (loop for w in (ampc-normalize-windows)
              for props = (with-current-buffer (window-buffer w)
                            (when (eq (car ampc-type) 'tag)
                              (ampc-set-dirty t)
                              (plist-get (cdr ampc-type) :tag)))
              when props
              collect props
              end)
        with song-props = (ampc-with-buffer 'song
                            (ampc-set-dirty t)
                            (plist-get (cdr ampc-type) :properties))
        for origin = (and (search-forward-regexp "^file: " nil t)
                          (line-beginning-position))
        then next
        while origin
        do (goto-char (1+ origin))
        for next = (and (search-forward-regexp "^file: " nil t)
                        (line-beginning-position))
        while (or (not running) next)
        do (save-restriction
             (narrow-to-region origin (or next (point-max)))
             (ampc-fill-internal-db-entry tree tags song-props))
        (when running
          (delete-region origin next)
          (setf next origin))))

(defun ampc-tags ()
  (loop for w in (ampc-normalize-windows)
        for tag = (with-current-buffer (window-buffer w)
                    (when (eq (car ampc-type) 'tag)
                      (plist-get (cdr ampc-type) :tag)))
        when tag
        collect tag
        end))

(defun ampc-fill-internal-db-entry (tree tags song-props)
  (loop for tag in tags
        for data = (ampc-clean-tag tag (ampc-extract (ampc-extract-regexp tag)))
        do (unless (cdr tree)
             (setf (cdr tree) (ampc-create-tree)))
        (setf tree (avl-tree-enter (cdr tree)
                                   (cons data nil)
                                   (lambda (_ match)
                                     match))))
  (push (cons (cons "file" (ampc-extract (ampc-extract-regexp "file")))
              (loop for p in song-props
                    for data = (ampc-clean-tag (car p)
                                               (ampc-extract
                                                (ampc-extract-regexp (car p))))
                    when data
                    collect (cons (car p) data)
                    end))
        (cdr tree)))

(defun ampc-fill-status-var (tags)
  (loop for k in tags
        for v = (ampc-extract (ampc-extract-regexp k))
        for s = (intern k)
        do (if v
               (setf (cdr (or (assq s ampc-status)
                              (car (push (cons s nil) ampc-status))))
                     v)
             (callf2 assq-delete-all s ampc-status))))

(defun ampc-handle-current-song ()
  (ampc-fill-status-var (append ampc-status-tags '("Artist" "Title" "file")))
  (ampc-fill-status)
  (run-hook-with-args ampc-status-changed-hook ampc-status))

(defun ampc-handle-status ()
  (ampc-fill-status-var '("volume" "repeat" "random" "consume" "xfade" "state"
                          "song" "playlistlength"))
  (ampc-with-buffer 'current-playlist
    (when ampc-highlight-current-song-mode
      (font-lock-fontify-buffer)))
  (run-hook-with-args ampc-status-changed-hook ampc-status))

(defun ampc-handle-update ()
  (message "Database update started"))

(defun ampc-handle-command (status)
  (cond
   ((eq status 'error)
    (pop ampc-outstanding-commands))
   ((eq status 'running)
    (case (caar ampc-outstanding-commands)
      (listallinfo (ampc-fill-internal-db t))))
   (t
    (let ((command (pop ampc-outstanding-commands)))
      (case (car command)
        (idle
         (ampc-handle-idle))
        (setup
         (ampc-handle-setup status))
        (currentsong
         (ampc-handle-current-song))
        (status
         (ampc-handle-status))
        (update
         (ampc-handle-update))
        (listplaylistinfo
         (ampc-fill-playlist))
        (listplaylists
         (ampc-fill-playlists))
        (playlistinfo
         (ampc-fill-current-playlist))
        (mini-playlistinfo
         (ampc-mini-impl))
        (mini-currentsong
         (ampc-status))
        (shuffle-listplaylistinfo
         (ampc-shuffle-playlist (plist-get (cadr command) :playlist)))
        (listallinfo
         (ampc-handle-listallinfo))
        (outputs
         (ampc-fill-outputs))))
    (unless ampc-outstanding-commands
      (ampc-update)))))

(defun* ampc-shuffle-playlist (playlist &aux songs)
  (ampc-iterate-source nil "file" (file)
    (push (cons file (random)) songs))
  (ampc-send-command 'playlistclear '(:full-remove t) (ampc-quote playlist))
  (loop for file in (mapcar 'car (sort songs
                                       (lambda (a b) (< (cdr a) (cdr b)))))
        do (ampc-send-command 'playlistadd
                              '(:keep-prev t)
                              (ampc-quote playlist)
                              file)))


(defun ampc-handle-listallinfo ()
  (ampc-fill-internal-db nil)
  (ampc-set-dirty 'tag t)
  (ampc-set-dirty 'song t))

(defun ampc-filter (_process string)
  (assert (buffer-live-p (process-buffer ampc-connection)))
  (with-current-buffer (process-buffer ampc-connection)
    (when string
      (when (and ampc-debug (not (eq ampc-debug t)))
        (message "ampc: <- %s" string))
      (goto-char (process-mark ampc-connection))
      (insert string)
      (set-marker (process-mark ampc-connection) (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((success))
        (if (or (progn
                  (when (search-forward-regexp
                         "^ACK \\[\\(.*\\)\\] {.*} \\(.*\\)\n\\'"
                         nil
                         t)
                    (message "ampc command error: %s (%s; %s)"
                             (match-string 2)
                             (match-string 1)
                             (funcall (if ampc-debug 'identity 'car)
                                      (car ampc-outstanding-commands)))
                    t))
                (when (search-forward-regexp "^OK\\(.*\\)\n\\'" nil t)
                  (setf success t)))
            (progn
              (let ((match-end (match-end 0)))
                (save-restriction
                  (narrow-to-region (point-min) match-end)
                  (goto-char (point-min))
                  (ampc-handle-command (if success (match-string 1) 'error)))
                (delete-region (point-min) match-end))
              (unless ampc-no-implicit-next-dispatch
                (ampc-send-next-command))))
        (ampc-handle-command 'running)))))

(defun* ampc-set-tab-offsets
    (&rest properties &aux (min 2) (optional-padding 0))
  (unless properties
    (return-from ampc-set-tab-offsets))
  (set (make-local-variable 'tab-stop-list) nil)
  (loop for (title . props) in properties
        for min- = (plist-get props :min)
        do (incf min (or (plist-get props :width) min-))
        (when min-
          (incf optional-padding (- (plist-get props :max) min-))))
  (loop for (title . props) in properties
        with offset = 2
        do (push offset tab-stop-list)
        (incf offset (or (plist-get props :width)
                         (let ((min- (plist-get props :min))
                               (max (plist-get props :max)))
                           (if (>= min (window-width))
                               min-
                             (min max
                                  (+ min-
                                     (floor (* (/ (float (- max min-))
                                                  optional-padding)
                                               (- (window-width)
                                                  min))))))))))
  (callf nreverse tab-stop-list))

(defun* ampc-configure-frame-1 (split &aux (split-type (car split)))
  (if (memq split-type '(vertical horizontal))
      (let* ((sizes))
        (loop with length = (if (eq split-type 'horizontal)
                                (window-total-width)
                              (window-total-height))
              with rest = length
              with rest-car
              for (size . subsplit) in (cdr split)
              do (if (equal size 1.0)
                     (progn (push t sizes)
                            (setf rest-car sizes))
                   (let ((l (if (integerp size) size (round (* size length)))))
                     (decf rest l)
                     (push l sizes)))
              finally do (setf (car rest-car) rest))
        (let ((first-window (selected-window)))
          (callf nreverse sizes)
          (loop for size in (copy-sequence sizes)
                for window on (cdr sizes)
                do (select-window
                    (setf (car window)
                          (split-window nil size (eq split-type 'horizontal)))))
          (setf (car sizes) first-window))
        (loop for subsplit in (cdr split)
              for window in sizes
              with result
              do (with-selected-window window
                   (setf result
                         (or (ampc-configure-frame-1 (cdr subsplit)) result)))
              finally return result))
    (setf (window-dedicated-p (selected-window)) nil)
    (pop-to-buffer-same-window
     (get-buffer-create
      (concat "*"
              (mapconcat (lambda (s) (concat (upcase (substring s 0 1))
                                             (substring s 1)))
                         (if (memq split-type '(tag song))
                             (list (or (plist-get (cdr split) :tag) "song"))
                           (split-string (symbol-name split-type) "-"))
                         " ")
              "*")))
    (if (memq split-type '(tag song))
        (ampc-tag-song-mode)
      (let ((mode (intern (concat "ampc-" (symbol-name split-type) "-mode"))))
        (unless (fboundp mode)
          (setf mode 'ampc-mode))
        (unless (eq major-mode 'mode)
          (funcall mode))))
    (destructuring-bind
        (&key (properties nil) (dedicated t) (mode-line t) &allow-other-keys)
        (cdr split)
      (apply 'ampc-set-tab-offsets properties)
      (setf ampc-type split
            (window-dedicated-p (selected-window)) dedicated
            mode-line-format (when mode-line
                               (default-value 'mode-line-format))))
    (set (make-local-variable 'mode-line-buffer-identification)
         '(:eval (let ((result
                        (concat (car-safe (propertized-buffer-identification
                                           (buffer-name)))
                                (when ampc-dirty
                                  " [Updating...]"))))
                   (if (< (length result) 12)
                       (concat result (make-string (- 12 (length result)) ? ))
                     result))))
    (ampc-update-header)
    (add-to-list 'ampc-all-buffers (current-buffer))
    (push (cons (or (plist-get (cdr split) :id) 9999) (selected-window))
          ampc-windows)
    (ampc-set-dirty t)
    (when (plist-get (cdr split) :select)
      (selected-window))))

(defun* ampc-configure-frame
    (split &optional no-update &aux (old-selection ampc-type) old-window-starts)
  (loop for w in (ampc-normalize-windows)
        do (with-selected-window w
             (with-current-buffer (window-buffer w)
               (push (cons (current-buffer) (window-start))
                     old-window-starts))))
  (if (not ampc-use-full-frame)
      (ampc-restore-window-configuration)
    (setf (window-dedicated-p (selected-window)) nil)
    (delete-other-windows))
  (setf ampc-windows nil)
  (let ((select-window (ampc-configure-frame-1 split)))
    (setf ampc-windows
          (mapcar (lambda (window)
                    (cons window (window-buffer window)))
                  (mapcar 'cdr (sort ampc-windows
                                     (lambda (a b) (< (car a) (car b)))))))
    (loop for w in (ampc-normalize-windows)
          do (with-selected-window w
               (let ((old-window-start (cdr (assq (current-buffer)
                                                  old-window-starts))))
                 (when old-window-start
                   (set-window-start nil old-window-start)))
               (when (and (derived-mode-p 'ampc-item-mode)
                          (> (length tab-stop-list) 1))
                 (ampc-set-dirty 'erase))))
    (select-window (or (loop for w in (ampc-normalize-windows)
                             thereis
                             (when (equal (with-current-buffer (window-buffer w)
                                            ampc-type)
                                          old-selection)
                               w))
                       select-window
                       (selected-window))))
  (unless no-update
    (ampc-update)))

(defun ampc-tagger-rename-artist-title (_changed-tags data)
  "Rename music file according to its tags.
This function is meant to be inserted into
`ampc-tagger-stored-hook'.  The new file name is
`Artist'_-_`Title'.`extension'.  Characters within `Artist' and
`Title' that are not alphanumeric are substituted with underscore."
  (let* ((artist (replace-regexp-in-string
                  "[^a-zA-Z0-9]" "_"
                  (or (cdr (assq 'Artist (cdr data))) "")))
         (title (replace-regexp-in-string
                 "[^a-zA-Z0-9]" "_"
                 (or (cdr (assq 'Title (cdr data))) "")))
         (new-file
          (expand-file-name (replace-regexp-in-string
                             "_\\(_\\)+"
                             "_"
                             (concat artist
                                     (when (and (> (length artist) 0)
                                                (> (length title) 0))
                                       "_-_")
                                     title
                                     (file-name-extension (car data) t)))
                            (file-name-directory (car data)))))
    (unless (equal (car data) new-file)
      (ampc-tagger-log "Renaming file " (abbreviate-file-name (car data))
                       " to " (abbreviate-file-name new-file) "\n")
      (rename-file (car data) new-file)
      (setf (car data) new-file))))

;;; *** interactives
(defun ampc-tagger-completion-at-point (&optional all-files)
  "Perform completion at point via `completion-at-point'.
If optional prefix argument ALL-FILES is non-nil, use all files
within the files list buffer as source for completion.  The
default behaviour is to use only the selected ones."
  (interactive "P")
  (let ((ampc-tagger-completion-all-files all-files))
    (completion-at-point)))

(defun ampc-tagger-reset (&optional reset-all-tags)
  "Reset all tag values within the tagger, based on the selection of files.
If optional prefix argument RESET-ALL-TAGS is non-nil, restore
all tags."
  (interactive "P")
  (when reset-all-tags
    (ampc-with-buffer 'tagger
      no-se
      (erase-buffer)
      (loop for tag in ampc-tagger-tags
            do (insert (ampc-pad (list (concat (symbol-name tag) ":") "dummy"))
                       "\n"))
      (goto-char (point-min))
      (re-search-forward ":\\( \\)+")))
  (ampc-with-buffer 'tagger
    (loop while (search-forward-regexp
                 (concat "^\\([ \t]*\\)\\("
                         (mapconcat 'symbol-name ampc-tagger-tags "\\|")
                         "\\)\\([ \t]*\\):\\([ \t]*.*\\)$")
                 nil
                 t)
          do (replace-match "" nil nil nil 1)
          (replace-match "" nil nil nil 3)
          (replace-match (concat (make-string (- (car tab-stop-list)
                                                 (1+ (length (match-string 2))))
                                              ?  )
                                 "<keep>")
                         nil nil nil 4)))
  (ampc-tagger-update)
  (ampc-with-buffer 'tagger
    no-se
    (when (looking-at "[ \t]+")
      (goto-char (match-end 0)))))

(defun* ampc-tagger-save (&optional quit &aux tags)
  "Save tags.
If optional prefix argument QUIT is non-nil, quit tagger
afterwards.  If the numeric value of QUIT is 16, quit tagger and
do not trigger a database update"
  (interactive "P")
  (ampc-with-buffer 'tagger
    (loop do (loop until (eobp)
                   while (looking-at "^[ \t]*$")
                   do (forward-line))
          until (eobp)
          do (unless (and (looking-at
                           (concat "^[ \t]*\\("
                                   (mapconcat 'symbol-name
                                              ampc-tagger-tags
                                              "\\|")
                                   "\\)[ \t]*:"
                                   "[ \t]*\\(.*\\)[ \t]*$"))
                          (not (assq (intern (match-string 1)) tags)))
               (error "Malformed line \"%s\""
                      (buffer-substring (line-beginning-position)
                                        (line-end-position))))
          (push (cons (intern (match-string 1))
                      (let ((val (match-string 2)))
                        (if (string= "<keep>" val)
                            t
                          (set-text-properties 0 (length val) nil val)
                          val)))
                tags)
          (forward-line)))
  (callf2 rassq-delete-all t tags)
  (with-temp-buffer
    (loop for (tag . value) in tags
          do (insert (symbol-name tag) "\n"
                     value "\n"))
    (let ((input-buffer (current-buffer)))
      (ampc-with-buffer 'files-list
        no-se
        (let ((reporter
               (make-progress-reporter "Storing tags"
                                       0
                                       (let ((count (count-matches "^\\* ")))
                                         (if (zerop count)
                                             1
                                           count))))
              (step 0))
          (ampc-with-selection nil
            (let* ((data (get-text-property (point) 'data))
                   (old-tags (loop for (tag . data) in (cdr data)
                                   collect (cons tag data)))
                   (found-changed (ampc-tagger-tags-modified (cdr data) tags)))
              (let ((pre-hook-tags (cdr data)))
                (run-hook-with-args 'ampc-tagger-store-hook found-changed data)
                (setf found-changed
                      (or found-changed
                          (ampc-tagger-tags-modified pre-hook-tags
                                                     (cdr data)))))
              (when found-changed
                (ampc-tagger-log
                  "Storing tags for file "
                  (abbreviate-file-name (car data)) "\n"
                  "\tOld tags:\n"
                  (loop for (tag . value) in old-tags
                        concat (concat "\t\t"
                                       (symbol-name tag) ": "
                                       value "\n"))
                  "\tNew tags:\n"
                  (loop for (tag . value) in (cdr data)
                        concat (concat "\t\t"
                                       (symbol-name tag) ": "
                                       value "\n")))
                (ampc-tagger-make-backup (car data))
                (ampc-tagger-report
                 (list "--set" (car data))
                 (with-temp-buffer
                   (insert-buffer-substring input-buffer)
                   (prog1
                       (call-process-region (point-min) (point-max)
                                            ampc-tagger-executable
                                            nil t nil
                                            "--set" (car data))
                     (when ampc-debug
                       (message "ampc-tagger: %s"
                                (buffer-substring
                                 (point-min) (point))))))))
              (run-hook-with-args 'ampc-tagger-stored-hook found-changed data)
              (let ((inhibit-read-only t))
                (move-beginning-of-line nil)
                (forward-char 2)
                (kill-line 1)
                (insert
                 (ampc-pad (loop for p in (plist-get (cdr ampc-type)
                                                     :properties)
                                 when (eq (car p) 'filename)
                                 collect (file-name-nondirectory (car data))
                                 else
                                 collect (cdr (assq (intern (car p))
                                                    (cdr data)))
                                 end))
                 "\n")
                (forward-line -1)
                (put-text-property (line-beginning-position)
                                   (1+ (line-end-position))
                                   'data data))
              (progress-reporter-update reporter (incf step))))
          (progress-reporter-done reporter)))))
  (when quit
    (ampc-tagger-quit (eq (prefix-numeric-value quit) 16))))

(defun ampc-tagger-quit (&optional no-update)
  "Quit tagger and restore previous window configuration.
With optional prefix NO-UPDATE, do not trigger a database update."
  (interactive "P")
  (set-window-configuration (or (car-safe ampc-tagger-previous-configuration)
                                ampc-tagger-previous-configuration))
  (when (car-safe ampc-tagger-previous-configuration)
    (unless no-update
      (ampc-trigger-update))
    (setf ampc-windows (cadr ampc-tagger-previous-configuration)))
  (setf ampc-tagger-previous-configuration nil))

(defun ampc-move-to-tab ()
  "Move point to next logical tab stop."
  (interactive)
  (let ((tab (loop for tab in
                   (or (get-text-property (point) 'tab-stop-list) tab-stop-list)
                   while (>= (current-column) tab)
                   finally return tab)))
    (when tab
      (goto-char (min (+ (line-beginning-position) tab) (line-end-position))))))

(defun ampc-mouse-play-this (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-play-this))

(defun ampc-mouse-delete (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-delete 1))

(defun ampc-mouse-add (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-add-impl))

(defun ampc-mouse-delete-playlist (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-delete-playlist t))

(defun ampc-mouse-load (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-load t))

(defun ampc-mouse-toggle-output-enabled (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-toggle-output-enabled 1))

(defun* ampc-mouse-toggle-mark (event &aux (inhibit-read-only t))
  (interactive "e")
  (let ((window (posn-window (event-end event))))
    (when (with-selected-window window
            (goto-char (posn-point (event-end event)))
            (unless (eobp)
              (move-beginning-of-line nil)
              (ampc-mark-impl (not (eq (char-after) ?*)) 1)
              t))
      (select-window window))))

(defun ampc-mouse-align-point (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-align-point))

(defun* ampc-unmark-all (&aux (inhibit-read-only t))
  "Remove all marks."
  (interactive)
  (assert (ampc-in-ampc-p t))
  (save-excursion
    (goto-char (point-min))
    (loop while (search-forward-regexp "^\\* " nil t)
          do (replace-match "  " nil nil)))
  (ampc-post-mark-change-update))

(defun ampc-trigger-update ()
  "Trigger a database update."
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'update))

(defun* ampc-toggle-marks (&aux (inhibit-read-only t))
  "Toggle marks.
Marked entries become unmarked, and vice versa."
  (interactive)
  (assert (ampc-in-ampc-p t))
  (save-excursion
    (loop for (a . b) in '(("* " . "T ")
                           ("  " . "* ")
                           ("T " . "  "))
          do (goto-char (point-min))
          (loop while (search-forward-regexp (concat "^" (regexp-quote a))
                                             nil
                                             t)
                do (replace-match b nil nil))))
  (ampc-post-mark-change-update))

(defun ampc-up (&optional arg)
  "Move selected entries ARG positions upwards.
ARG defaults to one."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-move (- (or arg 1))))

(defun ampc-down (&optional arg)
  "Move selected entries ARG positions downwards.
ARG defaults to one."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-move (or arg 1)))

(defun ampc-mark (&optional arg)
  "Mark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p t))
  (ampc-mark-impl t arg))

(defun ampc-unmark (&optional arg)
  "Unmark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p t))
  (ampc-mark-impl nil arg))

(defun ampc-set-volume (&optional arg)
  "Set volume to ARG percent.
If ARG is nil, read ARG from minibuffer."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl (or arg (read-number "Volume: "))))

(defun ampc-increase-volume (&optional arg)
  "Increase volume by prefix argument ARG or, if ARG is nil,
`ampc-volume-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl arg '+))

(defun ampc-decrease-volume (&optional arg)
  "Decrease volume by prefix argument ARG or, if ARG is nil,
`ampc-volume-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl arg '-))

(defun ampc-set-crossfade (&optional arg)
  "Set crossfade to ARG seconds.
If ARG is nil, read ARG from minibuffer."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl (or arg (read-number "Crossfade: "))))

(defun ampc-increase-crossfade (&optional arg)
  "Increase crossfade by prefix argument ARG or, if ARG is nil,
`ampc-crossfade-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl arg '+))

(defun ampc-decrease-crossfade (&optional arg)
  "Decrease crossfade by prefix argument ARG or, if ARG is nil,
`ampc-crossfade-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl arg '-))

(defun ampc-toggle-repeat (&optional arg)
  "Toggle MPD's repeat state.
With prefix argument ARG, enable repeating if ARG is positive,
otherwise disable it."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'repeat arg))

(defun ampc-toggle-consume (&optional arg)
  "Toggle MPD's consume state.
With prefix argument ARG, enable consuming if ARG is positive,
otherwise disable it.

When consume is activated, each song played is removed from the playlist."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'consume arg))

(defun ampc-toggle-random (&optional arg)
  "Toggle MPD's random state.
With prefix argument ARG, enable random playing if ARG is positive,
otherwise disable it."
  (interactive "P")
  (ampc-toggle-state 'random arg))

(defun ampc-play-this (&optional arg)
  "Play selected song.
With prefix argument ARG, play the ARG'th song located at the
zero-indexed position of the current playlist."
  (interactive "P")
  (assert (and (ampc-on-p) (or arg (ampc-in-ampc-p))))
  (if (not arg)
      (unless (eobp)
        (ampc-send-command 'play nil (1- (line-number-at-pos)))
        (ampc-send-command 'pause nil 0))
    (ampc-send-command 'play nil arg)
    (ampc-send-command 'pause nil 0)))

(defun* ampc-toggle-play
    (&optional arg &aux (state (cdr (assq 'state ampc-status))))
  "Toggle play state.
If MPD does not play a song already, start playing the song at
point if the current buffer is the playlist buffer, otherwise
start at the beginning of the playlist.

If ARG is 4, stop player rather than pause if applicable."
  (interactive "P")
  (assert (ampc-on-p))
  (unless state
    (return-from ampc-toggle-play))
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ecase (intern state)
    (stop
     (when (or (null arg) (> arg 0))
       (ampc-send-command
        'play
        '(:remove-other (pause))
        (if (and (eq (car ampc-type) 'current-playlist) (not (eobp)))
            (1- (line-number-at-pos))
          0))))
    (pause
     (when (or (null arg) (> arg 0))
       (ampc-send-command 'pause '(:remove-other (play)) 0)))
    (play
     (cond ((or (null arg) (< arg 0))
            (ampc-send-command 'pause '(:remove-other (play)) 1))
           ((eq arg 4)
            (ampc-send-command 'stop))))))

(defun ampc-next (&optional arg)
  "Play next song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (or arg 1)))

(defun ampc-previous (&optional arg)
  "Play previous song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (- (or arg 1))))

(defun ampc-rename-playlist (new-name)
  "Rename selected playlist to NEW-NAME.
If NEW-NAME is nil, read NEW-NAME from the minibuffer."
  (interactive "M")
  (unless new-name
    (setf new-name (read-from-minibuffer (concat "New name for playlist "
                                                 (ampc-playlist)
                                                 ": "))))
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist)
      (ampc-send-command 'rename '(:full-remove t) (ampc-quote new-name))
    (message "No playlist selected")))

(defun ampc-load (&optional at-point)
  "Load selected playlist in the current playlist.
If optional argument AT-POINT is non-nil (or if no playlist is
selected), use playlist at point rather than the selected one."
  (interactive)
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist at-point)
      (ampc-send-command
       'load '(:keep-prev t)
       (ampc-quote (ampc-playlist at-point)))
    (if at-point
        (message "No playlist at point")
      (message "No playlist selected"))))

(defun ampc-toggle-output-enabled (&optional arg)
  "Toggle the next ARG outputs.
If ARG is omitted, use the selected entries."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (let ((data (get-text-property (point) 'data)))
      (ampc-send-command (if (equal (cdr (assoc "outputenabled" data)) "1")
                             'disableoutput
                           'enableoutput)
                         '(:full-remove t)
                         (cdr (assoc "outputid" data))))))

(defun ampc-delete (&optional arg)
  "Delete the next ARG songs from the playlist.
If ARG is omitted, use the selected entries.  If ARG is non-nil,
all marks after point are removed nontheless."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (let ((first-del nil))
    (ampc-with-selection arg
      (unless (or first-del (when arg (< arg 0)))
        (setf first-del (point)))
      (let ((val (1- (- (line-number-at-pos) (if (or (not arg) (> arg 0))
                                                 index
                                               0)))))
        (if (and (not (eq (car ampc-type) 'current-playlist)) (ampc-playlist))
            (ampc-send-command 'playlistdelete
                               '(:keep-prev t)
                               (ampc-quote (ampc-playlist))
                               val)
          (ampc-send-command 'delete '(:keep-prev t) val))
        (ampc-mark-impl nil nil)))
    (when first-del
      (goto-char first-del))))

(defun ampc-shuffle ()
  "Shuffle playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (and (not (eq (car ampc-type) 'current-playlist)) (ampc-playlist))
      (ampc-send-command 'shuffle-listplaylistinfo
                         `(:playlist ,(ampc-playlist))
                         (ampc-quote (ampc-playlist)))
    (ampc-send-command 'shuffle)))

(defun ampc-clear ()
  "Clear playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (and (not (eq (car ampc-type) 'current-playlist)) (ampc-playlist))
      (ampc-send-command 'playlistclear '(:full-remove t)
                         (ampc-quote (ampc-playlist)))
    (ampc-send-command 'clear)))

(defun ampc-add (&optional arg)
  "Add the songs associated with the next ARG entries after point
to the playlist.
If ARG is omitted, use the selected entries in the current buffer."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (ampc-add-impl)))

(defun ampc-status (&optional no-print)
  "Display and return the information that is displayed in the status window.
If optional argument NO-PRINT is non-nil, just return the text.
If NO-PRINT is nil, the display may be delayed if ampc does not
have enough information yet."
  (interactive)
  (assert (ampc-on-p))
  (unless (or ampc-status no-print)
    (ampc-send-command 'status)
    (ampc-send-command 'mini-currentsong)
    (return-from ampc-status))
  (let* ((flags (mapconcat
                 'identity
                 (loop for (f . n) in '((repeat . "Repeat")
                                        (random . "Random")
                                        (consume . "Consume"))
                       when (equal (cdr (assq f ampc-status)) "1")
                       collect n
                       end)
                 "|"))
         (state (cdr (assq 'state ampc-status)))
         (status (concat "State:     " state
                         (when (and ampc-yield no-print)
                           (concat (make-string (- 10 (length state)) ? )
                                   (nth (% ampc-yield 4) '("|" "/" "-" "\\"))))
                         "\n"
                         (when (equal state "play")
                           (concat "Playing:   "
                                   (ampc-clean-tag
                                    'Artist
                                    (cdr (assq 'Artist ampc-status)))
                                   " - "
                                   (ampc-clean-tag
                                    'Title
                                    (cdr (assq 'Title ampc-status)))
                                   "\n"))
                         "Volume:    " (cdr (assq 'volume ampc-status)) "\n"
                         "Crossfade: " (cdr (assq 'xfade ampc-status))
                         (unless (equal flags "")
                           (concat "\n" flags)))))
    (unless no-print
      (message "%s" status))
    status))

(defun ampc-delete-playlist (&optional at-point)
  "Delete selected playlist.
If optional argument AT-POINT is non-nil (or if no playlist is
selected), use playlist at point rather than the selected one."
  (interactive)
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist at-point)
      (when (y-or-n-p (concat "Delete playlist " (ampc-playlist at-point) "?"))
        (ampc-send-command 'rm '(:full-remove t)
                           (ampc-quote (ampc-playlist at-point))))
    (if at-point
        (message "No playlist at point")
      (message "No playlist selected"))))

;;;###autoload
(defun ampc-tagger-dired (&optional arg)
  "Start the tagging subsystem on dired's marked files.
With optional prefix argument ARG, use the next ARG files."
  (interactive "P")
  (assert (derived-mode-p 'dired-mode))
  (ampc-tag-files
   (loop for file in (dired-map-over-marks (dired-get-filename) arg)
         unless (file-directory-p file)
         collect file
         end)))

;;;###autoload
(defun ampc-tag-files (files)
  "Start the tagging subsystem.
FILES should be a list of absolute file names, the files to tag."
  (unless files
    (message "No files specified")
    (return-from ampc-tagger-files t))
  (when (memq (car ampc-type) '(files-list tagger))
    (message "You are already within the tagger")
    (return-from ampc-tagger-files t))
  (let ((reporter (make-progress-reporter "Grabbing tags" 0 (length files))))
    (loop for file in-ref files
          for i from 1
          do (run-hook-with-args 'ampc-tagger-grab-hook file)
          (with-temp-buffer
            (ampc-tagger-call "--get" file)
            (setf file
                  (apply 'list
                         file
                         (loop for tag in ampc-tagger-tags
                               collect
                               (cons tag (or (ampc-extract (ampc-extract-regexp
                                                            (symbol-name tag)))
                                             ""))))))
          (run-hook-with-args 'ampc-tagger-grabbed-hook file)
          (progress-reporter-update reporter i))
    (progress-reporter-done reporter))
  (unless ampc-tagger-previous-configuration
    (setf ampc-tagger-previous-configuration (current-window-configuration)))
  (ampc-configure-frame (cdr (assq 'tagger ampc-views)) t)
  (ampc-with-buffer 'files-list
    (erase-buffer)
    (loop for (file . props) in files
          do (insert (propertize
                      (concat
                       "  "
                       (ampc-pad
                        (loop for p in (plist-get (cdr ampc-type) :properties)
                              when (eq (car p) 'filename)
                              collect (file-name-nondirectory file)
                              else
                              collect (cdr (assq (intern (car p)) props))
                              end))
                       "\n")
                      'data (cons file props))))
    (ampc-set-dirty nil)
    (ampc-toggle-marks))
  (ampc-with-buffer 'tagger
    no-se
    (ampc-tagger-reset t)
    (goto-char (point-min))
    (search-forward-regexp ": *")
    (ampc-set-dirty nil))
  nil)

(defun* ampc-tagger (&optional arg &aux files)
  "Start the tagging subsystem.
The files to tag are collected by using either the selected
entries within the current buffer or the next ARG entries at
point if numeric perfix argument ARG is non-nil, the file
associated with the entry at point, or, if both sources did not
provide any files, the audio file that is currently played by
MPD."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (unless ampc-tagger-version-verified
    (with-temp-buffer
      (ampc-tagger-call "--version")
      (goto-char (point-min))
      (let ((version (buffer-substring (line-beginning-position)
                                       (line-end-position))))
        (unless (equal version ampc-tagger-version)
          (message (concat "The reported version of %s is not supported - "
                           "got \"%s\", want \"%s\"")
                   ampc-tagger-executable
                   version
                   ampc-tagger-version)
          (return-from ampc-tagger))))
    (setf ampc-tagger-version-verified t))
  (unless ampc-tagger-genres
    (with-temp-buffer
      (ampc-tagger-call "--genres")
      (loop while (search-backward-regexp "^\\(.+\\)$" nil t)
            do (push (match-string 1) ampc-tagger-genres))))
  (unless ampc-tagger-music-directories
    (message (concat "ampc-tagger-music-directories is nil.  Fill it via "
                     "M-x customize-variable RET ampc-tagger-music-directories "
                     "RET"))
    (return-from ampc-tagger))
  (case (car ampc-type)
    (current-playlist
     (save-excursion
       (ampc-with-selection arg
         (callf nconc files (list (cdr (assoc "file" (get-text-property
                                                      (line-end-position)
                                                      'data))))))))
    ((playlist tag song)
     (save-excursion
       (ampc-with-selection arg
         (ampc-on-files (lambda (file) (push file files)))))
     (callf nreverse files))
    (t
     (let ((file (cdr (assoc 'file ampc-status))))
       (when file
         (setf files (list file))))))
  (loop for file in-ref files
        for read-file = (locate-file file ampc-tagger-music-directories)
        do (unless read-file
             (error "Cannot locate file %s in ampc-tagger-music-directories"
                    file)
             (return-from ampc-tagger))
        (setf file (expand-file-name read-file)))
  (setf ampc-tagger-previous-configuration
        (list (current-window-configuration) ampc-windows))
  (when (ampc-tag-files files)
    (setf ampc-tagger-previous-configuration nil)))

(defun ampc-store (&optional name-or-append)
  "Store current playlist as NAME-OR-APPEND.
If NAME is non-nil and not a string, append selected entries
within the current playlist buffer to the selected playlist.  If
NAME-OR-APPEND is a negative integer, append the next (-
NAME-OR-APPEND) entries after point within the current playlist
buffer to the selected playlist.  If NAME-OR-APPEND is nil, read
playlist name from the minibuffer."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (unless name-or-append
    (setf name-or-append (read-from-minibuffer "Save playlist as: ")))
  (if (stringp name-or-append)
      (ampc-send-command 'save '(:full-remove t) (ampc-quote name-or-append))
    (if (not (ampc-playlist))
        (message "No playlist selected")
      (ampc-with-buffer 'current-playlist
        (when name-or-append
          (callf prefix-numeric-value name-or-append))
        (ampc-with-selection (if (and name-or-append (< name-or-append 0))
                                 (- name-or-append)
                               nil)
          (ampc-send-command
           'playlistadd
           '(:keep-prev t)
           (ampc-quote (ampc-playlist))
           (ampc-quote (cdr (assoc "file"
                                   (get-text-property (point) 'data))))))))))

(defun* ampc-goto-current-song (&aux (song (cdr (assq 'song ampc-status))))
  "Select the current playlist window and move point to the current song."
  (interactive)
  (assert (ampc-in-ampc-p))
  (let ((window (ampc-with-buffer 'current-playlist
                  (selected-window))))
    (when window
      (select-window window)
      (when song
        (goto-char (point-min))
        (forward-line (string-to-number song)))
      (ampc-align-point))))

(defun ampc-previous-line (&optional arg)
  "Go to previous ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p t))
  (ampc-next-line (* (or arg 1) -1)))

(defun ampc-next-line (&optional arg)
  "Go to next ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p t))
  (forward-line arg)
  (if (eobp)
      (progn (forward-line -1)
             (forward-char 2)
             t)
    (ampc-align-point)
    nil))

(defun* ampc-suspend (&optional (run-hook t))
  "Suspend ampc.
This function resets the window configuration, but does not close
the connection to MPD or destroy the internal cache of ampc.
This means subsequent startups of ampc will be faster."
  (interactive)
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (ampc-restore-window-configuration)
  (loop for b in ampc-all-buffers
        do (when (buffer-live-p b)
             (kill-buffer b)))
  (setf ampc-windows nil
        ampc-all-buffers nil
        ampc-working-timer nil)
  (when run-hook
    (run-hooks 'ampc-suspend-hook)))

(defun ampc-mini ()
  "Select song to play via `completing-read'."
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'mini-playlistinfo))

(defun ampc-quit (&optional arg)
  "Quit ampc.
If prefix argument ARG is non-nil, kill the MPD instance that
ampc is connected to."
  (interactive "P")
  (when (ampc-on-p)
    (set-process-filter ampc-connection nil)
    (when (equal (car-safe ampc-outstanding-commands) '(idle nil))
      (ampc-send-command-impl "noidle")
      (with-current-buffer (process-buffer ampc-connection)
        (loop do (goto-char (point-min))
              until (search-forward-regexp "^\\(ACK\\)\\|\\(OK\\).*\n\\'" nil t)
              while (ampc-on-p)
              do (accept-process-output ampc-connection nil 50))))
    (ampc-send-command-impl (if arg "kill" "close"))
    (delete-process ampc-connection))
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (ampc-suspend nil)
  (setf ampc-connection nil
        ampc-internal-db nil
        ampc-outstanding-commands nil
        ampc-status nil)
  (run-hooks 'ampc-quit-hook))

;;;###autoload
(defun ampc-suspended-p ()
  "Return non-nil if ampc is suspended."
  (interactive)
  (and (ampc-on-p) (not ampc-windows)))

;;;###autoload
(defun ampc-on-p ()
  "Return non-nil if ampc is connected to the daemon."
  (interactive)
  (and ampc-connection (memq (process-status ampc-connection) '(open run))))

;;;###autoload
(defun ampc (&optional host port suspend)
  "Ampc is an asynchronous client for the MPD media player.
This function is the main entry point for ampc.

HOST and PORT specify the MPD instance to connect to.  The values
default to the ones specified in `ampc-default-server'."
  (interactive)
  (unless (byte-code-function-p (symbol-function 'ampc))
    (message "You should byte-compile ampc"))
  (run-hooks 'ampc-before-startup-hook)
  (unless host
    (setf host (or (car ampc-default-server) (read-string  "Host: "))))
  (unless port
    (setf port (or (cdr ampc-default-server) (read-string "Port: "))))
  (when (and ampc-connection
             (not (and (equal host ampc-host)
                       (equal port ampc-port)
                       (ampc-on-p))))
    (ampc-quit))
  (unless ampc-connection
    (let ((connection (open-network-stream "ampc"
                                           (with-current-buffer
                                               (get-buffer-create " *ampc*")
                                             (erase-buffer)
                                             (current-buffer))
                                           host
                                           port
                                           :type 'plain :return-list t)))
      (unless (car connection)
        (error "Failed connecting to server: %s"
               (plist-get ampc-connection :error)))
      (setf ampc-connection (car connection)
            ampc-host host
            ampc-port port))
    (set-process-coding-system ampc-connection 'utf-8-unix 'utf-8-unix)
    (set-process-filter ampc-connection 'ampc-filter)
    (set-process-query-on-exit-flag ampc-connection nil)
    (setf ampc-outstanding-commands '((setup))))
  (if suspend
      (ampc-update)
    (ampc-configure-frame (cddadr ampc-views)))
  (run-hooks 'ampc-connected-hook)
  (when suspend
    (ampc-suspend))
  (ampc-filter (process-buffer ampc-connection) nil))

;;;; ChangeLog:

;; 2012-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/ampc/ampc.el: Add proper file trailer.
;; 
;; 2012-08-06  Christopher Schmidt  <christopher@ch.ristopher.com>
;; 
;; 	* ampc: Sync to version 0.2.
;; 
;; 2012-06-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* ampc.el: Sync to version 0.1.3.
;; 


(provide 'ampc)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;; \\*+"
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
;;; ampc.el ends here
