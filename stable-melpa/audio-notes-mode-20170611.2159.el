;;; audio-notes-mode.el --- Play audio notes synced from somewhere else.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/audio-notes-mode
;; Package-Version: 20170611.2159
;; Version: 1.1.1
;; Keywords: hypermedia convenience
;; ShortName: anm
;; Separator: /

;;; Commentary:
;;
;; `audio-notes-mode' is a way to manage small audio recordings that
;; you make in order to record thoughts.
;;
;; After much struggle, I finally decided to stop trying to make
;; speech recognition work from my phone. Instead, I decided to just
;; record audio notes, and I wrote this package to automate the
;; process of playing them back at the computer.
;;
;; I found this to be even faster, because I don't have to wait and
;; see if the speech recognition worked and I don't have to repeat the
;; message 1/3 of the time.
;;
;; A tasker profile (which records and uploads these notes) is also
;; provided at the github page.
;;
;; The idea is that you sync voice notes you record on your
;; smartphone into a directory on your PC. But you're free to use it
;; in other ways.
;;
;; When you activate this mode, it will play the first audio note in a
;; specific directory and wait for you to write it down. Once you're
;; finished, just call the next note with \\[anm/play-next]. When you
;; do this, `audio-notes-mode' will DELETE the note which was already
;; played and start playing the next one. Once you've gone through all
;; of them, `audio-notes-mode' deactivates itself.

;;; Instructions:
;;
;; INSTALLATION
;;
;; Configuration is simple. Require the package and define the following two variables:
;;           (require 'audio-notes-mode)
;;           (setq anm/notes-directory "~/Directory/where/your/notes/are/")
;;           (setq anm/goto-file "~/path/to/file.org") ;File in which you'll write your notes as they are played.
;;
;; Then just choose how you want to activate it.
;; 1) If you use `org-mobile-pull', you can do
;;       (setq anm/hook-into-org-pull t)
;;    and `audio-notes-mode' will activate whenever you call
;;    org-mobile-pull.
;;    
;; 2) The second options is to just bind `audio-notes-mode' to
;;    some key and call it when you want.
;;       (global-set-key [f8] 'audio-notes-mode)
;;
;; If you installed manually, first require the feature with:
;; then use one of the methods above.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 1.1.1 - 20130909 - Fix 'internal bug
;; 1.1 - 20130817 - Fix -is-mplayer
;; 1.0 - 20130721 - Implemented mplayer controls from https://github.com/markhepburn/mplayer-mode.
;; 0.7 - 20130714 - anm/delete-command.
;; 0.7 - 20130714 - No modeline display if no notes present.
;; 0.7 - 20130714 - Changed default directory.
;; 0.7 - 20130714 - Don't activate after org-pull if there are no notes.
;; 0.6 - 20130712 - bugfix.
;; 0.5 - 20130712 - Added a default player..
;; 0.5 - 20130712 - Full package functionality implemented.
;; 0.1 - 20130710 - Created File.

;;; Code:

(defconst anm/version "1.1.1" "Version of the audio-notes-mode.el package.")
(defconst anm/version-int 7 "Version of the audio-notes-mode.el package, as an integer.")
(defun anm/bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and anm versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/audio-notes-mode/issues/new")
  (message "Your anm/version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           anm/version emacs-version))

(defun anm/customize ()
  "Open the customization menu in the `audio-notes-mode' group."
  (interactive)
  (customize-group 'audio-notes-mode t))


(defcustom anm/display-greeting t
  "Whether we explain the keybindings upon starting the mode."
  :type 'boolean
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defcustom anm/notes-directory (concat
                                (if (boundp 'org-directory)
                                    org-directory
                                  "~/Dropbox/") "AudioNotes/")
  "Directory where recorded notes are stored."
  :type 'string
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.7"))

(defcustom anm/goto-file nil
  "File to visit when `audio-notes-mode' is entered. This should be your TODO-list file.

If nil, nothing will be visited.
If a string, it is the path to the file which will be visited
when you activate `audio-notes-mode'."
  :type '(choice string nil)
  :group 'audio-notes-mode)

(defcustom anm/file-regexp "^[^\\.].*\.\\(mp[34]\\|wav\\|3ga\\|3gpp\\|m4a\\)$"
     "Regexp which filenames must match to be managed by OAN.

Default is to play only mp4, mp3, wav and 3ga, and to exclude hidden files."
     :type 'regexp
     :group 'audio-notes-mode)

(defcustom anm/lighter (if (char-displayable-p ?▶) " ▶" " anm")
  "Ligher for the mode-line."
  :type 'string
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defcustom anm/hook-into-org-pull nil
  "Whether we should interact with `org-mobile-pull'.

If this is non-nil, `audio-notes-mode' will be called every
time (after) you do an `org-mobile-pull' IF there are any audio
notes in `anm/notes-directory'."
  :type '(choice (const :tag "Always, activate on org-pull." t)
                 ;; (const :tag "Activate on org-pull, only if is there's an entry \"* AUDIO\" on your \"from-mobile.org\" file." 'sometimes)
                 (const :tag "Don't activate on org-pull." nil))
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defcustom anm/after-play-hook '()
  "Hooks run every time a note is played (immediately after playing it)."
  :type 'hook
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defcustom anm/before-play-hook '()
  "Hooks run every time a note is played (immediately before playing it).

I personally recommend setting it to something like:
 (lambda () (goto-char (point-max))
      (insert \"\\n\\n* \"))"
  :type 'hook
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defcustom anm/process-buffer-name "*Audio notes player*"
  "Name of the process buffer."
  :type 'string
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defvar anm/default-mplayer '("mplayer" "-quiet" file) "Default value for `anm/player-command'.")
(defvar anm/default-vlc '("vlc" file) "Default value for `anm/player-command'.")
(defvar anm/player-command-documentation
  "Which media player to use for the audio files, must be a symbol or a list.

If it's the symbol 'internal: uses emacs' internal player.

If it's a list: the first element is the executable name (like
\"mplayer\") and all following elements are arguments to be
passed to it. All arguments must either be strings or the symbol
'file, which will be replaced by the filename (you probably
should include 'file at least once). For example, the default
value (if you have mplayer installed) is

    %S

Emacs internal player should be able to play wav files, but not
mp4, so your decision on which to use should be based on this." "")

(defcustom anm/player-command (cond
                               ((executable-find "mplayer") anm/default-mplayer)
                               ((executable-find "smplayer") anm/default-smplayer)
                               ((executable-find "vlc") anm/default-vlc)
                               (t 'internal))
  (format anm/player-command-documentation anm/default-mplayer)
  :type '(choice (const :tag "Emacs internal player" internal)
                 (cons (string :tag "Executable name")
                       (repeat (choice (const :tag "File Name" file)
                                       (string :tag "Other Arguments")))))
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.1"))

(defvar anm/dired-buffer     nil "The buffer displaying the notes.")
(defvar anm/goto-file-buffer nil "The buffer the user asked to open.")
(defvar anm/process-buffer   nil "Process buffer.")
(defvar anm/process          nil "Process.")
(defvar anm/mode-line-color  "ForestGreen" "")
(defvar anm/current          nil "Currently played file.")
(defvar anm/did-visit        nil "Did we visit a file and mess up the configuration.")
(defvar anm/found-files      nil "")

(defconst anm/greeting
  "You're in `audio-notes-mode'. This mode will deactivate after you go through your notes, to quit manually use \\[audio-notes-mode].
\\[anm/play-next]: DELETES this audio note and moves to the next one.
\\[anm/play-current]: Replays this audio note.
To disable this message, edit `anm/display-greeting'."
  "Greeting message when entering mode.")

;;; Code borrowed from markhepburn's mplayer-mode at https://github.com/markhepburn/mplayer-mode
(defcustom anm/default-seek-step 5
  "The number of seconds that the skip command will use."
  :type 'integer
  :group 'audio-notes-mode
  :version '(audio-notes-mode . "1.0"))

;;; Utilities:
(defun anm/-mplayer-send (cmd)
  (if (anm/-is-mplayer-p)
      (if (anm/-is-alive-p)
          (process-send-string anm/process (concat cmd "\n"))
        (message "There's nothing playing!"))
    (message "Not using mplayer!")))

(defun anm/-mplayer-parse-seconds (seconds)
  (cond
   ((null seconds) anm/default-seek-step)
   ((numberp seconds) seconds)
   ((listp seconds)
    (* anm/default-seek-step (1+ (log (abs (car seconds)) 4))))))

;;; Interactive Commands:
(defun anm/mplayer-seek-forward (N)
  "Skip forward in the recording by `anm/default-seek-step' seconds.

With numeric prefix N, skip that many times the default step.
With raw prefix N, skip that many times +1."
  (interactive "P")
  (let ((seconds (anm/-mplayer-parse-seconds N)))
    (anm/-mplayer-send (format "seek %d 0" seconds))))
(defun anm/mplayer-seek-backward (N)
  "Skip backward in the recording by `anm/default-seek-step' seconds.

With numeric prefix N, skip that many times the default step.
With raw prefix N, skip that many times +1."
  (interactive "P")
  (anm/mplayer-seek-forward (- (anm/-mplayer-parse-seconds N))))

;;; End of borrowed functions

(defun anm/-is-mplayer-p ()
  "Checks if process is alive and if we're using mplayer. "
  (and (listp anm/player-command) (string= (car anm/player-command) "mplayer")))

(defun anm/-is-alive-p ()
  (and anm/process (eq (process-status anm/process) 'run)))

;;;###autoload
(defun anm/display-on-modeline (t-or-nil-or-color)
  "Interactive: toggle displaying number of audio notes on the modeline.
Noninteractive: deactivate with a nil argument, activate otherwise.

If T-OR-NIL-OR-COLOR is a string, also sets it as the color to
use for displaying (default is ForestGreen)."
  (interactive "i")
  ;; If interactive, just toggle display.
  (if (called-interactively-p 'any)
      (if (member '(:eval (anm/global-mode-string)) global-mode-string)
          (setq global-mode-string (delete '(:eval (anm/global-mode-string)) global-mode-string))
        (add-to-list 'global-mode-string '(:eval (anm/global-mode-string))))
    ;; Else, set according to argument
    (if (not t-or-nil-or-color)
        (setq global-mode-string (delete '(:eval (anm/global-mode-string)) global-mode-string))
      (add-to-list 'global-mode-string '(:eval (anm/global-mode-string)))
      (when (stringp t-or-nil-or-color)
        (setq anm/mode-line-color t-or-nil-or-color)))))

;;;###autoload
(defadvice org-mobile-pull (after anm/after-org-mobile-pull-advice activate)
  "Check for audio notes after every org-pull."
  (when (and anm/hook-into-org-pull
             (anm/list-files))
             ;; ;;This code has been aborted. We only activate if there are notes anyway.
             ;; (or (not (equal anm/hook-into-org-pull 'sometimes))
             ;;     (if org-mobile-inbox-for-pull
             ;;      (with-temp-file org-mobile-inbox-for-pull
             ;;        (insert-file-contents org-mobile-inbox-for-pull)
             ;;        (goto-char (point-min))
             ;;        (when (search-forward-regexp "^\\* +AUDIO *$" nil t)
             ;;          (replace-match "") t))
             ;;      (error "You set `anm/hook-into-org-pull' to %S, but your `org-mobile-inbox-for-pull' variable isn't set!"
             ;;             anm/hook-into-org-pull)))
    (audio-notes-mode 1)))

(defun anm/global-mode-string ()
  "ANM string for displaying on the mode-line."
  (let ((l (anm/list-files)))
    (when l
      (propertize (format "%s Notes" (length l))
                  'face `(:foreground ,anm/mode-line-color)))))

(defcustom anm/delete-command '(delete-file file t)
  "Command which will be used to delete a file.

It is evaluated with (eval anm/delete-command), where 'file will
be the (full) path of the file to be deleted."
  :type 'sexp
  :group 'audio-notes-mode
  :package-version '(audio-notes-mode . "0.7"))

(defun anm/play-next ()
  "Play next audio note. If no more notes, exit `audio-notes-mode'."
  (interactive)
  ;; Delete previously played note
  (if (file-readable-p anm/current)
      (if (file-writable-p anm/current)
          (let ((file anm/current))
            (eval anm/delete-command)
            (setq anm/current nil))
        (audio-notes-mode -1)
        (error "File %s can't be deleted.\nCheck file permissions and fix this.\n(Exiting)" anm/current))
    (warn "File %s not found for deletion." anm/current))
  ;; Play the next one. If there isn't one, just exit play-notes-mode.
  (anm/play-current))

(defalias 'anm/play-current 'anm/play-pause-current)
(defun anm/play-pause-current ()
  "Play current audio note.

If called while a note is already playing, AND if
`anm/player-command' is an external command (i.e. it's value is
not 'internal), then this function pauses the playing audio."
  (interactive)
  (if (and anm/current (anm/-is-alive-p)) 
      (if (anm/-is-mplayer-p)
          (anm/-mplayer-send "pause")
        (anm/stop))
    (let* ((files (anm/list-files))
           (file (or anm/current (car files)))
           (sn (if file (file-name-nondirectory file) "")))
      (if file
          (progn
            (if anm/current
                (message "Replaying %s" sn)
              (setq anm/current file)
              (message "%s notes left. Playing %s" (length files) sn))
            (with-current-buffer anm/dired-buffer
              (goto-char (point-min))
              (search-forward sn)
              (revert-buffer))
            (with-current-buffer anm/process-buffer (erase-buffer))
            (run-hooks 'anm/before-play-hook)
            (anm/play-file file)
            (run-hooks 'anm/after-play-hook))
        (message "No more notes. Exiting `audio-notes-mode'.")
        (audio-notes-mode -1)))))

(defun anm/stop ()
  "Stop current note (by killing the player)."
  (interactive)
  (if (anm/-is-alive-p)
      (kill-process anm/process)
    (message "There's nothing playing!")))

(defun anm/play-file (file)
  "Play sound file.

Also kills the process before starting a new one."
  (unless (file-readable-p file) (audio-notes-mode -1) (error "FILE isn't a file!"))
  (cond
   ((eq anm/player-command 'internal) 
    (condition-case data
        (play-sound-file (expand-file-name file)) 
      (error
       (audio-notes-mode -1)
       (if (equal (cdr data) '("Unknown sound format"))
           (error "Oops! Emacs internal player, can't play the format of the file %s.
Change `anm/player' to a command name (like \"mplayer\")." file)
         (error (cdr data))))))
   ((listp anm/player-command)
    (when (and anm/process (eq (process-status anm/process) 'run))
      (kill-process anm/process))
    (setq anm/process (eval (concatenate 'list
                                         '(start-process "anm/player-command" anm/process-buffer)
                                         (map 'list 'eval anm/player-command))))
    (set-process-query-on-exit-flag anm/process nil))
   (t (error "`anm/player-command' invalid: %s" anm/player-command))))

(defun anm/list-files ()
  "List all non-hidden files in `anm/notes-directory'."
  (directory-files anm/notes-directory t anm/file-regexp))

;;;###autoload
(define-minor-mode audio-notes-mode
  "`audio-notes-mode' is a way to manage small audio recordings that you make in order to record thoughts.

When you activate it, it will play the first audio note in a
specific directory and wait for you to write it down. Once you're
finished, just call the next note with C-c C-j.
When you do this, `audio-notes-mode' will DELETE the note which
was already played and start playing the next one. Once you've
gone through all of them, `audio-notes-mode' deactivates itself.

\\{audio-notes-mode-map}"
  nil anm/lighter
  '(("\n" . anm/play-next)
    ("" . anm/play-pause-current)
    ("" . anm/play-next)
    ("" . anm/play-pause-current)
    ("" . anm/stop)
    ("" . audio-notes-mode))
  :global t
  :group 'audio-notes-mode
  (if audio-notes-mode
      ;; ON
      (if anm/player-command
          (let ((file (car (anm/list-files))))
            (if (not file)
                (audio-notes-mode -1)
              (setq anm/found-files t)
              (when anm/display-greeting (message (substitute-command-keys anm/greeting)))
              (window-configuration-to-register :anm/before-anm-configuration)
              (delete-other-windows)
              (when anm/goto-file
                (setq anm/did-visit t)
                (setq anm/goto-file-buffer (find-file anm/goto-file)))
              (let ((focusWin (selected-window))
                    diredSize)
                ;; Created dired window
                (select-window (split-window-right))
                (setq anm/dired-buffer (find-file anm/notes-directory))
                (when (fboundp 'hl-line-mode) (hl-line-mode 1))
                (revert-buffer)
                (goto-char (point-min))
                (search-forward (file-name-nondirectory file))
                ;; Create process window
                (when (listp anm/player-command)
                  (setq diredSize (line-number-at-pos (point-max)))
                  (select-window (split-window-below (1- diredSize)))
                  (setq anm/process-buffer
                        (switch-to-buffer
                         (generate-new-buffer anm/process-buffer-name))))
                ;; Back to writing window
                (select-window focusWin))
              (anm/play-current)))
        ;; If anm/player-command was nil
        (audio-notes-mode -1)
        (error "`anm/player-command' can't be nil."))
    ;; OFF
    (setq anm/current nil)
    (when (buffer-live-p anm/process-buffer)
      (kill-buffer anm/process-buffer))
    (if (not anm/found-files)
        (message "[OAN]:No audio notes found in \"%s\"." anm/notes-directory)
      (setq anm/found-files nil)
      (jump-to-register :anm/before-anm-configuration)
      (when anm/did-visit
        (setq anm/did-visit nil)
        (bury-buffer anm/goto-file-buffer))
      (when (get-buffer-window anm/dired-buffer)
        (condition-case nil        ;Don't bug me if it's the only window
            (delete-window (get-buffer-window anm/dired-buffer))
          (error nil)))
      (bury-buffer anm/dired-buffer)))
  (when (anm/-is-mplayer-p)
    (define-key audio-notes-mode-map "" 'anm/mplayer-seek-forward)
    (define-key audio-notes-mode-map "" 'anm/mplayer-seek-backward)))

(provide 'audio-notes-mode)
;;; audio-notes-mode.el ends here.
