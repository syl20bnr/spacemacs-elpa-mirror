;;; ql.el --- Control Quod Libet                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: multimedia
;; Package-Version: 20180418.2020
;; URL: https://github.com/ieure/ql-el
;; Package-Requires: ((emacs "24"))
;; Version: 1.1

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

;; Interface to Quod Libet

;;; Code:

(require 'dbus)

(defconst ql-service "net.sacredchao.QuodLibet")
(defconst ql-path "/net/sacredchao/QuodLibet")
(defconst ql-interface "net.sacredchao.QuodLibet")

(defun ql-call* (method &rest args)
  "Helper function to invoke QL function METHOD via D-Bus, with ARGS."
  (apply #'dbus-call-method :session ql-service ql-path ql-interface
         method args))

(defun ql-status ()
  "Display the status of Quod Libet in the echo area."
  (let ((song (ql-call* "CurrentSong")))
    (if (ql-playingp*)
        (message (format "Playing: %s - %s"
                         (cadr (assoc "artist" song))
                         (cadr (assoc "title" song))))
      (message "Paused"))))

(defun ql-playingp* ()
  "Return t if Quod Libet is playing a song, nil otherwise."
  (ql-call* "IsPlaying"))

;;;###autoload
(defun ql-play-pause ()
  "Toggle playing status."
  (interactive)
  (ql-call* "PlayPause")
  (when (ql-playingp*)
    (ql-status)))

;;;###autoload
(defun ql-pause ()
  "Pause the current song."
  (interactive)
  (ql-call* "Pause"))

;;;###autoload
(defun ql-play ()
  "Begin playing."
  (interactive)
  (ql-call* "Play")
  (ql-status))

;;;###autoload
(defun ql-next ()
  "Skip to the next track."
  (interactive)
  (ql-call* "Next")
  (ql-status))

;;;###autoload
(defun ql-previous ()
  "Skip backwards to the previous track."
  (interactive)
  (ql-call* "Previous")
  (ql-status))

;;;###autoload
(define-minor-mode ql-minor-mode
  "Minor mode for controlling Quod Libet"
  t nil (make-sparse-keymap)
  :global t
  :require 'ql)

(define-key ql-minor-mode-map (kbd "<XF86AudioPlay>") #'ql-play-pause)
(define-key ql-minor-mode-map (kbd "<XF86AudioNext>") #'ql-next)
(define-key ql-minor-mode-map (kbd "<XF86AudioPrev>") #'ql-previous)

(provide 'ql)
;;; ql.el ends here
