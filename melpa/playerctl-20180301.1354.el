;;; playerctl.el --- Control your music player (e.g. Spotify) with playerctl
;;
;; Author: Thomas Luquet <thomas@luquet.net>
;; Keywords: multimedia, playerctl, music
;; Package-Version: 20180301.1354
;; URL: https://github.com/thomasluquet/playerctl.el
;; Version: 0.0.2
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;
;;; Commentary:
;; Play music with Spotify, vlc, audacious, bmp, xmms2, and others
;; music player with playerctl: <https://github.com/acrisci/playerctl>
;;
;;; Usage:
;; (require 'playerctl) ; unless installed from a package
;; (define-key global-map (kbd "C-c C-SPC") 'playerctl-play-pause-song)
;; (define-key global-map (kbd "C-c C-n") 'playerctl-next-song)
;;
;;; Code:

(defun playerctl--command (cmd msg)
  (let ((proc (start-process "playerctl.el" "foo" "playerctl" cmd)))
    (if (equal cmd "status")
        (set-process-filter proc (lambda
                                  (proc line)
                                  (message "Status : %s" line)))
      (message msg)
        )
    )
  )

;;;###autoload
(defun playerctl-play-pause-song()
  "Play or pause the song."
  (interactive)
  (playerctl--command "play-pause" "Play or pause the current song"))

;;;###autoload
(defun playerctl-next-song()
  "Change the song to the next one."
  (interactive)
  (playerctl--command "next" "Next song"))

;;;###autoload
(defun playerctl-previous-song()
  "Change the song to the last one."
  (interactive)
  (playerctl--command "previous" "Previous song"))

;;;###autoload
(defun playerctl-stop-song()
  "Stop song."
  (interactive)
  (playerctl--command "stop" "Stop music"))

;;;###autoload
(defun playerctl-status()
  "Get status of playerctl."
  (interactive)
  (playerctl--command "status" "status"))



(provide 'playerctl)
;;; playerctl.el ends here
