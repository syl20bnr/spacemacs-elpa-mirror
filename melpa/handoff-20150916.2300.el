;;; handoff.el --- Get your hand off that mouse, damn it!

;; Copyright (C) 2015 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20150916.2300
;; URL: http://github.com/rejeep/handoff.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar handoff-zone-programs
  [zone-pgm-jitter
   zone-pgm-putz-with-case
   zone-pgm-dissolve
   zone-pgm-whack-chars
   zone-pgm-rotate
   zone-pgm-rotate-LR-lockstep
   zone-pgm-rotate-RL-lockstep
   zone-pgm-rotate-LR-variable
   zone-pgm-rotate-RL-variable
   zone-pgm-rat-race
   zone-pgm-paragraph-spaz
   zone-pgm-random-life]
  "List of zone programs to randomly use on handoff.")

(defvar handoff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-1>") 'handoff!)
    (define-key map (kbd "<down-mouse-2>") 'handoff!)
    (define-key map (kbd "<down-mouse-3>") 'handoff!)
    (define-key map (kbd "<wheel-up>") 'handoff!)
    (define-key map (kbd "<wheel-down>") 'handoff!)
    map)
  "Keymap for `handoff-mode'.")

(defun handoff! ()
  "Get your hand off that mouse, damn it!"
  (interactive)
  (let ((zone-programs handoff-zone-programs))
    (call-interactively 'zone)))

;;;###autoload
(define-minor-mode handoff-mode
  "Get your hand off that mouse, damn it!"
  :init-value nil
  :keymap handoff-mode-map)

;;;###autoload
(defun turn-on-handoff-mode ()
  "Turn on `handoff-mode'."
  (interactive)
  (handoff-mode +1))

;;;###autoload
(defun turn-off-handoff-mode ()
  "Turn off `handoff-mode'."
  (interactive)
  (handoff-mode -1))

;;;###autoload
(define-globalized-minor-mode handoff-global-mode
  handoff-mode
  turn-on-handoff-mode)

(provide 'handoff)

;;; handoff.el ends here
