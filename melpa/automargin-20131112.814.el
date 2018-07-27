;;; automargin.el --- add margins to windows not-splitted, and center them

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Version: 20131112.814
;; Version: 0.0.2

;;; Commentary:

;; Require this script and enable automargin-mode.
;;
;;   (when (require 'automargin nil t)
;;     (automargin-mode 1))
;;
;; then, if the frame is not splitted horizontally, margins are set automatically.

;;; Change Log:

;; 0.0.1 test release
;; 0.0.2 added minor-mode

;;; Code:

;; * constants

(defconst automargin-version "0.0.2")

;; * custom

(defgroup automargin nil
  "automatically add margins to windows"
  :group 'emacs)

(defcustom automargin-target-width 128
  "width of the margined window"
  :group 'automargin)

;; * main

(define-minor-mode automargin-mode
  "automatically add margins to windows"
  :init-value nil
  :global t
  (if automargin-mode
      (add-hook 'window-configuration-change-hook 'automargin-function)
    (remove-hook 'window-configuration-change-hook 'automargin-function)))

(defun automargin--window-width (&optional window)
  (let ((margins (window-margins window))
        (width (window-width window)))
    (+ width
       (or (car margins) 0)
       (or (cdr margins) 0))))

(defun automargin-function ()
  (let* ((automargin-margin
          (/ (- (frame-width) automargin-target-width) 2))
         (automargin-margin
          (if (< automargin-margin 0) 0 automargin-margin)))
    (dolist (window (window-list))
      (let ((margin (if (= (frame-width) (automargin--window-width window))
                        automargin-margin 0)))
        (set-window-margins window margin margin)))))

;; * provide

(provide 'automargin)

;;; automargin.el ends here
