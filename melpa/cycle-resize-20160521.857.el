;;; cycle-resize.el --- Cycle resize the current window horizontally or vertically

;; Copyright (C) 2015 Pierre Lecocq

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

;; Author: Pierre Lecocq
;; URL: https://github.com/pierre-lecocq/cycle-resize
;; Package-Version: 20160521.857
;; Version: 1.0 (stable)

;;; Commentary:

;; Load this package
;;
;; (require 'cycle-resize)
;;
;; You can then call these two methods, once you have at least 2 windows:
;;
;; M-x cycle-resize-window-vertically
;; M-x cycle-resize-window-horizontally
;;
;; and eventually bind some keys like:
;;
;; (global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
;; (global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)
;;
;; You also can configure the dimensions (in %) the package will cycle through
;; By default, it is: 80% -> 50% -> 20% -> 50%, and so on...
;;
;; (setq cycle-resize-steps '(80 50 20 50))

;;; Change Log:
;;
;; 2015-06-02
;;    * Better code in order to avoid compilation errors
;;
;; 2015-02-08
;;    * Coding convention fixes thanks to Steve Purcell
;;    * Melpa package is now available
;;
;; 2015-02-04
;;    * First public release, beta version

;;; Code:

(defvar cycle-resize-steps '(80 50 20 50)
  "The steps used to resize the current frame.")

(defun cycle-resize--first-element-at-the-end (alist)
  "Take the first element of ALIST and place it at the end."
  (append (cdr alist) (list (car alist))))

(defun cycle-resize--calculate-window-size (percentage direction)
  "Calculate the PERCENTAGE window size according to the frame size and DIRECTION."
  (if (string= direction "vertical")
      (* (frame-height) (/ percentage 100.0))
    (* (frame-width) (/ percentage 100.0))))

(defun cycle-resize--calculate-window-delta (new-size direction)
  "Calculate the window delta according to the window NEW-SIZE and DIRECTION."
  (if (string= direction "vertical")
      (truncate (- new-size (window-body-height)))
    (truncate (- new-size (window-body-width)))))

(defun cycle-resize--cycle-resize-window (direction)
  "Cycle resize the current window according to the DIRECTION."
  (let* ((new-size (cycle-resize--calculate-window-size (car cycle-resize-steps) direction))
         (delta (cycle-resize--calculate-window-delta new-size direction)))
    (if (>= (length (window-list)) 2)
        (progn
          (if (string= direction "vertical")
              (enlarge-window delta)
            (enlarge-window-horizontally delta))
          (setq cycle-resize-steps (cycle-resize--first-element-at-the-end cycle-resize-steps)))
      (message "Not enough windows to cycle resize"))))

;;;###autoload
(defun cycle-resize-window-vertically ()
  "Cycle resize vertically the current window."
  (interactive)
  (cycle-resize--cycle-resize-window "vertical"))

;;;###autoload
(defun cycle-resize-window-horizontally ()
  "Cycle resize horizontally the current window."
  (interactive)
  (cycle-resize--cycle-resize-window "horizontal"))

(provide 'cycle-resize)

;;; cycle-resize.el ends here
