;;; zone-rainbow.el --- Zone out with rainbow.

;; Filename: zone-rainbow.el
;; Description: Zone out with rainbow
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2015-12-20
;; Version: 1.160120
;; Package-Version: 20160120.534
;; Package-Requires: ((emacs "24.3"))
;; Keywords: games
;; URL: https://github.com/kawabata/zone-rainbow

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

;; Zone out with rainbow.  This code is inspired by
;; https://gist.github.com/mrkuc/7121179.
;;
;; It can be directly invoked by `M-x zone-rainbow'.
;;
;; Or, it can be added to zone programs by using
;; `zone-select-add-program' of `zone-select' package.

;;; Code:

(require 'zone)
(require 'color)
(require 'cl-lib)

(defgroup zone-rainbow nil
  "Zone out with rainbow."
  :group 'games)

(defcustom zone-rainbow-hue-factor 50 "Hue factor." :group 'zone-rainbow)
(defcustom zone-rainbow-sat 1.0 "Saturation." :group 'zone-rainbow)
(defcustom zone-rainbow-light 0.5 "Light." :group 'zone-rainbow)
(defcustom zone-rainbow-background nil "If not nil, Background color." :group 'zone-rainbow)

;;;###autoload
(defun zone-pgm-rainbow ()
  "Zone out with rainbow."
  (cl-loop
   while (not (input-pending-p))
   with k = 0 do
   (cl-loop
    for i from (window-start) to (1- (window-end)) do
    (add-text-properties
     i (1+ i)
     `(face ((foreground-color
              . ,(apply 'color-rgb-to-hex
                        (color-hsl-to-rgb
                         (/ (* (% (+ i k) zone-rainbow-hue-factor) 1.0)
                            zone-rainbow-hue-factor)
                         zone-rainbow-sat zone-rainbow-light)))
             ,@(when zone-rainbow-background
                 `((background-color
                   . ,zone-rainbow-background)))))))
   (sit-for 0.1)
   (cl-incf k)))

;;;###autoload
(defun zone-rainbow ()
  "Zone out with rainbow."
  (interactive)
  (let ((zone-programs [zone-pgm-rainbow]))
    (zone)))

(provide 'zone-rainbow)

;;; zone-rainbow.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
