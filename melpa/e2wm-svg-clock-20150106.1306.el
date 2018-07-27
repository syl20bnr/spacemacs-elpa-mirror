;;; e2wm-svg-clock.el --- e2wm plugin for svg-clock

;; Description: e2wm plugin for svg-clock
;; Author: Yuhei Maeda <yuhei.maeda_at_gmail.com>
;; Maintainer: Yuhei Maeda
;; Copyright (C) 2012 myuhe all rights reserved.
;; Created: :2013-03-02
;; Version: 0.0.1
;; Package-Version: 20150106.1306
;; Keywords: convenience, e2wm
;; URL: https://github.com/myuhe/e2wm-svg-clock.el
;; Package-Requires: ((e2wm "20130225.1602")  (svg-clock "0.4"))

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
;;
;; e2wm plugin for svg-clock.

;;; Code:

(require 'e2wm)
(require 'svg-clock)

(defun e2wm:def-plugin-svg-clock (frame wm winfo) 
  (let* ((buf (get-buffer-create "*clock*")))
    (with-current-buffer buf
      (unless svg-clock-timer
        (setq svg-clock-timer
              (run-with-timer 0 1 'svg-clock-update))
        (svg-clock-mode)))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))

(e2wm:plugin-register 'svg-clock
                     "SVG-clock"
                     'e2wm:def-plugin-svg-clock)

(defun e2wm:dp-code-toggle-svg-clock-command ()
  (interactive)
  (let* ((wm (e2wm:pst-get-wm))
         (prev (e2wm:pst-window-plugin-get wm 'history))
         (next (if (eq prev 'history-list)
                   'svg-clock 'history-list)))
    (e2wm:pst-window-plugin-set wm 'history next)
    (e2wm:pst-update-windows)))

(provide 'e2wm-svg-clock)
;;; e2wm-svg-clock.el ends here
