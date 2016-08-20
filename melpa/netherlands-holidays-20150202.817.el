;;; netherlands-holidays.el --- Netherlands holidays for Emacs calendar.

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/netherlands-holidays
;; Package-Version: 20150202.817
;; Version: 1.0.0
;; Keywords: calendar

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Replace holidays:
;;
;; (setq calendar-holidays holiday-netherlands-holidays)
;;
;; Or append holidays:
;;
;; (setq calendar-holidays (append calendar-holidays holiday-netherlands-holidays))

;;; Code:

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

;;;###autoload
(defvar holiday-netherlands-holidays
  '((holiday-fixed 1 1 "Niewjaar")
    (holiday-easter-etc -2 "Goede Vrijdag")
    (holiday-easter-etc 0 "Paasdag")
    (holiday-easter-etc 1 "Tweede Paasdag")
    (holiday-fixed 4 27 "Koningsdag")
    (holiday-fixed 5 5 "Bevrijdingsdag")
    (holiday-easter-etc 39 "Hemelvaartsdag")
    (holiday-easter-etc 49 "Pinksteren")
    (holiday-easter-etc 50 "Tweede Pinksterdag")
    (holiday-fixed 12 5 "Sinterklaasavond")
    (holiday-fixed 12 25 "Kerstmis")
    (holiday-fixed 12 26 "Tweede Kerstdag"))
  "Netherlands holidays.")

(provide 'netherlands-holidays)

;;; netherlands-holidays.el ends here
