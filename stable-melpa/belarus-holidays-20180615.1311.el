;;; belarus-holidays.el --- Belarus holidays whith transfers
;;; Commentary:
;; Provide Belarus holidays with working day transfers for calendar.el
;;
;; To highlight non-working days in calendar buffer u can use this code:
;;
;;(defadvice calendar-generate-month
;;(after highlight-weekend-days (month year indent) activate)
;;"Highlight weekend days.
;;If STRING contains `\(нерабочы\)' day is non-working.
;;If STRING contains `\(рабочы\)' day is working."
;;(dotimes (i 31)
;;  (let ((date (list month (1+ i) year)) (working nil) (non-working nil)
;; (hlist nil))
;;     (setq hlist (calendar-check-holidays date))
;;        (dolist (cursor hlist)
;;           (if (string-match-p "\(рабочы\)" cursor)
;;	        (setq working t))
;;            (if (string-match-p "\(нерабочы\)" cursor)
;;	        (setq non-working t)))
;;        (if (and (not working)
;;               (or (= (calendar-day-of-week date) 0)
;;                   (= (calendar-day-of-week date) 6)
;;	           non-working))
;;	    (calendar-mark-visible-date date 'holiday)))))
;;
;; And with `use-package` try so:
;;
;;(use-package belarus-holidays
;;  :ensure t
;;  :config
;;  (setq calendar-holidays belarus-holidays)
;;  (defadvice calendar-generate-month
;;    (after highlight-weekend-days (month year indent) activate)
;;  "Highlight weekend days. If STRING contains `\(нерабочы\)' day is non-working. If STRING contain `\(рабочы\)' day is working."
;;(dotimes (i 31)
;;  (let ((date (list month (1+ i) year)) (working nil) (non-working nil) (hlist nil))
;;    (setq hlist (calendar-check-holidays date))
;;    (dolist (cursor hlist)
;;      (if (string-match-p "\(рабочы\)" cursor)
;;	  (setq working t))
;;      (if (string-match-p "\(нерабочы\)" cursor)
;;	  (setq non-working t)))
;;    (if (and (not working)
;;	     (or (= (calendar-day-of-week date) 0)
;;		 (= (calendar-day-of-week date) 6)
;;		 non-working))
;;	(calendar-mark-visible-date date 'holiday))))))

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright 2018 Yauhen Makei

;; Author: Yauhen Makei <yauhen.makei@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20180615.1311
;; URL: http://bitbucket.org/EugeneMakei/belarus-holidays.el

;;; Code:

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar belarus-holidays nil
  "Святы Беларусі.")

(setq belarus-holidays
  `(
    (holiday-fixed 1 1 "Новы год (нерабочы)")
    (belarus-holidays--holiday-once 1 2 2018 "Перанос рабочага дня (нерабочы)")
    (belarus-holidays--holiday-once 1 20 2018 "Перанос рабочага дня (рабочы)")
    (holiday-fixed 2 23 "Дзень абаронцаў Айчыны і Узброеных Сіл Рэспублікі Беларусь")
    (belarus-holidays--holiday-once 3 3 2018 "Перанос рабочага дня (рабочы)")
    (holiday-fixed 3 8 "Дзень жанчын (нерабочы)")
    (belarus-holidays--holiday-once 3 9 2018 "Перанос рабочага дня (нерабочы)")
    (holiday-fixed 3 15 "Дзень Канстытуцыі")
    (holiday-fixed 4 2 "Дзень яднання народаў Беларусі і Расіі")
    (belarus-holidays--holiday-once 4 14 2018 "Перанос рабочага дня (рабочы)")
    (belarus-holidays--holiday-once 4 16 2018 "Перанос рабочага дня (нерабочы)")
    (belarus-holidays--holiday-once 4 28 2018 "Перанос рабочага дня (рабочы)")
    (belarus-holidays--holiday-once 4 30 2018 "Перанос рабочага дня (нерабочы)")
    (holiday-fixed 5 1 "Свята працы (нерабочы)")
    (holiday-fixed 5 9 "Дзень Перамогі (нерабочы)")
    (holiday-float 5 0 2 "Дзень Дзяржаўнага сцяга Рэспублікі Беларусь і Дзяржаўнага герба Рэспублікі Беларусь")
    (belarus-holidays--holiday-once 7 2 2018 "Перанос рабочага дня (нерабочы)")
    (holiday-fixed 7 3 "Дзень Незалежнасці (Дзень Рэспублікі) (нерабочы)")
    (belarus-holidays--holiday-once 7 7 2018 "Перанос рабочага дня (рабочы)")
    (holiday-fixed 11 7 "Дзень Кастрычніцкай рэвалюцыі (нерабочы)")
    (belarus-holidays--holiday-once 12 22 2018 "Перанос рабочага дня (рабочы)")
    (belarus-holidays--holiday-once 12 24 2018 "Перанос рабочага дня (нерабочы)")
    (belarus-holidays--holiday-once 12 29 2018 "Перанос рабочага дня (рабочы)")
    (belarus-holidays--holiday-once 12 31 2018 "Перанос рабочага дня (нерабочы)")
    (holiday-fixed 1 7 "Ражджаство Хрыстова (праваслаўнае) (нерабочы)")
    (belarus-holidays--holiday-eastern-etc 0 "Вялікдзень (каталіцкі) (нерабочы)")
    (belarus-holidays--holiday-eastern-etc 0 "Вялікдзень (праваслаўны) (нерабочы)")
    (belarus-holidays--holiday-eastern-etc 9 "Радаўніца (нерабочы)")
    (holiday-fixed 12 25 "Ражджаство Хрыстова (каталіцкае) (нерабочы)")
    ))

(defun belarus-holidays--holiday-eastern-etc (&optional n string)
  "Date of Nth day after Orthodox Easter (named STRING)
, if visible in calendar window.
Negative values of N are interpreted as days before Easter.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after Easter.
URL: https://www.emacswiki.org/emacs/ukrainian-holidays.el"
(list (list
 (if (= n 0)
  (belarus-holidays--easter-eastern displayed-year)
   (calendar-gregorian-from-absolute
    (+ n (calendar-absolute-from-gregorian
	  (belarus-holidays--easter-eastern displayed-year)))))
   string)))

(defun belarus-holidays--easter-eastern (year)
  "Date of Easter in YEAR."
  (let* ((x (% (+ (* (% year 19) 19) 15) 30))
	 (day (- (+ x 10)
		 (% (+ (/ (* year 5) 4) x) 7))))
    (if (< day 31)
	(list 4 day year)
      (list 5 (- day 30) year))))

(defun belarus-holidays--holiday-once (month day year string)
  "Holiday on MONTH, DAY (Gregorian), YEAR called STRING.
If MONTH, DAY, YEAR is visible, the value returned
is the list (((MONTH DAY YEAR) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month))
    (calendar-increment-month m year (- 11 month))
    (if (> m 9)
        (list (list (list month day year) string)))))

(provide 'belarus-holidays)

;;; belarus-holidays.el ends here
