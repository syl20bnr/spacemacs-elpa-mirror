;;; russian-holidays.el --- Russian holidays for the calendar

;; Author: Alexander I.Grafov <siberian@laika.name>
;; URL: https://github.com/grafov/russian-holidays
;; Package-Version: 20170109.1340
;; Version: 0.4

;; This file is not part of GNU Emacs

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
;; The list of official Russian holidays and commemorative days that
;; are day offs. With regional holidays included.
;;
;; Installation:
;;
;; To use `russian-holidays' exclusively for the calendar
;;
;;  (setq calendar-holidays russian-holidays)
;;
;; To use 'russian-holidays' additionally to other calenders
;;
;;  (setq calendar-holidays (append calendar-holidays russian-holidays))
;;
;; If you'd like to add regional holidays, you can use it like in this
;; example for Chuvash respublic
;;
;;  (setq calendar-holidays (append calendar-holidays russian-holidays russian-holidays-ch-holidays))
;;
;; This works for for all regions noted in
;; https://ru.wikipedia.org/wiki/Праздники_России article.
;;
;;  `russian-holidays-ad-holidays' for Adygea
;;  `russian-holidays-ba-holidays' for Bashkiria
;;  `russian-holidays-ta-holidays' for Tatarstan
;;  etc.
;;
;; The code inspired by german-holidays.el and ukrainian-holidays.el.

;;; Code:
(eval-when-compile
  (require 'calendar)
  (require 'holidays))

; Holidays data based on the article
; https://ru.wikipedia.org/wiki/Праздники_России
(defvar russian-holidays
  '((holiday-fixed 1 1 "Новый год") ; New Year day
    (holiday-fixed 1 2 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 1 3 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 1 4 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 1 5 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 1 6 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 1 7 "Рождество Христово") ; Christmas day according
					     ; to the Julian calendar
					     ; used by the Russian
					     ; Orthodox Church
    (holiday-fixed 1 8 "Новогодние каникулы") ; New Year holidays
    (holiday-fixed 2 23 "День защитника Отечества") ; Defender of the
						   ; Fatherland Day
    (holiday-fixed 3 8 "Международный женский день") ; International
						     ; Women's Day
    (holiday-fixed 5 1 "Праздник Весны и Труда") ; Spring and Labour
						 ; Day
    (holiday-fixed 5 9 "День Победы") ; Victory Day
    (holiday-fixed 6 12 "День России") ; Russia Day
    (holiday-fixed 11 4 "День народного единства")) ; Unity Day
  "Official state holidays in Russian Federation.")

; Regional holidays specific for Rebuplic of Adygea (Республика
; Адыгея)
(defvar russian-holidays-ad-holidays
  '((holiday-fixed 10 5 "День образования Республики Адыгея"))
  "Official holidays in Republic of Adygea of Russian Federation.")

; Regional holidays specific for Bashkir Rebuplic (Республика
; Башкортостан)
(defvar russian-holidays-ba-holidays
  '((holiday-fixed 10 11 "День Республики"))
  "Official holidays in Bashkortostan Republic of Russian Federation.")

; Regional holidays specific for Rebuplic of Dagestan (Республика
; Дагестан)
(defvar russian-holidays-da-holidays
  '((holiday-fixed 7 26 "День Конституции Республики Дагестан")
	(holiday-fixed 9 15 "День единства народов Дагестана"))
  "Official holidays in Republic of Dagestan of Russian Federation.")

; Regional holidays specific for Kabardino-Balkar Republic
; (Кабардино-Балкарская Республика)
(defvar russian-holidays-kb-holidays
  '((holiday-fixed 3 28 "День возрождения балкарского народа")
	(holiday-fixed 5 21 "День памяти адыгов (черкесов)")
	(holiday-fixed 9 1 "День Республики")
	(holiday-fixed 9 20 "День адыгов (черкесов)"))
  "Official holidays in Kabardino-Balkar Republic of Russian Federation.")

; Regional holidays specific for Republic of Kalmykia (Республика
; Калмыкия)
(defvar russian-holidays-kl-holidays
  '((holiday-fixed 4 5 "День принятия Степного Уложения (Конституции) Республики Калмыкии")
	(holiday-fixed 12 28 "День памяти жертв депортации калмыцкого народа"))
  "Official holidays in Republic of Kalmykia of Russian Federation.")

; Regional holidays specific for Rebuplic of Karachay-Cherkessia
; (Карачаево-Черкесская Республика)
(defvar russian-holidays-kc-holidays
  '((holiday-fixed 5 3 "День возрождения карачаевского народа")
	(holiday-fixed 7 3 "День единения народов Карачаево-Черкесской Республики")
	(holiday-fixed 9 7 "День образования Карачаево-Черкесской Республики"))
  "Official holidays in Republic of Karachay-Cherkessia of Russian Federation.")

; Regional holidays specific for Komi Rebuplic (Республика Коми)
(defvar russian-holidays-ko-holidays
  '((holiday-fixed 8 22 "День Республики Коми"))
  "Official holidays in Komi Republic of Russian Federation.")

; Regional holidays specific for Rebuplic of Crimea (Республика Крым)
(defvar russian-holidays-ko-holidays
  '((holiday-fixed 3 18 "День воссоединения Крыма с Россией"))
  "Official holidays in Republic of Crimea of Russian Federation.")

; Regional holidays specific for Sakha Republic (Республика Саха
; (Якутия))
(defvar russian-holidays-sa-holidays
  '((holiday-fixed 4 27 "День Республики Саха (Якутия)")
	(holiday-fixed 6 21 "Ысыах"))
  "Official holidays in Sakha Republic of Russian Federation.")

; Regional holidays specific for Tatarstan Republic (Республика
; Татарстан).
(defvar russian-holidays-ta-holidays
  '((holiday-fixed 8 30 "День Республики")
	(holiday-fixed 11 6 "День Конституции Татарстана"))
  "Official holidays in Tatarstan Republic of Russian Federation.")

; Regional holidays specific for Rebuplic of Udmurtia (Республика
; Удмуртия)
(defvar russian-holidays-ud-holidays
  '((holiday-fixed 11 4 "День государственности Удмуртии"))
  "Official holidays in Republic of Udmurtia of Russian Federation.")

; Regional holidays specific for Chechen Rebuplic (Чеченская
; Республика)
(defvar russian-holidays-ce-holidays
  '((holiday-fixed 3 23 "День Конституции Чеченской Республики")
	(holiday-fixed 4 16 "День мира в Чеченской республике"))
  "Official holidays in Chechen Republic of Russian Federation.")

; Regional holidays specific for Chuvash Rebuplic (Чувашская
; республика)
(defvar russian-holidays-cu-holidays
  '((holiday-fixed 6 24 "День Республики"))
  "Official holidays in Chuvash Republic of Russian Federation.")

; Regional holidays specific for Penza Oblast (Пензенская область)
(defvar russian-holidays-pnz-holidays
  '((russian-holidays-eastern-nth 9 "Радоница - день поминовения усопших"))
  "Official holidays in Penza Oblast of Russian Federation.")

; Regional holidays specific for Saratov Oblast (Саратовская область)
(defvar russian-holidays-sar-holidays
  '((russian-holidays-eastern-nth 9 "Радоница - день поминовения усопших"))
  "Official holidays in Penza Oblast of Russian Federation.")

; Regional holidays specific for Sebastopol city (город Севастополь)
(defvar russian-holidays-sebastopol-holidays
  '((holiday-fixed 3 18 "День возвращения города Севастополя в Россию "))
  "Official holidays in Sebastopol of Russian Federation.")

; Adapted from ukrainian-holidays.el, thanks to abo-abo.
(defun russian-holidays-eastern-nth (n string)
  "Date of Nth day after Orthodox Easter (named STRING).
Negative values of N are interpreted as days before Easter.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after Easter."
  (list (list
	 (if (= n 0)
	     (russian-holidays-easter-eastern displayed-year)
	   (calendar-gregorian-from-absolute
	    (+ n (calendar-absolute-from-gregorian
			  (russian-holidays-easter-eastern displayed-year)))))
	 string)))

; Formula taken from ukrainian-holidays.el, thanks to abo-abo.
(defun russian-holidays-easter-eastern (year)
  (let* ((x (% (+ (* (% year 19) 19) 15) 30))
	 (day (- (+ x 10)
		 (% (+ (/ (* year 5) 4) x) 7))))
    (if (< day 31)
	(list 4 day year)
	(list 5 (- day 30) year))))

(provide 'russian-holidays)

;;; russian-holidays.el ends here
