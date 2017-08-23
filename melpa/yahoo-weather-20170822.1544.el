;;; yahoo-weather.el --- Displays weather information in mode-line  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; URL: https://github.com/lujun9972/yahoo-weather-mode
;; Package-Version: 20170822.1544
;; Package-X-Original-Version: 20160426.529
;; Package-Requires: ((emacs "24"))
;; Keywords: weather, mode-line
;; Created: 2015-12-28
;; Version: 0.1

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

;; customize the `yahoo-weather-location' which supports chinese characters and then 
;;
;;   M-x yahoo-weather-mode
;;
;; default it will only display the weather and temperature.
;; you can specify `yahoo-weather-format' to display more information

;;; Code:

(require 'url)
(require 'json)

(defgroup yahoo-weather nil
  "Yahoo-Weather minor mode"
  :group 'emacs)

(defcustom yahoo-weather-location "东莞"
  "location"
  :type 'string
  :group 'yahoo-weather)

(defcustom yahoo-weather-format "[%(weather) %(temperature)℃]"
  "how to display the weather information.

%(weather) %(temperature) %(wind-chill) %(wind-direction) %(wind-direction) %(wind-speed)
%(atmosphere-humidity) %(atmosphere-pressure) %(atmosphere-rising) %(atmosphere-visibility)
%(sunrise-time) %(sunset-time) will be replaced by the real value"
  :type 'string
  :group 'yahoo-weather)

(defcustom yahoo-weather-use-F nil
  "Set t to use Fahrenheit"
  :type 'boolean
  :group 'yahoo-weather)

(defcustom yahoo-weather-temperture-format "%.2f"
  "Precision to display string format.  Use '%d' for integer or '%.2f' for two decimals."
  :type 'string
  :group 'yahoo-weather)

(defcustom yahoo-weather-update-interval 3600
  "Seconds after which the weather information will be updated."
  :type 'integer
  :group 'yahoo-weather)

(defvar yahoo-weather-env (url-hexify-string "store://datatables.org/alltableswithkeys"))

(defvar yahoo-weather-info)

(defun yahoo-weather-get-query-url (location env)
  "generate url that used to fetch weather information"
  (let* ((yql_query (url-hexify-string (format "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text='%s')" location)))
         (url (format 
               "https://query.yahooapis.com/v1/public/yql?q=%s&format=json&env=%s" yql_query env)))
    url))

(defun yahoo-weather--extract-from-json-object (json-object extract-place-list)
  "extract data from JSON-OBJECT which responsed by yahoo weather"
  (let* ((place (car extract-place-list))
         (extract-place-list (cdr extract-place-list))
         (json-object (cdr (assoc place json-object))))
    (if extract-place-list
        (yahoo-weather--extract-from-json-object json-object extract-place-list)
      json-object)))

(defun yahoo-weather-info-format (json-object format-string)
  (let* ((temperature-F (string-to-number (yahoo-weather--extract-from-json-object json-object '(query results channel item condition temp))))
         (temperature (yahoo-weather--f_to_c temperature-F))
         (text (yahoo-weather--extract-from-json-object json-object '(query results channel item condition text)))
         (wind-chill (yahoo-weather--extract-from-json-object json-object '(query results channel wind chill)))
         (wind-direction (yahoo-weather--extract-from-json-object json-object '(query results channel wind direction)))
         (wind-speed (yahoo-weather--extract-from-json-object json-object '(query results channel wind speed)))
         (atmosphere-humidity (yahoo-weather--extract-from-json-object json-object '(query results channel atmosphere humidity)))
         (atmosphere-pressure (yahoo-weather--extract-from-json-object json-object '(query results channel atmosphere pressure)))
         (atmosphere-rising (yahoo-weather--extract-from-json-object json-object '(query results channel atmosphere rising)))
         (atmosphere-visibility (yahoo-weather--extract-from-json-object json-object '(query results channel atmosphere visibility)))
         (sunrise-time (yahoo-weather--extract-from-json-object json-object '(query results channel astronomy sunrise)))
         (sunset-time (yahoo-weather--extract-from-json-object json-object '(query results channel astronomy sunset)))
         )
    (setq format-string (replace-regexp-in-string "%(weather)" text format-string t))
    (if yahoo-weather-use-F
        (setq format-string (replace-regexp-in-string "%(temperature)" (format yahoo-weather-temperture-format temperature-F) format-string t))
      (setq format-string (replace-regexp-in-string "%(temperature)" (format yahoo-weather-temperture-format temperature) format-string t)))
      (setq format-string (replace-regexp-in-string "%(wind-chill)" wind-chill format-string t))
      (setq format-string (replace-regexp-in-string "%(wind-direction)" wind-direction format-string t))
      (setq format-string (replace-regexp-in-string "%(wind-speed)" wind-speed format-string t))
      (setq format-string (replace-regexp-in-string "%(atmosphere-humidity)" atmosphere-humidity format-string t))
      (setq format-string (replace-regexp-in-string "%(atmosphere-pressure)" atmosphere-pressure format-string t))
      (setq format-string (replace-regexp-in-string "%(atmosphere-rising)" atmosphere-rising format-string t))
      (setq format-string (replace-regexp-in-string "%(atmosphere-visibility)" atmosphere-visibility format-string t))
      (setq format-string (replace-regexp-in-string "%(sunrise-time)" sunrise-time format-string t))
      (setq format-string (replace-regexp-in-string "%(sunset-time)" sunset-time format-string t))
      format-string))

(defun yahoo-weather-update-info-cb (status &rest cbargs)
  (let (content)
    (goto-char (point-min))
    (when (search-forward-regexp "^$" nil t)
      (setq content (buffer-substring-no-properties (+ (point) 1) (point-max))))
    (kill-buffer)
    (force-mode-line-update t)
    (setq yahoo-weather-info (json-read-from-string content))))

(defun yahoo-weather-update-info ()
  "update weather information"
  (interactive)
  (let ((url (yahoo-weather-get-query-url yahoo-weather-location yahoo-weather-env)))
    (url-retrieve url #'yahoo-weather-update-info-cb nil t)))

(defun yahoo-weather--f_to_c (temp)
  "convert fahrenheit to celsius"
  (/ (* (- temp 32.0) 5.0) 9.0))


;;; Glboal Minor-mode

(defcustom yahoo-weather-mode-line
  '(:eval (yahoo-weather-info-format yahoo-weather-info yahoo-weather-format))
  "Mode line lighter for yahoo-weather-mode."
  :type 'sexp
  :risky t
  :group 'yahoo-weather)

(defvar yahoo-weather-update-info-timer nil)

;;;###autoload
(define-minor-mode yahoo-weather-mode
  "Toggle weather information display in mode line (yahoo-weather information mode).
With a prefix argument ARG, enable yahoo-weather mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'yahoo-weather
  (unless global-mode-string
    (setq global-mode-string '("")))
  (when (timerp yahoo-weather-update-info-timer)
    (cancel-timer yahoo-weather-update-info-timer))
  (if (not yahoo-weather-mode)
      (setq global-mode-string
            (delq 'yahoo-weather-mode-line global-mode-string))
    (setq yahoo-weather-update-info-timer (run-at-time nil yahoo-weather-update-interval #'yahoo-weather-update-info))
    (add-to-list 'global-mode-string 'yahoo-weather-mode-line t)
    (yahoo-weather-update-info)))

(provide 'yahoo-weather)
;;; yahoo-weather.el ends here
