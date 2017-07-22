;;; noaa.el --- Get NOAA weather data -*- lexical-binding: t -*-

;; Copyright (C) 2017 David Thompson
;; Author: David Thompson
;; Version: 0.1
;; Package-Version: 20170719.1136
;; Keywords:
;; Homepage: https://github.com/thomp/noaa
;; URL: https://github.com/thomp/noaa
;; Package-Requires: ((request "0.2.0") (cl-lib "0.5") (emacs "24"))

;;; Commentary:

;; This package provides a way to view an NOAA weather
;; forecast for a specific geographic location.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)

(defgroup noaa ()
  "View an NOAA weather forecast for a specific geographic location."
  :group 'external)

(defcustom noaa-latitude 36.7478
  "The latitude corresponding to the location of interest."
  :group 'noaa
  :type '(number))

(defcustom noaa-longitude -119.771
  "The latitude corresponding to the location of interest."
  :group 'noaa
  :type '(number))

(defvar noaa-buffer-spec "*noaa.el*"
  "Buffer or buffer name.")

;; noaa-forecast (vs noaa-current)

;;;###autoload
(defun noaa ()
  "Request weather forecast data. Display the data in the buffer specified by ‘noaa-buffer-spec’."
  (interactive)
  (if (and (numberp noaa-latitude)
	   (numberp noaa-longitude))
      (noaa-url-retrieve (noaa-url noaa-latitude noaa-longitude))
      (message "To use NOAA, first set NOAA-LATITUDE and NOAA-LONGITUDE.")))

(defun noaa-aval (alist key)
  "Utility function to retrieve value associated with key KEY in alist ALIST."
  (let ((pair (assoc key alist)))
    (if pair
	(cdr pair)
      nil)))

(defun noaa-handle-noaa-result (result)
  "Handle the data described by RESULT (presumably the result of an HTTP request for NOAA forecast data)."
  (switch-to-buffer noaa-buffer-spec)
  ;; retrieve-fn accepts two arguments: a key-value store and a key
  ;; retrieve-fn returns the corresponding value
  (let ((retrieve-fn 'noaa-aval))
    (let ((properties (funcall retrieve-fn result 'properties)))
      (if (not properties)
	  (message "Couldn't find properties. The NOAA API spec may have changed.")
	(let ((periods (funcall retrieve-fn properties 'periods))
	      ;; LAST-DAY-NUMBER is used for aesthetics --> separate data by day
	      (last-day-number -1)
	      (day-field-width 16)
	      (temp-field-width 5))
	 (erase-buffer)
	 (dotimes (i (length periods))
	   (let ((period (elt periods i)))
	     (let ((start-time (funcall retrieve-fn period 'startTime)))
	       (let ((day-number (noaa-iso8601-to-day start-time))
		     (name (funcall retrieve-fn period 'name))
		     (temp (funcall retrieve-fn period 'temperature))
		     (short-forecast (funcall retrieve-fn period 'shortForecast)))
		 ;; simple output w/some alignment
		 (progn (if (not (= last-day-number day-number))
			    (newline))
			(insert (format "%s" name))
			(move-to-column day-field-width t)
			(insert (format "%s" temp))
			(move-to-column (+ day-field-width temp-field-width) t)
			(insert (format "%s" short-forecast))
			(newline))
		 (setq last-day-number day-number)))))
	 (goto-char (point-min)))))))

;; emacs built-ins aren't there yet for handling ISO8601 values -- leaning on date is non-portable but works nicely for systems where date is available
(defun noaa-iso8601-to-day (iso8601-string)
  "Return a day value for the time specified by ISO8601-STRING."
  (elt (parse-time-string (shell-command-to-string (format "date -d %s --iso-8601=date" iso8601-string))) 3))

;;;###autoload
(defun noaa-quit ()
  "Leave the buffer specified by ‘noaa-buffer-spec’."
  (interactive)
  (kill-buffer noaa-buffer-spec))

(defun noaa-url (&optional latitude longitude)
  "Return a string representing a URL. LATITUDE and LONGITUDE should be numbers."
  (format "https://api.weather.gov/points/%s,%s/forecast" (or latitude noaa-latitude) (or longitude noaa-longitude)))

(defun noaa-url-retrieve (url &optional http-callback)
  "Return the buffer containing only the 'raw' body of the HTTP response. Call HTTP-CALLBACK with the buffer as a single argument."
  (noaa-url-retrieve-tkf-emacs-request url http-callback))

;; async version relying on tfk emacs-request library
(defun noaa-url-retrieve-tkf-emacs-request (&optional url http-callback)
  (request (or url (noaa-url noaa-latitude noaa-longitude))
	   :parser 'buffer-string ;'json-read
	   :error (function*
		   (lambda (&key data error-thrown response symbol-status &allow-other-keys)
		     (message "data: %S " data) 
		     (message "symbol-status: %S " symbol-status)
		     (message "E Error response: %S " error-thrown)
		     (message "response: %S " response)))
	   :status-code '((500 . (lambda (&rest _) (message "Got 500 -- the NOAA server seems to be unhappy"))))
	   :success (or http-callback 'noaa-http-callback)))

;; forecast-http-callback
(cl-defun noaa-http-callback (&key data response error-thrown &allow-other-keys)
  (let ((noaa-buffer (get-buffer-create noaa-buffer-spec)))
    (switch-to-buffer noaa-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown))))
    (goto-char (point-min))
    (let ((result (json-read-from-string data)))
      (noaa-handle-noaa-result result)
      (noaa-mode))))

(cl-defun noaa-http-callback--simple (&key data response error-thrown &allow-other-keys)
  (let ((noaa-buffer (get-buffer-create noaa-buffer-spec)))
    (switch-to-buffer noaa-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown)))
      (noaa-insert data))))

(defun noaa-parse-json-in-buffer ()
  "Parse and return the JSON object present in the buffer specified by ‘noaa-buffer-spec’."
  (switch-to-buffer noaa-buffer-spec)
  (json-read))

(defun noaa-insert (x)
  "Insert X into the buffer specified by ‘noaa-buffer-spec’."
  (switch-to-buffer noaa-buffer-spec)
  (insert x))

;;
;; noaa mode
;;

;;;###autoload
(define-derived-mode noaa-mode text-mode "noaa"
  "Major mode for displaying NOAA weather data
\\{noaa-mode-map}
"
  )

(defvar noaa-mode-map (make-sparse-keymap)
  "Keymap for `noaa-mode'.")

(define-key noaa-mode-map (kbd "q") 'noaa-quit)

(provide 'noaa)
;;; noaa.el ends here
