;;; bart-mode.el --- Real time BART departures info.  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Michael Schuldt

;; Author: Michael Schuldt <mbschuldt@gmail.com>
;; Version: 1.5.2
;; Package-Version: 20180131.1829
;; URL: https://github.com/mschuldt/bart-mode
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience transit

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Display real time bart departures information from the BART API:
;;   http://api.bart.gov/docs/overview/index.aspx

;;; Code:

(require 'url)
(require 'xml)
(require 'dom)

;; User variables:
(defgroup bart nil
  "Major mode viewing real-time BART departures info"
  :prefix "bart-"
  :group 'applications)

(defcustom bart-api-key "MW9S-E7SL-26DU-VV8V"
  "Bart API key.
See http://www.bart.gov/schedules/developers/api
for reasons why you might want to register for your own"
  :type 'string
  :group 'bart)

(defcustom bart-abbreviate-station-names nil
  "If non-nil use station abbreviations instead of full names."
  :type 'boolean
  :group 'bart)

(defcustom bart-rtd-update-interval 60
  "Display update interval in seconds."
  :type 'integer
  :group 'bart)

(defcustom bart-manage-window nil
  "If non-nil actively manage the bart buffer in a seporate fitted window."
  :type 'boolean
  :group 'bart)

(defcustom bart-stations
  '(("12th St. Oakland City Center" . 12th)
    ("16th St. Mission (SF)" . 16th)
    ("19th St. Oakland" . 19th)
    ("24th St. Mission (SF)" . 24th)
    ("Ashby (Berkeley)" . ashb)
    ("Balboa Park (SF)" . balb)
    ("Bay Fair (San Leandro)" . bayf)
    ("Castro Valley" . cast)
    ("Civic Center (SF)" . civc)
    ("Coliseum" . cols)
    ("Colma" . colm)
    ("Concord" . conc)
    ("Daly City" . daly)
    ("Downtown Berkeley" . dbrk)
    ("Dublin/Pleasanton" . dubl)
    ("El Cerrito del Norte" . deln)
    ("El Cerrito Plaza" . plza)
    ("Embarcadero (SF)" . embr)
    ("Fremont" . frmt)
    ("Fruitvale (Oakland)" . ftvl)
    ("Glen Park (SF)" . glen)
    ("Hayward" . hayw)
    ("Lafayette" . lafy)
    ("Lake Merritt (Oakland)" . lake)
    ("MacArthur (Oakland)" . mcar)
    ("Millbrae" . mlbr)
    ("Montgomery St. (SF)" . mont)
    ("North Berkeley" . nbrk)
    ("North Concord/Martinez" . ncon)
    ("Oakland Int'l Airport" . oakl)
    ("Orinda" . orin)
    ("Pittsburg/Bay Point" . pitt)
    ("Pleasant Hill" . phil)
    ("Powell St. (SF)" . powl)
    ("Richmond" . rich)
    ("Rockridge (Oakland)" . rock)
    ("San Bruno" . sbrn)
    ("San Francisco Int'l Airport" . sfia)
    ("San Leandro" . sanl)
    ("South Hayward" . shay)
    ("South San Francisco" . ssan)
    ("Union City" . ucty)
    ("Warm Springs/South Fremont" . warm)
    ("Walnut Creek" . wcrk)
    ("West Dublin" . wdub)
    ("West Oakland" . woak))
  "List of station - abbreviation pairs.
source: http://api.bart.gov/docs/overview/abbrev.aspx"
  :type '(alist :key-type (string :tag "Station name")
                :value-type (string :tag "Station abbreviation"))
  :group 'bart)

(defcustom bart-station 'civc
  "Default bart station abbreviation.
Must be a recognized station abbreviation.
`bart-stations' provides the mapping"
  :type `(choice ,@(mapcar (lambda (x)
                             `(const :tag ,(car x) ,(cdr x)))
                           bart-stations))
  :group 'bart)

(defface bart-bold
  '((t :weight ultra-bold))
  "bart-mode bold face."
  :group 'bart-faces)

(defface bart-blue
  '((((class color) (background light)) (:foreground "#0099cc"))
    (((class color) (background dark)) (:foreground "#0099cc")))
  "Bart blue face."
  :group 'bart-faces)

(defface bart-green
  '((((class color) (background light)) (:foreground "#339933"))
    (((class color) (background dark)) (:foreground "#339933")))
  "Bart green face."
  :group 'bart-faces)

(defface bart-red
  '((((class color) (background light)) (:foreground "#ff0000"))
    (((class color) (background dark)) (:foreground "#ff0000")))
  "Bart red face."
  :group 'bart-faces)

(defface bart-yellow
  '((((class color) (background light)) (:foreground "#ffff33"))
    (((class color) (background dark)) (:foreground "#ffff33")))
  "Bart yellow face."
  :group 'bart-faces)

(defface bart-orange
  '((((class color) (background light)) (:foreground "#ff9933"))
    (((class color) (background dark)) (:foreground "#ff9933")))
  "Bart orange face."
  :group 'bart-faces)

(defface bart-header
  '((t :inherit (header-line-highlight)))
  "Face for bart header"
  :group 'bart-faces)

(defface bart-header-bold
  '((t :inherit (bart-header bart-bold)))
  "Face for bart bold header"
  :group 'bart-faces)

;; Internal variables:
(defvar bart--rtd-buffer nil
  "The BART info display buffer.")

(defvar bart--rtd-update-timer nil
  "Timer used for updating display buffer.")

(defvar bart--rtd-initial-window-height 10
  "Initial window height, the window gets re-sized after each update.")

(defvar bart--face-map #s(hash-table size 30 test equal data ("BLUE" bart-blue
                                                              "GREEN" bart-green
                                                              "RED" bart-red
                                                              "YELLOW" bart-yellow
                                                              "ORANGE" bart-orange
                                                              ))
  "Maps colors from API to bart-mode faces")

(defun bart-select-station ()
  "Interactivly select the current BART station."
  (interactive)
  (let ((station (completing-read "station: " (mapcar 'car bart-stations))))
    (when station
      (setq bart-station (cdr (assoc station bart-stations)))
      (when bart--rtd-buffer
        (bart-update)))))

(defun bart-quit ()
  "Exit bart-mode."
  (interactive)
  (bart--cleanup))

(defun bart-update ()
  "Update the BART real time data."
  (interactive)
  (bart--rtd-request))

(defun bart-toggle-station-abbreviation ()
  "Toggle the use of abbreviated station names."
  (interactive)
  (setq bart-abbreviate-station-names (not bart-abbreviate-station-names))
  (when bart--rtd-buffer
    (bart-update)))

(defvar bart-mode-map
  (let ((map (make-sparse-keymap 'bart-mode-map)))
    (define-key map (kbd "s") 'bart-select-station)
    (define-key map (kbd "q") 'bart-quit)
    (define-key map (kbd "g") 'bart-update)
    (define-key map (kbd "a") 'bart-toggle-station-abbreviation)
    map)
  "The bart-mode keymap.")

(defun bart ()
  "Display real time bart departure information."
  (interactive)
  (bart--cleanup)
  (setq bart--rtd-buffer (get-buffer-create "*BART Departures*"))
  (if bart-manage-window
      (let ((w (get-largest-window)))
        (setq w (split-window w
                              (- (window-height w)
                                 bart--rtd-initial-window-height 2)
                              nil))
        (set-window-buffer w bart--rtd-buffer)
        (select-window w)
        (bart-mode))
    (with-current-buffer bart--rtd-buffer
      (bart-mode))
    (display-buffer  bart--rtd-buffer nil)))

(defun bart--make-color-square (color)
  (propertize (char-to-string ?\x25A0) 'font-lock-face (gethash color bart--face-map 'bart-red)))

(defun bart--bold (str)
  (propertize str 'font-lock-face 'bart-bold))

(defun bart--header (str)
  (propertize str 'font-lock-face 'bart-header))

(defun bart--header-bold (str)
  (propertize str 'font-lock-face 'bart-header-bold))

(defun bart--rtd-update-buffer (xml)
  "Update the current buffer using the bart data XML."
  (read-only-mode -1)
  (erase-buffer)
  (let* ((root (car (dom-by-tag xml 'root)))
         (station (dom-by-tag root 'station))
         (time (caddar (dom-by-tag root 'time)))
         (name (caddar (dom-by-tag station 'name)))
         (destinations (dom-by-tag station 'etd))
         dest abr min len color station-name)
    (insert (concat (bart--header-bold (format " %s" name))
                    (bart--header " Departures as of ")
                    (bart--header-bold (format "%s\n" time))))
    (dolist (station destinations)
      (setq dest (caddar (dom-by-tag station 'destination))
            abr (caddar (dom-by-tag station 'abbreviation))
            station-name (if bart-abbreviate-station-names
                             (format "%-8s" abr)
                           (format "%-30s" dest)))
      (insert (bart--bold station-name))
      (dolist (etd (dom-by-tag station 'estimate))
        (setq min (caddar (dom-by-tag etd 'minutes))
              ;;plat (caddar (dom-by-tag etd 'platform))
              ;;dir (caddar (dom-by-tag etd 'direction))
              len (caddar (dom-by-tag etd 'length))
              color (caddar (dom-by-tag etd 'color)))

        (insert (format "%-25s" (concat (bart--make-color-square color)
                                        (bart--bold (if (string= min "Leaving")
                                                        (concat " " min " ")
                                                      (format " %s min " min)))
                                        (format "(%s car)" len)))))
      (insert "\n")))
  (insert "\n")
  (goto-char 1)
  (read-only-mode 1)
  (when bart-manage-window
    (fit-window-to-buffer (get-buffer-window (current-buffer)))))

(defun bart--rtd-request-callback (xml)
  "The bart-mode timer callback.
XML is the data received from `url-retrieve'"
  (if (buffer-live-p bart--rtd-buffer)
      (with-current-buffer bart--rtd-buffer
        (bart--rtd-update-buffer xml))
    (bart--cleanup)))

(defun bart--request (type keys cb)
  "Perform a request to the BART api.
TYPE is the bart api type.  KEYS are the url keys.  CB is the callback function"
  (url-retrieve (concat "http://api.bart.gov/api/" type "?"
                        (mapconcat (lambda (x)
                                     (concat (car x) "=" (cdr x)))
                                   (cons (cons "key" bart-api-key) keys)
                                   "&"))
                (lambda (status)
                  status ;;TODO: check status
                  (funcall cb (xml-parse-region)))))

(defun bart--rtd-request (&optional station)
  "Perform a bart rtd (real-time data) request for STATION."
  ;; http://api.bart.gov/docs/etd/etd.aspx
  (bart--request "etd.aspx" (list (cons "orig" (symbol-name (or station bart-station)))
                                  (cons "cmd" "etd"))
                 #'bart--rtd-request-callback))

(defun bart--cleanup ()
  "Cleanup bart-mode state."
  (when bart--rtd-update-timer
    (cancel-timer bart--rtd-update-timer)
    (setq bart--rtd-update-timer nil))
  (when (buffer-live-p bart--rtd-buffer)
    (delete-windows-on bart--rtd-buffer)
    (kill-buffer bart--rtd-buffer)
    (setq bart--rtd-buffer nil)))

(defun bart--rtd-buffer-killed-hook-fn ()
  "Cleanly exit bart-mode state when buffer is killed."
  (when (eq (current-buffer) bart--rtd-buffer)
    (setq bart--rtd-buffer nil)
    (bart--cleanup)))

(define-derived-mode bart-mode special-mode "Bart"
  "Mode for displaying real-time bart departures"
  (if (called-interactively-p nil)
      (error "Use M-x bart")
    (add-hook 'kill-buffer-hook 'bart--rtd-buffer-killed-hook-fn)
    (setq truncate-lines t)
    (read-only-mode 1)
    (setq bart--rtd-buffer (current-buffer))
    (setq bart--rtd-update-timer
          (run-at-time t bart-rtd-update-interval 'bart-update))
    (bart-update)))

(provide 'bart-mode)

;;; bart-mode.el ends here
