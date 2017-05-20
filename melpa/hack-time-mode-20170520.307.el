;;; hack-time-mode.el --- Forge time   -*- lexical-binding: t ; eval: (read-only-mode 1) -*-


;; THIS FILE HAS BEEN GENERATED.

;; [[id:bdf129d9-29f3-477c-9fab-a7879bdb7e5a][inner-program]]
;; [[id:e83c08f0-f37a-44c3-b9e9-bf6bb7a58402][prologue]]


;; Copyright 2017 Marco Wahl
;;
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: 2017
;; Version: 0.0.1
;; Package-Version: 20170520.307
;; Package-Requires: ((emacs "24.4"))
;; Keywords: time, convenience
;; URL: https://gitlab.com/marcowahl/hack-time-mode
;;
;; This file is not part of Emacs.
;;
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


;;; Commentary:

;; M-x hack-time-mode RET -1 12:05 RET
;;
;; sets current-time back to yesterday 12:05 PM.
;;
;; M-x hack-time-mode RET
;;
;; disables hack-time-mode and brings back time to normal.

;; See https://gitlab.com/marcowahl/hack-time-mode for the source.

;; Use cases:

;; - View Org agenda as if today was another day.  Achieve this by
;;   hacking the time to the desired date and open the agenda.

;; - Mark Org-todo-items done at another day conveniently.  Achieve
;;   this by hacking the time to the desired date and change the
;;   todo-state of the item in question.

;; Limitations:

;; 'hack-time-mode' has actually limitted control over time.  There
;; are time sources in Emacs _not_ controlled by 'hack-time-mode'.
;; Watch out!



;;; Code:
;; prologue ends here
;; [[id:e0a33b2d-e274-4dd4-bb43-a7e324383984][ht-minor-mode-config]]


;;;###autoload
(define-minor-mode hack-time-mode
  "Toggle hack-time-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `hack-time-mode'."
  :group 'hack-time
  :global t
  :lighter " ht"
  (if hack-time-mode
      (call-interactively #'hack-time-mode-set-current-time)
    (hack-time-mode--current-time-back-to-normal-with-message)))
;; ht-minor-mode-config ends here
;; [[id:e62ab536-0322-4583-9994-0150a330445c][freeze-current-time-core]]


(let (hack-time-mode-day)

  (defun hack-time-mode--freeze-advicer (x)
    "Can be advicer for ‘current-time’."
    (ignore x)
    (append (date-to-time (concat hack-time-mode-day " 11:55")) (list 0 0)))

  (defun hack-time-mode--current-time-back-to-normal ()
    "Remove all time hacks."
    (if (advice-member-p #'hack-time-mode--freeze-advicer #'current-time)
        (advice-remove #'current-time #'hack-time-mode--freeze-advicer)))

  (defun hack-time-mode--current-time-back-to-normal-with-message ()
    "Set current time back to normal and shout."
    (hack-time-mode--current-time-back-to-normal)
    (message "%s" (format-time-string
                   "Time is back to normal.  current-time is: %Y-%m-%d %H:%M"
                   (current-time))))

  (defun hack-time-mode--current-time-do-freeze (yyyy-mm-dd-??:??-string)
    "Change ‘current-time’ to return the chosen date until reset.

Advice ‘current-time’ to return time YYYY-MM-DD-??:??-STRING.

If no hours and minutes given then use 11:55.

Note: This change does not affect every functionality that
depends on time in Emacs.  E.g. ‘format-time-string’ is not
affected."
    (hack-time-mode--current-time-back-to-normal)
    (setf hack-time-mode-day (concat yyyy-mm-dd-??:??-string " 11:55"))
    (advice-add #'current-time :filter-return #'hack-time-mode--freeze-advicer))

  (list 'hack-time-mode--current-time-back-to-normal-with-message
        'hack-time-mode--current-time-back-to-normal
        'hack-time-mode--freeze-advicer
        'hack-time-mode--current-time-do-freeze))
;; freeze-current-time-core ends here
;; [[id:5febcc2d-8798-4b1b-98ae-eb0f478db53d][commands]]


(declare-function org-read-date "org")


;; Commands
(defun hack-time-mode-set-current-time (target-date)
  "Ask user for a date and set it as current time.
The current time does not move until call of
`hack-time-mode-current-time-back-to-normal'.

Examples for specifying the current time.

- \"-1\" to set current time to yesterday at 11:55 am.
- \"-1 12:05\" to set current time to yesterday at 12:05 pm.

See `org-read-date' for more about how to specify the current
time."
  (interactive (list (org-read-date)))
  (hack-time-mode--current-time-do-freeze target-date)
  (message "%s" (format-time-string "current-time is: %Y-%m-%d %H:%M"
                                    (current-time))))
;; commands ends here
;; inner-program ends here


(provide 'hack-time-mode)


;;; hack-time-mode.el ends here
