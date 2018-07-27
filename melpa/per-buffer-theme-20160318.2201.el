;;; per-buffer-theme.el --- Change theme according to buffer name or major mode.

;; Copyright (C) 2015-6 Free Software Foundation, Inc.

;; Author: Iñigo Serna <inigoserna@gmail.com>
;; URL: https://bitbucket.com/inigoserna/per-buffer-theme.el
;; Package-Version: 20160318.2201
;; Version: 1.5
;; Keywords: themes
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; `per-buffer-theme.el' is an Emacs library that automatically changes
;; the global theme according to buffer name or major mode.
;;
;; If buffer name matches any of `per-buffer-theme/ignored-buffernames-regex'
;; no theme change occurs.
;;
;; Customizable variable `per-buffer-theme/themes-alist' contains the
;; association between themes and buffer name or major modes.
;;
;; Special `notheme' theme name can be used to make unload all themes and use
;; Emacs default theme.
;;
;; If no theme matches then it will load the theme stored in
;; `per-buffer-theme/default-theme' variable.
;;
;; There are two different methods in which buffer and theme can be checked.
;; It is controlled by customizable boolean `per-buffer-theme/use-timer':
;;
;; - 't' will use a timer, triggered every `per-buffer-theme/timer-idle-delay'
;;   seconds.  This is the default as it works smoothly.
;;   If it slows down Emacs a bit choose a bigger delay value.
;;
;; - 'nil' uses a function advice to `select-window' so it could introduce
;;   some Emacs windows flickering when switching buffers due to how
;;   `select-window' internally works.

;;; Updates:

;; 2015/04/12 Initial version.
;; 2015/04/13 Changed `per-buffer-theme/theme-list' data type from plist
;;            to alist to make customization easier.
;;            Make code comply with (some) Emacs Lisp Code Conventions:
;;            - added public function to unload hook
;; 2015/09/25 Use advice function instead of hooks, it's more robust.
;; 2015/10/13 As themes are cumulative, remove previous theme definitions
;;            before applying new one.
;; 2016/03/18 Don't update theme if temporary or hidden buffers.
;;            Added alternative timer-base method to check buffer and theme.
;;            Thanks to Clément Pit--Claudel and T.V. Raman for the suggestions.


;;; Code:
(require 'cl-lib)

;;; Variables
(defvar pbt/current-theme nil
  "Theme in use.")

;;; Customization
(defgroup per-buffer-theme nil
  "Change theme according to buffer name or major mode."
  :link '(emacs-library-link :tag "Source Lisp File" "per-buffer-theme.el")
  :prefix "per-buffer-theme/"
  :group 'customize)

(defcustom per-buffer-theme/default-theme 'deeper-blue
  "Default theme to be used if no matching is found."
  :type 'symbol
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/ignored-buffernames-regex '("*[Mm]ini" "*[Hh]elm" "*[Oo]rg" "*[Cc]alendar" "*[Cc]alc" "*SPEEDBAR*" "*NeoTree*")
  "If current buffer name matches one of these it won't change the theme."
  :type '(repeat string)
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/themes-alist
      '(((:theme . notheme)
         (:buffernames . ("*eww" "*w3m" "*mu4e"))
         (:modes . (eww-mode w3m-mode cfw:calendar-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode mu4e-about-mode mu4e-update-mode)))
        ((:theme . adwaita)
         (:buffernames . ("*Help*"))
         (:modes . (nil))))
  "An alist with default associations (theme buffernames modes).
Special `notheme' theme can be used to unload all themes."
  :type '(repeat alist)
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/use-timer t
  "Use timer (t, default) or check buffer when switching (nil)."
  :type 'boolean
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/timer-idle-delay 0.5
  "Number of seconds between buffer and theme checks."
  :type 'float
  :group 'per-buffer-theme)


;;; Internal functions
(defun pbt~match-theme (buffer)
  "Return theme if BUFFER name or major-mode match in
`per-buffer-theme/themes-alist' or nil."
  (let ((alist per-buffer-theme/themes-alist)
        newtheme)
    ;; (message "THEMES: %s" (prin1-to-string alist))
    (while alist
      (let* ((elm (pop alist))
             (theme (cdr (assoc :theme elm)))
             (buffernames (cdr (assoc :buffernames elm)))
             (modes (cdr (assoc :modes elm))))
        ;; (message "Theme: %s" (prin1-to-string theme))
        ;; (message "    Buffer names: %s" (prin1-to-string buffernames))
        ;; (message "    Major modes: %s" (prin1-to-string modes))
        (when (and (car buffernames) (cl-some (lambda (regex) (string-match regex (buffer-name buffer))) buffernames))
          ;; (message "=> Matched buffer name with regex '%s' => Theme: %s " (match-string 0 (buffer-name buffer)) theme)
          (setq newtheme theme
                alist nil))
        (when (member major-mode modes)
          ;; (message "=> Matched with major mode '%s' => Theme: %s " major-mode theme)
          (setq newtheme theme
                alist nil))))
    newtheme))

;;; Public interface
(defun per-buffer-theme/change-theme-if-buffer-matches (&optional buffer)
  "Change theme according to BUFFER major mode or name.
Don't do anything if buffer name matches in
`per-buffer-theme/ignored-buffernames-regex'.
Search for theme matches in `per-buffer-theme/themes-alist'
customizable variable.
If none is found uses default theme stored in `per-buffer-theme/default-theme'.
Special `notheme' theme can be used to disable all loaded themes."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let ((bufname (buffer-name buffer))
        theme)
    (when (and (not (string-prefix-p " " bufname))
               (get-buffer-window buffer)
               (cl-notany (lambda (regex) (string-match regex bufname)) per-buffer-theme/ignored-buffernames-regex))
      (setq theme (pbt~match-theme buffer))
      ;; (message "=> Returned theme: %S" theme)
      (unless (equal pbt/current-theme theme)
        ; as themes are cumulative, remove previous theme definitions before applying new one
        (mapc #'disable-theme custom-enabled-themes)
        (cond
         ((equal theme 'notheme)
          t) ; mapc #'disable... already executed above
         ((equal theme nil)
          (load-theme per-buffer-theme/default-theme t))
         (t
          (load-theme theme t)))
        (setq pbt/current-theme theme)))))


;;; Initialiation
(defvar pbt~timer nil
  "Private variable to store idle timer.")

(defun per-buffer-theme/advice-function (window &optional norecord)
  "Advice function to `select-window'."
  (per-buffer-theme/change-theme-if-buffer-matches (window-buffer window)))

;;;###autoload
(defun per-buffer-theme/enable ()
  "Enable `per-buffer-theme' package."
  (interactive)
  (if per-buffer-theme/use-timer
      (setq pbt~timer (run-with-idle-timer per-buffer-theme/timer-idle-delay t #'per-buffer-theme/change-theme-if-buffer-matches))
    (advice-add 'select-window :before 'per-buffer-theme/advice-function))
  (message "per-buffer-theme package activated."))

;;;###autoload
(defun per-buffer-theme/disable ()
  "Disable `per-buffer-theme' package."
  (interactive)
  (if per-buffer-theme/use-timer
      (progn
        (cancel-timer pbt~timer)
        (setq pbt~timer nil))
    (advice-remove 'select-window 'per-buffer-theme/advice-function))
  (message "per-buffer-theme package disabled."))

(per-buffer-theme/enable)
;; (global-set-key '[C-f3] 'per-buffer-theme/change-theme-if-buffer-matches)

(provide 'per-buffer-theme)
;;; per-buffer-theme.el ends here
