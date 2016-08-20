;;; rand-theme.el --- Random Emacs theme at start-up!
;;
;; Filename: rand-theme.el
;; Description: Get a random theme
;; Author: Daniel Gopar
;; Maintainer: Daniel Gopar
;; Created: Tue Oct 20 22:21:57 2015 (-0700)
;; Version: 0.1
;; Package-Version: 20151219.1535
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/gopar/rand-theme
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This package gives the ability to load a random theme. The random
;; theme can be picked from a white list or you set a black list of
;; themes you never want to choose (For example, the stock themes).
;; The white list has higher precedence than the black list.
;; It also provides functions to iterate forward and backwards
;; through the defined themes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)

(defcustom rand-theme-unwanted nil
  "List of themes that you *don't* want to randomly select."
  :type 'sexp
  :group 'theme-list)

(defcustom rand-theme-wanted nil
  "List of themes that you *only* want to randomly select.
If this is non-nil then it will have a higher precedence than `rand-theme-unwanted'."
  :type 'sexp
  :group 'theme-list)

(defvar rand-theme-previous-position nil
  "Place holder for the position of the theme behind the current one.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
(defun rand-theme--get-theme-list ()
  "Return a list of themes to use"
  (if (null (or rand-theme-unwanted rand-theme-wanted))
      (error "Neither `rand-theme-unwanted' nor `rand-theme-wanted' have been set."))
  (let ((available-themes (custom-available-themes)) (theme nil))
    (if (null rand-theme-wanted)
        ;; Filter out unwanted themes
        (mapc (lambda (unwanted) (setq available-themes (remove unwanted available-themes))) rand-theme-unwanted)
      ;; No need to filter since we already have a list we want to use
      (setq available-themes rand-theme-wanted))
    ;; return themes to use
    available-themes))

(defun rand-theme--load-theme (theme)
  ""
  ;; Disable ALL themes
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Loaded Theme: %s" (symbol-name theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;;;###autoload
(defun rand-theme ()
  "Randomly pick a theme from `rand-theme-unwanted' or if non-nil from `rand-theme-wanted'.
Will raise error if both of these variables are nil."
  (interactive)
  (let ((available-themes (rand-theme--get-theme-list))
        (theme nil))
    ;; Randomly choose a theme
    (setq theme (nth (random (length available-themes)) available-themes))
    ;; Now load it
    (rand-theme--load-theme theme)))

;;;###autoload
(defun rand-theme-iterate ()
  "Iterate through the list of themes.
In case you want to go incremental."
  (interactive)
  (let ((available-themes (rand-theme--get-theme-list))
        (curr-pos nil)
        ;; current enabled theme. Only get first one. Ignore others
        (curr-theme (nth 0 (mapcar 'symbol-name custom-enabled-themes))))
    ;; Get the next theme in line
    (setq curr-pos (1+ (cl-position (intern curr-theme) available-themes)))
    (setq rand-theme-previous-position (1- curr-pos))
    ;; Make sure the 1+ didn't go over the highest possible index
    (if (>= curr-pos (length available-themes))
        (setq curr-pos 0))
    (setq curr-theme (nth curr-pos available-themes))
    (rand-theme--load-theme curr-theme)))

;;;###autoload
(defun rand-theme-iterate-backwards ()
  "Iterate backwards through list of themes.
In case you accidentally pass the theme you wanted."
  (interactive)
  (let ((available-themes (rand-theme--get-theme-list))
        (curr-theme nil))
    ;; If previous position is defined
    (if rand-theme-previous-position
        ;; Get the theme we should load
        (setq curr-theme (nth rand-theme-previous-position available-themes))
      ;; if previous is not defined then just get index of current theme
      ;; and subtract one from it and go to that index
      (setq rand-theme-previous-position
            (1- (cl-position (car custom-enabled-themes) available-themes)))
      ;; Get the theme we should load
      (setq curr-theme (nth rand-theme-previous-position available-themes)))
    ;; just update to the next previous-position value
    (if (>= 0 rand-theme-previous-position)
        (setq rand-theme-previous-position (1- (length available-themes)))
      (setq rand-theme-previous-position (1- rand-theme-previous-position)))
    ;; Load the theme
    (rand-theme--load-theme curr-theme)))

(provide 'rand-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rand-theme.el ends here
