;;; backlight.el --- backlight brightness adjustment -*- lexical-binding: t -*-

;; Copyright (C) 2018 Michael Schuldt

;; Author: Michael Schuldt <mbschuldt@gmail.com>
;; Version: 1.0
;; Package-Version: 20180616.1804
;; URL: https://github.com/mschuldt/backlight.el
;; Package-Requires: ((emacs "24.3"))
;; Keywords: hardware

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
;; 
;; A simple utility for setting backlight brightness on some
;; GNU/Linux systems using sysfs files.
;; 
;; This works like most system provided backlight brightness
;; controls but allows for increased resolution when the
;; brightness percentage nears zero.
;; 
;; USAGE
;; 
;;  M-x backlight
;;   Then use '<' or '>' to adjust backlight brightness, 'C-g' when done.
;; 
;;  M-x backlight-set-raw
;;   prompts for a value to write directly to the device file.


;;; Code:

(defgroup backlight nil
  "Set backlight brightness."
  :group 'backlight)

(defcustom backlight-large-inc-amount 10
  "Adjustment percentage used when brightness is above `backlight-threshold'."
  :type 'number
  :group 'backlight)

(defcustom backlight-small-inc-amount 1
  "Adjustment percentage used when brightness is below `backlight-threshold'."
  :type 'number
  :group 'backlight)

(defcustom backlight-threshold 10
  "Percentage level for using small brightness incrments."
  :type 'number
  :group 'backlight)

(defcustom backlight-sys-dir "/sys/class/backlight/"
  "Location of backlight device files."
  :type 'string
  :group 'backlight)

(defvar backlight--device nil
  "Filename of the backlight device.")

(defvar backlight--max-brightness nil
  "Max brightness value as reported by the backlight device.")

(defvar backlight--current-brightness nil
  "Current backlight brightness level.")

(defvar backlight--initialized nil
  "Non-nil when backlight device is found variables initialized.")

(defun backlight--filepath (file)
  "Create full filepath to backlight device attribute FILE."
  (concat backlight-sys-dir backlight--device "/" file))

(defun backlight--set (file value)
  "Write VALUE to backlight device FILE."
  (with-temp-buffer
    (insert (format "%s" value))
    (write-region nil nil (backlight--filepath file) nil 'silent)))

(defun backlight--get (file)
  "Read value from backlight device FILE."
  (with-temp-buffer
    (insert-file-contents-literally (backlight--filepath file))
    (string-to-number (buffer-string))))

(defun backlight--init ()
  "Find the backlight device file and read initial values."
  (let ((devices (cddr (and (file-exists-p backlight-sys-dir)
                            (directory-files backlight-sys-dir)))))
    (when (> (length devices) 1)
      ;; Don't know if this is actually possible
      (message "Warning: Multiple backlight devices. Using the first."))
    (if (null devices)
        (message "Error: Unable to find backlight device")
      (setq backlight--device (car devices))
      (setq backlight--max-brightness (backlight--get "max_brightness"))
      (setq backlight--current-brightness (backlight--get "actual_brightness"))
      (setq backlight--initialized t)))
  backlight--initialized)

(defun backlight--check ()
  "Verify initialization."
  (unless backlight--initialized
    (unless (backlight--init)
      (error "backlight initialization failed"))))

(defun backlight--current-percentage ()
  "Calculate the current brightness percentage."
  (* (/ backlight--current-brightness
        (* backlight--max-brightness 1.0))
     100))

(defun backlight--set-brightness (value)
  "Set and verify the backlight brightness to raw VALUE."
  (backlight--set "brightness" value)
  (let ((actual (backlight--get "actual_brightness")))
    (when (not (equal actual value))
      (error "Failed to set backlight brightness"))
    (setq backlight--current-brightness actual)))

(defun backlight--from-percent (percent)
  "Convert a PERCENT to a brightness value the device accepts."
  (floor (* (/ (* percent 1.0) 100) backlight--max-brightness)))

(defun backlight--get-inc-amount ()
  "Return the amount by which to adjust the brightness."
  (if (<= (backlight--current-percentage) backlight-threshold)
      backlight-small-inc-amount
    backlight-large-inc-amount))

(defun backlight--adjust (percent)
  "Adjust the backlight brightness by PERCENT, which can be negative."
  (backlight--check)
  (let* ((threshold (backlight--from-percent backlight-threshold))
         (was-above-theshold (>= backlight--current-brightness threshold))
         (new (+ backlight--current-brightness
                 (backlight--from-percent percent))))
    (when (and was-above-theshold
               (< (- new threshold) 1))
      (setq new (1- threshold)))
    (setq new (min new backlight--max-brightness))
    (setq new (max new 0))
    (backlight--set-brightness new)))

(defun backlight--minibuf-update (&optional decrement)
  "Do a brightess increment, or DECREMENT, and update minibuffer."
  (if decrement
      (backlight-dec)
    (backlight-inc))
  (move-beginning-of-line 1)
  (kill-line)
  (insert (format "%%%s" (floor (backlight--current-percentage)))))

(defvar backlight--minibuffer-keymap
  (let ((map (copy-keymap minibuffer-local-map))
        (inc-keys '("right" ">" "." "+"))
        (dec-keys '("left" "<" "," "-")))
    (dolist (key inc-keys)
      (define-key map (kbd key)
        (lambda ()
          (interactive)
          (backlight--minibuf-update))))
    (dolist (key dec-keys)
      (define-key map (kbd key)
        (lambda ()
          (interactive)
          (backlight--minibuf-update t))))
    map)
  "Key map used for interactive minibuffer brightness adjustment.")

;;;###autoload
(defun backlight ()
  "Interactively adjust the backlight brightness in the minibuffer."
  (interactive)
  (backlight--check)
  (read-from-minibuffer "brightness: "
                        (format "%%%s"
                                (floor (backlight--current-percentage)))
                        backlight--minibuffer-keymap))

;;;###autoload
(defun backlight-inc ()
  "Increment the backlight brightness."
  (interactive)
  (backlight--adjust (backlight--get-inc-amount)))

;;;###autoload
(defun backlight-dec ()
  "Decrements the backlight brightness."
  (interactive)
  (backlight--adjust (- (backlight--get-inc-amount))))

;;;###autoload
(defun backlight-set-raw ()
  "Interactively set the raw backlight brightness value."
  (interactive)
  (backlight--check)
  (let ((new (read-from-minibuffer
              (format "raw brightness (%s max): "
                      backlight--max-brightness)
              (number-to-string backlight--current-brightness))))
    (backlight--set-brightness (number-to-string new))))

(backlight--init)

(provide 'backlight)

;;; backlight.el ends here
