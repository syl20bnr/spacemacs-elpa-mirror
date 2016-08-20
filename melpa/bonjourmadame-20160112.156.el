;;; bonjourmadame.el --- Say "Hello ma'am!"

;; Time-stamp: <2015-09-14 12:06:14>
;; Copyright (C) 2015 Pierre Lecocq
;; Version: 0.6
;; Package-Version: 20160112.156

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

;; Display the image from bonjourmadame.fr
;; Updated every day at 10AM (on Europe/Paris timezone)
;;
;; Keys:
;;
;; - n: get the next image
;; - p: get the previous image
;; - h: hide the buffer (switch to the previous one)
;; - q: quit (kill the buffer)

;;;; Changelog:

;; v0.6: change base URL after the main domain outage - by ShadowMitia (Dimitri Belopopsky)
;; v0.5: display title, use rx and stick to XDG_CACHE_HOME standard - by Schnouki (Thomas Jost)
;; v0.4: display and time bug fixes
;; v0.3: add page navigation
;; v0.2: make it a major mode
;; v0.1: first release

;;;; Contributors:

;; - Schnouki (Thomas Jost)

;;; Code:

(require 'rx)
(require 'web-mode nil t)

(declare-function web-mode-dom-entities-replace "web-mode")

(defgroup bonjourmadame nil
  "Say \"Hello ma'am!\""
  :group 'image)

(defvar bonjourmadame--cache-dir (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/bonjourmadame"))
(defvar bonjourmadame--buffer-name "*Bonjour Madame*")
(defvar bonjourmadame--base-url "http://ditesbonjouralamadame.tumblr.com")
(defvar bonjourmadame--refresh-hour 10)
(defvar bonjourmadame--regexp
  (rx
   "<img" (1+ space)
   "src=\"" (group "http://" (1+ nonl) "tumblr.com" (1+ nonl) "." (or "png" "jpg" "jpeg" "gif")) "\""
   (1+ space)
   "alt=\"" (group (0+ (not (any "\"")))) "\""
   (0+ (not (any ">"))) ">"))
(defvar bonjourmadame--image-time nil)
(defvar bonjourmadame--image-url "")
(defvar bonjourmadame--image-title "")
(defvar bonjourmadame--previous-buffer nil)
(defvar bonjourmadame--page 1)

(define-derived-mode bonjourmadame-mode special-mode "bonjourmadame"
  "Say Hello ma'am!"
  :group 'bonjourmadame)

(define-key bonjourmadame-mode-map (kbd "n") 'bonjourmadame-next)
(define-key bonjourmadame-mode-map (kbd "p") 'bonjourmadame-prev)
(define-key bonjourmadame-mode-map (kbd "h") 'bonjourmadame-hide)
(define-key bonjourmadame-mode-map (kbd "q") 'bonjourmadame-quit)

(defun bonjourmadame--get-image-url ()
  "Get the image URL."
  (let ((url (concat bonjourmadame--base-url "/page/" (number-to-string bonjourmadame--page))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward bonjourmadame--regexp nil t)
      (setq bonjourmadame--image-url (match-string 1)
            bonjourmadame--image-title (match-string 2))
      (kill-buffer)))
  bonjourmadame--image-url)

(defun bonjourmadame--get-image-path ()
  "Get the local image path."
  (set-time-zone-rule "Europe/Paris")
  (setq bonjourmadame--image-time (current-time))
  (when (> bonjourmadame--page 1)
    (setq bonjourmadame--image-time (time-subtract bonjourmadame--image-time (seconds-to-time (* (- bonjourmadame--page 1) 60 60 24)))))
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (when (< current-hour bonjourmadame--refresh-hour)
      (message "Wait at most %dh to get a newer image!" (- bonjourmadame--refresh-hour current-hour))
      (setq bonjourmadame--image-time (time-subtract bonjourmadame--image-time (seconds-to-time (* bonjourmadame--refresh-hour 60 60))))))
  (concat
   (file-name-as-directory bonjourmadame--cache-dir)
   (format "%s.png" (format-time-string "%Y-%m-%d" bonjourmadame--image-time))))

(defun bonjourmadame--get-title ()
  "Get the image title."
  (let* ((title-path (concat (bonjourmadame--get-image-path) ".txt"))
         (title (if (file-exists-p title-path)
                    (with-temp-buffer
                      (save-excursion (insert-file-contents-literally title-path))
                      ;; Escaping HTML entities is hard!
                      (iso-sgml2iso (point-min) (point-max))
                      (html2text)
                      (when (featurep 'web-mode)
                        (web-mode-dom-entities-replace))
                      (buffer-substring-no-properties (point-min) (point-max)))
                  "")))
    (replace-regexp-in-string "^\s+" "" (replace-regexp-in-string "\s+\\'" "" (replace-regexp-in-string (rx (1+ (any blank " "))) " " title)))))

(defun bonjourmadame--download-image ()
  "Download and store the image."
  (unless (file-accessible-directory-p bonjourmadame--cache-dir)
    (make-directory bonjourmadame--cache-dir t))
  (let* ((image-path (bonjourmadame--get-image-path))
         (title-path (concat image-path ".txt")))
    (unless (file-exists-p image-path)
      (url-copy-file (bonjourmadame--get-image-url) image-path))
    (unless (file-exists-p title-path)
      (with-temp-file title-path
        (insert bonjourmadame--image-title)))))

(defun bonjourmadame--max-image-size (buf)
  "Determine the max size to use to display the image.
BUF must be the target buffer."
  (let* ((window (get-buffer-window buf))
         (frame (window-frame window)))
    (cons (window-pixel-width window)
          (- (window-pixel-height window) (* 3 (frame-char-height frame))))))
(defcustom bonjourmadame-max-image-size-function 'bonjourmadame--max-image-size
  "Function used to compute the max size for the image.
The return value must be a (max-width . max-height) cons cell."
  :type '(function))

(defun bonjourmadame--display-image ()
  "Display the image."
  (unless (display-graphic-p)
    (error "bonjourmadame is only available in graphical mode. You might want to execute `bonjourmadame-browse' instead."))
  (bonjourmadame--download-image)
  (let* ((image-path (bonjourmadame--get-image-path))
         (title (bonjourmadame--get-title))
         (buf (current-buffer))
         (max-size (if bonjourmadame-max-image-size-function
                       (apply bonjourmadame-max-image-size-function (list buf))
                     nil))
         (extra-params (when (and max-size
                                  (image-type-available-p 'imagemagick))
                         (list 'imagemagick nil
                               :max-width (car max-size)
                               :max-height (cdr max-size))))
         (image (apply 'create-image (cons image-path extra-params))))
    (when (not (equal (buffer-name buf) bonjourmadame--buffer-name))
      (setq bonjourmadame--previous-buffer buf))
    (switch-to-buffer bonjourmadame--buffer-name)
    (when buffer-read-only
      (setq inhibit-read-only t))
    (erase-buffer)
    (insert-image image)
    (insert (format "\n\n%s: %s" (format-time-string "%Y-%m-%d" bonjourmadame--image-time) title))
    (bonjourmadame-mode)
    (read-only-mode)
    (goto-char (point-min))
    (when max-size
      (add-hook 'window-configuration-change-hook 'bonjourmadame--display-image nil t))))

(defun bonjourmadame-next ()
  "Display the next image."
  (interactive)
  (setq bonjourmadame--page (+ bonjourmadame--page 1))
  (bonjourmadame--display-image))

(defun bonjourmadame-prev ()
  "Display the previous image."
  (interactive)
  (when (> bonjourmadame--page 1)
    (setq bonjourmadame--page (- bonjourmadame--page 1))
    (bonjourmadame--display-image)))

(defun bonjourmadame-hide ()
  "Hide the buffer."
  (interactive)
  (switch-to-buffer bonjourmadame--previous-buffer))

(defun bonjourmadame-quit ()
  "Quit."
  (interactive)
  (setq bonjourmadame--page 1)
  (setq bonjourmadame--image-time nil)
  (setq bonjourmadame--image-url "")
  (kill-buffer (get-buffer bonjourmadame--buffer-name))
  (switch-to-buffer bonjourmadame--previous-buffer)
  (message "Au revoir madame"))

;;;###autoload
(defun bonjourmadame-browse ()
  "Browse to the site."
  (interactive)
  (browse-url bonjourmadame--base-url))

;;;###autoload
(defun bonjourmadame ()
  "Say Hello ma'am!"
  (interactive)
  (bonjourmadame--display-image))

(provide 'bonjourmadame)

;;; bonjourmadame.el ends here
