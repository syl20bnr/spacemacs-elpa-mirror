;;; ibuffer-rcirc.el --- Ibuffer integration for rcirc

;; Copyright (C) 2015  Fabián Ezequiel Gallina

;; Author: Fabián Ezequiel Gallina <fgallina@gnu.org>
;; Keywords: buffer, convenience, comm
;; Package-Version: 20150215.2118
;; Package-Requires: ((cl-lib "0.2"))
;; X-URL: https://github.com/fgallina/ibuffer-rcirc
;; URL: https://github.com/fgallina/ibuffer-rcirc
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

;; Provides rcirc activity tracking and server filtering for ibuffer.

;;; Installation:

;; Add this to your .emacs:

;; (add-to-list 'load-path "/folder/containing/file")
;; (require 'ibuffer-rcirc)

;;; Usage:

;; To group buffers by irc server:

;;   M-x ibuffer-rcirc-set-filter-groups-by-server

;; To sort them by activity status (nick/keyword mentions, unread messages):

;;   M-x ibuffer-do-sort-by-rcirc-activity-status

;; To filter by server:

;;   M-x ibuffer-filter-by-rcirc-server

;; If you'd like to see activity status as a `ibuffer' column, include
;; `rcirc-activity-status-one-char', `rcirc-activity-status' or
;; `rcirc-activity-status-mini' in `ibuffer-formats':

;; (setq ibuffer-formats
;;       '((mark modified read-only rcirc-activity-status-one-char " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " "
;;               ;; (rcirc-activity-status 20 18 :left) " "
;;               ;; (rcirc-activity-status-mini 5 3 :center) " "
;;               filename-and-process)))

;; The `rcirc-activity-status-one-char' column is ideal to be appended
;; to the default ibuffer first column (that shows the current mark
;; and modified/read-only flags).  The example above does that.

;; Finally, If you want to combine the server filter groups with your
;; own, you can use `ibuffer-rcirc-generate-filter-groups-by-server'.

;;; Code:
(require 'cl-lib)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'rcirc)


(defun ibuffer-rcirc--server-buffers ()
  "Return the list of current server buffers."
  (let ((rcirc-buffers
         (cl-remove-if-not
          (lambda (buffer)
            (with-current-buffer buffer
              (and rcirc-server-buffer
                   (eq major-mode 'rcirc-mode))))
          (buffer-list))))
    (cl-remove-duplicates
     (mapcar
      (lambda (buffer)
        (with-current-buffer buffer
          rcirc-server-buffer))
      rcirc-buffers)
     :test #'equal)))

;;;###autoload
(defun ibuffer-rcirc-generate-filter-groups-by-server ()
  "Create a set of ibuffer filter groups based on the current irc servers.
Use this to programatically create your own filter groups."
  (mapcar
   (lambda (server-buffer)
     (list
      (buffer-name server-buffer)
      `(predicate . (equal rcirc-server-buffer ,server-buffer))))
   (ibuffer-rcirc--server-buffers)))

;;;###autoload
(defun ibuffer-rcirc-set-filter-groups-by-server ()
  "Set filter group by rcirc servers."
  (interactive)
  (setq ibuffer-filter-groups
        (ibuffer-rcirc-generate-filter-groups-by-server))
  (ibuffer-update nil t))

(defun ibuffer-rcirc--activity-status-strings (buffer)
  "Return rcirc activiy status string for BUFFER.
Returns a cons cell where the car is a string for the mini
status and the cdr is a string for the full status."
  (with-current-buffer buffer
    (if (and rcirc-server-buffer)
        (cond
         ((memq 'nick rcirc-activity-types)
          (cons "m" "Nickname mentioned."))
         ((memq 'keyword rcirc-activity-types)
          (cons "k" "Keyword mentioned."))
         (rcirc-activity-types
          (cons "*" "Unread messages."))
         (t (cons " " "No activity.")))
      (cons " " " "))))

(define-minor-mode ibuffer-rcirc-track-minor-mode
  "Global minor mode for tracking activity status in rcirc buffers."
  :init-value nil
  :lighter ""
  :global t
  :group 'ibuffer-rcirc
  (or global-mode-string (setq global-mode-string '("")))
  (if ibuffer-rcirc-track-minor-mode
      (add-hook 'window-configuration-change-hook
                'rcirc-window-configuration-change)
    (remove-hook 'window-configuration-change-hook
		 'rcirc-window-configuration-change)))

;;;###autoload (autoload 'ibuffer-make-column-rcirc-activity-status-one-char "ibuffer-rcirc")
(define-ibuffer-column rcirc-activity-status-one-char
  (:name "I")
  (progn
    (ibuffer-rcirc-track-minor-mode 1)
    (car
     (ibuffer-rcirc--activity-status-strings (current-buffer)))))

;;;###autoload (autoload 'ibuffer-make-column-rcirc-activity-status-mini "ibuffer-rcirc")
(define-ibuffer-column rcirc-activity-status-mini
  (:name "IRC")
  (progn
    (ibuffer-rcirc-track-minor-mode 1)
    (car
     (ibuffer-rcirc--activity-status-strings (current-buffer)))))

;;;###autoload (autoload 'ibuffer-make-column-rcirc-activity-status "ibuffer-rcirc")
(define-ibuffer-column rcirc-activity-status
  (:name "IRC activity")
  (progn
    (ibuffer-rcirc-track-minor-mode 1)
    (cdr
     (ibuffer-rcirc--activity-status-strings (current-buffer)))))

;;;###autoload (autoload 'ibuffer-filter-by-rcirc-server "ibuffer-rcirc")
(define-ibuffer-filter rcirc-server
  "Toggle current view to buffers matching QUALIFIER server name."
  (:description
   "vc root dir"
   :reader
   (completing-read
    "Filter by server: "
    (mapcar #'buffer-name (ibuffer-rcirc--server-buffers))
    nil t))
  (with-current-buffer buf
    (when rcirc-server-buffer
      (equal qualifier (buffer-name rcirc-server-buffer)))))

;;;###autoload (autoload 'ibuffer-do-sort-by-rcirc-activity-status "ibuffer-rcirc")
(define-ibuffer-sorter rcirc-activity-status
  "Sort the buffers by their rcirc status."
  (:description "rcirc status")
  (let ((status-a (car (ibuffer-rcirc--activity-status-strings (car a))))
        (status-b (car (ibuffer-rcirc--activity-status-strings (car b)))))
    (if (and (string= status-a status-b)
             (string= status-a " "))
        ;; When no activity happens, sort by buffer name.
        (string-lessp (buffer-name (car a)) (buffer-name (car b)))
      (string-lessp status-b status-a))))


(provide 'ibuffer-rcirc)
;;; ibuffer-rcirc.el ends here
