;;; anybar.el --- Control AnyBar from Emacs

;; Copyright (c) 2016  Christopher Shea

;; Author: Christopher Shea <cmshea@gmail.com>
;; Version: 0.1.1
;; Package-Version: 20160816.721
;; Keywords: anybar

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Commentary:

;; AnyBar is an application that puts an indicator in the menubar in
;; OS X. This package lets you interact with that indicator from
;; Emacs. See: https://github.com/tonsky/AnyBar

;; Basic usage:
;;
;;   (require 'anybar)
;;
;; Start AnyBar:
;;
;;   (anybar-start)
;;
;; Set indicator to a color:
;;
;;   (anybar-set "red")
;;
;; Quit AnyBar:
;;
;;   (anybar-quit)
;;
;; Those functions also take an optional argument to specify a port
;; number, if you want to run multiple instances or use a different
;; port than AnyBar's default, 1738.
;;
;; `anybar-set' will complain if you try to set the indicator to an
;; invalid style, which is anything outside of the default styles (see
;; `anybar-styles') or any custom images set in "~/.AnyBar". To
;; refresh the list of images anybar.el knows about, call
;; `anybar-images-reset'.
;;
;; These functions may be called interactively.
;;
;; If you have installed AnyBar to a location other than
;; /Applications/AnyBar.app, you'll need to customize
;; `anybar-executable-location' so that `anybar-start' may succeed.
;;
;; Enjoy!

;;; Code:

(defgroup anybar nil
  "Control AnyBar from Emacs"
  :group 'external
  :link '(url-link "https://github.com/tie-rack/anybar-el"))

(defcustom anybar-executable-location
  "/Applications/AnyBar.app"
  "The location of the AnyBar.app"
  :type 'string
  :safe #'stringp)

(defconst anybar-default-port
  1738
  "The default port AnyBar runs on.")

(defconst anybar-styles
  (list "white"
        "red"
        "orange"
        "yellow"
        "green"
        "cyan"
        "blue"
        "purple"
        "black"
        "question"
        "exclamation")
  "The built-in styles for AnyBar.")

(defvar anybar-images nil
  "Images available to set as the AnyBar style.")

(defun anybar-images-reset ()
  "Sets anybar-images to a list of images available for AnyBar."
  (interactive)
  (setq anybar-images
        (and (file-directory-p "~/.AnyBar")
             (mapcar
              (lambda (filename)
                (save-match-data
                  (and (string-match "\\(.*?\\)\\(_alt\\)?\\(@2x\\)?.png$" filename)
                       (match-string 1 filename))))
              (directory-files "~/.AnyBar" nil "\.png$"))))
  (delete-dups anybar-images))

(anybar-images-reset)

(defun anybar--read-style ()
  (completing-read "Style: "
                   (append anybar-styles anybar-images)))

(defun anybar--read-port ()
  (read-number "Port: " anybar-default-port))

;;;###autoload
(defun anybar-send (command &optional port)
  "Sends the command to the AnyBar instance running on port."
  (interactive (list (read-string "Command: ")
                     (anybar--read-port)))
  (let* ((port (or port anybar-default-port))
         (conn (make-network-process
                :name "anybar"
                :type 'datagram
                :host 'local
                :service port)))
    (process-send-string conn command)
    (delete-process conn)))

;;;###autoload
(defun anybar-set (style &optional port)
  "Sets the AnyBar running on the specified port to style. Will
warn if the style is not valid."
  (interactive (list (anybar--read-style)
                     (anybar--read-port)))
  (let ((port (or port anybar-default-port))
        (available-styles (append anybar-styles anybar-images)))
    (if (member style available-styles)
        (anybar-send style port)
      (display-warning "AnyBar" (format "Not a style: %s" style)))))

;;;###autoload
(defun anybar-quit (&optional port)
  "Quit the AnyBar instance running on the specified port."
  (interactive (list (anybar--read-port)))
  (let ((port (or port anybar-default-port)))
    (anybar-send "quit" port)))

;;;###autoload
(defun anybar-start (&optional port)
  "Start an instance of AnyBar on the specified port."
  (interactive (list (anybar--read-port)))
  (let* ((port (or port anybar-default-port))
         (command (format "ANYBAR_PORT=%d open -n %s"
                          port
                          anybar-executable-location)))
    (shell-command command)))

(provide 'anybar)
;;; anybar.el ends here
