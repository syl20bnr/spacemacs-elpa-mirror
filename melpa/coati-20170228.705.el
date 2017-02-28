;;; coati.el --- Communication with Coati                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: Andreas Stallinger <astallinger@coati.io>
;; Keywords: external, tool
;; Package-Version: 20170228.705
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; License:

;; This file is not part of GNU Emacs

;; The MIT License (MIT)
;; Copyright (c) 2016 Coati Software OG

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Commentary:

;;emacs-coati
;;===========

;;emacs-coati is a plugin for Emacs to communicate with Coati_.

;;.. _Coati: https://coati.io

;;Install
;;-------

;;Usage
;;-----

;;From Coati to Emacs
;;~~~~~~~~~~~~~~~~~~~

;;* enable coati-mode in Emacs
;;* Right click in coati -> **Set IDE Curor**
;;* In the Emacs should now open the file and put the cursor in the position form coati.

;;From Emacs to Coati
;;~~~~~~~~~~~~~~~~~~~

;;* Navigate your cursor to the location in the text.
;;* Sent location to coati

;; ;+ Press **M-x** and enter **coati-send-loation**
;; ;+ bind **coati-send-location** to a key sequence and use it.

;;Preferences
;;-----------

;;* **M-x** customize
;;* search for coati
;;* 3 Settins should be displayed now

;;Emacs Coati Ip
;;~~~~~~~~~~~~~~

;;Ip address for the Tcp communcation, default is ``localhost``

;;Emacs Coati Port Coati
;;~~~~~~~~~~~~~~~~~~~~~~

;;Port Coati listens to, default is ``6667``

;;Emacs Coati Port Emacs
;;~~~~~~~~~~~~~~~~~~~~~~

;;Port Coati listens to, default is ``6666``


;;; Code:
(require 'subr-x)

(defgroup coati nil
  "Settings for the coati plugin."
  :group 'external)

(defcustom coati-port-coati 6667
  "Port Coati listens to."
  :group 'coati
  :type '(number))

(defcustom coati-port-emacs 6666
  "Port for listening to Coati."
  :group 'coati
  :type '(number))

(defcustom coati-ip "localhost"
  "Ip for communication with coati."
  :group 'coati
  :type '(string))

(defconst coati-server-name "coati-server")
(defconst coati-server-buffer "*-coati-server-buffer*")
(defvar coati-server nil)
(defvar coati-client nil)
(defvar coati-col nil)
(defvar coati-row nil)
(defvar coati-file nil)
(defvar coati-message nil)

(defun buildTokenLocationMessage nil
  "Building a formated message for sending to coati."
  (setq coati-col (number-to-string (current-column)))
  (setq coati-row (number-to-string (line-number-at-pos)))
  (setq coati-file (buffer-file-name))
  (setq coati-message (mapconcat 'identity (list "setActiveToken" coati-file coati-row coati-col) ">>"))
  (setq coati-message (mapconcat 'identity (list coati-message "<EOM>") "")))

(defun coati-send-ping nil
  "Sending ping to coati."
  (setq coati-client
	(open-network-stream "coati-client"
					   "*coati-client*" coati-ip coati-port-coati))
	(process-send-string coati-client "ping>>Emacs<EOM>"))

(defun coati-send-message(message)
  "Sending message to coati."
  (setq coati-client
    (open-network-stream "coati-client"
			"*coati-client*" coati-ip coati-port-coati))
    (process-send-string coati-client message))

(defun coati-server-start nil
  "Start tcp server."
  (unless coati-server
	(setq coati-server
	  (make-network-process :name (or coati-server-name "*coati-server")
							:server t
							:service (or coati-port-emacs 6666)
							:family 'ipv4
							:buffer coati-server-buffer
							:filter 'coati-listen-filter))
	(if coati-server
	  (set-process-query-on-exit-flag coati-server nil)
	  (error "Could not start server process"))
	(coati-send-ping)))

(defun coati-listen-filter (proc string)
  "Tcp listener filter.  No need for PROC.  STRING is the command send from coati."
  (process-buffer proc)
  (if (string-suffix-p "<EOM>" string)
	  (progn
	    ;; split message
		(setq coati-message (split-string (string-remove-suffix "<EOM>" string) ">>"))
		(when (string= (car coati-message) "moveCursor")
		  ;;moveCuror message
		  ;; filepath
		  (setq coati-file (nth 1 coati-message))
		  ;; row and col
		  (setq coati-row (string-to-number (nth 2 coati-message)))
		  (setq coati-col (string-to-number (nth 3 coati-message)))
		  ;; open file
		  (find-file coati-file)
		  ;; move cursor
		  (forward-line (- coati-row (line-number-at-pos)))
		  (move-to-column coati-col)
		)
		(when (string= (car coati-message) "ping")
		  (coati-send-ping)
		)
	)
	(message "Could not process the message from coati: %s" string)
	)
)

(defun coati-server-stop nil
  "Stops TCP Listener for Coati."
  (when coati-server
    (delete-process coati-server-name)
	(setq coati-server nil)))

;;;###autoload
(defun coati-send-location nil
  "Sends current location to Coati."
  (interactive)
  (coati-send-message (buildTokenLocationMessage))
)

;;;###autoload
(define-minor-mode coati-mode
  "Start/stop coati mode."
  :global t
  :lighter " coati"
  ; value of coati-mode is toggled before this implicitly
  (if coati-mode (coati-server-start) (coati-server-stop)))

(provide 'coati)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; coati.el ends here
