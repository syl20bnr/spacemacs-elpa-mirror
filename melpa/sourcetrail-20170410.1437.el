;;; sourcetrail.el --- Communication with Sourcetrail                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: Andreas Stallinger <astallinger@sourcetrail.com>
;; Keywords: external, tool
;; Package-Version: 20170410.1437
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

;;emacs-sourcetrail
;;===========

;;emacs-sourcetrail is a plugin for Emacs to communicate with Sourcetrail_.

;;.. _Sourcetrail: https://sourcetrail.com

;;Install
;;-------

;;Usage
;;-----

;;From Sourcetrail to Emacs
;;~~~~~~~~~~~~~~~~~~~

;;* enable sourcetrail-mode in Emacs
;;* Right click in sourcetrail -> **Set IDE Curor**
;;* In the Emacs should now open the file and put the cursor in the position form sourcetrail.

;;From Emacs to Sourcetrail
;;~~~~~~~~~~~~~~~~~~~

;;* Navigate your cursor to the location in the text.
;;* Sent location to sourcetrail

;; ;+ Press **M-x** and enter **sourcetrail-send-loation**
;; ;+ bind **sourcetrail-send-location** to a key sequence and use it.

;;Preferences
;;-----------

;;* **M-x** customize
;;* search for sourcetrail
;;* 3 Settins should be displayed now

;;Emacs Sourcetrail Ip
;;~~~~~~~~~~~~~~

;;Ip address for the Tcp communcation, default is ``localhost``

;;Emacs Sourcetrail Port Sourcetrail
;;~~~~~~~~~~~~~~~~~~~~~~

;;Port Sourcetrail listens to, default is ``6667``

;;Emacs Sourcetrail Port Emacs
;;~~~~~~~~~~~~~~~~~~~~~~

;;Port Sourcetrail listens to, default is ``6666``


;;; Code:
(require 'subr-x)

(defgroup sourcetrail nil
  "Settings for the sourcetrail plugin."
  :group 'external)

(defcustom sourcetrail-port-sourcetrail 6667
  "Port Sourcetrail listens to."
  :group 'sourcetrail
  :type '(number))

(defcustom sourcetrail-port-emacs 6666
  "Port for listening to Sourcetrail."
  :group 'sourcetrail
  :type '(number))

(defcustom sourcetrail-ip "localhost"
  "Ip for communication with sourcetrail."
  :group 'sourcetrail
  :type '(string))

(defconst sourcetrail-server-name "sourcetrail-server")
(defconst sourcetrail-server-buffer "*-sourcetrail-server-buffer*")
(defvar sourcetrail-server nil)
(defvar sourcetrail-client nil)
(defvar sourcetrail-col nil)
(defvar sourcetrail-row nil)
(defvar sourcetrail-file nil)
(defvar sourcetrail-message nil)

(defun buildTokenLocationMessage nil
  "Building a formated message for sending to sourcetrail."
  (setq sourcetrail-col (number-to-string (current-column)))
  (setq sourcetrail-row (number-to-string (line-number-at-pos)))
  (setq sourcetrail-file (buffer-file-name))
  (setq sourcetrail-message (mapconcat 'identity (list "setActiveToken" sourcetrail-file sourcetrail-row sourcetrail-col) ">>"))
  (setq sourcetrail-message (mapconcat 'identity (list sourcetrail-message "<EOM>") "")))

(defun sourcetrail-send-ping nil
  "Sending ping to sourcetrail."
  (setq sourcetrail-client
	(open-network-stream "sourcetrail-client"
					   "*sourcetrail-client*" sourcetrail-ip sourcetrail-port-sourcetrail))
	(process-send-string sourcetrail-client "ping>>Emacs<EOM>"))

(defun sourcetrail-send-message(message)
  "Sending message to sourcetrail."
  (setq sourcetrail-client
    (open-network-stream "sourcetrail-client"
			"*sourcetrail-client*" sourcetrail-ip sourcetrail-port-sourcetrail))
    (process-send-string sourcetrail-client message))

(defun sourcetrail-server-start nil
  "Start tcp server."
  (unless sourcetrail-server
	(setq sourcetrail-server
	  (make-network-process :name (or sourcetrail-server-name "*sourcetrail-server")
							:server t
							:service (or sourcetrail-port-emacs 6666)
							:family 'ipv4
							:buffer sourcetrail-server-buffer
							:filter 'sourcetrail-listen-filter))
	(if sourcetrail-server
	  (set-process-query-on-exit-flag sourcetrail-server nil)
	  (error "Could not start server process"))
	(sourcetrail-send-ping)))

(defun sourcetrail-listen-filter (proc string)
  "Tcp listener filter.  No need for PROC.  STRING is the command send from sourcetrail."
  (process-buffer proc)
  (if (string-suffix-p "<EOM>" string)
	  (progn
	    ;; split message
		(setq sourcetrail-message (split-string (string-remove-suffix "<EOM>" string) ">>"))
		(when (string= (car sourcetrail-message) "moveCursor")
		  ;;moveCuror message
		  ;; filepath
		  (setq sourcetrail-file (nth 1 sourcetrail-message))
		  ;; row and col
		  (setq sourcetrail-row (string-to-number (nth 2 sourcetrail-message)))
		  (setq sourcetrail-col (string-to-number (nth 3 sourcetrail-message)))
		  ;; open file
		  (find-file sourcetrail-file)
		  ;; move cursor
		  (forward-line (- sourcetrail-row (line-number-at-pos)))
		  (move-to-column sourcetrail-col)
		)
		(when (string= (car sourcetrail-message) "ping")
		  (sourcetrail-send-ping)
		)
	)
	(message "Could not process the message from sourcetrail: %s" string)
	)
)

(defun sourcetrail-server-stop nil
  "Stops TCP Listener for Sourcetrail."
  (when sourcetrail-server
    (delete-process sourcetrail-server-name)
	(setq sourcetrail-server nil)))

;;;###autoload
(defun sourcetrail-send-location nil
  "Sends current location to Sourcetrail."
  (interactive)
  (sourcetrail-send-message (buildTokenLocationMessage))
)

;;;###autoload
(define-minor-mode sourcetrail-mode
  "Start/stop sourcetrail mode."
  :global t
  :lighter " sourcetrail"
  ; value of sourcetrail-mode is toggled before this implicitly
  (if sourcetrail-mode (sourcetrail-server-start) (sourcetrail-server-stop)))

(provide 'sourcetrail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sourcetrail.el ends here
