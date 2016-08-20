;;; jack-connect.el --- Manage jack connections within Emacs

;; Copyright (C) 2014 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.1
;; Package-Version: 20141207.407

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

;; jack-connect and jack-disconnect allow to manage connections of
;; jackd audio server from Emacs minibuffer.

;;; Code:

(require 'ido)

;;; helper functions
(defun jack-port-get (port tag)
  "Helper function to get properties from port alist.
PORT is an element of the alist.
TAG is a symbol."
  (cdr (assq tag port)))

(defun jack-port-filter (pred alist)
  "Filter ports according to PRED.
PRED is a function of a port record returning boolean.
ALIST is an annotated alist of ports as produced by jack_lsp."
  (let ((res nil))
    (dolist (port alist res)
      (when (funcall pred port)
	(setq res (cons port res))))))

(defun jack-connectable-port (port alist)
  "Select the ports that can be connected to PORT from ALIST."
  (let ((port-connections (jack-port-get port 'connections))
	(res nil)
	(port-type (jack-port-get port 'type))
	(match-str (if (string-match "input," (jack-port-get port 'properties)) "output," "input,")))
    (dolist (oprt alist res)
      (when (and (not (equal (car oprt) (car port)))
		 (equal (jack-port-get oprt 'type) port-type)
		 (string-match match-str (jack-port-get oprt 'properties))
		 (not (member (car oprt) port-connections)))
	(setq res (cons (car oprt) res))))))

(defun jack-lsp ()
  "Build a port alist parsing the output of jack_lsp."
  (let ((lines (reverse (process-lines "jack_lsp" "-ctp")))
	(res) (port-type) (connections) (properties) (port-name))
    (let ((ports
	   (dolist (line lines res )
	     (cond ((string-match "^ \\{3\\}\\(.*\\)" line)
		    (let ((connection (replace-match "\\1" nil nil line)))
		      (setq connections (cons connection connections))))
		   ((string-match "^[ \t]+properties: \\(.*\\)" line)
		    (let ((property (replace-match "\\1" nil nil line)))
		      (setq properties property)))
		   ((string-match "^[ \t]+\\(.*\\)" line)
		    (setq port-type (replace-match "\\1" nil nil line)))
		   (t
		    (let ((alist (cons line
				       (list
					(cons 'connections connections)
					(cons 'type port-type)
					(cons 'properties properties)))))
		      (setq res (cons alist res))
		      (setq connections nil)
		      (setq port-type nil)
		      (setq properties nil)
		      ))))))
      (dolist (port ports)
      	(setcdr port (push (cons 'connectable (jack-connectable-port port ports))
			   (cdr port))))
      ports)))

(defun jack-connect (port1 port2)
  "Connect PORT1 to PORT2."
  (interactive
   (let ((from-ports (jack-port-filter (lambda (p) (jack-port-get p 'connectable)) (jack-lsp))))
     (if from-ports
	 (let* 	((from-port-string (ido-completing-read "Output port: " (mapcar 'car from-ports)))
		 (from-port (assoc from-port-string from-ports))
		 (to-ports (jack-port-get from-port 'connectable))
		 (to-port-string (ido-completing-read
				  (format "Connect %s (%s) to: " from-port-string (jack-port-get from-port 'type))
				  to-ports)))
	   (list from-port-string to-port-string))
       (progn (message "No port can be connected")
	      (list nil nil)))))
  (when port1
   (call-process "jack_connect" nil nil nil port1 port2)))

(defun jack-disconnect (port1 port2)
  "Disconnect the two connected ports PORT1 and PORT2."
  (interactive
   (let ((from-ports (jack-port-filter (lambda (p) (jack-port-get p 'connections)) (jack-lsp))))
     (if from-ports
	 (let* ((from-port-string (ido-completing-read "Disconnect port: " (mapcar 'car from-ports)))
		(from-port (assoc from-port-string from-ports))
		(to-ports (jack-port-get from-port 'connections))
		(to-port-string (ido-completing-read "From port: " to-ports)))
	   (list from-port-string to-port-string))
       (progn (message "No port can be disconnected")
	      (list nil nil)))))
  (when port1
   (call-process "jack_disconnect" nil nil nil port1 port2)))


(defun jack-disconnect-all-from (from connections)
  "Disconnect all the ports connected to FROM port.
CONNECTIONS is the list of ports connected to FROM."
  (interactive
   (let ((from-ports (jack-port-filter (lambda (p) (jack-port-get p 'connections)) (jack-lsp))))
     (if from-ports
	 (let ((from-port-string (ido-completing-read "Disconnect all connections from port: "
						      (mapcar 'car from-ports))))
	   (if (yes-or-no-p (format "Disconnecting all connections from %s. Are you sure"
				      from-port-string))
	       (list from-port-string (jack-port-get (assoc from-port-string from-ports) 'connections) )
	     (list nil nil)))
       (progn (message "No port can be disconnected")
	      (list nil nil)))))
  (when from
    (dolist (to connections)
      (jack-disconnect from to))))


;; (defvar jackd-default-driver "alsa")

;; (defun jack-start ()
;;   (interactive)
;;   (start-process "jack" (get-buffer-create "*jackd-output*") "jackd" "-d" jackd-default-driver)
;;   (switch-to-buffer-other-window "*jackd-output*" )
;;   )

;; (defun jack-stop ()
;;   (interactive)
;;   (stop-process "jack"))

(provide 'jack-connect)

;;; jack-connect.el ends here


