;;; gnomenm.el --- Emacs interface to Gnome nmcli command

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, hardware
;; Package-Version: 20150316.1918
;; URL: http://github.com/nicferrier/emacs-nm
;; Version: 0.0.8
;; Package-requires: ((s "1.9.0")(dash "2.3.0")(kv "0.0.19"))

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

;; nmcli is a pain to use so here's a simple Emacs interface.

;;; Code:

(require 's)
(require 'dash)

(defvar gnomenm/enabled nil
  "Whether gnomenm is enabled or not.")

(defun gnomenm/list ()
  "List the connections.

Produces a list like:

 (\"PGGuest\" \"03a11865-3953-4052-aa67-34bc891d3c61\" \"802-11-wireless\" \"Thu 13 Feb 2014 14:12:27 GMT\")
 (\"Norwegian Internet Access 9ca4914a-8feb-45bf-bf90-ea472d2ee98a\" \"802-11-wireless\" \"Wed 20 Nov 2013 17:45:45 GMT\")
 (\"GIB_PUBLIC\" \"631d8f69-1574-4687-9a00-18d29386d2c4\" \"802-11-wireless\" \"Wed 23 Oct 2013 19:19:29 BST\")
 (\"bwinparty\" \"23534f3e-f5c5-4ec8-86a9-7c3f8866d6c7\" \"vpn\" \"Sat 15 Feb 2014 08:03:32 GMT\")
 (\"SIL\" \"d047093c-b6c8-42db-8d3e-7888bbd39a21\" \"802-11-wireless\" \"never\")
 (\"NETGEAR_EXT\" \"3eab3930-7e67-402c-9fec-471cd062c97d\" \"802-11-wireless\" \"Tue 10 Dec 2013 12:14:26 GMT\")
"
  (cdr
   (->> (split-string (shell-command-to-string "nmcli con list") "\n")
     (-keep (lambda (l)
              (->> (split-string l "  ")
                (-keep (lambda (f)
                         (let ((a (s-trim f)))
                           (when (not (equal a "")) a))))))))))

(defun gnomenm/enable ()
  "Turn on WIFI."
  (shell-command-to-string "nmcli -t -f net-enabled nm wifi on")
  (message "gnomenm wifi enabled")
  (setq gnomenm/enabled t))

(defun gnomenm/disable ()
  "Turn off WIFI."
  (shell-command-to-string "nmcli -t -f net-enabled nm wifi off")
  (message "gnomenm wifi disabled")
  (setq gnomenm/enabled nil))

(defun gnomenm-status ()
  "What's the network status?"
  (interactive)
  (message "gnomenm network is %s"
           (if gnomenm/enabled "on" "off")))

;;;###autoload
(defun gnomenm-toggle-enabled (&optional status)
  "Toggle whether WIFI is enabled or not."
  (interactive "P")
  (cond
    ((null status)
     (if gnomenm/enabled (gnomenm/disable) (gnomenm/enable)))
    ((> (prefix-numeric-value status) 0)
     (gnomenm/enable))
    ((< (prefix-numeric-value status) 1)
     (gnomenm/disable))))

;;;###autoload
(defalias 'toggle-gnomenm-enabled 'gnomenm-toggle-enabled)

(defun gnomenm/connected ()
  "What AP are we currently connected to?"
  (car
   (split-string
    (shell-command-to-string "nmcli -t -f name con status")
    "\n")))

(defun gnomenm/list-aps ()
  "Make a list of all APs."
  (split-string
   (shell-command-to-string "nmcli -t -f name con list")
   "\n"))

(defun gnomenm/disconnect (ap)
  "Disconnect from the specified AP."
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con down id \"%s\"" ap))
    "\n")))


(defvar gnomenm/connect-history '()
  "History of all you have connected to.")

(defun gnomenm/connect (ap)
  "Connect to the specified AP."
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con up id \"%s\"" ap))
    "\n"))
  (setq
   gnomenm/connect-history
   (append (list ap) gnomenm/connect-history)))

;;;###autoload
(defun gnomenm-disconnect ()
  "Disconnect from the current Access Point."
  (interactive)
  (let ((current-ap (gnomenm/connected)))
    (gnomenm/disconnect current-ap)))

(defvar gnomenm-connect-history nil
  "The history of APs you've connected to.")

;;;###autoload
(defun gnomenm-connect (ap)
  "Connect to a specific AP."
  (interactive
   (list
    (let ((ap-list (gnomenm/list-aps)))
      (completing-read
       "What access point? " ap-list nil t
       (if gnomenm-connect-history
           (car gnomenm-connect-history)
           nil)
       'gnomenm-connect-history))))
  (let ((current-ap (gnomenm/connected)))
    (if (equal ap current-ap)
        (message "nm: already connected to %s" ap)
        ;; Else let's try and connect to it
        (if (equal "802-11-wireless" (elt (kva ap (gnomenm/list)) 2))
            (unwind-protect
                 (gnomenm/disconnect current-ap)
              (gnomenm/connect ap))
            ;; Else just connect
            (gnomenm/connect ap)))))

;;;###autoload
(defun gnomenm-flip ()
  "Flip the AP to the last but one connected to.

If you don't have two APs in the history it does nothing.

This is really useful if you switch between a pair of APs like I
do.  I recommend using a keychord like:

 (key-chord-define-global \"90\"  'gnomenm-flip)

See http://www.emacswiki.org/KeyChord for details on KeyChord."
  (interactive)
  (let* ((ap (gnomenm/connected))
         (new-ap
          (loop for previous-ap in gnomenm/connect-history
             if (and previous-ap (not (equal ap previous-ap)))
             return  previous-ap)))
    (when new-ap
      (gnomenm-connect new-ap))))

(provide 'gnomenm)

;;; gnomenm.el ends here
