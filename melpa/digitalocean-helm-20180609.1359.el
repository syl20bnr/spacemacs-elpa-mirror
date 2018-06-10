;;; digitalocean-helm.el --- Create and manipulate digitalocean droplets -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://gitlab.com/olymk2/digitalocean-api
;; Package-Version: 20180609.1359
;; Keywords: Processes tools
;; Version: 0.1
;; Created 01 July 2018
;; Package-Requires: ((emacs "24.3")(helm "2.5")(digitalocean "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implid warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Digitalocean api extension that lets you select a droplet with helm and run actions against the selected droplet 

;;; Code:

(defvar digitalocean-helm-droplet-source
  ;; Helm droplet list and actions
  '((name . "Digitalocean Droplets")
    (candidates . digitalocean-droplet-list)
    (action . (("Open Shell" .
		(lambda (candidate)
		  (digitalocean-launch-shell
		   (digitalocean-get-droplet-name-from-candidate candidate)
                   (digitalocean-build-ssh-path candidate "~/"))))
	       ("Snapshot" .
		(lambda (candidate)
		  (digitalocean-exec-droplet-action
		   (digitalocean-get-droplet-id-from-candidate candidate)
                   "snapshot")))
	       ("Power Off" .
		(lambda (candidate)
		  (digitalocean-exec-droplet-action
		   (digitalocean-get-droplet-id-from-candidate candidate)
                   "power_off")))
	       ("Power On" .
		(lambda (candidate)
		  (digitalocean-exec-droplet-action
		   (digitalocean-get-droplet-id-from-candidate candidate)
                   "power_on")))
	       ("Restart" .
		(lambda (candidate)
		  (digitalocean-exec-droplet-action
		   (digitalocean-get-droplet-id-from-candidate candidate)
                   "restart")))
	       ("Destroy" .
		(lambda (candidate)
		  (digitalocean-exec-droplet-action
		   (digitalocean-get-droplet-id-from-candidate candidate)
                   "destroy")))))))

;;;autoload
(defun digitalocean-helm-droplets ()
  "Show helm droplet list."
  (interactive)
  (helm :sources '(digitalocean-helm-droplet-source)))


(defvar digitalocean-helm-image-source
  ;; Helm image list and actions
  '((name . "Digitalocean Images")
    (candidates . digitalocean-images-list)
    (action . (("Test" . (lambda (candidate)
			   (message-box
			    "selected: %s"
			    (helm-marked-candidates))))))))

;;;autoload
(defun digitalocean-helm-images ()
  "Show helm image list."
  (interactive)
  (helm :sources '(digitalocean-helm-image-source)))

(defvar digitalocean-helm-region-source
  ;; Helm region list and actioons
  '((name . "Digitalocean Regions")
    (candidates . digitalocean-regions-list)
    (action . (("Test" . (lambda (candidate)
			   (message-box
			    "selected: %s"
			    (helm-marked-candidates))))))))

;;;autoload
(defun digitalocean-helm-regions ()
  "Show helm region list."
  (interactive)
  (helm :sources '(digitalocean-helm-region-source)))


(defun digitalocean-helm-build-ssh-path (candidate dir)
  "Give a helm CANDIDATE & DIR return a tramp ssh path."
  (format "/ssh:root@%s:%s"
	  (digitalocean-get-droplet-ip4-from-id
	   (digitalocean-helm-get-droplet-id-from-candidate candidate)) dir))


(defun digitalocean-helm-get-droplet-id-from-candidate (candidate)
  "Give a helm droplet CANDIDATE get the droplets id."
  (number-to-string (cdr (assoc 'id candidate))))

(defun digitalocean-helm-get-droplet-name-from-candidate (candidate)
  "Give a helm droplet CANDIDATE get the droplets name."
  (cdr (assoc 'name candidate)))

;;; (Features)
(provide 'digitalocean-helm)
;;; digitalocean-helm.el ends here
