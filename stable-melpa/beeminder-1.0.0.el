;;; beeminder.el --- Emacs interface for Beeminder

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>
;; Keywords: beeminder
;; Package-Version: 1.0.0

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA

;;; Commentary:

;; beeminder.el provides a simple way for Emacs to interact with the Beeminder
;; API. It's pretty basic at the moment, but can be used to fetch and submit
;; data.

;; Please set `beeminder-username' and `beeminder-auth-token' before using.

;; You can find your auth token by logging in to Beeminder and then visiting the
;; following URI: https://www.beeminder.com/api/v1/auth_token.json

;;; Keyboard bindings:

;; All keyboard bindings have a C-c b prefix

;; C-c b g    - Insert your goals as an org-mode list
;; C-c b m    - Display username in message line

;;; TODO:

;; [todo] - Replace goalval with the "math_is_hard" values

;;; Code:

(require 'json)

;; Configuration

(defgroup beeminder nil
  "Emacs interface for the Beeminder API"
  :group 'processes
  :prefix "beeminder-")

(defcustom beeminder-username nil
  "Your Beeminder username"
  :group 'beeminder
  :type '(string))

(defcustom beeminder-auth-token nil
  "Your Beeminder API key"
  :group 'beeminder
  :type '(string))

(defcustom beeminder-goal-org-tags ":GOAL:BEEMINDER:"
  "Tags that will be applied to inserted goal headlines"
  :group 'beeminder
  :type '(string))

(defvar beeminder-scratch)
(make-variable-buffer-local 'beeminder-scratch)

(defvar beeminder-request-data nil
  "An assoc list of parameter names to values.")

(defconst beeminder-v1-api
  "https://www.beeminder.com/api/v1/"
  "The endpoint for the Beeminder API (version 1.0)")


;; Keyboard Bindings

(global-set-key "\C-cba" 'beeminder-add-data)
(global-set-key "\C-cbw" 'beeminder-whoami)
(global-set-key "\C-cbg" 'beeminder-my-goals-org)


;; org-mode hooks

(defun beeminder-on-org-task-completed ()
  "Fires when an org-mode task is marked as DONE"
  (interactive)

  ;; Only fire if task is complete and a beeminder task
  (when (string= org-state "DONE")
    (when (org-entry-get (point) "beeminder" t)
      
      ;; If "value" property set, use that as the data, otherwise default to 1
      (let* ((datapoint (or (org-entry-get (point) "value" t) 1))
             (title (nth 4 (org-heading-components)))
             (goal (org-entry-get (point) "beeminder" t)))
        
        ;; Send to beeminder
        (beeminder-add-data goal datapoint title)))))

(add-hook 'org-after-todo-state-change-hook 'beeminder-on-org-task-completed) 


;; Functions

(defun beeminder-whoami ()
  "Displays the Beeminder username for your auth token"
  (interactive)
  (setq beeminder-scratch
        (beeminder-fetch (format "users/me.json?auth_token=%s" beeminder-auth-token)))
  (message "Your Beeminder Username: %s" (assoc-default 'username beeminder-scratch)))

(defun beeminder-my-goals ()
  "Displays your goals in the Message buffer (kind of useless)"
  (interactive)
  (beeminder-fetch-goals beeminder-username)
  
  (mapc (lambda (goal)
          (progn
            (message "Goal: %s" (assoc-default 'title goal))))
        beeminder-scratch))


;; ORG-Mode stuff

(defun beeminder-my-goals-org ()
  "Inserts your Beeminder goals as an org-mode headline list"
  (interactive)
  
  ;; Fetch the current user's goals
  (beeminder-fetch-goals beeminder-username)
  
  ;; Insert the main headline
  (insert (format "* Beeminder goals for %s\n" beeminder-username))
  
  ;; Insert each goal
  (mapc
   (lambda (goal)
     (progn
       
       ;; Insert the goal name and tags
       (insert (format "** TODO %s %s\n" (assoc-default 'title goal) beeminder-goal-org-tags))
       
       ;; Insert deadline
       (insert (format "   DEADLINE: <%s>\n" (format-time-string "%Y-%m-%d %a %H:%M" (seconds-to-time (assoc-default 'goaldate goal)))))

       ;; Insert goal properties
       (insert "   :PROPERTIES:\n")
       
       (insert (format "   :type:   %s\n" (assoc-default 'goal_type goal)))
       (insert (format "   :pledge: %s\n" (assoc-default 'pledge goal)))
       (insert (format "   :target: %s\n" (assoc-default 'goalval goal)))
       
       (insert "   :END:\n")))
   beeminder-scratch))


;; Main API Endpoints

(defun beeminder-fetch-goals (username)
  "Fetch a list of all goals for a single username"
  (setq beeminder-scratch
        (beeminder-fetch 
         (format "users/%s/goals.json?auth_token=%s" username beeminder-auth-token))))

(defun beeminder-add-data (goal value comment)
  "Post data to a Beeminder goal"
  (interactive "MGoal: \nnValue: \nMComment: \n")
  
  ;; Send the request
  (setq beeminder-scratch
        (beeminder-post
         (format "users/%s/goals/%s/datapoints.json" beeminder-username goal)
         (format "auth_token=%s&value=%s&comment=%s"
                 beeminder-auth-token
                 value
                 (url-hexify-string comment))))
  
  ;; Show what happened
  (message 
   "Data added at %s"
   (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time (assoc-default 'timestamp beeminder-scratch)))))


;; GET/POST Helpers

(defun beeminder-fetch (action)
  "Perform a request to the Beeminder API."
  (let* ((action (if (symbolp action) (symbol-name action) action))
         (url (format "%s%s" beeminder-v1-api action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun beeminder-post (action args)
  "Perform a POST request to the Beeminder API."
  (let* ((url-request-method "POST")
         (url-request-data args)
         (url (format "%s%s" beeminder-v1-api action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))


(provide 'beeminder)

;;; beeminder.el ends here
