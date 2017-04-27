;;; beeminder.el --- Emacs interface for Beeminder

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>
;; Keywords: beeminder
;; Package-Version: 20160209.1903
;; URL: http://www.philnewton.net/code/beeminder-el/
;; Created: March 22nd, 2014
;; Version: 1.0.0
;; Package-Requires: ((org "7"))
;;
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
;; API.  It's pretty basic at the moment, but can be used to fetch and submit
;; data.

;; Please set `beeminder-username' and `beeminder-auth-token' before using.

;; You can find your auth token by logging in to Beeminder and then visiting the
;; following URI: https://www.beeminder.com/api/v1/auth_token.json

;; Load beeminder.el with (require 'beeminder) after your Org is set up.

;;; Keyboard bindings:

;; We recommend binding the commands to the C-c b prefix

;; C-c b g    - Insert your goals as an org-mode list
;; C-c b m    - Display username in message line

;; You can use C-c C-x p (org-set-property) to add the beeminder
;; property to projects or tasks that are associated with beeminder
;; goals.  Set it to the identifier of your goal (the short name that's
;; in the URL).
;;
;; By default, completing those tasks will log one point.  You can set
;; the beeminder-value property to "prompt" in order to interactively
;; specify the value whenever you complete the task.  Set
;; beeminder-value to "time-today" in order to log the time you
;; clocked today (see "Clocking work time" in the Org manual).
;;
;; To do so, add these to your init.el:

;; (global-set-key "\C-cba" 'beeminder-add-data)
;; (global-set-key "\C-cbw" 'beeminder-whoami)
;; (global-set-key "\C-cbg" 'beeminder-my-goals-org)
;; (global-set-key "\C-cbr" 'beeminder-refresh-goal)

;;; TODO:

;; [todo] - Replace goalval with the "math_is_hard" values

;;; Code:

;; Dependencies

(require 'json)
(require 'org)
(require 'url-http)

(defvar org-state)
(defvar url-http-end-of-headers)

;; Configuration

(defgroup beeminder nil
  "Emacs interface for the Beeminder API"
  :group 'processes
  :prefix "beeminder-")

(defcustom beeminder-username nil
  "Your Beeminder username."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-auth-token nil
  "Your Beeminder API key."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-goal-org-tags ":GOAL:BEEMINDER:"
  "Tags that will be applied to inserted goal headlines."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-properties
  '((slug . "beeminder")
    (pledge . "beeminder-pledge")
    (goal_type . "beeminder-type")
    (goalval . "beeminder-target")
    (lane . "beeminder-lane")
    (value . "beeminder-value")
    (progress . "beeminder-progress")
    (updated_at . "beeminder-updated-at"))
  "Alist mapping property names for Beeminder goals.
The key should be the symbol that the Beeminder API returns, and the
value should be the name of the property updated in Org."
  :group 'beeminder
  :type '(repeat
          (cons
           (symbol "Symbol")
           (string "Property name"))))

(defconst beeminder-v1-api
  "https://www.beeminder.com/api/v1/"
  "The endpoint for version 1.0 of the Beeminder API.")


;; org-mode hooks

(defun beeminder-on-org-task-completed ()
  "Fires when an 'org-mode' task is marked as DONE."
  ;; Only fire if task is complete and a beeminder task
  (when (and (member org-state org-done-keywords)
             (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
    ;; If "value" property set, use that as the data, otherwise default to 1
    (let* ((datapoint (or (org-entry-get (point) (assoc-default 'value beeminder-properties) t) "1"))
           (title (nth 4 (org-heading-components)))
           (goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t)))
      (cond
       ((string= datapoint "prompt")
        (setq datapoint (read-string "Beeminder value: ")))
       ((string= datapoint "time-today")
        (org-clock-sum-today)
        (org-back-to-heading)
        (setq datapoint (/ (get-text-property (point) :org-clock-minutes) 60.0))))
      ;; Send to beeminder
      (beeminder-add-data goal datapoint title)
      (beeminder-refresh-goal))))

(add-hook 'org-after-todo-state-change-hook 'beeminder-on-org-task-completed)


;; Functions

(defun beeminder-whoami ()
  "Displays the Beeminder username for your auth token."
  (interactive)
  (let ((result (beeminder-fetch (format "users/me.json?auth_token=%s" beeminder-auth-token))))
    (message "Your Beeminder username: %s" (assoc-default 'username result))))

(defun beeminder-my-goals ()
  "Displays your goals in the Message buffer (kind of useless)."
  (interactive)
  (message
   "%s"
   (mapconcat (lambda (goal)
                (format "Goal: %s" (assoc-default 'title goal)))
              (beeminder-fetch-goals beeminder-username)
              "\n")))

(defun beeminder-refresh-goal ()
  "Fetch data for the current goal headline and update it."
  (interactive)

  ;; Get the goal at current point
  (when (org-entry-get (point) (assoc-default 'slug beeminder-properties) t)

    (let* ((goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
           ;; Get the updated goal from Beeminder
           (result (beeminder-fetch-goal beeminder-username goal)))

      ;; Update properties
      (mapc (lambda (prop)
              (when (assoc (car prop) result)
                (org-entry-put (point)
                               (cdr prop)
                               (format "%s"
                                       (assoc-default (car prop) result)))))
            beeminder-properties)
      ;; Add percentage
      (when (cdr (assoc 'goalval result))
        (org-entry-put (point)
                       (cdr (assoc 'progress beeminder-properties))
                       (format "%d%%"
                               (/ (* 100.0
                                     (assoc-default 'curval result nil 0))
                                  (assoc-default 'goalval result nil 0)))))

      ;; Update deadline
      (org-deadline nil
                    (format-time-string
                     "%Y-%m-%d %a %H:%M"
                     (seconds-to-time
                      (or (assoc-default 'losedate result)
                          (assoc-default 'goaldate result))))))))


;; ORG-Mode stuff

(defun beeminder-my-goals-org ()
  "Insert your Beeminder goals as an 'org-mode' headline list."
  (interactive)

  ;; Insert the main headline
  (insert
   (format "* Beeminder goals for %s\n" beeminder-username)
   (mapconcat
    (lambda (goal)
      ;; Insert the goal name and tags
      (format (concat "** TODO %s %s\n"
                      "  DEADLINE: <%s>\n"
                      "  SCHEDULED: <%s .+1w>\n"
                      "   :PROPERTIES:\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :STYLE: habit\n"
                      "   :END:\n")
              (assoc-default 'title goal)
              beeminder-goal-org-tags
              (format-time-string
               "%Y-%m-%d %a %H:%M"
               (seconds-to-time (assoc-default 'losedate goal)))
              (format-time-string
               "%Y-%m-%d %a"
               (current-time))
              (assoc-default 'slug beeminder-properties)
              (assoc-default 'slug goal)
              (assoc-default 'goal_type beeminder-properties)
              (assoc-default 'goal_type goal)
              (assoc-default 'pledge beeminder-properties)
              (assoc-default 'pledge goal)
              (assoc-default 'updated_at beeminder-properties)
              (assoc-default 'updated_at goal)
              (assoc-default 'lane beeminder-properties)
              (assoc-default 'lane goal)
              (assoc-default 'goalval beeminder-properties)
              (assoc-default 'goalval goal)))
    (beeminder-fetch-goals beeminder-username)
    "\n")))

(defun beeminder-submit-clocked-time ()
  "Submits all clocked time for a goal since the last submission date.

Will submit the number of minutes worked, but can also be used to
submit hours using beeminder-unit: hours."

  (interactive)

  ;; Store cursor position and get goal information
  (let ((previous-position (point-marker))
        (title (nth 4 (org-heading-components)))
        (goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
        (datapoint nil)
        (last-submitted (org-entry-get (point) (assoc-default 'updated_at beeminder-properties) t)))

    ;; Get the number of minutes worked since the last submission
    (org-clock-sum (seconds-to-time (string-to-number last-submitted)))
    (org-back-to-heading)
    (setq datapoint (get-text-property (point) :org-clock-minutes))

    ;; If no valid time clocked, prompt for it
    (if (not datapoint)
        (setq datapoint (read-from-minibuffer "Value (in minutes): ")))

    ;; Find the headline that contains the beeminder goal
    (search-backward ":beeminder:")
    (org-back-to-heading)

    ;; Prompt for note
    (setq title (read-from-minibuffer "Comment: " title))

    ;; Send data to beeminder and refresh the goal
    (beeminder-add-data goal datapoint title)
    (beeminder-refresh-goal)

    ;; Restore the cursor to original position
    (goto-char previous-position)))


;; Main API Endpoints

(defun beeminder-fetch-goals (&optional username)
  "Fetch a list of all goals for a single USERNAME."
  (beeminder-fetch
   (format "users/%s/goals.json?auth_token=%s" (or username beeminder-username) beeminder-auth-token)))

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-fetch
   (format "users/%s/goals/%s.json?auth_token=%s" username goal beeminder-auth-token)))

(defun beeminder-add-data (goal value comment)
  "Update Beeminder GOAL with VALUE and COMMENT."
  (interactive "MGoal: \nnValue: \nMComment: \n")
  (let ((result
         ;; Send the request
         (beeminder-post
          (format "users/%s/goals/%s/datapoints.json" beeminder-username goal)
          (format "auth_token=%s&value=%s&comment=%s"
                  beeminder-auth-token
                  value
                  (url-hexify-string comment)))))
    ;; Show what happened
    (message
     "Data added at %s"
     (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time (assoc-default 'timestamp result))))))


;; GET/POST Helpers

(defun beeminder-fetch (action)
  "Perform ACTION on the Beeminder API."
  (let* ((action (if (symbolp action) (symbol-name action) action))
         (url (format "%s%s" beeminder-v1-api action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

;;;###autoload
(defun beeminder-post (action args)
  "Perform a POST request to ACTION with ARGS."
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
