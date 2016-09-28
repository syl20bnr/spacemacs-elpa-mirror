;;; aws-ec2.el --- Manage AWS EC2 instances -*- lexical-binding: t -*-

;; Copyright (C) 2016 Yuki Inoue

;; Author: Yuki Inoue <inouetakahiroki _at_ gmail.com>
;; URL: https://github.com/Yuki-Inoue/aws.el
;; Package-Version: 20160927.843
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (dash-functional "1.2.0") (magit-popup "2.6.0") (tablist "0.70"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Manipulate AWS ec2 from emacs.

;;; Code:

(require 'json)
(require 'tablist)
(require 'dash)
(require 'dash-functional)
(require 'magit-popup)


(defun aws--shell-command-to-string (&rest args)
  (let ((cmd (funcall 'combine-and-quote-strings (append (aws-bin) args))))
    (message cmd)
    (shell-command-to-string cmd)))

(defun aws-bin ()
  (if aws-current-profile
      (list "aws" "--profile" aws-current-profile)
    (list "aws")))

(defun aws-ec2-all-raw-instances ()
  (interactive)
  (json-read-from-string
   (aws--shell-command-to-string "ec2" "describe-instances")))

(defun aws-ec2-normalize-raw-instances (raw-instances)
  (->>
   raw-instances
   (assoc-default 'Reservations)
   ((lambda (v) (append v nil)))
   (mapcar (apply-partially 'assoc-default 'Instances))
   (mapcar (lambda (v) (append v nil)))
   (-mapcat 'identity)
   (mapcar 'aws-instance-fix-tag)))

(defun aws-instance-fix-tag (instance)
  (mapcar
   (lambda (entry)
     (if (not (equal (car entry) 'Tags))
         entry
       (cons 'Tags
             (mapcar (lambda (kvassoc)
                       (cons (cdr (assoc 'Key kvassoc))
                             (cdr (assoc 'Value kvassoc))))
                     (cdr entry)))))
   instance))

(defvar aws-current-profile nil
  "The currently used aws profile")

(defun aws-set-profile (profile)
  "Configures which profile to be used."
  (interactive "sProfile: ")
  (if (string= "" profile)
      (setq profile nil))
  (setq aws-current-profile profile))

(define-derived-mode aws-instances-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."

  (define-key aws-instances-mode-map "I" 'aws-instances-inspect-popup)
  (define-key aws-instances-mode-map "S" 'aws-instances-state-popup)
  (define-key aws-instances-mode-map "A" 'aws-instances-action-popup)
  (define-key aws-instances-mode-map "C" 'aws-instances-configure-popup)
  (define-key aws-instances-mode-map "P" 'aws-set-profile)

  (setq tabulated-list-format
        '[("Repository" 10 nil)
          ("InstType" 10 nil)
          ("Name" 30 nil)
          ("Status" 10 nil)
          ("IP" 15 nil)
          ("Settings" 20 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'aws-instances-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))


(defun aws-instances-refresh ()
  "Refresh elasticsearch snapshots."

  (setq
   tabulated-list-entries
   (->>
    (aws-ec2-normalize-raw-instances
     (aws-ec2-all-raw-instances))
    (mapcar (lambda (instance)
              (list (cdr (assoc 'InstanceId instance))
                    (vector (assoc-default 'InstanceId instance)
                            (assoc-default 'InstanceType instance)
                            (or (assoc-default "Name" (assoc-default 'Tags instance)) "")
                            (assoc-default 'Name (assoc-default 'State instance))
                            (prin1-to-string (assoc-default 'PrivateIpAddress instance) t)
                            (or  "..." (prin1-to-string instance))
                            ))))
    )))

(defun aws-select-if-empty (&optional arg)
  "Select current row is selection is empty."
  (save-excursion
    (when (null (tablist-get-marked-items))
      (tablist-put-mark))))

(defmacro aws-define-popup (name doc &rest args)
  "Wrapper around `aws-utils-define-popup'."
  `(progn
     (magit-define-popup ,name ,doc ,@args)
     (add-function :before (symbol-function ',name) #'aws-select-if-empty)))

(aws-define-popup
 aws-instances-inspect-popup
 'aws-instances-popups
 :actions  '((?I "Inspect" aws-instances-inspect-selection)))

(aws-define-popup
 aws-instances-state-popup
 'aws-instances-popups
 :actions  '((?O "Stop" aws-instances-stop-selection)
             (?T "Terminate" aws-instances-terminate-selection)
             (?S "start" aws-instances-start-selection)))

(aws-define-popup
 aws-instances-action-popup
 'aws-instances-popups
 :actions  '((?R "Rename Instance" aws-instances-rename-instance)))

(aws-define-popup
 aws-instances-configure-popup
 'aws-instances-popups
 :actions  '((?C "Configure ssh-config" aws-instances-configure-ssh-config)))



(defun aws-ec2-command-on-selection (command)
  (apply 'aws--shell-command-to-string
         "ec2" command "--instance-ids"
         (-map #'car (tablist-get-marked-items))))

(defun aws-instances-stop-selection ()
  (interactive)
  (aws-ec2-command-on-selection "stop-instances"))

(defun aws-instances-terminate-selection ()
  (interactive)
  (if (yes-or-no-p (format "Really Terminate the %d instances?"
                           (length
                            (tablist-get-marked-items))))
      (aws-ec2-command-on-selection "terminate-instances")))

(defun aws-instances-start-selection ()
  (interactive)
  (aws-ec2-command-on-selection "start-instances"))

(defun aws-instances-inspect-selection ()
  (interactive)
  (let ((args (->>
               (tablist-get-marked-items)
               (mapcar 'car)
               (append '("ec2" "describe-instances" "--instance-ids")))))
    (apply 'aws--shell-command-to-result-buffer args)))

(defun aws--shell-command-to-result-buffer (&rest args)
  (let ((result (apply 'aws--shell-command-to-string args))
        (buffer (get-buffer-create "*aws result*")))

    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-max))
      (insert result))
    (display-buffer buffer)))

(defun aws-instances-rename-instance ()
  (interactive)
  (let ((ids (mapcar 'car (tablist-get-marked-items))))
    (if (/= 1 (length ids))
        (error "Multiple instances cannot be selected.")
      (let ((new-name (read-string "New Name: "))
            (id (nth 0 ids)))
        (aws--shell-command-to-string
         "ec2" "create-tags"
         "--resources" id "--tags" (format "Key=Name,Value=%s" new-name))))))

(defconst aws-instances-ssh-config-entry-template
  "
Host %s
  HostName %s
  User %s
  IdentityFile %s
%s
")

(defcustom aws-ec2-key-alist '()
  "The Key String to KeyPath alist"
  :type '(alist :key-type (string :tag "Key Name")
                :value-type (string :tag "KeyPath")))

(defcustom aws-instances-ssh-config-user-name "admin"
  "The ssh user name for ssh-config."
  :type 'string)

(defcustom aws-instances-ssh-config-option-entries '()
  "The ssh config entry"
  :type '(repeat string)
  )

(defun aws-instances-configure-ssh-config ()
  (interactive)
  (let ((aws-instances
         (->>
          (tablist-get-marked-items)
          (-map #'car)
          (apply 'aws--shell-command-to-string
                 "ec2" "describe-instances"
                 "--instance-ids")
          (json-read-from-string)
          (aws-ec2-normalize-raw-instances))))

    (-each aws-instances
      (lambda (aws-instance)
        (let* ((host-name (->>
                          aws-instance
                          (assoc-default 'Tags)
                          (assoc-default "Name")))
               (host-ip (assoc-default 'PrivateIpAddress aws-instance))
               (key-name (assoc-default 'KeyName aws-instance))
               (key-path (assoc-default key-name aws-ec2-key-alist))
               (snippet (format
                         aws-instances-ssh-config-entry-template
                         host-name host-ip
                         aws-instances-ssh-config-user-name
                         key-path
                         (->>
                          aws-instances-ssh-config-option-entries
                          (-map (lambda (str) (concat "  " str)))
                          (s-join "\n")))))
          (write-region snippet nil "~/.ssh/config" 'append)
          )))))

;;;###autoload
(defun aws-instances ()
  "List aws instances using aws-cli. (The `aws` command)."
  (interactive)
  (pop-to-buffer "*aws-instances*")
  (tabulated-list-init-header)
  (aws-instances-mode)
  (tabulated-list-revert))

(defvar aws-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'aws-instances)
    map))

(provide 'aws-ec2)

;;; aws-ec2.el ends here
