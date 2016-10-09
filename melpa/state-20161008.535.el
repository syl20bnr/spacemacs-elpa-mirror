;;; state.el --- Quick navigation between workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords: convenience, workspaces
;; Package-Requires: ((emacs "24"))
;; Package-Version: 20161008.535
;; Package-X-Original-Version: 0.1
;; URL: https://github.com/thisirs/state.git

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
;; This library allows you to switch back and forth between predefined
;; workspaces. See the README file for more information.

;;; Installation:

;; (require 'state)
;; (state-global-mode 1)

;; There is no predefined workspaces to switch to. To switch back and
;; forth to the *Messages* buffer by pressing C-c s m:

;; (state-define-state
;;  message
;;  :key "m"
;;  :switch "*Messages*")

;; See full documentation on https://github.com/thisirs/state#state

;;; Code:

(require 'cl-lib)

;;; Compatibility
(unless (functionp 'cl-struct-slot-info)
  (defun cl-struct-slot-info (struct-type)
    "Return a list of slot names of struct STRUCT-TYPE.
     Each entry is a list (SLOT-NAME . OPTS), where SLOT-NAME is a
     slot name symbol and OPTS is a list of slot options given to
     `cl-defstruct'.  Dummy slots that represent the struct name
     and slots skipped by :initial-offset may appear in the list."
    (get struct-type 'cl-struct-slots))
  (put 'cl-struct-slot-info 'side-effect-free t))

;;; Customization
(defgroup state nil
  "Quick navigation between workspaces"
  :prefix "state-"
  :group 'convenience)

(defcustom state-keymap-prefix (kbd "C-c s")
  "The prefix command for state's keymap.
The value of this variable is checked as part of loading state mode.
After that, changing the prefix key requires manipulating `state-mode-map'."
  :type 'string
  :group 'state)

;;; Core
(cl-defstruct state
  "Structure representing a state.
Slots:

`name'
    Symbol identifying the state.
`key'
    Key used to switch to this state.
`switch'
    Form that performs the switch.
`exist'
    Form that tells if the state is existing.
`create'
    Form to create the state.
`in'
    Form that returns true if we are in this state.
`bound'
    If non-nil, this state is accessible only from another state.
`priority'
    Priority of state if there is more than one we want to switch to.
`keep'
    What to do when we keep pressing the key after switching.
`before'
    Action to perform before switching to another state.
`origin'
    Store state symbol name we are coming from.
`current'
    Data used to restore this state; usually a wconf."
  name key switch exist create in bound priority keep before origin current)

(defvar state--states nil
  "List of all defined states.")

(defvar state--default-state
  (make-state :name 'default
              :switch '(state--switch-default 'default)
              :before '(state--before-default 'default))
  "Default state when not in any other state.")

(defconst state-min-priority -100000
  "Lowest value of priority, ie. highest priority.")

(defun state--message (msg &rest args)
  (if (minibufferp)
      (apply #'minibuffer-message msg args)
    (apply #'message msg args)))

(defun state--filter (collection slot pred-or-value)
  "Return all states found in COLLECTION with SLOT's value satisfying PRED-OR-VALUE.

If PRED-OR-VALUE is a function, call it with slot's value as
first argument. Otherwise, compare slot's value with `equal'."
  (unless (memq slot (mapcar #'car (cl-struct-slot-info 'state)))
    (error "Unknown slot name: %s" slot))
  (let ((predicate (if (functionp pred-or-value)
                       pred-or-value
                     (lambda (v) (equal pred-or-value v)))))
    (cl-remove-if-not
     (lambda (state)
       (funcall predicate (funcall (intern (format "state-%s" slot)) state)))
     collection)))

(defun state--get-state-by-name (name)
  "Return a state object with name NAME found in `state--states'.

If NAME is equal to `default', return the default state
`state--default-state', nil otherwise."
  (if (stringp name) (setq name (intern name)))
  (if (eq name 'default)
      state--default-state
    (cl-find-if (lambda (state) (eq name (state-name state))) state--states)))

(defun state--get-state-in ()
  "Return the current state or default state if not in any."
  (or (cl-find-if (lambda (state) (state-call state 'in)) state--states)
      state--default-state))

(defun state-call (state slot &rest args)
  "Call or eval the value of slot SLOT in state STATE. Call with
ARGS if supplied."
  (let ((value (funcall (intern (format "state-%s" slot)) state)))
    (if (functionp value)
        (apply value args)
      (eval value))))

(defun state--select-states (key from-name)
  "Return states to switch to when KEY is pressed and when coming from FROM-NAME."
  (let* ((states (state--filter state--states 'key key))
         (unbound (state--filter states 'bound 'not))
         (bound (state--filter states 'bound
                               (lambda (v)
                                 (cond ((symbolp v)
                                        (eq v from-name))
                                       ((functionp v)
                                        (funcall v))
                                       (t
                                        (eval v)))))))
    (if bound
        (cl-loop for state in bound
                 collect (cons (state-priority state) state) into pairs
                 collect (state-priority state) into priorities
                 finally return
                 (mapcar 'cdr (cl-remove-if-not
                               (lambda (pair) (= (apply 'min priorities) (car pair)))
                               pairs)))
      unbound)))

(defun state--do-switch (key)
  "Perform the switch process when KEY is pressed."
  (let* ((from (state--get-state-in))
         (to (state--choose-state-to-switch key from)))
    (if (eq to from)
        (state--switch-back from)
      (state--switch-to to from key))))

(defun state--choose-state-to-switch (key from)
  "Return state to switch to by pressing KEY when coming from FROM.

Return FROM if we are switching back. Otherwise, return state
described by KEY, asking the user if there is more than one
state."
  (let ((states (if (equal key (state-key from))
                    (list from)
                  (state--select-states key (state-name from)))))
    (cond ((not states)
           (error "Non-existent state"))
          ((= 1 (length states))
           (car states))
          (t
           (state--get-state-by-name
            (completing-read "Choose state: " (mapcar 'state-name states)
                             nil t))))))

(defun state--switch-back (from)
  "Perform the actual switch back to state FROM."
  (state-call from 'before)
  (let ((origin (state-origin from)))
    (if (not origin)
        (user-error "Not coming from anywhere")
      (let ((wconf (state-current (state--get-state-by-name origin))))
        (if (not (window-configuration-p wconf))
            (user-error "No wconf stored for `%s' state" origin)
          (set-window-configuration wconf)
          (if (eq origin 'default)
              (state--message "Back to default state")
            (state--message "Back to `%s' state" origin)))))))

(defun state--switch-to (to from key)
  "Perform the actual switch to state TO coming from FROM by
pressing KEY."
  ;; Not switching back but switching to, so save original state
  (setf (state-origin to) (state-name from))

  ;; Save current wconf to restore it if we switch back
  (setf (state-current from) (current-window-configuration))

  ;; Executes any other user defined "before" form
  (state-call from 'before)

  (cond ((state-call to 'exist)
         (state-call to 'switch)
         (state-call to 'before))
        (t
         (state-call to 'create)
         (unless (state-call to 'in)
           (state-call to 'switch))
         (state-call to 'before)))
  (state--message "Switched to `%s' state" (state-name to))

  ;; If keep in non-nil install transient keymap
  (if (state-keep to)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd key)
           (lambda ()
             (interactive)
             (state-call to 'keep to)))
         map) t)))

;;; Autoload as a defun to avoid too early autoloading
;;;###autoload (autoload 'state-define-state "state")
(defmacro state-define-state (name &rest args)
  "Define a new state named NAME with property list ARGS.

:name Symbol representing the state.

:key String of length 1 used as a key in keymap `state-mode-map'
to switch to the state.

:in Field that is used to say if emacs currently displays the
state. If it is a string, return non-nil if current buffer is
visiting a file that is an ancestor of that string. If it is a
form or function, call it.

:switch Field that is used to perform the actual switch. It is
called if it is a function or a form. If it is a valid path,
switch to a buffer visiting that file or switch to the buffer
with that name. If that field is not specified, infer a suitable
one if :in is a string.

:exist Function or form called to say if the state exists. Some
states might require a set up when first called. :exist is used
to say if that set up has already been made.

:create Function or form called to create the state. It is linked
to the :exist property. When the state does not exists, :create
is called.

:before Function or form called just before switching. It allows
the current state to save its state. By default, it saves the
current windows configuration.

:bound Field saying if the current state should only be
accessible from another state. It is the name of another state or
a form to be called.

:priority A number indicating the priority of a state when
several states hold the same key. The state with the lowest
priority is preferred. If several states have the same lowest
priority, ask the user to choose. By convention, nil is of
infinite priority.

:keep A form or function that is called if we keep pressing the
key after switching. Leave nil is you don't want this feature."
  (declare (indent defun))
  (let ((state (or (state--get-state-by-name name) (make-state)))
        (key (plist-get args :key))
        (switch (plist-get args :switch))
        (before (plist-get args :before))
        (in (plist-get args :in))
        (bound (plist-get args :bound))
        (priority (plist-get args :priority))
        (exist (plist-get args :exist))
        (keep (plist-get args :keep))
        (create (plist-get args :create))
        (defun-sym (intern (format "state--switch-to-%s" name))))

    (setf (state-name state) name)
    (if key
        (setf (state-key state) key)
      (error "No property key defined"))
    (setf (state-priority state) (or priority state-min-priority))
    (setf (state-bound state) bound)
    (setf (state-keep state) keep)
    (setf (state-create state) (state--rewrite-create create in switch))
    (setf (state-in state) (state--rewrite-in in switch))
    (setf (state-exist state) (state--rewrite-exist exist in switch))
    (setf (state-switch state) (state--rewrite-switch switch name in))
    (setf (state-before state) (state--rewrite-before before name))
    (setf (state-current state) nil)

    `(progn
       (unless (state--get-state-by-name ',name)
           (add-to-list 'state--states ,state))

       ;; Define command switching to NAME
       (defun ,defun-sym ()
         ,(format "Switch to state `%s'" name)
         (interactive)
         (state--do-switch ,key))

       ;; And bind it to KEY
       (define-key state-prefix-map (kbd ,key) ',defun-sym))))

(defun state--rewrite-create (create in switch)
  "Return a modified CREATE propery based on IN or SWITCH.

If CREATE is nil, infer one base on SWITCH or IN properties if
they are strings. Otherwise leave nil."
  (cond (create)
        ((and (stringp switch) (file-name-absolute-p switch))
         `(state--create-switch-file ,switch))
        ((stringp switch)
         `(state--create-switch-buffer ,switch))
        ((and (stringp in) (file-directory-p in))
         `(state--create-in-directory ,in))
        ((stringp in)
         `(state--create-in-file ,in))))

(fset 'state--create-in-directory 'dired-noselect)
(fset 'state--create-in-file 'dired-noselect)
(fset 'state--create-switch-file 'find-file-noselect)
(fset 'state--create-switch-buffer 'get-buffer-create)


(defun state--rewrite-in (in switch)
  "Return a modified IN property based on SWITCH."
  (cond ((stringp in)
         `(state--in-in-file ,in))
        (in)
        ((and (stringp switch) (file-name-absolute-p switch))
         `(state--in-switch-file ,switch))
        ((stringp switch)
         `(state--in-switch-buffer ,switch))
        ((null in)
         (error "No :in property or not able to infer one"))))

(defun state--buffer-file-name-prefix-p (buf prefix)
  "Return true if buffer BUF is visiting a file whose filename is
prefixed by PREFIX. If no filename, use `default-directory' instead."
  (with-current-buffer buf
    (string-prefix-p
     (file-truename prefix)
     (file-truename (or (buffer-file-name) default-directory "/")))))

(defun state--in-in-file (in)
  (state--buffer-file-name-prefix-p (current-buffer) in))

(defun state--in-switch-file (switch)
  (eq (current-buffer) (find-buffer-visiting switch)))

(defun state--in-switch-buffer (switch)
  (eq (current-buffer) (get-buffer switch)))

(defun state--rewrite-exist (exist in switch)
  "Return a modified EXIST property based on IN or SWITCH.

If the EXIST property is nil, infer one base on SWITCH or IN if
they are strings. Otherwise leave nil."
  (cond (exist)
        ((stringp in)
         `(state--exist-in-file ,in))
        ((stringp switch)
         `(state--exist-switch-buffer ,switch))))

(defun state--find-file-name-prefix-buffer (prefix)
  (cl-find-if (lambda (buf) (state--buffer-file-name-prefix-p buf prefix))
              (buffer-list)))

(fset 'state--exist-in-file 'state--find-file-name-prefix-buffer)
(fset 'state--exist-switch-buffer 'get-buffer)

(defun state--rewrite-switch (switch name in)
  "Return a modified SWITCH property based on NAME or IN."
  (cond ((and (stringp switch) (file-name-absolute-p switch))
         `(state--switch-switch-file ,switch))
        ((stringp switch)
         `(state--switch-switch-buffer ,switch))
        (switch)
        ((stringp in)
         `(state--switch-in-file ,in ',name))
        (t
         `(state--switch-default ',name))))

(defun state--switch-switch-file (switch)
  (if current-prefix-arg
      (switch-to-buffer-other-window
       (find-file-noselect switch))
    (find-file-existing switch)))

(defun state--switch-switch-buffer (switch)
  (if current-prefix-arg
      (switch-to-buffer-other-window switch)
    (switch-to-buffer switch)))

(defun state--switch-in-file (in name)
  (let ((state (state--get-state-by-name name)))
    (if (window-configuration-p (state-current state))
        (set-window-configuration (state-current state))
      (let ((buffer (or (state--find-file-name-prefix-buffer in)
                        (and (file-directory-p in)
                             (dired-noselect in))
                        (error "Unable to switch to `%s' state" name))))
        (delete-other-windows)
        (switch-to-buffer buffer)))))

(defun state--switch-default (name)
  (let ((state (state--get-state-by-name name)))
    (if (window-configuration-p (state-current state))
        (set-window-configuration (state-current state)))))

(defun state--rewrite-before (before name)
  "Return a modified BEFORE property based on NAME."
  (or before `(state--before-default ',name)))

(defun state--before-default (name)
  "Default :before property base on NAME of state.

Store the current window configuration in the slot curent."
  (let ((state (state--get-state-by-name name)))
    (when state
      (setf (state-current state) (current-window-configuration)))))

;;; Minor mode
(defvar state-prefix-map (make-sparse-keymap)
  "Prefix map for state mode.")

(defvar state-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map state-keymap-prefix state-prefix-map)
    map)
  "Keymap for state mode.")

;;;###autoload
(define-minor-mode state-mode
  "Minor mode to switch between workspaces."
  :lighter " St"
  :keymap state-mode-map)

;;;###autoload
(define-globalized-minor-mode state-global-mode
  state-mode
  state-on)

;;;###autoload
(defun state-on ()
  "Enable State minor mode."
  (state-mode 1))

;;; Utility function
(defun state-switch-buffer-other-window (buf)
  "Select window BUF is shown, otherwise display BUF in other window."
  (if (get-buffer-window buf)
      (select-window (get-buffer-window buf))
    (switch-to-buffer-other-window buf)))

(provide 'state)

;;; state.el ends here
