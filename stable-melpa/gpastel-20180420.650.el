;;; gpastel.el --- Integrates GPaste with the kill-ring  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/DamienCassou/desktop-environment
;; Package-Version: 20180420.650
;; Package-requires: ((emacs "24.3"))
;; Version: 0.3.0
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GPaste is a clipboard management system.  The Emacs package gpastel
;; makes sure that every copied text in GPaste is also in the Emacs
;; kill-ring.

;; Emacs has built-in support for synchronizing the system clipboard
;; with the `kill-ring' (see, `interprogram-paste-function', and
;; `save-interprogram-paste-before-kill').  This support is not
;; optimal because it makes the `kill-ring' only contain the last text
;; of consecutive copied texts.  In other words, a user cannot copy
;; multiple pieces of text from an external application without going
;; back to Emacs in between.
;;
;; On the contrary, gpastel supports this scenario by hooking into the
;; GPaste clipboard manager.  This means that the `kill-ring' will
;; always contain everything the user copies in external applications,
;; not just the last piece of text.

;; Additionally, when using EXWM (the Emacs X Window Manager), gpastel
;; makes it possible for the user to use the `kill-ring' from external
;; applications.

;;; Code:

(require 'dbus)

(defgroup gpastel nil
  "Configure GPaste integration."
  :group 'environment)

(defcustom gpastel-gpaste-client-command "gpaste-client"
  "GPaste client name or path."
  :type 'string
  :group 'gpastel)

(defvar gpastel--dbus-object nil
  "D-Bus object remembering the return value of `dbus-register-signal'.
This can be used to unregister from the signal.")

(defconst gpastel--dbus-arguments
  '(:session
    "org.gnome.GPaste"
    "/org/gnome/GPaste"
    "org.gnome.GPaste1")
  "List of arguments referencing GPaste for the D-Bus API.")

(defun gpastel--handle-event-p (action _target index)
  "Return non-nil if gpastel should do anything about an event.

The event is represented by the D-Bus parameters of the Update
signal (i.e., ACTION, TARGET and INDEX).  See
`gpastel--update-handler'."
  (and (string= action "REPLACE") (= index 0)))

(defun gpastel--update-handler (action target index)
  "Update `kill-ring' when GPaste's clipboard is changing.

The function parameters are the one defined in the \"Update\"
signal sent by GPaste:

  - ACTION is a string representing how things have changed;

  - TARGET is a clipboard name

  - INDEX is a number indicating which element in the clipboard
    changed (usually 0)

This handler is executed each time GPaste changes the clipboard's
content.  The handler makes sure that the `kill-ring' contains
all text in the GPaste clipboard."
  (when (gpastel--handle-event-p action target index)
    ;; Setting `interprogram-cut-function' to nil make sure we don't
    ;; send the new kill back to system clipboard as that would start
    ;; infinite recursion:
    (let ((interprogram-cut-function nil)
          (copied-text (gpastel-get-copied-text)))
      ;; Prevent killed text from Emacs that have been sent to the
      ;; system clipboard with `interprogram-cut-function' to be
      ;; saved again to the `kill-ring':
      (unless (string= copied-text (car kill-ring))
        (kill-new copied-text)))))

(defun gpastel--start-gpaste-daemon ()
  "(Re)Start GPaste daemon and return non-nil upon success."
  (zerop (condition-case nil
             (call-process gpastel-gpaste-client-command nil nil nil "daemon-reexec")
           (error 1)))) ;; ⇐ should be a non-zero number

(defun gpastel-dbus-call (function &rest args)
  "Call FUNCTION passing `gpastel--dbus-arguments' and ARGS."
  (apply function (append gpastel--dbus-arguments args)))

(defun gpastel-get-copied-text (&optional index)
  "Return GPaste clipboard content at INDEX, or 0."
  (gpastel-dbus-call #'dbus-call-method "GetElement" :uint64 (or index 0)))

;;;###autoload
(defun gpastel-start-listening ()
  "Start listening for GPaste events."
  (interactive)
  (when (gpastel--start-gpaste-daemon)
    ;; No need for `interprogram-paste-function' because GPaste will
    ;; tell us as soon as text is added to clipboard:
    (setq interprogram-paste-function (lambda ()))
    ;; No need to save the system clipboard before killing in
    ;; Emacs because Emacs already knows about its content:
    (setq save-interprogram-paste-before-kill nil)
    ;; Register an handler for GPaste Update signals so we can
    ;; immediately update the `kill-ring':
    (setq gpastel--dbus-object
          (gpastel-dbus-call #'dbus-register-signal "Update" #'gpastel--update-handler))))

(provide 'gpastel)
;;; gpastel.el ends here
