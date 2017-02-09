;;; gnus-desktop-notify.el --- Gnus Desktop Notification global minor mode

;; Author: Yuri D'Elia <wavexx AT thregr.org>
;; Contributors: Philipp Haselwarter <philipp.haselwarter AT gmx.de>
;; Version: 1.4
;; Package-Version: 20170208.546
;; URL: http://www.thregr.org/~wavexx/software/gnus-desktop-notify.el/
;; GIT: git://src.thregr.org/gnus-desktop-notify.el/
;; Package-Requires: ((gnus "1.0"))
;; Package-Suggests: ((alert "1.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Change Log:

;; 1.4:
;; * Use `alert' package by default if available (`gnus-desktop-notify-alert').
;;
;; 1.3:
;; * Use `notifications' in emacs 24 (if available) by default (new function
;;   `gnus-desktop-notify-dbus').
;; * Renamed `gnus-desktop-notify-send' to `gnus-desktop-notify-behavior'.
;; * `gnus-desktop-notify-behavior' is now consistent across all notification
;;   functions (exec/send/dbus).
;;
;; 1.2:
;; * Collapse group names by default (see
;;   `gnus-desktop-notify-uncollapsed-levels').

;;; Commentary:

;; Desktop notification integration in Gnus!? Ohh goody!
;;
;; ``gnus-desktop-notify.el`` provides a simple mechanism to notify the user
;; when new messages are received. For basic usage, to be used in conjunction
;; with `gnus-daemon', put the following:
;;
;; (require 'gnus-desktop-notify)
;; (gnus-desktop-notify-mode)
;; (gnus-demon-add-scanmail)
;;
;; into your ``.gnus`` file. The default is to use `alert' if available, which
;; works on every operating system and allows the user to customize the
;; notification through emacs. See https://github.com/jwiegley/alert#for-users
;; for further info. If not available, the `notifications' library (part of
;; emacs >= 24) is used, so no external dependencies are required. With emacs
;; <= 23 instead the generic ``notify-send`` program is used, which (in Debian
;; or Ubuntu) is available in the ``libnotify-bin`` package.
;;
;; You can also call any program directly by changing the
;; `gnus-desktop-notify-exec-program' variable, or change the behavior entirely
;; by setting a different `gnus-desktop-notify-function' function.
;;
;; By default, all groups are notified when new messages are received. You can
;; exclude a single group by setting the `group-notify' group parameter to
;; t. You can also selectively monitor groups instead by changing the
;; `gnus-desktop-notify-groups' variable to `gnus-desktop-notify-explicit' and
;; then manually selecting which groups to include. Press 'G c' in the group
;; buffer to customize group parameters interactively.
;;
;; The behavior of the notification can be tuned by changing the
;; `gnus-desktop-notify-behavior' variable.
;;
;; See the `gnus-desktop-notify' customization group for more details.
;;
;; Feel free to send suggestions and patches to wavexx AT thregr.org

;;; Code:

(require 'format-spec)
(require 'gnus-group)

(unless (require 'alert nil t)
  (require 'notifications nil t))

;;; Custom variables

(defgroup gnus-desktop-notify nil
  "Gnus external notification framework"
  :group 'gnus)

(defcustom gnus-desktop-notify-function
  (cond ((featurep 'alert)         'gnus-desktop-notify-alert)
        ((featurep 'notifications) 'gnus-desktop-notify-dbus)
        (t                         'gnus-desktop-notify-send))
  "Function called when a group receives new messages. The first
argument will be an alist containing the groups and the number of
new messages. The default is to use `gnus-desktop-notify-alert'
if the `alert' package is available, `gnus-desktop-notify-dbus'
on emacs >= 24, or fallback to the generic
`gnus-desktop-notify-send' otherwise.

  The following functions available (see the documentation for
each function):

`gnus-desktop-notify-alert': use the `alert' library.
`gnus-desktop-notify-dbus': use the `notifications' library.
`gnus-desktop-notify-send': call the 'notify-send' program.
`gnus-desktop-notify-exec': call a customizable program."
  :type 'function)

(defcustom gnus-desktop-notify-exec-program "xmessage"
  "Executable called by the `gnus-desktop-notify-exec'
function. Each argument will be formatted according to
`gnus-desktop-notify-format'"
  :type 'file)

(defcustom gnus-desktop-notify-send-program "notify-send"
  "Path to the `notify-send' executable.
This is usually bundled as part of libnotify's utilities."
  :type 'file)

(defcustom gnus-desktop-notify-send-switches
  '("-i" "/usr/share/icons/gnome/32x32/actions/mail_new.png")
  "List of strings to pass as extra options to `notify-send'.
See `gnus-desktop-notify-send-program'."
  :type '(repeat (string :tag "Argument")))

(defcustom gnus-desktop-notify-behavior 'gnus-desktop-notify-multi
  "Desktop notification behavior. Can be either:

`gnus-desktop-notify-single': display a single notification for
                              each group.

`gnus-desktop-notify-multi': display a multi-line notification
                             for all groups at once."
  :type 'symbol)

(defcustom gnus-desktop-notify-send-subject "New mail"
  "Text used in the notification subject when new messages are received.
Depending on your notification agent, some HTML formatting may be
supported (awesome and KDE are known to work)."
  :type 'string)

(defcustom gnus-desktop-notify-format "%n:%G"
  "Format used to generate the notification text. When using
notifications, some agents may support HTML formatting (awesome
and KDE are known to work).

%n    Number of new messages in the group
%G    Group name"
  :type 'string)

(defcustom gnus-desktop-notify-uncollapsed-levels gnus-group-uncollapsed-levels
  "Number of group name elements to leave alone when making a shortened name
for display from a group name.
Value can be `gnus-group-uncollapsed-levels', an integer or nil to
deactivate shortening completely."
  :type '(choice (const :tag "Standard `gnus-group-uncollapsed-levels'"
                        gnus-group-uncollapsed-levels)
                 (integer)
                 (const :tag "nil (deactivate feature)" nil)))

(defcustom gnus-desktop-notify-groups 'gnus-desktop-notify-all-except
  "Gnus group notification mode. Can be either:

`gnus-desktop-notify-all-except': monitor all groups by default
                                  except excluded ones,

`gnus-desktop-notify-explicit': monitor only requested groups.

Groups can be included or excluded by setting the `group-notify'
group parameter to `t'. This can be set either in the
`gnus-parameters' variable, or interactively by pressing `G c' in
the group buffer."
  :type 'symbol)

;;; Group parameters

(gnus-define-group-parameter
  group-notify
   :type bool
   :parameter-type '(const :tag "Include/exclude this group from
the notification of new messages (depending on the value of
`gnus-desktop-notify-groups')." t))

;;; Internals

(defvar gnus-desktop-notify-counts ()
  "Map Gnus group names to their total number of articles.")

(defvar gnus-desktop-notify-html-lut
  '(("&" . "&amp;")
    ("<" . "&lt;" )
    (">" . "&gt;" ))
  "Map special characters to their HTML entities.")

;; FIXME: Do not reinvent the wheel if possible
(defun gnus-desktop-notify-escape-html-entities (str)
  "Escape HTML character entity references."
  (let* ((lut   gnus-desktop-notify-html-lut)
         (chars (format "[%s]" (mapconcat #'car lut ""))))
    (replace-regexp-in-string
     chars (lambda (s) (cdr (assoc-string s lut))) str)))

(defun gnus-desktop-notify-read-count (group)
  (let* ((range (gnus-range-normalize (gnus-info-read group)))
         (count (gnus-last-element range)))
    (or (cdr-safe count) count)))

(defun gnus-desktop-short-group-name (group)
  "Collapse GROUP name.
See `gnus-desktop-notify-uncollapsed-levels' for ways to control
collapsing."
  (if gnus-desktop-notify-uncollapsed-levels
      (gnus-short-group-name group gnus-desktop-notify-uncollapsed-levels)
    group))

(defun gnus-desktop-notify-format-1 (group)
  "Convert GROUP to its printed representation.
GROUP should have the form (NAME . COUNT), where NAME is the
group name to display and COUNT is the corresponding number of
articles."
  (let ((name  (gnus-desktop-notify-escape-html-entities (car group)))
        (count (cdr group)))
    (format-spec gnus-desktop-notify-format
                 (format-spec-make ?n count
                                   ?G name))))

(defun gnus-desktop-notify-format-n (groups)
  "Return a list of the printed representations of GROUPS.

GROUPS should be a list of cons cells accepted by
`gnus-desktop-notify-format-1', which see.

Depending on the value of `gnus-desktop-notify-behavior', the
returned list will comprise either a single multiline string or
multiple uniline strings."
  (mapcar (lambda (body) (mapconcat #'identity body "\n"))
          ;; Iterate over the groups either individually or as a whole
          (let ((bodies (mapcar #'gnus-desktop-notify-format-1 groups)))
            (cond ((eq gnus-desktop-notify-behavior 'gnus-desktop-notify-single)
                   (mapcar #'list bodies))
                  ((eq gnus-desktop-notify-behavior 'gnus-desktop-notify-multi)
                   `(,bodies))))))

(defun gnus-desktop-notify-check (&rest _ignored)
  "Check all groups for and notify of new articles."
  (interactive)
  (let ((updated-groups ()))
    (dolist (g gnus-newsrc-alist)
      (let* ((name   (gnus-info-group g))
             (read   (gnus-desktop-notify-read-count g))
             (unread (gnus-group-unread name)))
        (when (and (numberp read) (numberp unread))
          (let* ((count     (+ read unread))
                 (old-count (lax-plist-get gnus-desktop-notify-counts name))
                 (delta     (- count (or old-count count)))
                 (notify    (gnus-group-find-parameter name 'group-notify)))
            (when (eq gnus-desktop-notify-groups
                      (if notify
                          'gnus-desktop-notify-explicit
                        'gnus-desktop-notify-all-except))
              (setq gnus-desktop-notify-counts
                    (lax-plist-put gnus-desktop-notify-counts name count))
              (when (and (> unread 0) (> delta 0))
                (push (cons (gnus-desktop-short-group-name name) delta)
                      updated-groups)))))))
    (when (and updated-groups (not (called-interactively-p 'any)))
      (mapc gnus-desktop-notify-function
            (gnus-desktop-notify-format-n updated-groups)))))

(defun gnus-desktop-shell-command (&rest args)
  "Execute ARGS as a synchronous shell command without I/O."
  (call-process-shell-command
   (mapconcat #'shell-quote-argument args " ") nil 0 nil))

;;; Notification backends

(defun gnus-desktop-notify-exec (body)
  "Call a program defined by `gnus-desktop-notify-exec-program'.
with each argument being a group formatted according to
`gnus-desktop-notify-format' and calling behavior is defined by
`gnus-desktop-notify-behavior'."
  (funcall #'gnus-desktop-shell-command gnus-desktop-notify-exec-program body))

(defun gnus-desktop-notify-send (body)
  "Invoke the configured `notify-send' program.
See `gnus-desktop-notify-send-program',
`gnus-desktop-notify-send-switches' and
`gnus-desktop-notify-behavior' for configuration options."
  (apply #'gnus-desktop-shell-command
         `(,gnus-desktop-notify-send-program
           ,@gnus-desktop-notify-send-switches
           "--"
           ,gnus-desktop-notify-send-subject
           ,body)))

(defun gnus-desktop-notify-dbus (body)
  "Generate a notification directly using `notifications' with
the behavior defined by `gnus-desktop-notify-behavior'."
  (notifications-notify :title gnus-desktop-notify-send-subject :body body))

(defun gnus-desktop-notify-alert (body)
  "Generate a notification directly using `alert' with
the behavior defined by `gnus-desktop-notify-behavior'."
  (alert body :title gnus-desktop-notify-send-subject))

;;; Minor mode

;;;###autoload
(define-minor-mode gnus-desktop-notify-mode
  "Gnus Desktop Notification mode uses libnotify's 'notify-send'
program to generate popup messages or call external executables
whenever a group receives new messages through gnus-demon (see
`gnus-demon-add-handler').

  You can actually call any program by changing the
`gnus-desktop-notify-exec-program' variable, or change the
behavior entirely by setting a different
`gnus-desktop-notify-function' function.

  See the `gnus-desktop-notify' customization group for more
details."
  :init-value nil
  :group 'gnus-desktop-notify
  :require 'gnus
  :global t
  (cond
   (gnus-desktop-notify-mode
    (add-hook 'gnus-after-getting-new-news-hook #'gnus-desktop-notify-check)
    (add-hook 'gnus-started-hook #'gnus-desktop-notify-check))
   (t
    (remove-hook 'gnus-after-getting-new-news-hook #'gnus-desktop-notify-check)
    (remove-hook 'gnus-started-hook #'gnus-desktop-notify-check))))

(provide 'gnus-desktop-notify)
;;; gnus-desktop-notify.el ends here
