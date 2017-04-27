;;; The Zeitgeist Emacs Script -- integrates Emacs with Zeitgeist.
;; Package-Version: 20131228.1009
;;; Copyright (C) 2010, Patrick M. Niedzielski <PatrickNiedzielski@gmail.com>
;;; Copyright (C) 2011, Tassilo Horn <tassilo@member.fsf.org>
;;; Copyright (C) 2012, Jo Lund Steffensen <jonlst@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; For more information about the Zeitgeist Project, see
;;; <http://zeitgeist-project.com/>.

;;* Code

(require 'cl)
(require 'dbus)

(defvar zeitgeist-emacs-application "application://emacs23.desktop")

(defvar zeitgeist-ignore-files-re nil)

(defvar zeitgeist-prevent-send nil)

;;** General Functions

(defun zeitgeist-call (method &rest args)
  "Call the zeitgeist method METHOD with ARGS over dbus"
  (condition-case err
	  (apply 'dbus-call-method
		 :session                            ; use the session (not system) bus
		 "org.gnome.zeitgeist.Engine"        ; service name
		 "/org/gnome/zeitgeist/log/activity" ; path name
		 "org.gnome.zeitgeist.Log"           ; interface name
		 method args)
  (dbus-error (message(format "zeitgeist-call %s failed due to D-Bus error %s" method err)))))

(defun zeitgeist-event-timestamp ()
  "Get the timestamp in zeitgeist format."
  (let* ((now-time (current-time))
         (hi       (car now-time))
         (lo       (car (cdr now-time)))
         (msecs    (car (cdr (cdr now-time))))) ; This is *micro*seconds.

    (substring (number-to-string
		(+ (/ msecs 1000)
		   (* (+ lo (* hi 65536.0))
		      1000)))
	       0 -2)))			   ; Convert system time to milliseconds.

(defun zeitgeist-get-mime-type ()
  "Get the mime type from the extension."
  (let ((ext (file-name-extension buffer-file-name)))
    ;; Maybe use file mode later...
    (cond ((string= "el" ext)      "text/x-script.elisp")

	  ((string= "cpp" ext)     "text/x-c++src")
	  ((string= "C" ext)       "text/x-c++src")
	  ((string= "c++" ext)     "text/x-c++src")
	  ((string= "cxx" ext)     "text/x-c++src")
	  ((string= "cc" ext)      "text/x-c++src")

	  ((string= "hpp" ext)     "text/x-c++hdr")
	  ((string= "h++" ext)     "text/x-c++hdr")
	  ((string= "hxx" ext)     "text/x-c++hdr")
	  ((string= "hh" ext)      "text/x-c++hdr")

	  ((string= "csv" ext)     "text/comma-separated-values")

	  ((string= "h" ext)       "text/x-chdr")

	  ((string= "c" ext)       "text/x-csrc")

	  ((string= "java" ext)    "text/x-java")

	  ((string= "p" ext)       "text/x-pascal")
	  ((string= "pas" ext)     "text/x-pascal")

	  ((string= "tcl" ext)     "text/x-tcl")
	  ((string= "tk" ext)      "text/x-tcl")

	  ((string= "tex" ext)     "text/x-tex")
	  ((string= "sty" ext)     "text/x-tex")
	  ((string= "cls" ext)     "text/x-tex")

	  ((string= "html" ext)    "text/html")
	  ((string= "htm" ext)     "text/html")

	  ((string= "latex" ext)   "application/x-latex")
	  ((string= "ltx" ext)     "application/x-latex")

	  ((string= "sh" ext)      "application/x-sh")

	  ((string= "pl" ext)      "application/x-perl")
	  ((string= "pm" ext)      "application/x-perl")

	  ((string= "texinfo" ext) "application/x-texinfo")
	  ((string= "texi" ext)    "application/x-texinfo")

	  ((string= "t" ext)       "application/x-troff")
	  ((string= "tr" ext)      "application/x-troff")
	  ((string= "roff" ext)    "application/x-troff")

	  ((string= "xml" ext)     "text/xml")
	  ((string= "xsd" ext)     "text/xml")

	  ((string= "xslt" ext)    "application/xslt+xml")
	  ((string= "xsl"  ext)    "application/xslt+xml")

	  ((string= "txt" ext)     "text/plain")
	  (t                       "text/plain"))))

(defun zeitgeist-event-interpretation (event)
  "Get the Event Interpretation of EVENT."
  (case event
    (zeitgeist-open-file-event
     "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#AccessEvent")
    (zeitgeist-close-file-event
     "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#LeaveEvent")
    (zeitgeist-create-file-event
     "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#CreateEvent")
    (zeitgeist-modify-file-event
     "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#ModifyEvent")
    (t (error "Unknown event %s" event))))

(defun zeitgeist-create-event (event-interpr uri subject-interpr subject-manifest
					     origin mimetype text storage
					     &optional payload)
  "Create an event according to the InsertEvents signature."
  (list
   ;; Signature: asaasay
   (list :struct
	 ;; Event metadata (as = array of strings)
	 (list
	  ""                          ;; ID, must not be set by clients!
	  (zeitgeist-event-timestamp) ;; Timpstamp
	  event-interpr               ;; Event Interpretation (what happened?)
	  ;; Manifestation
	  "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#UserActivity"
	  zeitgeist-emacs-application ;; Actor (the application, aka emacs)
      )
	 ;; List of subjects (aas)
	 (list
	  (list
     	   uri		   ;; The URI of the subject
	   subject-interpr ;; Interpretation (Is the subject a file, a mail, a video?)
	   subject-manifest ;; Manifestiation (Is the subject a local file, a web page?)
	   origin           ;; Origin
	   mimetype         ;; Mimetype
	   text             ;; Subject text
	   storage          ;; Storage
	   ))
	 ;; The payload (ay = array of bytes)
	 (or payload '(:array :byte 0)))))

(defun zeitgeist-send (event &rest props)
  "Send zeitgeist the EVENT with PROPS."
  (let ((event-interpretation (zeitgeist-event-interpretation event)))
    (condition-case err
        (case event
          ;; File handling events
          ((zeitgeist-open-file-event
            zeitgeist-close-file-event
            zeitgeist-create-file-event
            zeitgeist-modify-file-event)
           (unless (or zeitgeist-prevent-send
                       (some (lambda (re)
                               (string-match re (plist-get props :file)))
                             zeitgeist-ignore-files-re))
             (zeitgeist-call
              "InsertEvents"
              (zeitgeist-create-event
               event-interpretation
               (concat "file://" (plist-get props :file))
               "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#Document"
               "http://www.semanticdesktop.org/ontologies/nfo#FileDataObject"
               (concat "file://" (file-name-directory (plist-get props :file)))
               (zeitgeist-get-mime-type)
               (file-name-nondirectory
                (file-name-sans-versions (plist-get props :file)))
               ""))
             (message "zeitgeist event: %s" (file-name-nondirectory
                                             (file-name-sans-versions (plist-get props :file))))))
          ;; Email events
          ((zeitgeist-mail-read-event
            zeitgeist-mail-sent-event)
           ;; TODO: Implement me!
           ))
      ;; Ouch, something failed when trying to communicate with zeitgeist!
      (error (message "ERROR (ZEITGEIST): %s" (prin1-to-string err))))))

;;** Usual File Reading/Editing

(defun zeitgeist-open-file ()
  "Tell zeitgeist we opened a file!"
  (if (eq nil (buffer-file-name))
      (message "You are not on a file.")
    (zeitgeist-send 'zeitgeist-open-file-event
                    :file buffer-file-name)))

(defun zeitgeist-close-file ()
  "Tell zeitgeist we closed a file!"
  (when buffer-file-name
    (zeitgeist-send 'zeitgeist-close-file-event
                    :file buffer-file-name)))

(defun zeitgeist-create-file ()
  "Tell zeitgeist we created a file!"
  (zeitgeist-send 'zeitgeist-create-file-event
		  :file buffer-file-name))

(defun zeitgeist-modify-file ()
  "Tell zeitgeist we modified a file!"
  (zeitgeist-send 'zeitgeist-modify-file-event
		  :file buffer-file-name))

(defun zeitgeist-find-file-hook ()
  "Call zeitgeist-open-file if the file exists."
  (when (file-exists-p buffer-file-name)
    (zeitgeist-open-file)))

(defun zeitgeist-kill-buffer-hook ()
  "Call zeitgeist-close-file if the file exists."
  (if (and (not (eq nil (buffer-file-name)))
	   (file-exists-p (buffer-file-name)))
      (zeitgeist-close-file)))

(defun zeitgeist-kill-emacs-hook ()
  "Call zeitgeist-close-file on all files that exist."
  (mapc '(lambda (buffer)
	   (set-buffer buffer)
	   (zeitgeist-close-file))
	(buffer-list)))

(defun zeitgeist-before-save-hook ()
  "Call zeitgeist-modify-file or zeitgeist-create-file."
  (if (and (not (eq nil (buffer-file-name)))
	   (file-exists-p (buffer-file-name)))
      (zeitgeist-modify-file)
    (zeitgeist-create-file)))

(add-hook 'find-file-hook   'zeitgeist-find-file-hook)
(add-hook 'kill-buffer-hook 'zeitgeist-kill-buffer-hook)
(add-hook 'kill-emacs-hook  'zeitgeist-kill-emacs-hook)
(add-hook 'before-save-hook 'zeitgeist-before-save-hook)

;;** Record Reading/Writing Mail With Gnus

;; Hook run when mail is marked read: gnus-mark-article-hook

;(defun zeitgeist-gnus-message-read ())

;; Hook run when a message is sent off: message-sent-hook

;;* The End

(provide 'zeitgeist)

;;; zeitgeist.el ends here
