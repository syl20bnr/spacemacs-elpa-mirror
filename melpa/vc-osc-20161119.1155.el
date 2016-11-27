;;; vc-osc.el --- non-resident support for osc version-control
;; Package-Version: 20161119.1155

;; Copyright (C) 2012 Adam Spiers <aspiers@suse.com>

;; Author:      Adam Spiers (see vc.el for full credits)
;; Maintainer:  Adam Spiers <aspiers@suse.com>

;; This file is not (yet) part of GNU Emacs.

;; vc-osc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; vc-osc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; osc is the CLI tool for interacting with the Open Build Service
;; http://www.open-build-service.org/
;;
;; Thanks and credit to Stefan Monnier and all other contributors
;; to vc-svn.el, on which this is heavily based.

;;; Code:

(eval-when-compile
  (require 'vc))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'OSC 'vc-functions nil)

;;;
;;; Customization options
;;;

(defcustom vc-osc-program "osc"
  "Name of the OSC executable."
  :type 'string
  :group 'vc)

(defcustom vc-osc-global-switches nil
  "Global switches to pass to any OSC command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc)

(defcustom vc-osc-register-switches nil
  "Switches for registering a file into OSC.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "22.1"
  :group 'vc)

(defvar vc-osc-header '()
  "Header keywords to be inserted by `vc-insert-headers'.")

;; We want to autoload it for use by the autoloaded version of
;; vc-osc-registered, but we want the value to be compiled at startup, not
;; at dump time.
;; ;;;###autoload
(defconst vc-osc-admin-directory ".osc"
  "The name of the \".osc\" subdirectory or its equivalent.")

;;; Properties of the backend

(defun vc-osc-revision-granularity () 'repository)
(defun vc-osc-checkout-model (files) 'implicit)

;;;
;;; State-querying functions
;;;

(defun vc-osc-admin-path (dir)
  "The full path to the \".osc\" subdirectory for the given directory."
  (let ((path (expand-file-name vc-osc-admin-directory dir)))
    (or (file-directory-p path)
        (error "%s is not a valid directory" path))
    path))

(defun vc-osc-project (file)
  "Return the name of the Build Service project containing the
package which contains the given file."
  (vc-osc-admin-file-contents (file-name-directory file) "_project"))

(defun vc-osc-package (file)
  "Return the name of the Build Service package containing the
package which contains the given file."
  (vc-osc-admin-file-contents (file-name-directory file) "_package"))

(defun vc-osc-admin-file-contents (admin-dir admin-file)
  "Return the contents of a file in the .osc/ meta-data sub-directory
of the given directory."
  (let ((path (expand-file-name admin-file
                                (vc-osc-admin-path admin-dir))))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-substring (point-min) (1- (point-max))))))  

(defun vc-osc-registered (file)
  "Check if FILE is OSC registered."
  (when (file-readable-p (expand-file-name (concat vc-osc-admin-directory
						   "/_files")
					   (file-name-directory file)))
    (with-temp-buffer
      (cd (file-name-directory file))
      (let* (process-file-side-effects
	     (status
             (condition-case nil
                 ;; Ignore all errors.
                 (vc-osc-command t t file "status" "-v")
               ;; Some problem happened.  E.g. We can't find an `osc'
               ;; executable.  We used to only catch `file-error' but when
               ;; the process is run on a remote host via Tramp, the error
               ;; is only reported via the exit status which is turned into
               ;; an `error' by vc-do-command.
               (error nil))))
        (when (eq 0 status)
	  (let ((parsed (vc-osc-parse-status file)))
	    (and parsed (not (memq parsed '(ignored unregistered))))))))))

(defun vc-osc-state (file)
  "OSC-specific version of `vc-state'."
  (let (process-file-side-effects)
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-osc-command t 0 file "status" "-v")
      (vc-osc-parse-status file))))

(defun vc-osc-after-dir-status (callback)
  (let ((state-map '((?A . added)
                     (?C . conflict)
                     (?M . edited)
                     (?D . removed)
                     (?! . removed)
                     (?? . unregistered)
                     ))
	(re "^\\([ ACDGMSU!]\\)    \\(.+?\\) *")
       result)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((state (cdr (assq (aref (match-string 1) 0) state-map)))
	    (filename (match-string 2)))
        ;; FIXME are there other possible combinations?
        (cond ((eq state 'edited) (setq state 'needs-merge))
              ((not state) (setq state 'needs-update)))
	(when (and state (not (string= "." filename)))
         (setq result (cons (list filename state) result)))))
    (funcall callback result)))

(defun vc-osc-dir-status (dir callback)
  "Run 'osc status' for DIR and update BUFFER via CALLBACK.
CALLBACK is called as (CALLBACK RESULT BUFFER), where
RESULT is a list of conses (FILE . STATE) for directory DIR."
  ;; FIXME should this rather be all the files in dir?
  (vc-osc-command (current-buffer) 'async nil "status")
  (vc-exec-after
     `(vc-osc-after-dir-status ',callback)))

(defun vc-osc-dir-status-files (dir files default-state callback)
  (apply 'vc-osc-command (current-buffer) 'async nil "status" files)
  (vc-exec-after
   `(vc-osc-after-dir-status ',callback)))

(defvar vc-osc-dir-extra-headers
  '((project    "Project"    "Project name")
    (package    "Package"    "Package name")
    (path       "Path"       "Path"        )
    (api-url    "API URL"    "API URL"     )
    (source-url "Source URL" "Source URL"  )
    (revision   "Revision"   "Revision"    ))
  "Associative list mapping header symbols to names.")

(defun vc-osc-dir-extra-headers (dir)
  "Generate extra status headers for a osc working copy."
  (let (process-file-side-effects)
    (vc-osc-command "*vc*" 0 nil "info"))

  (mapconcat
   (lambda (elt)
     (let* ((sym    (car elt))
            (name   (nth 1 elt))
            (regexp (concat "^" (nth 2 elt) ": *\\(.+\\)"))
            (value  (with-current-buffer "*vc*" (vc-parse-buffer regexp 1))))
       (cond (value
              (format "%-11s%s %s"
                       (propertize name  'face 'font-lock-type-face)
                       (propertize ":"   'face 'font-lock-type-face)
                       (propertize value 'face 'font-lock-variable-name-face)))
             (t ""))))
   vc-osc-dir-extra-headers "\n"))

(defun vc-osc-working-revision (file)
  "OSC-specific version of `vc-working-revision'."
  ;; There is no need to consult RCS headers under OSC, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in OSC.
  (vc-osc-registered file)
  (vc-file-getprop file 'vc-working-revision))

;; vc-osc-mode-line-string doesn't exist because the default implementation
;; works just fine.

(defun vc-osc-previous-revision (file rev)
  (let ((newrev (1- (string-to-number rev))))
    (when (< 0 newrev)
      (number-to-string newrev))))

(defun vc-osc-next-revision (file rev)
  (let ((newrev (1+ (string-to-number rev))))
    ;; The "working revision" is an uneasy conceptual fit under osc;
    ;; we use it as the upper bound until a better idea comes along.  If the
    ;; workfile version W coincides with the tree's latest revision R, then
    ;; this check prevents a "no such revision: R+1" error.  Otherwise, it
    ;; inhibits showing of W+1 through R, which could be considered anywhere
    ;; from gracious to impolite.
    (unless (< (string-to-number (vc-file-getprop file 'vc-working-revision))
               newrev)
      (number-to-string newrev))))


;;;
;;; State-changing functions
;;;

;; FIXME: support osc mkpac

(defun vc-osc-register (files &optional rev comment)
  "Register FILES into the OSC version-control system.
The COMMENT argument is ignored  This does an add but not a commit.
Passes either `vc-osc-register-switches' or `vc-register-switches'
to the OSC command."
  (apply 'vc-osc-command nil 0 files "add" (vc-switches 'OSC 'register)))

(defun vc-osc-responsible-p (file)
  "Return non-nil if OSC thinks it is responsible for FILE."
  (file-directory-p (expand-file-name vc-osc-admin-directory
				      (if (file-directory-p file)
					  file
					(file-name-directory file)))))

(defalias 'vc-osc-could-register 'vc-osc-responsible-p
  "Return non-nil if FILE could be registered in OSC.
This is only possible if OSC is responsible for FILE's directory.")

(defun vc-osc-checkin (files rev comment)
  "OSC-specific version of `vc-backend-checkin'."
  (if rev (error "Committing to a specific revision is unsupported in OSC"))
  (let ((status (apply
                 'vc-osc-command nil 1 files "ci"
                 (nconc (list "-m" comment) (vc-switches 'OSC 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; Check checkin problem.
      (cond
       ((search-forward "Transaction is out of date" nil t)
        (dolist (file files) (vc-file-setprop file 'vc-state 'needs-merge))
        (error (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Update file properties
    (dolist (file files)
      (vc-file-setprop
       file 'vc-working-revision
       (vc-parse-buffer "^Committed revision \\([0-9]+\\)" 1)))
    ))

(defun vc-osc-find-revision (file rev buffer)
  "OSC-specific retrieval of a specified version into a buffer."
  (let (process-file-side-effects)
    (apply 'vc-osc-command
	   buffer 0 file
           (vc-switches 'OSC 'checkout)
           "cat"
	   (and rev (not (string= rev ""))
		(concat "-r" rev))
	   (vc-osc-project file)
           (vc-osc-package file)
           nil)))

(defun vc-osc-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-osc-update file editable rev (vc-switches 'OSC 'checkout)))
  (vc-mode-line file 'OSC)
  (message "Checking out %s...done" file))

(defun vc-osc-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, there's nothing to do.
      nil
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-working-revision nil)
    (apply 'vc-osc-command nil 0 file
	   "update"
	   (if (or (null rev) (eq rev t) (equal rev ""))
               nil
             (concat "-r" rev))
	   switches)))

(defun vc-osc-delete-file (file)
  (vc-osc-command nil 0 file "remove"))

(defun vc-osc-rename-file (old new)
  (vc-osc-command nil 0 new "move" (file-relative-name old)))

(defun vc-osc-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (unless contents-done
    (vc-osc-command nil 0 file "revert")))

(defun vc-osc-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-working-revision nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-osc-command nil 0 file "update")
  ;; Analyze the merge result reported by osc, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new working revision
    (if (re-search-forward
	 "^\\(Updated to\\|At\\) revision \\([0-9]+\\)" nil t)
	(vc-file-setprop file 'vc-working-revision (match-string 2)))
    ;; get file status
    (goto-char (point-min))
    (prog1
        (if (looking-at "At revision")
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([ACGDU]\\)    "
                       (regexp-quote (file-name-nondirectory file)))
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((string= (match-string 1) "U")
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "G")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze osc update result")))
      (message "Merging changes into %s...done" file))))

;;;
;;; History functions
;;;

(defvar log-view-per-file-logs)

(define-derived-mode vc-osc-log-view-mode log-view-mode "OSC-Log-View"
  (require 'add-log)
  (set (make-local-variable 'log-view-per-file-logs) nil))

(defun vc-osc-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log(s) associated with FILES."
  (if shortlog (error "Generating a short log is unsupported in OSC"))
  ;; FIXME: support limiting of log entries.
  ;; vc-print-log defaults to limiting the number of entries to the value of
  ;; `vc-log-show-limit', so we can't generate an error - just have to silently
  ;; ignore the limit for now.
  ;; (if limit (error "Limiting the number of log entries is unsupported in OSC"))
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      ;; Dump log for the entire directory.
      (apply 'vc-osc-command buffer 0 nil "log"
             (list (if start-revision (format "-r%s" start-revision) nil))))))

(defun vc-osc-diff (files &optional oldvers newvers buffer async)
  "Get a difference report using OSC between two revisions of fileset FILES."
  (and oldvers
       (not newvers)
       files
       (catch 'no
	 (dolist (f files)
	   (or (equal oldvers (vc-working-revision f))
	       (throw 'no nil)))
	 t)
       ;; Use nil rather than the current revision because osc handles
       ;; it better (i.e. locally).  Note that if _any_ of the files
       ;; has a different revision, we fetch the lot, which is
       ;; obviously sub-optimal.
       (setq oldvers nil))
  (let* ((switches nil))
      (apply 'vc-osc-command buffer
	     (if async 'async 0)
	     files "diff"
	     (append
	      switches
	      (when oldvers
		(list "-r" (if newvers (concat oldvers ":" newvers)
			     oldvers)))))
      (if async 1		      ; async diff => pessimistic assumption
	;; For some reason `osc diff' does not return a useful
	;; status w.r.t whether the diff was empty or not.
	(buffer-size (get-buffer buffer)))))

;;;
;;; Miscellaneous
;;;

;; osc makes backups for us, so don't bother.
;; (defun vc-osc-make-version-backups-p (file)
;;   "Return non-nil if version backups should be made for FILE."
;;  (vc-stay-local-p file 'OSC))

(defun vc-osc-check-headers ()
  "Check if the current file has any headers in it."
  nil)


;;;
;;; Internal functions
;;;

(defun vc-osc-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-osc.el.
The difference to vc-do-command is that this function always invokes `osc',
and that it passes `vc-osc-global-switches' to it before FLAGS."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-osc-program file-or-list
         (if (stringp vc-osc-global-switches)
             (cons vc-osc-global-switches flags)
           (append vc-osc-global-switches
                   flags))))

(defun vc-osc-repository-hostname (dirname)
  (with-temp-buffer
    (let ((coding-system-for-read
	   (or file-name-coding-system
	       default-file-name-coding-system)))
      (vc-insert-file (expand-file-name (concat vc-osc-admin-directory
						"/entries")
					dirname)))
    (goto-char (point-min))
    (when (re-search-forward
	   ;; Old `osc' used name="osc:this_dir", newer use just name="".
	   (concat "name=\"\\(?:osc:this_dir\\)?\"[\n\t ]*"
		   "\\(?:[-a-z]+=\"[^\"]*\"[\n\t ]*\\)*?"
		   "url=\"\\(?1:[^\"]+\\)\""
                   ;; Yet newer ones don't use XML any more.
                   "\\|^\ndir\n[0-9]+\n\\(?1:.*\\)") nil t)
      ;; This is not a hostname but a URL.  This may actually be considered
      ;; as a feature since it allows vc-osc-stay-local to specify different
      ;; behavior for different modules on the same server.
      (match-string 1))))

(defun vc-osc-resolve-when-done ()
  "Call \"osc resolved\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-osc-command nil 0 buffer-file-name "resolved")
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook 'vc-osc-resolve-when-done t))))

;; Inspired by vc-arch-find-file-hook.
(defun vc-osc-find-file-hook ()
  (when (eq ?C (vc-file-getprop buffer-file-name 'vc-osc-status))
    ;; If the file is marked as "conflicted", then we should try and call
    ;; "osc resolved" when applicable.
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
        ;; There are conflict markers.
        (progn
          (smerge-start-session)
          (add-hook 'after-save-hook 'vc-osc-resolve-when-done nil t))
      ;; There are no conflict markers.  This is problematic: maybe it means
      ;; the conflict has been resolved and we should immediately call "osc
      ;; resolved", or it means that the file's type does not allow Osc to
      ;; use conflict markers in which case we don't really know what to do.
      ;; So let's just punt for now.
      nil)
    (message "There are unresolved conflicts in this file")))

(defun vc-osc-parse-status (&optional filename)
  "Parse output of \"osc status\" command in the current buffer.
Set file properties accordingly.  Unless FILENAME is non-nil, parse only
information about FILENAME and return its status."
  (let (file status)
    (goto-char (point-min))
    (while (re-search-forward
            ;; Ignore the files with status X.
	    "^\\([ ACDGMSU!]\\)    \\(.+?\\) *" nil t)
      ;; If the username contains spaces, the output format is ambiguous,
      ;; so don't trust the output's filename unless we have to.
      (setq file (or filename
                     (expand-file-name
                      (buffer-substring (point) (line-end-position)))))
      (setq status (char-after (line-beginning-position)))
      (if (eq status ??)
	  (vc-file-setprop file 'vc-state 'unregistered)
	;; Use the last-modified revision, so that searching in vc-print-log
	;; output works.
	(vc-file-setprop file 'vc-working-revision (match-string 3))
        ;; Remember Osc's own status.
        (vc-file-setprop file 'vc-osc-status status)
	(vc-file-setprop
	 file 'vc-state
	 (cond
	  ((eq status ?\ )
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-update
             (vc-file-setprop file 'vc-checkout-time
                              (nth 5 (file-attributes file)))
	     'up-to-date))
	  ((eq status ?A)
	   ;; If the file was actually copied, (match-string 2) is "-".
	   (vc-file-setprop file 'vc-working-revision "0")
	   (vc-file-setprop file 'vc-checkout-time 0)
	   'added)
	  ((eq status ?C)
	   (vc-file-setprop file 'vc-state 'conflict))
	  ((eq status '?M)
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-merge
	     'edited))
	  ((eq status ?I)
	   (vc-file-setprop file 'vc-state 'ignored))
	  ((memq status '(?D ?R))
	   (vc-file-setprop file 'vc-state 'removed))
	  (t 'edited)))))
    (when filename (vc-file-getprop filename 'vc-state))))

(defun vc-osc-valid-revision-number-p (tag)
  "Return non-nil if TAG is a valid revision number."
  (or (string-match "^[1-9][0-9]*$" tag)
      (string-match "^[0-9a-f]\\{32\\}$" tag)))

(provide 'vc-osc)

;;; vc-osc.el ends here
