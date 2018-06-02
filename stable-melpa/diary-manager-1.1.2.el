;;; diary-manager.el --- Simple personal diary

;; Copyright (C) 2017 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 28 Dec 2017
;; Homepage: https://github.com/raxod502/diary-manager
;; Keywords: extensions
;; Package-Version: 1.1.2
;; Package-Requires: ((emacs "25"))
;; Version: 1.1.2

;;; Commentary:

;; diary-manager is a tool for keeping a simple personal diary from
;; the command line or from within other tools. The primary
;; implementation is in Python and is designed to be used from the
;; command line. diary-manager.el provides an alternative Emacs
;; interface.

;; Please see https://github.com/raxod502/diary-manager for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'parse-time)
(require 'seq)
(require 'subr-x)

;;;; Dynamically loaded functions

;; org.el
(declare-function org-read-date "org")

;;;; Basic user options

(defgroup diary-manager nil
  "Emacs interface for a simple personal diary."
  :group 'applications
  :prefix "dm-")

(defcustom dm-diary-location
  (getenv "DIARY_LOCATION")
  "Directory containing diary entries.
Defaults to the DIARY_LOCATION environment variable, if set."
  :group 'diary-manager
  :type '(choice
          (directory :tag "Directory")
          (const :tag "Not set" nil)))

(defcustom dm-diary-date-format
  (or (getenv "DIARY_DATE_FORMAT") "%Y-%m-%d-%a")
  "Format string for date in diary entry filenames.
This is passed to `format-time-string'. Defaults to
DIARY_DATE_FORMAT environment variable, if set."
  :group 'diary-manager
  :type 'string)

(defcustom dm-diary-entry-extension
  (or (getenv "DIARY_ENTRY_EXTENSION") ".md")
  "File extension for diary entries.
Defaults to DIARY_ENTRY_EXTENSION, if set."
  :group 'diary-manager
  :type 'string)

(defcustom dm-enable-git-integration t
  "Whether to integrate with Git when inside a Git repository."
  :group 'diary-manager
  :type 'boolean)

;;;; Utility functions

(defun dm-ensure-dm-diary-location-set ()
  "If `dm-diary-location' is not set, raise `user-error'."
  (unless dm-diary-location
    (user-error "Please set `dm-diary-location' first")))

(defun dm-ensure-org-read-date-defined ()
  "Load `org'. If it does not define `org-read-date', raise `error'."
  (require 'org)
  (unless (fboundp 'org-read-date)
    (error "Loading `org' did not define `org-read-date'")))

(defun dm-read-date (&optional prompt)
  "Interactively select a date using `org-read-date'.
PROMPT is a string to display for the prompt."
  (dm-ensure-org-read-date-defined)
  (apply #'encode-time
         (cl-mapcar
          (lambda (date-comp default-comp)
            (or date-comp default-comp))
          (parse-time-string (org-read-date nil nil nil prompt))
          (decode-time))))

(defcustom dm-read-date-function #'dm-read-date
  "Function used to select a date in interactive commands.
It takes one (optional) argument, the prompt to be displayed, and
returns a date object which may be formatted using
`format-time-string'."
  :group 'diary-manager
  :type 'function)

(defcustom dm-error-buffer-name "*diary-manager error*"
  "Name for buffer used by `dm-error-popup' to display errors."
  :group 'diary-manager
  :type 'string)

(defun dm-error-popup (message)
  "Pop up a temporary buffer in `special-mode' displaying MESSAGE.
The name of the buffer used is controlled by
`dm-error-buffer-name'."
  (when-let ((buf (get-buffer dm-error-buffer-name)))
    (kill-buffer buf))
  (with-current-buffer (get-buffer-create dm-error-buffer-name)
    (insert message)
    (special-mode)
    (pop-to-buffer (current-buffer))))

(defun dm-call-process (program &rest args)
  "Call PROGRAM with ARGS.
ARGS may be followed by keyword arguments, as follows. `:output'
may be `:stdout' (capture stdout), `:stderr' (capture stderr),
nil (capture neither), or t (capture both, and interleave them;
the default). Return a plist with keys `:args' (PROGRAM and the
arguments passed to it), `:kwargs' (keyword arguments passed to
this function, with defaults substituted), `:returncode' (an
integer, or nil if PROGRAM cannot be found), and `:output' (a
string or nil, whose meaning is determined by the `:output'
keyword argument)."
  (let* ((prog-args (seq-take-while #'stringp args))
         (kwargs   (seq-drop-while #'stringp args))
         (output-mode (if (plist-member kwargs :output)
                          (plist-get kwargs :output)
                        t)))
    (with-temp-buffer
      (let ((returncode
             (ignore-errors
               (apply #'call-process program nil
                      (pcase output-mode
                        (:stdout t)
                        (:stderr '(nil t))
                        (`nil nil)
                        (`t '(t t))
                        (_ (error "Invalid `:output' mode: %S"
                                  output-mode)))
                      nil prog-args))))
        `(:args (,program . ,prog-args)
                :kwargs ,(thread-first kwargs
                           (plist-put :output output-mode))
                :returncode ,returncode
                :output ,(and output-mode (buffer-string)))))))

(defun dm-format-process-error (message result)
  "Construct an error message string about a failed command.
MESSAGE is displayed at the beginning; an example is \"Command
failed\". RESULT is as returned by `dm-call-process'."
  (let ((cmd-string (string-join (mapcar #'shell-quote-argument
                                         (plist-get result :args))
                                 " ")))
    (if-let ((returncode (plist-get result :returncode)))
        (replace-regexp-in-string
         "\n*$" "\n"
         (format "%s [code %S]: %s\n\n%s"
                 message returncode cmd-string
                 (let ((output (plist-get result :output))
                       (no-output nil)
                       (name nil)
                       (return nil))
                   (pcase (thread-first result
                            (plist-get :kwargs)
                            (plist-get :output))
                     (:stdout (setq no-output "No output on stdout.")
                              (setq name "Stdout"))
                     (:stderr (setq no-output "No output on stderr.")
                              (setq name "Stderr"))
                     (`t      (setq no-output "No output on stdout or stderr.")
                              (setq name "Output"))
                     (`nil    (setq return "Output not captured."))
                     (output-mode (error "Invalid `:output' mode: %S"
                                         output-mode)))
                   (or return
                       (if (string-empty-p output)
                           no-output
                         (format "%s:\n%s\n\n%s"
                                 name
                                 (make-string 70 ?-)
                                 output))))))
      (format "%s [command not found]: %s\n"
              message cmd-string))))

(defun dm-validate-process (pred result)
  "If PRED applied to RESULT returns a message, throw an error.
This means display a popup and throw to `dm-error'."
  (when-let ((message (funcall pred result)))
    (dm-error-popup (dm-format-process-error
                     message result))
    (throw 'dm-error nil))
  result)

(defun dm-validator-program-found (result)
  "Check that the command's executable was found.
This is a predicate for use with `dm-validate-process'. RESULT is
as returned by `dm-call-process'."
  (unless (plist-get result :returncode)
    "Command failed"))

(defun dm-validator-command-succeeded (result)
  "Check that the command had a return code of 0.
This is a predicate for use with `dm-validate-process'. RESULT is
as returned by `dm-call-process'."
  (unless (= 0 (plist-get result :returncode))
    "Command failed"))

(defun dm-run-process (program &rest args)
  "Call PROGRAM with ARGS, and pop up an error if it cannot be run.
Return value and keyword arguments are as in `dm-call-process'."
  (thread-last (apply #'dm-call-process program args)
    (dm-validate-process #'dm-validator-program-found)))

(defun dm-check-process (program &rest args)
  "Call PROGRAM with ARGS, and pop up an error if it returns non-zero.
Return value and keyword arguments are as in `dm-call-process'."
  (thread-last (apply #'dm-run-process program args)
    (dm-validate-process #'dm-validator-command-succeeded)))

(defun dm-git-enabled-p ()
  "Return non-nil if `default-directory' is version-controlled with Git.
Throw an error if it is, but the repository is malformed or Git
is not installed."
  (when (and dm-enable-git-integration
             (locate-dominating-file default-directory ".git"))
    (thread-last (dm-check-process "git" "rev-parse" "--is-inside-work-tree")
      (dm-validate-process
       (lambda (result)
         (thread-first result
           (plist-get :output)
           (string-trim)
           (string= "true")
           (unless "Unexpected output")))))
    t))

(defun dm-git-file-exists-in-index (&optional file)
  "Return non-nil if FILE exists in the index.
FILE defaults to `buffer-file-name'."
  (let ((file (or file buffer-file-name)))
    (thread-first
        (dm-check-process
         "git" "ls-files" "--" file
         :output :stdout)
      (plist-get :output)
      (string-empty-p)
      (not))))

(defun dm-git-modified-p (&optional file)
  "Return non-nil if FILE is untracked or changed relative to HEAD.
FILE defaults to `buffer-file-name'."
  (let ((file (or file buffer-file-name)))
    (or
     ;; This catches untracked files.
     (and (file-exists-p file)
          (not (dm-git-file-exists-in-index file)))
     ;; This catches everything else.
     (thread-first
         (dm-run-process
          "git" "diff" "--exit-code" "HEAD" "--"
          file)
       (plist-get :returncode)
       (/= 0)))))

(defun dm-git-file-exists-in-head (&optional file)
  "Return non-nil if FILE exists in HEAD.
FILE defaults to `buffer-file-name'."
  (let ((file (or file buffer-file-name)))
    (thread-first
        (let ((default-directory
                (file-name-directory file)))
          (dm-run-process
           "git" "cat-file" "-e"
           (concat "HEAD:./"
                   (file-name-nondirectory file))))
      (plist-get :returncode)
      (= 0))))

(defun dm-git-rm (&optional file)
  "Use Git to remove FILE from the index and worktree.
FILE defaults to `buffer-file-name'."
  (let ((file (or file buffer-file-name)))
    (when (dm-git-file-exists-in-index file)
      (dm-check-process "git" "rm" "--cached" "--" file))
    (delete-file file)))

;;;; Minor mode

(defvar dm-edit-mode)

(defvar-local dm-buffer-date nil
  "Date for the current buffer's diary entry, or nil.
This is used internally by `diary-manager' to construct a commit
message when the entry is completed, if Git integration is
enabled.")

(defvar-local dm-buffer-original-contents nil
  "Original contents of the current buffer's diary entry.
This is a string or nil (if the entry did not previously exist).
It is used internally by `diary-manager' to determine whether
changes have been made to the entry.")

(defvar-local dm-buffer-saved-contents nil
  "Current saved contents of the current buffer's diary entry.
This is a string or nil (if the entry has not been saved). It is
used internally by `diary-manager' to determine whether changes
have been made to the entry.")

(defvar-local dm-buffer-dedicated nil
  "Non-nil if buffer was just created for editing a diary entry.
This means it can be killed without a problem if `dm-edit-mode'
fails to be enabled.")

(defun dm-update-saved-buffer-contents ()
  "Set `dm-buffer-saved-contents'."
  (setq dm-buffer-saved-contents (buffer-string)))

(defun dm-ensure-buffer-visiting-diary-entry ()
  "Raise `user-error' if current buffer is not visiting a diary entry.
Diary entries can only be visited correctly using `dm-edit'."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (unless dm-edit-mode
    (user-error "Buffer does not have `dm-edit-mode' enabled")))

(cl-defun dm-save-entry ()
  "Save the diary entry in the current buffer."
  (interactive)
  (dm-ensure-buffer-visiting-diary-entry)
  (save-buffer)
  (let ((entry-name (if dm-buffer-date
                        (concat "for " (format-time-string
                                        dm-diary-date-format
                                        dm-buffer-date))
                      (format "'%s'"
                              (file-name-nondirectory
                               buffer-file-name)))))
    (catch 'dm-error
      (cond
       ((dm-git-enabled-p)
        (if (dm-git-modified-p)
            (progn
              (dm-check-process "git" "add" buffer-file-name)
              (dm-check-process
               "git" "commit" "-m"
               (format "%s entry %s"
                       (if (dm-git-file-exists-in-head)
                           "Edit"
                         "Create")
                       entry-name))
              (message "Entry %s saved" entry-name))
          (message "No changes")))
       ((equal dm-buffer-original-contents dm-buffer-saved-contents)
        (message "No changes"))
       (t (message "Entry %s saved" entry-name)))
      (kill-buffer))))

(cl-defun dm-discard-entry ()
  "Discard the diary entry in the current buffer."
  (interactive)
  (dm-ensure-buffer-visiting-diary-entry)
  (let ((entry-name (if dm-buffer-date
                        (concat "for " (format-time-string
                                        dm-diary-date-format
                                        dm-buffer-date))
                      (format "'%s'"
                              (file-name-nondirectory
                               buffer-file-name))))
        (msg "Entry dismissed"))
    (catch 'dm-error
      (when (buffer-modified-p)
        (if (yes-or-no-p "Discard unsaved changes? ")
            (setq msg "Unsaved changes to entry %s discarded")
          (cl-return-from dm-discard-entry)))
      (unless (equal dm-buffer-original-contents
                     dm-buffer-saved-contents)
        (when (yes-or-no-p "Discard saved changes? ")
          (if dm-buffer-original-contents
              (progn
                (erase-buffer)
                (insert dm-buffer-original-contents)
                (save-buffer)
                (setq msg "Saved changes to entry %s discarded"))
            (delete-file buffer-file-name)
            (setq msg "Entry %s discarded"))))
      (when (and (dm-git-enabled-p) (dm-git-modified-p))
        (if (dm-git-file-exists-in-head)
            (when (yes-or-no-p "Revert to last commit? ")
              (dm-check-process "git" "checkout" "--" buffer-file-name)
              (setq msg "Entry %s reverted"))
          (when (yes-or-no-p "Delete entry? ")
            (dm-git-rm)
            (setq msg "Entry %s deleted"))))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (message msg entry-name))))

(defcustom dm-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'dm-save-entry)
    (define-key map (kbd "C-c C-k") #'dm-discard-entry)
    map)
  "Keymap for use in `dm-edit-mode'."
  :group 'diary-manager
  :type 'sexp)

(defcustom dm-edit-mode-message
  "Type \\[dm-save-entry] to finish, or \\[dm-discard-entry] to cancel"
  "Message displayed when entering `dm-edit-mode'.
This is passed to `substitute-command-keys' before being
displayed. If nil, no message is displayed."
  :group 'diary-manager
  :type 'string)

;;;###autoload
(define-minor-mode dm-edit-mode
  "Minor mode for editing diary entries.
Use \\[dm-edit] to edit a diary entry in `dm-diary-location', or
\\[dm-find-file] to edit an arbitrary file as a diary entry.
Alternatively, you can invoke this mode to "
  :keymap dm-edit-mode-map
  (if dm-edit-mode
      (unwind-protect
          (catch 'dm-error
            ;; If we get an error and `dm-edit-mode' doesn't get set
            ;; back to t below, then this will cause the mode to be
            ;; automatically disabled again by `unwind-protect'.
            (setq dm-edit-mode nil)
            ;; Validate that we can perform editing.
            (unless buffer-file-name
              (user-error "Buffer is not visiting a file"))
            (dm-git-enabled-p)
            ;; Set up variables.
            (setq dm-buffer-original-contents
                  (and (file-exists-p buffer-file-name) (buffer-string)))
            (setq dm-buffer-saved-contents dm-buffer-original-contents)
            ;; Take care of remaining setup.
            (add-hook 'after-save-hook
                      #'dm-update-saved-buffer-contents
                      nil 'local)
            ;; Finish.
            (setq dm-edit-mode t)
            ;; We need to do this when `dm-edit-mode' is non-nil,
            ;; since otherwise the `substitute-command-keys' won't
            ;; work right.
            (message "%s" (substitute-command-keys dm-edit-mode-message)))
        (unless dm-edit-mode
          (dm-edit-mode -1)
          (message "Failed to enable `dm-edit-mode'")))
    (if dm-buffer-dedicated
        (kill-buffer)
      (remove-hook 'after-save-hook #'dm-update-saved-buffer-contents 'local)
      (kill-local-variable 'dm-buffer-date)
      (kill-local-variable 'dm-buffer-original-contents)
      (kill-local-variable 'dm-buffer-saved-contents))))

;; FIXME: make sure duplicate messages aren't printed

;;;; Interactive functions

;;;###autoload
(defun dm-edit (date)
  "Edit the diary entry for DATE.
Interactively, select DATE using `dm-read-date-function'."
  (interactive
   (progn
     (dm-ensure-dm-diary-location-set)
     (list (funcall dm-read-date-function "[Entry to edit]"))))
  (dm-ensure-dm-diary-location-set)
  (find-file
   (expand-file-name
    (concat (format-time-string dm-diary-date-format date)
            dm-diary-entry-extension)
    dm-diary-location))
  (setq dm-buffer-date date)
  (setq dm-buffer-dedicated t)
  (dm-edit-mode +1))

;;;###autoload
(defun dm-find-file (file)
  "Edit FILE as a diary entry.
Interactively, select DATE using `read-file-name'."
  (interactive
   (list (read-file-name "[File to edit] ")))
  (find-file file)
  (setq dm-buffer-dedicated t)
  (dm-edit-mode +1))

;;;###autoload
(defun dm-remove (date)
  "Remove the diary entry for DATE.
Interactively, select DATE using `dm-read-date-function'."
  (interactive
   (progn
     (dm-ensure-dm-diary-location-set)
     (list (funcall dm-read-date-function "[Entry to remove]"))))
  (dm-ensure-dm-diary-location-set)
  (let* ((entry-name (format-time-string dm-diary-date-format date))
         (filename (expand-file-name
                    (concat entry-name dm-diary-entry-extension))))
    (unless (file-exists-p filename)
      (user-error "No entry for %s" entry-name))
    (catch 'dm-error
      (when (yes-or-no-p (format "Delete entry for %s? " entry-name))
        (if (dm-git-enabled-p)
            (progn
              (dm-git-rm filename)
              (dm-check-process
               "git" "commit" "-m"
               (format "Delete entry for %s" entry-name)))
          (delete-file buffer-file-name))
        (message "Entry for %s deleted" entry-name)))))

(cl-defun dm-move-or-copy (task &optional old-date new-date)
  "Run TASK (`move' or `copy') on OLD-DATE and NEW-DATE.
This either moves or copies a diary entry. OLD-DATE and NEW-DATE
can both be nil, in which case they are determined by prompting
the user."
  (unless (memq task '(move copy))
    (error "Invalid task `%S'" task))
  (unless old-date
    (setq old-date
          (funcall dm-read-date-function
                   (format "[Entry to %s]"
                           (symbol-name task)))))
  (let* ((src-name (format-time-string dm-diary-date-format old-date))
         (src-path (expand-file-name
                    (concat src-name dm-diary-entry-extension)
                    dm-diary-location)))
    (unless (file-exists-p src-path)
      (user-error "No entry for %s" src-name))
    (unless new-date
      (setq new-date
            (funcall dm-read-date-function
                     (format "[Destination for %s]"
                             (symbol-name task)))))
    (let* ((dst-name (format-time-string dm-diary-date-format new-date))
           (dst-path (expand-file-name
                      (concat dst-name dm-diary-entry-extension)
                      dm-diary-location)))
      (when (string= src-path dst-path)
        (user-error "Cannot move `%s' to itself" src-path))
      (when (yes-or-no-p
             (format "%s entry for %s to %s? "
                     (capitalize (symbol-name task)) src-name dst-name))
        (unless (and (file-exists-p dst-path)
                     (not (yes-or-no-p
                           (format "Overwrite existing entry for %s? "
                                   dst-name))))
          (funcall (pcase task
                     (`move #'rename-file)
                     (`copy #'copy-file))
                   src-path dst-path)
          (catch 'dm-error
            (when (dm-git-enabled-p)
              (dm-check-process "git" "add" src-path dst-path)
              (dm-check-process
               "git" "commit" "-m"
               (format "%s entry for %s to %s"
                       (capitalize (symbol-name task))
                       src-name dst-name)))
            (message "Entry for %s %s to %s"
                     src-name
                     (pcase task
                       (`move "moved")
                       (`copy "copied"))
                     dst-name)))))))

;;;###autoload
(defun dm-move (&optional old-date new-date)
  "Move the diary entry for OLD-DATE to NEW-DATE.
If either of OLD-DATE and NEW-DATE are not given, they are read
interactively from the user."
  (interactive)
  (dm-move-or-copy 'move old-date new-date))

;;;###autoload
(defun dm-copy (&optional old-date new-date)
  "Copy the diary entry for OLD-DATE to NEW-DATE.
If either of OLD-DATE and NEW-DATE are not given, they are read
interactively from the user."
  (interactive)
  (dm-move-or-copy 'copy old-date new-date))

;;;###autoload
(defun dm-browse ()
  "Open Dired on `dm-diary-location'."
  (interactive)
  (dired dm-diary-location))

;;;; Closing remarks

(provide 'diary-manager)

;;; diary-manager.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; sentence-end-double-space: nil
;; End:
