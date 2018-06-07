;;; tfsmacs.el --- MS TFS source control interaction.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Sebastian Monia
;;
;; Author: Dino Chiesa <dpchiesa@outlook.com>, Sebastian Monia <smonia@outlook.com>
;; URL: http://github.com/sebasmonia/tfsmacs/
;; Package-Version: 20180606.2224
;; Package-Requires: ((emacs "25") (tablist "0.70"))
;; Version: 1.0
;; Keywords: tfs, vc

;; This file is not part of GNU Emacs.

;;; License: MIT

;;; Commentary:

;; Basic steps to setup:
;;   1. Obtain the Team Explorer Everywhere CLI tool from https://github.com/Microsoft/team-explorer-everywhere/releases
;;   2. Place `tfsmacs.el' in your `load-path'.
;;   3. In your .emacs file:
;;        (require 'tfsmacs)
;;        (setq tfsmacs-cmd  "location/of/TEE/tf")
;;        (setq tfsmacs-login "/login:domain\\userid,password")
;;   4. Also in your .emacs file,  set local or global key bindings for tfs commands.  Or use the provided keymap.
;;      Example:
;;        (global-set-key  "\C-ct" 'tfsmacs-map)
;;      OR:
;;        (global-set-key  "\C-ctp" 'tfsmacs-pendingchanges)
;;        (global-set-key  "\C-cto" 'tfsmacs-checkout)
;;        (global-set-key  "\C-cti" 'tfsmacs-checkin)
;;        (global-set-key  "\C-ctr" 'tfsmacs-rename)
;;        (global-set-key  "\C-ctg" 'tfsmacs-get)
;;        (global-set-key  "\C-cth" 'tfsmacs-history)
;;        (global-set-key  "\C-ctc" 'tfsmacs-changeset)
;;        (global-set-key  "\C-ctu" 'tfsmacs-undo)
;;        (global-set-key  "\C-ct-" 'tfsmacs-delete)
;;        (global-set-key  "\C-ct+" 'tfsmacs-add)
;; For a detailed user manual see:
;; https://github.com/sebasmonia/tfsmacs/blob/master/README.md

;;; Code:

(require 'dom)
(require 'tablist)

(defgroup tfsmacs nil
  "MS TFS source control interaction."
  :group 'extensions)

(defcustom tfsmacs-cmd  "C:/HomeFolder/TEE-CLC-14.134.0/tf.cmd"
  "Location of the 'Team Explorer Everywhere' command line tool."
  :type 'string)

(defcustom tfsmacs-login ""
  "Values for the -login option.  Ignored if empty."
  :type 'string)

(defcustom tfsmacs-collection-url ""
  "URL of the TFS Collection.  If empty, the TEE CLI will assume the collection from the existing folder mappings."
  :type 'string)

(defcustom tfsmacs-log-buffer-name "*TFS Log*"
  "Name of the TFS log buffer."
  :type 'string)

(defvar tfsmacs--buffer-status-dir nil)
(defvar tfsmacs--history-target "")
(defvar tfsmacs--process-name "TEECLI")
(defvar tfsmacs--changeset-buffer-name "*TFS Changeset*")
;; These two variables are used to accumulate the output of their respective commands.
;; Once the output can be parsed, that means the XML is completed and the command finished
;; processing. Then the variables are emptied for the next call.
;; I couldn't find a better way since "tf @" doesn't show a prompt. I could try something like
;; comint-mode but this seemed simpler than changing the way the underlying process works.
(defvar tfsmacs--history-xml-buffer "")
(defvar tfsmacs--status-xml-buffer "")

(define-prefix-command 'tfsmacs-map)
(define-key tfsmacs-map "p" 'tfsmacs-pending-changes)
(define-key tfsmacs-map "o" 'tfsmacs-checkout)
(define-key tfsmacs-map "i" 'tfsmacs-checkin)
(define-key tfsmacs-map "r" 'tfsmacs-rename)
(define-key tfsmacs-map "g" 'tfsmacs-get)
(define-key tfsmacs-map "d" 'tfsmacs-get-recursive)
(define-key tfsmacs-map "h" 'tfsmacs-history)
(define-key tfsmacs-map "c" 'tfsmacs-changeset)
(define-key tfsmacs-map "u" 'tfsmacs-undo)
(define-key tfsmacs-map "-" 'tfsmacs-delete)
(define-key tfsmacs-map "+" 'tfsmacs-add)

(defun tfsmacs--get-or-create-process ()
  "Create or return the TEE process."
  (let ((buffer-name (format "*%s*" tfsmacs--process-name)))
    (when (not (get-process tfsmacs--process-name))
      (tfsmacs--append-to-log "Creating new process instance...")
      (start-process tfsmacs--process-name buffer-name
                     tfsmacs-cmd "@"))
    (tfsmacs--append-to-log "Returned process handle.")
    (get-process tfsmacs--process-name)))

(defun tfsmacs--process-command-sync-to-buffer (command output-buffer-name)
  "Create a new instance of the TEE process, execute COMMAND and write output to OUTPUT-BUFFER-NAME."
  (let* ((collection-param (tfsmacs--get-collection-parameter))
         (login-param (tfsmacs--get-login-parameter))
         (params (append command (list collection-param login-param))))
    (tfsmacs--append-to-log (format "Command input (sync): %s" (prin1-to-string params)))
    (when (get-buffer output-buffer-name)
      (kill-buffer output-buffer-name))
    (apply 'call-process tfsmacs-cmd nil output-buffer-name nil params)))

(defun tfsmacs--process-command (command handler)
  "Set HANDLER as the filter function for the TEE CLI process and send COMMAND as string."
  (let* ((collection-param (tfsmacs--get-collection-parameter))
         (login-param (tfsmacs--get-login-parameter))
         (command-string (concat (mapconcat 'identity command " ") collection-param login-param "\n")))
    (set-process-filter (tfsmacs--get-or-create-process) handler)
    (tfsmacs--append-to-log (concat "Command input: " command-string))
    (process-send-string (tfsmacs--get-or-create-process) command-string)))

(defun tfsmacs--get-collection-parameter ()
  "Return the collection parameter if configured, or empty string."
  (if (not (string-empty-p tfsmacs-collection-url))
      (format " -collection:%s " tfsmacs-collection-url)
    ""))

(defun tfsmacs--get-login-parameter ()
  "Return the login parameter if configured, or empty string."
  (if (not (string-empty-p tfsmacs-login))
      (format " -login:%s " tfsmacs-login)
    ""))

(defun tfsmacs--message-callback (process output)
  "Show OUTPUT of PROCESS as message.  Also append to the TFS log."
  (tfsmacs--append-to-log output)
  (message (format "TFS:\n%s" output)))

(defun tfsmacs--log-callback (process output)
  "Append OUTPUT of PROCESS to the TFS log."
  (tfsmacs--append-to-log output))

(defun tfsmacs--determine-target-files (filename prompt)
  "Determine the name of the file to use in a TF command, or prompt for one.
If FILENAME, use it directly.  If called from a dired buffer it tries to
use the selection or the current file.  Else use PROMPT to get the user to
pick a file."
  (cond
   ((stringp filename)
    (list filename))
   ((derived-mode-p 'dired-mode)
    (dired-get-marked-files))
   (buffer-file-name
    (list buffer-file-name))
   (t
    (list (expand-file-name (read-file-name prompt nil nil t))))))

(defun tfsmacs--determine-target-directory (dirname  prompt)
  "Determine the name of a directory to use in a TF command, or prompt for one.
If DIRNAME, use it directly.  If called from a dired buffer it tries to
use the current directory.  Else use PROMPT to get the user to pick a dir."
  (cond
   ((stringp dirname)
    dirname)
   ((or (derived-mode-p 'dired-mode) (buffer-file-name))
    default-directory)
   (t
    (expand-file-name (read-directory-name prompt nil nil t)))))

(defun tfsmacs--get-last-dir-name (path)
  "Return only the last directory name in PATH.
From: https://stackoverflow.com/questions/27284851/emacs-lisp-get-directory-name-not-path-from-the-path"
  (file-name-nondirectory
   (directory-file-name
     (file-name-directory path))))

(defun tfsmacs--write-file-to-temp-directory (path version)
  "Write the VERSION of PATH to a temporary directory.
It spins off a new instance of the TEE tool by calling 'tfsmacs--process-command-sync-to-buffer'"
  ;remove quotes around  path if needed.
  (when (string-equal "\"" (substring path 0 1))
    (setq path (substring path 1 -1)))
  (let* ((only-name (file-name-nondirectory path))
         (filename (concat temporary-file-directory version ";" only-name))
         (command (list "print" (format "-version:%s" version) path)))
    (tfsmacs--process-command-sync-to-buffer command filename)
    filename))
    

(defun tfsmacs-checkout (&optional filename)
  "Perform a tf checkout (edit).

The file to checkout is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to checkout.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to checkout.

Checkout will occur only if the file is non-writable before the
call; checkout will fail if the specified file is currently
writable."
  (interactive)
  (let* ((files-to-checkout (tfsmacs--determine-target-files filename "File to checkout: "))
         (command (append '("checkout") files-to-checkout)))
    (when files-to-checkout
      (message "TFS: Checking out file(s)...")
      (tfsmacs--process-command command 'tfsmacs--message-callback))))

(defun tfsmacs-checkin ()
  "Perform a tf checkin on the file being visited by the current buffer.
Checkin happens only if the file is writable now.  This
function allows you to specify a checkin comment.  It checks in
only the current file being visited - pending changes for any
other files will not be checked in."
  (interactive)
  (if buffer-file-name
      (let* ((command (append '("checkin")
                              (tfsmacs--checkin-parameters-builder)
                              (list (tfsmacs--quote-string buffer-file-name)))))
        (tfsmacs--process-command command 'tfsmacs--message-callback))
    (error "Error tfsmacs-checkin: No file")))

(defun tfsmacs--checkin-parameters-builder ()
  "Build the parameters for the checkin command: comment, work item ID, overrie."
  (let* ((comment (read-string "Check in comment: "))
        (wid (read-string "Workitem ID (empty to skip): "))
        (override  (read-string "Override reason (empty to skip): "))
        (params (list (format "-comment:%s" (tfsmacs--quote-string comment)))))
    (when (not (string-empty-p wid))
      (add-to-list 'params (format "-associate:%s" wid)))
     (when (not (string-empty-p override))
       (add-to-list 'params (format "-override:%s" (tfsmacs--quote-string override))))
     params))

(defun tfsmacs-rename (&optional filename new-name)
  "Perform a tf rename on a file.

The file to rename is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.  Only one file can be marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

The file is renamed to NEW-NAME, a string, which should be the
name of a non-existent file in the current directory, specified
without a qualifying path.

If the rename is successful, and if the buffer is visiting the
file that is being renamed, then this function also renames the
buffer to the new name."
  (interactive)
  (let ((file-to-rename (tfsmacs--determine-target-files filename "File to rename: ")))
    (if (equal (length file-to-rename) 1)
        (let* ((source (car file-to-rename))
               (newname (or new-name (read-string (format "New name for %s: " source) nil nil nil)))
               (command (list "rename" source newname)))
          (tfsmacs--process-command command 'tfsmacs--message-callback)
          (if (string-equal source buffer-file-name)
              (set-visited-file-name newname)
            (error "Rename of %s was unsuccessful" file-to-rename)))
      (error "Couldn't determine file to rename"))))

(defun tfsmacs-add (&optional filename)
  "Perform a tf add on a file.

The file to add is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to add.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to add."
  (interactive)
  (let ((files-to-add (tfsmacs--determine-target-files filename "File(s) to add: ")))
    (if files-to-add
        (let* ((items (mapcar 'tfsmacs--quote-string files-to-add))
               (command (append '("add") items)))
          (tfsmacs--process-command command 'tfsmacs--message-callback))
      (error "Error tfsmacs-add: No file"))))

(defun tfsmacs-delete (&optional filename)
  "Perform a tf delete on a file.

The file to delete is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

If the delete is successful, and if the buffer is visiting the file that
is being deleted, then this function also kills the buffer."
  (interactive)
  (let ((files-to-delete (tfsmacs--determine-target-files filename "File to delete: ")))
    (if files-to-delete
        (let* ((items (mapcar 'tfsmacs--quote-string files-to-delete))
               (command (append '("delete") items)))
          (tfsmacs--process-command command 'tfsmacs--message-callback))
      (error "Error tfsmacs-delete: No file"))))

(defun tfsmacs-get (&optional filename version)
  "Perform a tf get on a file.

The file to get is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

If VERSION to get is not provided, it will be prompted."
  (interactive)
  (let ((files-to-get (tfsmacs--determine-target-files filename "File(s) to get: ")))
    (when files-to-get
        (let* ((items (mapcar 'tfsmacs--quote-string files-to-get))
               (version (list (tfsmacs--get-version-param version)))
               (command (append '("get") files-to-get version)))
          (tfsmacs--process-command command 'tfsmacs--message-callback)))))

(defun tfsmacs--get-version-param (&optional version)
  "Get a version spec string for a command that supports it.
If VERSION is provided return said version as changeset."
  (let ((param " -version:%s "))
    (when (not version)
      (setq version (read-string "Version spec (blank for latest): ")))
    (when (string-empty-p version)
      (setq version "T"))
    (format param version)))

(defun tfsmacs-get-recursive (&optional dirname force)
  "Perform a recursive tf get on a directory.
Use FORCE (or prefix arg) to overwrite writeable files not checked out
and get even up-to-date files.

The directory to get is deteremined this way:

 - if DIRNAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the current directory.

 - when there is a file backing the current buffer, it selects
   the file's directory

 - else, prompt the user for a dir."
  (interactive)
  (when current-prefix-arg
    (setq force t))
  (let* ((dir-to-get (tfsmacs--determine-target-directory dirname "Directory to get: "))
         (command (list "get" "-recursive" (tfsmacs--quote-string dir-to-get))))
    (when force
      (setq command (append command '("-force"))))
    (when dir-to-get
      (tfsmacs--process-command command 'tfsmacs--message-callback))))


(defun tfsmacs-undo (&optional filename)
  "Perform a tf undo on a file.

The file to undo is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file."
  (interactive)
  (let ((files-to-undo (tfsmacs--determine-target-files filename "File(s) to undo: ")))
    (when files-to-undo
        (let* ((items (mapcar 'tfsmacs--quote-string files-to-undo))
                (command (append '("undo") items)))
             (when (yes-or-no-p "Undo changes to file(s)? ")
               (tfsmacs--process-command command 'tfsmacs--message-callback))))))

(define-derived-mode tfsmacs-history-mode tabulated-list-mode "TFS history Mode" "Major mode TFS History, displays an item's history and allows compares."
  (setq tabulated-list-format [("Changeset" 10 t)
                               ("Type" 8. t)
                               ("Date" 20 t)
                               ("Committed by" 30 t)
                               ("Comment" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Changeset" t))
  (tabulated-list-init-header)
  (tablist-minor-mode))

(define-key tfsmacs-history-mode-map (kbd "T") 'tfsmacs--history-mode-get-this-version)
(define-key tfsmacs-history-mode-map (kbd "C") 'tfsmacs--history-mode-changeset-details)
(define-key tfsmacs-history-mode-map (kbd "D") 'tfsmacs--history-mode-difference)


(defun tfsmacs--history-mode-quote-path (item-pair)
  "Return the same ITEM-PAIR with only the path quoted."
  (list
   (car item-pair)
   (tfsmacs--quote-string (cadr item-pair))))

(defun tfsmacs--history-mode-get-marked-items ()
  "Return the selected items in ‘tfsmacs-history-mode’."
  (mapcar 'tfsmacs--history-mode-quote-path (mapcar 'car (tablist-get-marked-items))))

(defun tfsmacs--history-mode-get-this-version ()
  "Get the file version marked/selected in the ‘tfsmacs-history-mode’ buffer."
  (interactive)
  (let* ((items (tfsmacs--history-mode-get-marked-items))
         (to-get (car items)))
    (if (equal (length items) 1)
        (progn
          (message (format "TFS: Getting changeset %s" (car to-get)))
          (tfsmacs-get (cadr to-get) (car to-get)))
      (error "Only one item should be selected for this operation"))))

(defun tfsmacs--history-mode-changeset-details ()
  "Open changeset details for the selected item in ‘tfsmacs-history-mode’."
  (interactive)
  (let* ((items (tfsmacs--history-mode-get-marked-items))
         (to-get (car items)))
    (if (equal (length items) 1)
        (progn
          (message (format "TFS: Getting changeset details %s" (car to-get)))
          (tfsmacs-changeset (car to-get)))
      (error "Only one item should be selected for this operation"))))

(defun tfsmacs--history-mode-difference ()
  "Compare two selected items in ‘tfsmacs-history-mode’."
  (interactive)
  (let* ((items (tfsmacs--history-mode-get-marked-items))
         (first-item (car items))
         (second-item (cadr items)))
    (if (equal (length items) 2)
        (progn
          (tfsmacs--history-mode-ediff first-item second-item))
      (error "Only two items should be selected for this operation"))))

(defun tfsmacs--history-mode-ediff (file1 file2)
  "Compares FILE1 and FILE2 using ediff."
  (message "TFS: Retrieving files to compare. This operation can take a few seconds.")
  (let* ((temp1 (tfsmacs--write-file-to-temp-directory (cadr file1) (car file1)))
         (temp2 (tfsmacs--write-file-to-temp-directory (cadr file2) (car file2))))
    (ediff-files temp1 temp2)))

(defun tfsmacs-history (&optional filename)
  "Perform a tf history on a file.
How the file is determined:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.  Only one file can be marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file"
  
  (interactive)
  (let ((files-for-history (tfsmacs--determine-target-files filename "File: ")))
    (if (equal (length files-for-history) 1)
        (let* ((source (car files-for-history))
               (command (list "history" source)))
          (setq command (append command (tfsmacs--history-parameters-builder)))
          (message "TFS: Getting item history...")
          (setq tfsmacs--history-target source)
          (setq tfsmacs--history-xml-buffer "") ; just in case a previous invocation went wrong :)
          (tfsmacs--process-command command 'tfsmacs--history-callback))
      (error "Couldn't determine history target"))))

(defun tfsmacs--history-parameters-builder ()
  "Build the parameters for the history command: stopafter and user."
  (let ((user (read-string "Filter by user (blank to ignore): "))
        (stopafter (string-to-number (read-string "Number of items to retrieve (blank for 50): ")))
        (params (list "-recursive" "-format:xml")))
    (when (equal stopafter 0)
      (setq stopafter 50))
    (add-to-list 'params (format " -stopafter:%d " stopafter))
    (when (not (string-empty-p user))
       (add-to-list 'params (format " -user:%s " user)))
     params))

(defun tfsmacs--history-callback (process output)
  "Process the history output and display the ‘tfsmacs-history-mode’ buffer.
PROCESS is the TEE process
OUTPUT is the raw output"
  (setq tfsmacs--history-xml-buffer (concat tfsmacs--history-xml-buffer output))
  (let ((parsed-data (tfsmacs--get-history-data-for-tablist tfsmacs--history-xml-buffer)))
    (when parsed-data
      (setq tfsmacs--history-xml-buffer "")
      (let* ((short-target (file-name-nondirectory tfsmacs--history-target))
             (history-bufname (format "*TFS History %s *" short-target))
             (buffer (get-buffer-create history-bufname)))
             (with-current-buffer buffer
               (setq tabulated-list-entries parsed-data)
               (tfsmacs-history-mode)
               (tablist-revert)
               (switch-to-buffer buffer))))))

(defun tfsmacs--get-history-data-for-tablist (xml-status-data)
  "Format XML-STATUS-DATA from the history command for tabulated list."
  (with-temp-buffer
    (insert xml-status-data)
    (let* ((converted-xml-data (libxml-parse-xml-region (point-min) (point-max)))
           (changeset-nodes (dom-by-tag converted-xml-data 'changeset)))
      (mapcar 'tfsmacs--format-history-node changeset-nodes))))

(defun tfsmacs--format-history-node (changeset-node)
  "Extract from CHANGESET-NODE the info."
  (let ((changeset (cadr changeset-node))
        (comment (car (dom-by-tag changeset-node 'comment)))
        (item (cadr (car (dom-by-tag changeset-node 'item)))))
    (setq comment (nth 2 comment))
    (when (not comment)
      (setq comment ""))
    (list
     (list (alist-get 'id changeset) (alist-get 'server-item item))
     (vector
      (alist-get 'id changeset)
      (alist-get 'change-type item)
      (tfsmacs--format-history-node-date (alist-get 'date changeset))
      (alist-get 'committer changeset)
      comment))))

(defun tfsmacs--format-history-node-date (date-string)
  "Convert DATE-STRING to a better representation."
  ; Maybe it is worth it to do proper date formatting. TODO?
  (replace-regexp-in-string "T" " " (substring date-string  0 -9)))

(defun tfsmacs-changeset (&optional version)
  "Gets info on a changeset.
If VERSION to get is not provided, it will be prompted."
  (interactive)
  (when (not version)
    (setq version (read-string "Changeset number: (blank for latest): ")))
  (when (string-empty-p version)
      (setq version "-latest"))
  (when (get-buffer tfsmacs--changeset-buffer-name)
    (kill-buffer tfsmacs--changeset-buffer-name))
  (message "TFS: Getting changeset details...")
  (tfsmacs--process-command (list "changeset" version) 'tfsmacs--changeset-callback))

(defun tfsmacs--changeset-callback (process output)
  "Show the buffer with the changeset command result.
PROCESS is the TEE process
OUTPUT is the raw output"
  (let* ((buffer (get-buffer-create tfsmacs--changeset-buffer-name)))
    (with-current-buffer buffer
      (insert output)
      (display-buffer tfsmacs--changeset-buffer-name t))))

(define-derived-mode tfsmacs-status-mode tabulated-list-mode "TFS Status Mode" "Major mode TFS Status, displays current pending changes"
  (setq tabulated-list-format [("Change" 7 t)
                               ("Local Path" 100 t)
                               ("Server Path" 0 t)])
  (set (make-local-variable 'tfsmacs--buffer-status-dir) nil)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Local Path" nil))
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun tfsmacs--quote-string (param)
  "Surround PARAM with quotes using format.  Useful for paths and comments."
  (format "\"%s\"" param))

(defun tfsmacs--status-mode-checkin ()
  "Process files marked in ‘tfsmacs-status-mode’ for check in."
  (interactive)
  (let* ((items (tfsmacs--status-mode-get-marked-items))
         (command (append '("checkin") (tfsmacs--checkin-parameters-builder) items)))
    (tfsmacs--process-command command 'tfsmacs--message-callback)))

(defun tfsmacs--status-mode-revert ()
  "Revert (undo) the files marked using ‘tfsmacs-status-mode’."
  (interactive)
  (let* ((items (tfsmacs--status-mode-get-marked-items))
         (quoted-items (mapcar 'tfsmacs--quote-string items))
         (command '("undo")))
    (when (yes-or-no-p "Undo changes to the  files marked? ")
      (setq command (append command quoted-items))
      (tfsmacs--process-command command 'tfsmacs--message-callback))))

(defun tfsmacs--status-mode-difference ()
  "Compares pending change to latest version."
  (interactive)
  (let ((items (tfsmacs--status-mode-get-marked-items)))
    (if (equal (length items) 1)
        (progn
          (message "TFS: Retrieving files to compare. This operation can take a few seconds.")
          (let* ((local (substring (car items) 1 -1)) ; these items are always quoted, remove the quotes
                 (server (tfsmacs--write-file-to-temp-directory local "T")))
            (ediff-files local server)))
      (error "Select only one file to compare to latest version"))))

(defun tfsmacs--status-mode-get-marked-items ()
  "Obtain only the path of the files selected in the list."
  (mapcar 'tfsmacs--quote-string (mapcar 'car (tablist-get-marked-items))))

(defun tfsmacs--status-mode-visit-item ()
  "Visit the file under the cursor in ‘tfsmacs-status-mode’."
  (interactive)
  (find-file (tabulated-list-get-id)))

(define-key tfsmacs-status-mode-map (kbd "C") 'tfsmacs--status-mode-checkin)
(define-key tfsmacs-status-mode-map  (kbd "R") 'tfsmacs--status-mode-revert)
(define-key tfsmacs-status-mode-map (kbd "g") 'tfsmacs-pending-changes)
(define-key tfsmacs-status-mode-map (kbd "RET") 'tfsmacs--status-mode-visit-item)
(define-key tfsmacs-status-mode-map  (kbd "D") 'tfsmacs--status-mode-difference)
;(define-key tfsmacs-status-mode-map (kbd "RET") 'tfsmacs--status-mode-shelve)

(defun tfsmacs-pending-changes ()
  "Perform a recursive tf status.  Displays the result in a separate buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (derived-mode-p 'tfsmacs-status-mode)
        (tfsmacs--get-pending-changes tfsmacs--buffer-status-dir)
      (progn
        (let* ((status-dir (tfsmacs--select-status-directory))
               (buffer (get-buffer-create "*TFS Status [running]*")))
          (with-current-buffer buffer
            (setq tfsmacs--buffer-status-dir status-dir)
            (tfsmacs--get-pending-changes status-dir)
            (switch-to-buffer buffer)))))))

(defun tfsmacs--get-pending-changes (directory)
  "Internal call to run the status command in DIRECTORY."
  (let* ((command (list "status" directory "-recursive" "-nodetect"  "-format:xml")))
    (message "TFS: Obtaining list of pending changes...")
    (setq tfsmacs--status-xml-buffer "") ; just in case a previous invocation went wrong :)
    (tfsmacs--process-command command 'tfsmacs--status-callback)))

(defun tfsmacs--status-callback (process output)
  "Process the output of tf status and display the ‘tfsmacs-status-mode’ buffer.
PROCESS is the TEE process
OUTPUT is the raw output"
  (setq tfsmacs--status-xml-buffer (concat tfsmacs--status-xml-buffer output))
  (when (tfsmacs--status-xml-complete)
    (let ((parsed-data (tfsmacs--get-status-data-for-tablist tfsmacs--status-xml-buffer)))
      (setq tfsmacs--status-xml-buffer "")
      (let* ((directory tfsmacs--buffer-status-dir)
             (last-dir-in-path (tfsmacs--get-last-dir-name directory))
             (status-bufname (format "*TFS Status %s *" last-dir-in-path))
             (buffer (get-buffer status-bufname)))
        (when (not buffer)
          (setq buffer (get-buffer "*TFS Status [running]*"))
          (erase-buffer))
        (with-current-buffer buffer
          (tfsmacs-status-mode)
          ;Buffer local vars
          (setq tabulated-list-entries parsed-data)
          (setq tfsmacs--buffer-status-dir directory)
          (tablist-revert)
          (rename-buffer status-bufname)
          (switch-to-buffer buffer))))))

(defun tfsmacs--status-xml-complete ()
  "Confirm that the status closing tag is present."
  (or (string-match-p (regexp-quote "</status>") tfsmacs--status-xml-buffer)
      (string-match-p (regexp-quote "<status/>") tfsmacs--status-xml-buffer)))

(defun tfsmacs--get-status-data-for-tablist (xml-status-data)
  "Format XML-STATUS-DATA from the status command for tabulated list."
  (with-temp-buffer
    (insert xml-status-data)
    (let* ((converted-xml-data (libxml-parse-xml-region (point-min) (point-max)))
           (pending-change-nodes (dom-by-tag converted-xml-data 'pending-change)))
      (mapcar 'tfsmacs--format-status-node pending-change-nodes))))

(defun tfsmacs--format-status-node (pending-changes-node)
  "Extract from PENDING-CHANGES-NODE the info."
  (let ((data (cadr pending-changes-node)))
    (list
     (alist-get 'local-item data)
     (vector
      (alist-get 'change-type data)
      (alist-get 'local-item data)
      (alist-get 'server-item data)))))

(defun tfsmacs--select-status-directory ()
  "Prompt for a directory.  Try  projectile root first, else use current buffer's directory."
  (let ((tfsmacs-status-dir "")
        (default-dir-prompt "?"))
    (ignore-errors
      (when (fboundp 'projectile-project-root)
        (setq default-dir-prompt (projectile-project-root))))
    (when (string= default-dir-prompt "?")
      (setq default-dir-prompt default-directory))
    (read-directory-name "Status for directory: " default-dir-prompt nil t)))

(defun tfsmacs--append-to-log (text)
  "Append TEXT to the TFS Messages buffer.
Intended for internal use only."
  (let ((buf (current-buffer))
        (tfsbuffer (get-buffer-create tfsmacs-log-buffer-name)))
    (set-buffer tfsbuffer)
    (goto-char (point-max))
    (insert text)
    (insert "\n")
    (set-buffer buf)))


(provide 'tfsmacs)

;;; tfsmacs.el ends here
