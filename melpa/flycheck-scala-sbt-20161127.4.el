;;; flycheck-scala-sbt.el --- sbt-mode checker for Scala -*- lexical-binding: t -*-

;; Version: 0.1
;; Package-Version: 20161127.4
;; Url: https://www.github.com/rjmac/flycheck-scala-sbt
;; Package-requires: ((emacs "25.1") (flycheck "30") (sbt-mode "0.2"))

;;; Commentary:

;; Provides an sbt-mode based checker for scala files.  Call
;; flycheck-scala-sbt-init from your scala-mode-hook to set it up.

;;; Code:

(require 'flycheck)
(require 'sbt-mode)
(require 'cl-lib)

(defgroup flycheck-scala-sbt nil
  "Scala support for Flycheck via sbt-mode."
  :prefix "flycheck-scala-sbt-"
  :tag "Scala-SBT"
  :group 'flycheck)

(defcustom flycheck-scala-sbt-collect-from-other-files-p
  t
  "Whether to collect errors and warnings from other files.

If set, errors detected in other files are shown at the start of
the current buffer.  Valid values are t for \"always collect from
other files\", nil for \"never collect from other files\",
`:unopened-buffer' for \"only collect from files not currently
open\", and `:unknown-buffer' for \"only collect from files not
tied to the current SBT session\".  `:unknown-buffer' only works
if `flycheck-scala-sbt-trigger-other-buffers' is set."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Only for unopened files" :unopened-buffers)
                 (const :tag "Only for files unassociated with the current SBT session" :unknown-buffer))
  :tag "Collect from other files"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-auto-reload-p
  t
  "Whether to automatically reload the project when the build changes.

If set, changes to .sbt files and .scala files living in
directories named \"project\" trigger an SBT reload before
compiling."
  :type 'boolean
  :tag "Reload SBT automatically"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-auto-exit-console-p
  t
  "Whether to automatically exit from a console session.

If set, when SBT is in a console session flycheck will exit it to
issue a compile command."
  :type 'boolean
  :tag "Exit REPL automatically"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-enable-in-java-mode-p
  nil
  "Whether to register as a Java-mode checker.

If set, the checker will register itself as a `java-mode' checker
as well.  Changing this does not take immediate effect; Emacs must
be restarted, as it affects the way the checker is defined."
  :type 'boolean
  :tag "Register as a Java-mode checker"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-debug-p
  nil
  "Display extra debugging messages."
  :type 'boolean
  :tag "Display extra debugging messages"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-trigger-other-buffers-p
  nil
  "Initiate checks in all other buffers that share the same SBT session.

Changes in one file can affect other open files.  This can be
confusing when you make a change and then switch to another
existing buffer in the same project, as that other buffer will
not be rechecked unless it is either saved or `flycheck-buffer'
is explicitly called.  This configuration option causes
flychecking a Scala buffer automatically initiate checks in all
other buffers that share the same session.  SBT will only
actually compile once, but the errors will be distributed to all
relevant buffers.

This option is somewhat experimental."
  :type 'boolean
  :tag "Initiate checks in all buffers that share an SBT session"
  :group 'flycheck-scala-sbt)

;; This is a bit of a workaround for flycheck's lack of any idea of
;; project-wide diagnostic collection.  We'll use tabulated-list-mode
;; to manage a list of all our project-wide errors ourselves.
(define-derived-mode flycheck-scala-sbt-errors-mode tabulated-list-mode "flycheck-scala-sbt"
  (setq tabulated-list-format [("File" 30 t)
                               ("Type" 7 t)
                               ("Message" 0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "File" nil))
  (tabulated-list-init-header))

(defun flycheck-scala-sbt--error-buffer-name ()
  "Compute the name of the error buffer for the current buffer."
  (save-window-excursion
    (with-current-buffer (current-buffer)
      (concat (buffer-name (sbt:run-sbt)) "/errors"))))

(defun flycheck-scala-sbt--populate-sbt-errors (errors)
  "Populate the error buffer with ERRORS.

Returns the buffer.  Should only be called from the SBT buffer
itself."
  (with-current-buffer (get-buffer-create (flycheck-scala-sbt--error-buffer-name))
    (flycheck-scala-sbt-errors-mode)
    (let ((message-prefix (concat "\n" (make-string (+ 2 30 2 7) ?\s))))
      (setq tabulated-list-entries (mapcar (lambda (error)
                                             (cl-destructuring-bind (file row col type message) error
                                               (let* ((file-pos (if col
                                                                    (format ":%s:%s" row col)
                                                                  (format ":%s" row)))
                                                      (goto-error (lambda (button)
                                                                    (ignore button)
                                                                    (find-file-other-window file)
                                                                    (goto-char (point-min))
                                                                    (beginning-of-line row)
                                                                    (when col
                                                                      (forward-char (1- col)))))
                                                      (file-button (cons (concat (file-name-nondirectory file) file-pos)
                                                                         (list 'action goto-error
                                                                               'help-echo (concat file file-pos))))
                                                      (type (cons (symbol-name type) (list 'face (cl-ecase type
                                                                                                   (error 'error)
                                                                                                   (warning 'warning))
                                                                                           'action goto-error
                                                                                           'help-echo (concat file file-pos))))
                                                      (message (replace-regexp-in-string "\n" message-prefix message)))
                                                 (list nil (vector file-button type message)))))
                                           errors)))
    (tabulated-list-print)
    (current-buffer)))

(defun flycheck-scala-sbt-show-errors-list ()
  "Show the current SBT error list."
  (interactive)
  (let ((target-buffer (get-buffer (flycheck-scala-sbt--error-buffer-name))))
    (if target-buffer
        (pop-to-buffer target-buffer)
      (error "No SBT error list available for the current buffer"))))

(cl-defstruct flycheck-scala-sbt--check buffer checker project-p callback)
(cl-defstruct flycheck-scala-sbt--state current-timer active pending known-buffers recursing-p)
(defvar-local flycheck-scala-sbt--raw-state nil)
(defun flycheck-scala-sbt--state ()
  "Return the current buffer's state, creating it if necessary."
  (unless flycheck-scala-sbt--raw-state
    (setf flycheck-scala-sbt--raw-state (make-flycheck-scala-sbt--state :current-timer nil
                                                                        :active nil
                                                                        :pending nil
                                                                        :known-buffers (make-hash-table)
                                                                        :recursing-p nil)))
  flycheck-scala-sbt--raw-state)

(defun flycheck-scala-sbt--build-error-p (line)
  "Test whether LINE indicates that building the project itself failed."
  (string= "Project loading failed: (r)etry, (q)uit, (l)ast, or (i)gnore? " line))

(defun flycheck-scala-sbt--prompt-detection (line)
  "Test whether LINE represents an SBT input prompt."
  (cond
   ((flycheck-scala-sbt--build-error-p line) :build-error)
   ((string-match sbt:sbt-prompt-regexp line) :normal)
   ((string-match sbt:console-prompt-regexp line) :console)
   (t nil)))

(defun flycheck-scala-sbt--wait-for-prompt-then-call (f)
  "Wait for the SBT prompt in the current buffer, then call F.

F is called with `:normal' if a normal SBT, prompt was detected,
`:console' if a Scala console prompt was, or `:build-error' if a
build-script error prompt was found.

The function is called with same current buffer as was originally
active when the original call was made."
  (let ((original-buffer (current-buffer)))
    (let* ((last-line (save-excursion
                        (goto-char (point-max))
                        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
           (prompt-type (flycheck-scala-sbt--prompt-detection last-line)))
      (if prompt-type
          (funcall f prompt-type)
        (setf (flycheck-scala-sbt--state-current-timer (flycheck-scala-sbt--state))
              (run-at-time 0.1 nil (lambda ()
                                     (when (buffer-live-p original-buffer)
                                       (with-current-buffer original-buffer
                                         (setf (flycheck-scala-sbt--state-current-timer (flycheck-scala-sbt--state)) nil)
                                         (flycheck-scala-sbt--wait-for-prompt-then-call f))))))))))

(defun flycheck-scala-sbt--issue-retry ()
  "Issue a retry after a build script failure."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (comint-send-string (get-buffer-process (current-buffer)) "r\n"))
  (sbt:clear (current-buffer)))

(cl-defmacro flycheck-scala-sbt--wait-for-prompt-then ((&key prompt-type) &body body)
  "Wait for the SBT prompt in the current buffer, then evaluate BODY."
  (let ((result (or prompt-type (cl-gensym))))
    `(flycheck-scala-sbt--wait-for-prompt-then-call
      (lambda (,result) ,@body))))

(defun flycheck-scala-sbt--build-script-p (file)
  "Check whether FILE is likely a part of the build script.

FILE can be either a buffer or a filename.

Returns true if the name ends in \".sbt\" or if the last
directory in the path is \"project\"."
  (let ((filename (if (bufferp file) (buffer-file-name file) file)))
    (and filename (or (string-suffix-p ".sbt" filename)
                      (string-match "/project/[^/]+$" filename)))))

(defun flycheck-scala-sbt--debug (format &rest args)
  "Format FORMAT and ARGS in *Messages* when `flycheck-scala-sbt-debug-p' is true."
  (when flycheck-scala-sbt-debug-p
    (apply 'message format args)))

(defun flycheck-scala-sbt--actually-start ()
  "Run a (set of) checkers.

On entry, the current buffer is the SBT buffer in which to run
them, and there is a non-empty set of current checkers."
  ;; ok, this is sort of complicated.  We have to wait for various
  ;; prompts and issue commands, until finally we can collect errors
  ;; and invoke the right callbacks with them.
  (flycheck-scala-sbt--debug "Starting %s job(s)" (length (flycheck-scala-sbt--state-active (flycheck-scala-sbt--state))))
  (cl-labels ((kickoff-pending ()
                (let ((state (flycheck-scala-sbt--state)))
                  (cl-shiftf (flycheck-scala-sbt--state-active state) (flycheck-scala-sbt--state-pending state) nil)
                  (when (flycheck-scala-sbt--state-active state)
                    (flycheck-scala-sbt--debug "Starting deferred jobs")
                    (flycheck-scala-sbt--actually-start))))
              (finish ()
                (flycheck-scala-sbt--wait-for-prompt-then ()
                  ;; first, do all our callbacks
                  (let ((state (flycheck-scala-sbt--state)))
                    (condition-case err
                        (let ((errors (flycheck-scala-sbt--errors-list)))
                          (flycheck-scala-sbt--populate-sbt-errors errors)
                          (dolist (check (flycheck-scala-sbt--state-active state))
                            (condition-case err
                                (funcall (flycheck-scala-sbt--check-callback check)
                                         'finished
                                         (flycheck-scala-sbt--errors-convert check errors))
                              (error
                               (ignore-errors
                                 (funcall (flycheck-scala-sbt--check-callback check) 'errored (error-message-string err)))))))
                      (error ;; this is almsot certainly from accumulating the errors list
                       (ignore-errors
                         (broadcast-error (error-message-string err))))))
                  (kickoff-pending)))
              (finish-post-reload ()
                ;; We've just issued a reload.  Now we need to wait
                ;; for that to finish.
                (flycheck-scala-sbt--wait-for-prompt-then (:prompt-type prompt-type)
                  (cl-ecase prompt-type
                    (:normal
                     ;; Good -- it finished normally.  Now send a
                     ;; compile command without clearing the buffer,
                     ;; so we can collect diagnostics from both the
                     ;; reload and the compile.
                     (save-window-excursion
                       ;; First, delete our current prompt so that we
                       ;; don't immediately say "hey there's a prompt,
                       ;; we must be done".
                       (let ((inhibit-read-only t))
                         (goto-char (point-max))
                         (delete-region (line-beginning-position) (line-end-position)))
                       (let ((sbt:save-some-buffers nil)
                             (sbt:clear-buffer-before-command nil))
                         (sbt:command "test:compile" nil))))
                    (:console
                     (error "Didn't expect to end up in console mode here!"))
                    (:build-error
                     ;; The reload didn't succeed.  Ok then, we'll
                     ;; just collect what diagnostics we have.
                     t))
                  (finish)))
              (broadcast-error (msg)
                (let ((state (flycheck-scala-sbt--state)))
                  (dolist (check (flycheck-scala-sbt--state-active state))
                    (ignore-errors
                      (funcall (flycheck-scala-sbt--check-callback check)
                               'errored
                               msg))))))
    (flycheck-scala-sbt--wait-for-prompt-then (:prompt-type prompt-type)
      (save-window-excursion
        (cl-ecase prompt-type
          (:normal
           ;; ok, everything's good with the build script as far as we
           ;; know.  Let's try building stuff!
           (let ((sbt:save-some-buffers nil)
                 (state (flycheck-scala-sbt--state)))
             (if (and flycheck-scala-sbt-auto-reload-p
                      (cl-find-if #'flycheck-scala-sbt--check-project-p (flycheck-scala-sbt--state-active state)))
                 (progn
                   ;; we're in a build script (probably) so issue a
                   ;; reload to pick up changes to it.
                   (flycheck-scala-sbt--debug "One of our buffers was (probably) a build script; reloading")
                   (sbt:command "reload" nil)
                   (finish-post-reload))
               ;; Normal source file, just recompile.
               (flycheck-scala-sbt--debug "One of our buffers was (probably) not a build script; just compiling")
               (sbt:command "test:compile" nil)
               (finish))))
          (:console
           (if flycheck-scala-sbt-auto-exit-console-p
               (progn
                 (sbt:clear (current-buffer))
                 (comint-send-string (current-buffer) ":quit\n")
                 (flycheck-scala-sbt--actually-start))
             (broadcast-error "SBT is in a console session.  Exit it to reenable flycheck")
             (kickoff-pending)))
          (:build-error
           ;; nope, we left off in a bad place.  Issue a retry and
           ;; see if that fixes things.
           (if flycheck-scala-sbt-auto-reload-p
               (progn
                 (flycheck-scala-sbt--issue-retry)
                 (finish-post-reload))
             (broadcast-error "The project failed to load")
             (kickoff-pending))))))))

(defun flycheck-scala-sbt--start (checker callback)
  "The main entry point for the CHECKER.  Don't call this.  CALLBACK."
  (save-window-excursion
    (let* ((target-buffer (current-buffer))
           (project-p (flycheck-scala-sbt--build-script-p target-buffer)))
      (with-current-buffer (sbt:run-sbt)
        (let ((state (flycheck-scala-sbt--state))
              (check (make-flycheck-scala-sbt--check :buffer target-buffer :checker checker :project-p project-p :callback callback)))
          (if (and (flycheck-scala-sbt--state-active state) (not (flycheck-scala-sbt--state-recursing-p state)))
              (progn
                (flycheck-scala-sbt--debug "There is a check active for this SBT; deferring")
                (push check (flycheck-scala-sbt--state-pending state)))
            (flycheck-scala-sbt--debug "None active; starting")
            (push check (flycheck-scala-sbt--state-active state))
            (unless (flycheck-scala-sbt--state-recursing-p state)
              (when flycheck-scala-sbt-trigger-other-buffers-p
                (puthash target-buffer t (flycheck-scala-sbt--state-known-buffers state))
                (flycheck-scala-sbt--debug "There are %s known buffers sharing this SBT session" (hash-table-count (flycheck-scala-sbt--state-known-buffers state)))
                (unwind-protect
                    (progn
                      (setf (flycheck-scala-sbt--state-recursing-p state) t)
                      (dolist (buffer (hash-table-keys (flycheck-scala-sbt--state-known-buffers state)))
                        (if (buffer-live-p buffer)
                            (unless (eql buffer target-buffer)
                              (with-current-buffer buffer
                                (flycheck-buffer)))
                          (remhash buffer (flycheck-scala-sbt--state-known-buffers state)))))
                  (setf (flycheck-scala-sbt--state-recursing-p state) nil)))
              (flycheck-scala-sbt--actually-start)))
          check)))))

(defconst flycheck-scala-sbt--weird-buildscript-regex "^\\[\\(error\\|warn\\)][[:space:]]\\[\\(.*\\)]:\\([0-9]+\\):[[:space:]]\\(.*\\)$")
(defun flycheck-scala-sbt--errors-list ()
  "Find the current list of errors in the current buffer."
  (save-excursion
    (let ((acc '()))
      ;; The horror, the horror
      ;;
      ;; Ok so.
      ;;
      ;; The way this actually works is by finding the _carets_ that
      ;; indicate the column, and then working backward two lines to
      ;; find the relevant filename, row, and message.
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\[\\(error\\|warn\\)][[:space:]]\\)?[[:space:]]*^$" (point-max) t)
        (push (flycheck-scala-sbt--extract-error-info) acc))
      (goto-char (point-min))
      (while (re-search-forward flycheck-scala-sbt--weird-buildscript-regex (point-max) t)
        (push (flycheck-scala-sbt--extract-weird-error-info) acc))
      (sort acc (lambda (e1 e2)
                  (cl-destructuring-bind (file1 row1 col1 type1 message1) e1
                    (cl-destructuring-bind (file2 row2 col2 type2 message2) e2
                      ;; ew.  Surely there's a better way to write this.
                      (if (string-lessp file1 file2)
                          t
                        (if (string-lessp file2 file1)
                            nil
                          (if (< row1 row2)
                              t
                            (if (> row2 row1)
                                nil
                              (if (< (or col1 -1) (or col2 -1))
                                  t
                                (if (< (or col2 -1) (or col1 -1))
                                    nil
                                  (if (string-lessp type1 type2)
                                      t
                                    (if (string-lessp type2 type1)
                                        nil
                                      (string-lessp message1 message2))))))))))))))))

(defun flycheck-scala-sbt--extract-weird-error-info ()
  "Extract errors from build scripts in the occasional weird format.

This format is [error] [$PATH]:$LINE: $MESSAGE.  There's no
column information here, so we'll just set the column to nil"
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
      (string-match flycheck-scala-sbt--weird-buildscript-regex line)
      (let ((type (match-string 1 line))
            (file (match-string 2 line))
            (row (string-to-number (match-string 3 line)))
            (message (match-string 4 line)))
        (list file
              row
              nil
              (if (string= type "warn") 'warning 'error)
              message)))))

(defun flycheck-scala-sbt--extract-error-info ()
  "Extract the error at point.

The point should be placed on the last line of the error message,
where scala prints the caret indicating the column in which the
error occurred."
  (save-excursion
    (let ((final-line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
      (if (string-prefix-p "[" final-line)
          (flycheck-scala-sbt--extract-main-error-info)
        (flycheck-scala-sbt--extract-buildscript-error-info)))))

(defun flycheck-scala-sbt--extract-main-error-info ()
  "Extract the non-build-script error or warning at point."
  (let ((line-regexp "^\\[\\(error\\|warn\\)][[:space:]]+\\(.*?\\):\\([0-9]+\\):[[:space:]]+\\(?:\\(?:error\\|warning\\):[[:space:]]*\\)?\\(.*\\)$")
        (else-regexp "^\\[\\(error\\|warn\\)][[:space:]]\\(.*\\)$")
        (subsequent-lines '())
        (current-line nil)
        (error-column (current-column)))
    (forward-line -2) ;; skip caret and actual erroring line
    (while (not (string-match line-regexp (setq current-line
                                                (buffer-substring-no-properties (line-beginning-position)
                                                                                (line-end-position)))))
      (unless (string-match else-regexp current-line)
        (error "Expected to find an error continuation line"))
      (push (match-string 2 current-line) subsequent-lines)
      (forward-line -1))
    (let ((type (match-string 1 current-line))
          (file (match-string 2 current-line))
          (row (string-to-number (match-string 3 current-line)))
          (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
      (list file
            row
            (- error-column (+ 3 (length type)))
            (if (string= type "warn") 'warning 'error)
            message))))

(defun flycheck-scala-sbt--extract-buildscript-error-info ()
  "Extract the build-script error or warning at point.

This is a little less safe than extracting from the main build,
because the lines aren't marked by \"error\" or \"warning\" at
the start of the line."
  (let ((line-regexp "^\\(/.*?\\):\\([0-9]+\\):[[:space:]]\\(error\\|warning\\):[[:space:]]\\(.*\\)$")
        (subsequent-lines '())
        (current-line nil)
        (error-column (current-column)))
    (forward-line -2) ;; skip caret and actual erroring line
    (while (not (string-match line-regexp (setq current-line
                                                (buffer-substring-no-properties (line-beginning-position)
                                                                                (line-end-position)))))
      (push current-line subsequent-lines)
      (forward-line -1))
    (let ((file (match-string 1 current-line))
          (row (string-to-number (match-string 2 current-line)))
          (type (match-string 3 current-line))
          (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
      (list file
            row
            error-column
            (if (string= type "warning") 'warning 'error)
            message))))

(defun flycheck-scala-sbt--extract-build-error-info ()
  "Extract the error at point.

The point should be placed on the last line of the error message,
where scala prints the caret indicating the column in which the
error occurred."
  (save-excursion
    (let ((line-regexp "^\\[\\(error\\|warn\\)][[:space:]]+\\(.*?\\):\\([0-9]+\\):[[:space:]]+\\(.*\\)$")
          (else-regexp "^\\[\\(error\\|warn\\)][[:space:]]\\(.*\\)$")
          (subsequent-lines '())
          (current-line nil)
          (error-column (current-column)))
      (forward-line -2) ;; skip caret and actual erroring line
      (while (not (string-match line-regexp (setq current-line
                                                  (buffer-substring-no-properties (line-beginning-position)
                                                                                  (line-end-position)))))
        (unless (string-match else-regexp current-line)
          (error "Expected to find an error continuation line"))
        (push (match-string 2 current-line) subsequent-lines)
        (forward-line -1))
      (let ((type (match-string 1 current-line))
            (file (match-string 2 current-line))
            (row (string-to-number (match-string 3 current-line)))
            (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
        (list file
              row
              (- error-column (+ 3 (length type)))
              (if (string= type "warn") 'warning 'error)
              message)))))

(defun flycheck-scala-sbt--errors-convert (check errors)
  "For the check run CHECK, convert the list of ERRORS to flycheck error objects."
  (when (buffer-live-p (flycheck-scala-sbt--check-buffer check))
    (let ((state (flycheck-scala-sbt--state)))
      (with-current-buffer (flycheck-scala-sbt--check-buffer check)
        (let ((checker (flycheck-scala-sbt--check-checker check)))
          (cl-mapcan (lambda (error) (flycheck-scala-sbt--convert-error-info checker error (flycheck-scala-sbt--state-known-buffers state))) errors))))))

(defun flycheck-scala-sbt--interrupt (checker context)
  "Cancel CHECKER's pending check CONTEXT.

This only actually cancels the pending check if it is blocked
behind some other running check."
  (ignore checker)
  (flycheck-scala-sbt--debug "Asked to interrupt a job")
  (with-current-buffer (sbt:run-sbt)
    (let ((state (flycheck-scala-sbt--state)))
      (flycheck-scala-sbt--debug "Cancelling %s" (flycheck-scala-sbt--check-buffer context))
      (setf (flycheck-scala-sbt--state-pending state) (remove context (flycheck-scala-sbt--state-pending state)))
      (funcall (flycheck-scala-sbt--check-callback context) 'interrupted nil))))

(defun flycheck-scala-sbt--convert-error-info (checker error known-buffers)
  "Provide CHECKER an ERROR converted into a flycheck-error with KNOWN-BUFFERS.

ERROR should come from `flycheck-scala-sbt--extract-error-info'."
  (let* ((file (nth 0 error))
         (row (nth 1 error))
         (col (nth 2 error))
         (level (nth 3 error))
         (message (nth 4 error))
         (buffer (find-buffer-visiting file)))
    ;; it sure seems like you should be able to return errors in other
    ;; buffers, but it seems flycheck tries to highlight all errors in
    ;; the CURRENT buffer no matter what buffer they were sourced from.
    (cond
     ((eql buffer (current-buffer))
      (list (flycheck-error-new :buffer buffer :message message :checker checker :filename file :line row :column col :level level)))
     ((or (eql flycheck-scala-sbt-collect-from-other-files-p t)
          (and (eql flycheck-scala-sbt-collect-from-other-files-p :unopened-buffers)
               (null buffer))
          (and (eql flycheck-scala-sbt-collect-from-other-files-p :unknown-buffer)
               (not (gethash buffer known-buffers))))
      (list (flycheck-error-new :buffer (current-buffer)
                                :message (concat "from " file ":" (prin1-to-string row) ":" (prin1-to-string col) ":\n  " (replace-regexp-in-string "\n" "\n  " message))
                                :checker checker
                                :filename (buffer-file-name)
                                :line 1
                                :column 1
                                :level level)))
     (t nil))))

(flycheck-define-generic-checker
    'scala-sbt
  "Check scala buffers using sbt-mode"
  :modes (append '(scala-mode) (if flycheck-scala-sbt-enable-in-java-mode-p '(java-mode)))
  :predicate 'sbt:find-root
  :start 'flycheck-scala-sbt--start
  :next-checkers '((warning . scala-scalastyle))
  :interrupt 'flycheck-scala-sbt--interrupt)

(cl-pushnew 'scala-sbt flycheck-checkers)

(defun flycheck-scala-sbt--cleanup ()
  "Fail pending flychecks before killing an SBT buffer."
  (when flycheck-scala-sbt--raw-state
    (let ((state (flycheck-scala-sbt--state)))
      (when (flycheck-scala-sbt--state-current-timer state)
        (ignore-errors
          (cancel-timer (flycheck-scala-sbt--state-current-timer state))))
      (dolist (check (append (flycheck-scala-sbt--state-active state)
                             (flycheck-scala-sbt--state-pending state)))
        (ignore-errors (funcall (flycheck-scala-sbt--check-callback check) 'errored "The associated SBT process was killed")))
      (setq flycheck-scala-sbt--raw-state nil))))

(add-hook 'kill-buffer-hook 'flycheck-scala-sbt--cleanup)

;;;###autoload
(defun flycheck-scala-sbt-init ()
  "Call this from your scala-mode-hook to set up flycheck-sbt."
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-mode t))

(provide 'flycheck-scala-sbt)

;;; flycheck-scala-sbt.el ends here
