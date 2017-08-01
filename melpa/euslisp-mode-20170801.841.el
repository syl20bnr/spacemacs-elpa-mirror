;;; euslisp-mode.el --- Major mode for Euslisp-formatted text -*- lexical-binding: t; -*-

;; Author: iory <ab.ioryz@gmail.com>
;; Maintainer: iory <ab.ioryz@gmail.com>
;; Created: April 13, 2016
;; Version: 0.0.8
;; Package-Version: 20170801.841
;; Keywords: Euslisp, euslisp, GitHub
;; URL: https://github.com/iory/euslisp-mode
;; Package-Requires: ((emacs "23") (s "1.9") (exec-path-from-shell "0") (helm-ag 0.58))

(require 's)
(require 'exec-path-from-shell)
(require 'helm-ag)


;;; Constants =================================================================

(defconst euslisp-mode-version "0.0.8"
  "Euslisp mode version number.")

(defconst euslisp-output-buffer-name "*euslisp-output*"
  "Name of temporary buffer for euslisp command output.")

(defconst euslisp-mode-source-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory
    )
  "Source dir of euslisp-mode")

(defconst euslisp-choose-directory-default-directory "~/"
  "Initial starting point.")

(defvar euslisp-shell--parent-buffer nil)
(defvar euslisp-shell--interpreter)
(defvar euslisp-shell--interpreter-args)
(defvar euslisp-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior euslisp shell.
Do not set this variable directly, instead use
`euslisp-shell-prompt-set-calculated-regexps'.")

(defvar euslisp-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior euslisp shell.
Do not set this variable directly, instead use
`euslisp-shell-set-prompt-regexp'.")


(cond ((eq system-type 'gnu/linux)
       (defcustom euslisp-shell-interpreter "roseus"
         "Default Euslisp interpreter for shell."
         :type 'string
         :group 'euslisp))
      ((eq system-type 'darwin)
       (defcustom euslisp-shell-interpreter "irteus"
         "Default Euslisp interpreter for shell."
         :type 'string
         :group 'euslisp)))

(defcustom euslisp-shell-interpreter-args ""
  "Default arguments for the Euslisp interpreter."
  :type 'string
  :group 'euslisp)

;;; Mode Definition  ==========================================================

(defun euslisp-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "euslisp-mode, version %s" euslisp-mode-version))

;;;###autoload
(define-derived-mode euslisp-mode lisp-mode "Euslisp"
  "Major mode for editing Euslisp files."
  ;; Indentation
  (setq lisp-indent-function 'euslisp-indent-function)
  )

;;;###autoload
(setq auto-mode-alist
      (cons (cons "\\.l\\'" 'euslisp-mode) auto-mode-alist))

;;; Indentation ====================================================================

(defun euslisp-indent-function (indent-point state)
  "Indent the euslisp code."
  (interactive)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (string-match ":.*" function)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point)))))
    )
  )

(font-lock-add-keywords
 'euslisp-mode
 (list
  (list (concat "(" (regexp-opt '("defforeign") t) "\\>") '(1 font-lock-keyword-face nil t))
  (list "\\(self\\)\\>" '(1 font-lock-constant-face nil t))
  (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
  (list "\\(#\\(\\+\\|\\-\\)\.\*\\)" '(1 font-lock-variable-name-face))
  (list "\\(throw-error\\)" '(1 font-lock-warning-face nil t))
  (list (concat "(" (regexp-opt '("warn" "warning-message") t) "\\>") '(1 font-lock-warning-face nil t))
  (list (concat "(" (regexp-opt '("send" "send-all" "send-super") t) "\\>") '(1 font-lock-builtin-face nil t))
  (list "\\(\\*[^ ]*\\*\\)" '(1 font-lock-constant-face nil t))
  (list (concat "(" (regexp-opt '("load") t) "\\>") '(1 font-lock-keyword-face nil t))
  (list (concat "(" (regexp-opt '("setq") t) "\\>") '(1 font-lock-type-face nil t))
  )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; find definition ====================================================================

(defun euslisp-print-current-word ()
  "print current word."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9")
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))

(defun euslisp-helm-ag (query dirs)
  "wrapper of helm-ag"
  (helm-ag--init-state)
  (let ((dir dirs)
        targets)
    (when (listp dir)
      (setq basedir default-directory
            targets dir))
    (let ((helm-ag--default-directory (or basedir dir))
          (helm-ag--default-target targets))
      (setq helm-ag--last-query query)
      (helm-attrset 'search-this-file nil helm-ag-source)
      (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
      (helm :sources '(helm-ag-source) :buffer "*euslisp-helm-ag*" :keymap helm-ag-map
            :history 'helm-ag--helm-history))))

(defun euslisp-find-definition-ag (query)
  "search query by helm-ag"
  (euslisp-helm-ag (concat "(\\(:|\\(defun |\\(defclass |\\(defmacro |\\(defmethods )" query) (s-split ":" (getenv "ROS_PACKAGE_PATH"))))

(defun euslisp-find-definition-function ()
  "find defnition of euslisp"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (euslisp-goto-definition (buffer-substring (region-beginning) (region-end)))
    (euslisp-find-definition-ag (euslisp-print-current-word))))

;;; Shell ====================================================================


(defun euslisp-shell-get-or-create-process ()
  "Get or create an inferior Euslisp process for current buffer and return it."
  (let* ((bufname (format "*%s*" (euslisp-shell-get-process-name nil)))
         (dedbufname (format "*%s*" (euslisp-shell-get-process-name t)))
         (proc (get-buffer-process bufname))
         (dedproc (get-buffer-process dedbufname)))
    (if euslisp-dedicated-shells
        (if dedproc
            dedproc
          (run-euslisp (euslisp-shell-parse-command) t)
          (get-buffer-process dedbufname))
      (if dedproc
          dedproc
        (if proc
            proc
          (run-euslisp (euslisp-shell-parse-command))
          (get-buffer-process bufname))))))


(when (not (fboundp 'euslisp-shell-get-process-name))
  (defun euslisp-shell-get-process-name (dedicated)
    "Default shell name."
    "Euslisp"))


(defmacro euslisp-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`euslisp-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (euslisp-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (euslisp-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (euslisp-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (euslisp-shell-tramp-refresh-remote-path
            ,vec (euslisp-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (euslisp-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))


(defun euslisp-shell-make-comint (cmd proc-name &optional show internal)
  "Create a Euslisp shell comint buffer."
  (save-excursion
    (euslisp-shell-with-environment
      (let* ((proc-buffer-name
              (format (if (not internal) "*%s*" " *%s*") proc-name)))
        (when (not (comint-check-proc proc-buffer-name))
          (let* ((cmdlist (split-string-and-unquote cmd))
                 (interpreter (car cmdlist))
                 (args (cdr cmdlist))
                 (buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                                interpreter nil args))
                 (euslisp-shell--parent-buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 (euslisp-shell--interpreter interpreter)
                 (euslisp-shell--interpreter-args
                  (mapconcat #'identity args " ")))
            (with-current-buffer buffer
              (inferior-euslisp-mode))
            (when show (display-buffer buffer))
            (and internal (set-process-query-on-exit-flag process nil))))
        proc-buffer-name))))


(defun run-euslisp (&optional cmd dedicated show)
  "Run an inferior Euslisp process."
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Euslisp: " (euslisp-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (euslisp-shell-calculate-command) nil t)))
  (get-buffer-process
   (euslisp-shell-make-comint
    (or cmd (euslisp-shell-calculate-command))
    (euslisp-shell-get-process-name dedicated) show)))


(defun euslisp-shell-get-buffer ()
  "Return inferior Euslisp buffer for current buffer.
If current buffer is in `inferior-euslisp-mode', return it."
  (if (derived-mode-p 'inferior-euslisp-mode)
      (current-buffer)
    (let* ((dedicated-proc-name (euslisp-shell-get-process-name t))
           (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
           (global-proc-name  (euslisp-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
           (global-running (comint-check-proc global-proc-buffer-name)))
      ;; Always prefer dedicated
      (or (and dedicated-running dedicated-proc-buffer-name)
          (and global-running global-proc-buffer-name)))))


(defun euslisp-shell-get-process ()
  "Return inferior Euslisp process for current buffer."
  (get-buffer-process (euslisp-shell-get-buffer)))


(defun euslisp-shell-get-process-or-error (&optional interactivep)
  "Return inferior Euslisp process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (euslisp-shell-get-process)
      (if interactivep
          (user-error
           "Start a Euslisp process first with `M-x run-euslisp' or `%s'."
           ;; Get the binding.
           (key-description
            (where-is-internal
             #'run-euslisp overriding-local-map t)))
        (error
         "No inferior Euslisp process running."))))


(defun euslisp-shell-send-string (string &optional process msg)
  "Send STRING to inferior Euslisp PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Euslisp command: ") nil t))
  (let ((process (or process (euslisp-shell-get-process-or-error msg))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))


(defun euslisp-shell-calculate-command ()
  "Calculate the string used to execute the inferior Euslisp process."
  (format "%s %s"
          (shell-quote-argument euslisp-shell-interpreter)
          euslisp-shell-interpreter-args))

(when (not (fboundp 'euslisp-shell-calculate-process-environment))
  (defun euslisp-shell-calculate-process-environment ()
    "Compatibility function for older Emacsen."
    process-environment))

(when (not (fboundp 'euslisp-shell-calculate-exec-path))
  (defun euslisp-shell-calculate-exec-path ()
    "Compatibility function for older Emacsen."
    exec-path))

(defun euslisp-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (euslisp-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun euslisp-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from `tramp-open-connection-setup-interactive-shell'.
  (let ((env (append (when (fboundp #'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
                     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
        unset vars item)
    (while env
      (setq item (tramp-compat-split-string (car env) "="))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
          (push (format "%s %s" (car item) (cdr item)) vars)
        (push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
               tramp-end-of-heredoc
               (mapconcat 'identity vars "\n")
               tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defun euslisp-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^euslisp-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^euslisp-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(define-derived-mode inferior-euslisp-mode comint-mode "Inferior Euslisp"
  (when euslisp-shell--parent-buffer
    (euslisp-util-clone-local-variables euslisp-shell--parent-buffer))
  (compilation-shell-minor-mode 1))

(defun euslisp-send-current-statement ()
  (interactive)
  (let ((selection (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (euslisp-shell-send-string selection)))

(defun euslisp-shell-send-region (from to)
  (interactive "r")
  (let ((selection (buffer-substring-no-properties from to)))
    (euslisp-shell-send-string selection)))


(defun euslisp-path-from-shell-printf (str &optional args)
  (let* ((printf-bin (or (executable-find "printf") "printf"))
         (printf-command
          (concat printf-bin
                  " '__RESULT\\000" str "\\000__RESULT' "
                  (mapconcat #'exec-path-from-shell--double-quote args " ")))
         (shell (exec-path-from-shell--shell))
         (shell-name (car (last (s-split "/" shell))))
         (shell-args (append exec-path-from-shell-arguments
                             (list "-c"
                                   (if euslisp-path-ros-env
                                       (if (exec-path-from-shell--standard-shell-p shell)
                                           (format "source %s/setup.%s && %s" euslisp-path-ros-env shell-name printf-command)
                                         (if (string-equal shell-name "fish")
                                             (format "bass source %s/setup.bash; and sh -c %s" euslisp-path-ros-env (shell-quote-argument printf-command))
                                           (error "Not implemented %s" shell-name)))
                                     (if (exec-path-from-shell--standard-shell-p shell)
                                         printf-command
                                       (format "sh -c %s" (shell-quote-argument printf-command))))))))
    (with-temp-buffer
      (exec-path-from-shell--debug "Invoking shell %s with args %S" shell shell-args)
      (let ((exit-code (apply #'call-process shell nil t nil shell-args)))
        (exec-path-from-shell--debug "Shell printed: %S" (buffer-string))
        (unless (zerop exit-code)
          (error "Non-zero exit code from shell %s invoked with args %S.  Output was:\n%S"
                 shell shell-args (buffer-string))))
      (goto-char (point-min))
      (if (re-search-forward "__RESULT\0\\(.*\\)\0__RESULT" nil t)
          (match-string 1)
        (error "Expected printf output from shell, but got: %S" (buffer-string))))))

(defun euslisp-path-from-shell-getenvs (names)
  "Get the environment variables with NAMES from the user's shell.

Execute the shell according to `euslisp-path-from-shell-arguments'.
The result is a list of (NAME . VALUE) pairs."
  (let* ((random-default (md5 (format "%s%s%s" (emacs-pid) (random) (current-time))))
         (dollar-names (mapcar (lambda (n) (format "${%s-%s}" n random-default)) names))
         (values (split-string (euslisp-path-from-shell-printf
                                (mapconcat #'identity (make-list (length names) "%s") "\\000")
                                dollar-names) "\0")))
    (let (result)
      (while names
        (prog1
            (let ((value (car values)))
              (push (cons (car names)
                          (unless (string-equal random-default value)
                            value))
                    result))
          (setq values (cdr values)
                names (cdr names))))
      result)))

(defvar euslisp-path-ros-env nil)

(defun euslisp-path-from-shell-getenv (name)
  "Get the environment variable NAME from the user's shell.

Execute the shell as interactive login shell, have it output the
variable of NAME and return this output as string."
  (cdr (assoc name (euslisp-path-from-shell-getenvs (list name)))))

(defcustom euslisp-path-from-shell-shell-name nil
  "If non-nil, use this shell executable.
Otherwise, use either `shell-file-name' (if set), or the value of
the SHELL environment variable."
  :type '(choice
          (file :tag "Shell executable")
          (const :tag "Use `shell-file-name' or $SHELL" nil))
  :group 'euslisp)


;;;###autoload
(defun euslisp-path-from-shell-copy-envs (names)
  "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then `euslisp-path' and
`eshell-path-env' are also set appropriately.  The result is an alist,
as described by `euslisp-path-from-shell-getenvs'."
  (let ((pairs (euslisp-path-from-shell-getenvs names)))
    (when exec-path-from-shell-check-startup-files
      (exec-path-from-shell--maybe-warn-about-startup-files pairs))
    (mapc (lambda (pair)
            (exec-path-from-shell-setenv (car pair) (cdr pair)))
          pairs)))

(defun euslisp-switch-to-shell ()
  "Switch to inferior euslisp process buffer."
  (interactive)
  (pop-to-buffer (process-buffer (euslisp-shell-get-process-or-error))))

(defun euslisp-change-env (env-directory)
  "Change default source path"
  (interactive (list (read-directory-name "CATKIN_PATH: " euslisp-choose-directory-default-directory)))
  (let ((env-path env-directory))
    (setq euslisp-path-ros-env env-path)
    (euslisp-path-from-shell-copy-envs
     (list "PATH" "PYTHONPATH" "LD_LIBRARY_PATH" "EUSDIR" "ARCHDIR" "ROS_ROOT" "ROS_PACKAGE_PATH" "ROS_MASTER_URI"
           "ROS_ETC_DIR" "ROSLISP_PACKAGE_DIRECTORIES" "ROS_DISTRO" "CMAKE_PREFIX_PATH"))))


(provide 'euslisp-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; euslisp-mode.el ends here
