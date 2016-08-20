inf-php.el provides a REPL buffer connected to an php interactive shell.
Most codes are derived from inf-ruby.el ;)

Usage
put inf-php.el to load path, and add following code to your .emacs

(require 'inf-php)

TODO
* syntax highlight
* fix completion
* support load file
* better implementation


(require 'comint)
(require 'compile)
(require 'php-mode)

(defvar inf-php-first-prompt-pattern "^php > *"
  "First prompt regex pattern of php interpreter.")

(defvar inf-php-prompt-pattern "^php > *"
  "Prompt regex pattern of php interpreter.")

(defvar php-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ruby-load-file command.
Used for determining the default in the next one.")

(defvar inf-php-mode-hook nil
  "*Hook for customising inf-php mode.")

(defvar inf-php-preload-code
  "function p($x) { return var_dump($x); }\n"
  "php code to be loaded after launching php interactive shell.")

(defvar inf-php-runner-script-path "/tmp/inf-php.sh"
  "Path to runner script, which is used to launch php interactive
  shell on emacs, a dirty work around :(")

(defvar inf-php-enable-launch-workaround nil
  "The value t enable launcher script.")

(defvar inf-php-mode-map
  (let ((map (copy-keymap comint-mode-map)))
   (define-key map (kbd "C-c C-l") 'inf-php-load-file)
    (define-key map (kbd "C-x C-e") 'php-send-last-sexp)
    (define-key map (kbd "TAB") 'inf-php-complete-or-tab)
    map)
  "*Mode map for inf-php-mode")

(defun inf-php-keys ()
  "Set local key defs to invoke inf-php from php-mode."
  (define-key php-mode-map "\M-\C-x" 'php-send-definition)
  (define-key php-mode-map "\C-x\C-e" 'php-send-last-sexp)
  (define-key php-mode-map "\C-c\C-x" 'php-send-definition)
  (define-key php-mode-map "\C-c\M-x" 'php-send-definition-and-go)
  (define-key php-mode-map "\C-c\C-r" 'php-send-region)
  (define-key php-mode-map "\C-c\M-r" 'php-send-region-and-go)
  (define-key php-mode-map "\C-c\C-z" 'php-switch-to-inf)
 (define-key php-mode-map "\C-c\C-l" 'php-load-file)
  (define-key php-mode-map "\C-c\C-s" 'inf-php))

(defvar inf-php-buffer nil "Current php-cli process buffer.")

(defvar php-shell-output-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\{ "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\} "." table)
    table)
  "Syntax table for shell output.
It makes parens and quotes be treated as punctuation chars.")


(defun inf-php-mode ()
  "Major mode for interacting with an inferior php (irb) process.

The following commands are available:
\\{inf-php-mode-map}

A php process can be fired up with M-x inf-php.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inf-php-mode-hook (in that order).

You can send text to the inferior php process from other buffers containing
Php source.
    php-switch-to-inf switches the current buffer to the php process buffer.
    php-send-definition sends the current definition to the php process.
    php-send-region sends the current region to the php process.

    php-send-definition-and-go, php-send-region-and-go,
        switch to the php process buffer after sending their text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for php; with arugment, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inf-php-prompt-pattern)
  (setq major-mode 'inf-php-mode)
  (setq mode-name "Inf-Php")
  (setq mode-line-process '(":%s"))
  (use-local-map inf-php-mode-map)
  (setq comint-input-filter (function inf-php-input-filter))
  (setq comint-get-old-input (function inf-php-get-old-input))
 (make-local-variable 'compilation-error-regexp-alist)
 (setq compilation-error-regexp-alist inf-php-error-regexp-alist)
  (compilation-shell-minor-mode t)

  ;; TODO syntax highlight
  (set-syntax-table php-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(php-font-lock-keywords-3 nil nil nil nil))
  (set (make-local-variable 'syntax-propertize-function)
       (eval
        ;; XXX: Unfortunately eval is needed here to make use of the
        ;; dynamic value of `comint-prompt-regexp'.
        `(syntax-propertize-rules
          (,comint-prompt-regexp
           (0 (ignore
               (put-text-property
                comint-last-input-start end 'syntax-table
                php-shell-output-syntax-table)
               ;; XXX: This might look weird, but it is the easiest
               ;; way to ensure font lock gets cleaned up before the
               ;; current prompt, which is needed for unclosed
               ;; strings to not mess up with current input.
               (font-lock-unfontify-region comint-last-input-start end))))
          ;; (,(python-rx string-delimiter)
          ;;  (0 (ignore
          ;;      (and (not (eq (get-text-property start 'field) 'output))
          ;;           (python-syntax-stringify)))))
)))

  ;; first php interactive shell will exit
  (comint-send-string (inf-php-proc) "\n")

  ;; load the code
  (comint-send-string (inf-php-proc) inf-php-preload-code)
  (compilation-shell-minor-mode 1)
  (run-hooks 'inf-php-mode-hook)
)

(defvar inf-php-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun inf-php-input-filter (str)
  "Don't save anything matching inf-php-filter-regexp"
  (not (string-match inf-php-filter-regexp str)))

adapted from replace-in-string in XEmacs (subr.el)
(defun inf-php-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun inf-php-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inf-php-first-prompt-pattern)
      (inf-php-remove-in-string (buffer-substring (point) end)
                                 inf-php-prompt-pattern))))

(defun inf-php (&optional impl)
  "Run an inferior Php process in a buffer. With prefix argument,
prompts for which Php implementation to use. Runs the hooks
`inf-php-mode-hook' \(after the `comint-mode-hook' is run)."

  (interactive)
  (if inf-php-enable-launch-workaround
      (progn
        (shell-command (format "echo 'php -a >/dev/null && php -a' > %s" inf-php-runner-script-path))
        (run-php (format "sh %s" inf-php-runner-script-path) "php"))
    (run-php "php -a" "php")))

(defun run-php (&optional command name)
  "Run an inferior Php process, input and output via buffer *php*.
If there is a process already running in `*php*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `php-program-name').  Runs the hooks `inferior-php-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive)
  (setq command (or command "php -a"))
  (setq name (or name "php"))


  (if (not (comint-check-proc inf-php-buffer))
      (let ((commandlist (split-string command)))
        (set-buffer (apply 'make-comint name (car commandlist)
                           nil (cdr commandlist)))
        (inf-php-mode)
        ))
  (pop-to-buffer (setq inf-php-buffer (format "*%s*" name))))

(defun inf-php-proc ()
  "Returns the current php process. See variable inf-php-buffer."
  (or (get-buffer-process (if (eq major-mode 'inf-php-mode)
                              (current-buffer)
                            inf-php-buffer))
      (error "No current process. See variable inf-php-buffer")))

These commands are added to the php-mode keymap:

(defconst php-eval-separator " ")

(defun php-send-region (start end)
  "Send the current region to the inferior Php process."
  (interactive "r")
 (let (term (file (buffer-file-name)) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        ;; (while (progn
        ;;          (setq term (apply 'format php-send-terminator (random) (current-time)))
        ;;          (re-search-forward (concat "^" (regexp-quote term) "$") end t)))
        ))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (inf-php-proc))))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (insert php-eval-separator "\n")
        (set-marker m (point))))
   (comint-send-string (inf-php-proc) (format "eval(<<<%s\n" term))
    (comint-send-region (inf-php-proc) start end)
    (comint-send-string (inf-php-proc) "\n")
   (comint-send-string (inf-php-proc) (concat "\n" term "\n);\n"))
)


(defun php-send-definition ()
  "Send the current definition to the inferior Php process."
  (interactive)
  (save-excursion
    (php-end-of-defun)
    (let ((end (point)))
      (php-beginning-of-defun)
      (php-send-region (point) end))))

(defun php-send-last-sexp ()
  "Send the previous sexp to the inferior Php process."
  (interactive)
  (php-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun php-switch-to-inf (eob-p)
  "Switch to the php process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inf-php-buffer)
      (pop-to-buffer inf-php-buffer)
    (error "No current process buffer. See variable inf-php-buffer."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun php-send-region-and-go (start end)
  "Send the current region to the inferior Php process.
Then switch to the process buffer."
  (interactive "r")
  (php-send-region start end)
  (php-switch-to-inf t))

(defun php-send-definition-and-go ()
  "Send the current definition to the inferior Php.
Then switch to the process buffer."
  (interactive)
  (php-send-definition)
  (php-switch-to-inf t))


(defun php-load-file (file-name)
  "Load a Php file into the inferior Php process."
  (interactive (comint-get-source "Load Php file: " php-prev-l/c-dir/file
                                  php-source-modes t)) ;; T because LOAD needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq php-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inf-php-proc) (concat "(load \""
                                              file-name
                                              "\"\)\n")))

(defun inf-php-completions (seed)
  "Return a list of completions for the line of php code starting with SEED."
  (let* ((proc (get-buffer-process inf-php-buffer))
	 (comint-filt (process-filter proc))
	 (kept "") completions)
    (set-process-filter proc (lambda (proc string) (setf kept (concat kept string))))
    (process-send-string proc (format "puts IRB::InputCompletor::CompletionProc.call('%s').compact\n" seed))
    (while (not (string-match inf-php-prompt-pattern kept)) (accept-process-output proc))
    (if (string-match "^[[:alpha:]]+?Error: " kept) (error kept))
    (setf completions (butlast (split-string kept "[\r\n]") 2))
    (set-process-filter proc comint-filt)
    completions))

(defun inf-php-complete-or-tab (&optional command)
  "Either complete the php code at point or call
`indent-for-tab-command' if no completion is available.  Relies
on the irb/completion Module used by readline when running irb
through a terminal."
  (interactive (list (let* ((curr (thing-at-point 'line))
			    (completions (inf-php-completions curr)))
		       (case (length completions)
			 (0 nil)
			 (1 (car completions))
			 (t (completing-read "possible completions: " completions nil 'confirm-only curr))))))
  (if (not command)
      (call-interactively 'indent-for-tab-command)
    (move-beginning-of-line 1)
    (kill-line 1)
    (insert command)))

(eval-after-load 'php-mode
  '(add-hook 'php-mode-hook 'inf-php-keys))

(provide 'inf-php)
inf-php.el ends here
