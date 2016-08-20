How many times have you wanted to fire off a quick command, such as M-!,
but the directory you want to run the command in isn't the same as the
directory of the current buffer?  In those situations, you want a quick way
to change the default-directory *for only the next command*.  That is what
Ido-Springboard aims to solve.

It overrides command keys in ido-mode so that they use the
default-directory of the current viable completion, rather than the
default-directory of the buffer where you started the ido command.

(require 'ido)

(defgroup ido-springboard nil
  "Change default-directory for commands invoked at `ido-switch-buffer'."
  :group 'ido)

(defcustom ido-springboard-ignored-commands
  '(self-insert-command
    delete-backward-char
    abort-recursive-edit
    exit-minibuffer
    switch-to-buffer
    backward-char
    forward-char
    kill-line
    move-beginning-of-line
    move-end-of-line
    backward-kill-word
    forward-kill-word)
  "Commands that will not be trapped by Ido-Springboard."
  :type '(repeat command)
  :group 'ido-springboard)

(eval-when-compile
  (defvar ido-springboard-trapped nil))

(defun ido-springboard-match-directory ()
  (let ((item (or (let ((buf (get-buffer (car ido-matches))))
                    (and buf
                         (with-current-buffer buf
                           default-directory)))
                  (and ido-use-virtual-buffers ido-virtual-buffers
                       (cdr (assoc (car ido-matches)
                                   ido-virtual-buffers))))))
    (cond ((file-directory-p item)
           item)
          ((file-exists-p item)
           (file-name-directory item))
          (t
           nil))))

(defun ido-springboard-trap-command ()
  (unless ido-springboard-trapped
    (condition-case err
        (unless (or (memq this-command ido-springboard-ignored-commands)
                    (string-match "\\`ido-" (symbol-name this-command)))
          (let ((dir (ido-springboard-match-directory)))
            (when dir
              ;; (message "Trapped command: %s" this-command)
              (loop for buf in (buffer-list)
                    when (minibufferp buf)
                    do (with-current-buffer buf
                         (ido-springboard-remove-trap)))
              (setq ido-springboard-trapped t)
              (throw 'abort dir))))
      (error
       (message "Error occurred: %s" err)))))

(defun ido-springboard-add-trap ()
  (add-hook 'pre-command-hook 'ido-springboard-trap-command t t))

(defun ido-springboard-remove-trap ()
  (remove-hook 'pre-command-hook 'ido-springboard-trap-command t))

###autoload
(defadvice ido-switch-buffer (around ido-springboard-ido-switch-buffer activate)
  "Adds ability to set `default-directory' for commands at ido minibuffer."
  (interactive)
  (add-hook 'minibuffer-setup-hook 'ido-springboard-add-trap)
  (add-hook 'minibuffer-exit-hook 'ido-springboard-remove-trap)
  (unwind-protect
      (let* (ido-springboard-trapped
             ido-springboard-already-trapped
             (default-directory (catch 'abort (ignore ad-do-it))))
        (if default-directory
            (call-interactively this-command)))
    (remove-hook 'minibuffer-setup-hook 'ido-springboard-add-trap)
    (remove-hook 'minibuffer-exit-hook 'ido-springboard-remove-trap)))

(provide 'ido-springboard)

ido-springboard.el ends here
