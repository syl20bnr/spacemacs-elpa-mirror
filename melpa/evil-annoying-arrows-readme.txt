Entering evil-annoying-arrows-mode makes emacs ring the bell in your
face if you use the arrow keys to move long distances.

Set the `evil-annoying-arrows-too-far-count' to adjust the length.

Code:

(require 'cl-lib)

(defvar evil-annoying-arrows-too-far-count 10
  "Number of repeated arrow presses before emacs gets annoyed.")

(defvar evil-annoying-arrows-super-annoying-mode nil
  "Switches to and shows the annoying message in another buffer.")

(setq evil-annoying-commands '(next-line previous-line))
(setq eaa--old-time (float-time))

(defvar evil-annoying-arrows--current-count 2
  "Defaults to two because first two keypresses registered as one for some reason.")

(defun eaa--commands-with-shortcuts (cmds)
  (cl-remove-if (lambda (cmd)
               (and
                (>= (length (substitute-command-keys (format "\\[%S]" cmd))) 3)
                (string-equal
                 (substring (substitute-command-keys (format "\\[%S]" cmd)) 0 3)
                 "M-x"))) cmds))

Since evil-commands sometimes call themselves recursively (evil-forward-char calls itself 2-3 times, for example) we need to ensure that the user actually pressed the keys for those commands several times. We do this by ensuring that the time between command calls is longer than some threshold. Without this check, 3-4 calls of evil-forward char would be enough to trigger the bell with a too far count of 10.
(defun eaa--check-enough-time-passed (new-time)
  "Checks if the time between two commands is smaller than some threshold."
  (progn
    (setq eaa--old-temp eaa--old-time)
    (setq eaa--old-time new-time)
    (< 0.1 (- eaa--old-time eaa--old-temp))))

(defun eaa--maybe-complain (cmd)
  (when (eaa--check-enough-time-passed (float-time))
    (if (and (memq this-command evil-annoying-commands)
             (or (eq this-command last-command)
                 (eq (get cmd 'alternative-cmd) last-command)))
        (progn
          (incf evil-annoying-arrows--current-count)
          (when (> evil-annoying-arrows--current-count evil-annoying-arrows-too-far-count)
            (beep 1)
            (let* ((alts (eaa--commands-with-shortcuts (get cmd 'eaa--alts)))
                   (alt (nth (random (length alts)) alts))
                   (key (substitute-command-keys (format "\\[%S]" alt)))
                   (msg (format "Annoying! How about using %S (keymap: %s) instead?" alt key)))
              (if evil-annoying-arrows-super-annoying-mode
                  (progn (switch-to-buffer (get-buffer-create "Super-annoying-buffer"))
                         (insert msg))
                (message msg)))))
      (setq evil-annoying-arrows--current-count 2))))

(defmacro add-evil-annoying-arrows-advice (cmd alternatives &optional helper-cmd)
  `(progn
     (add-to-list 'evil-annoying-commands (quote ,cmd))
     (put (quote ,cmd) 'eaa--alts ,alternatives)
     (put (quote ,cmd) 'alternative-cmd (quote ,helper-cmd))
     (put (quote ,helper-cmd) 'alternative-cmd (quote ,cmd))
     (defadvice ,cmd (before evil-annoying-arrows activate)
       (when evil-annoying-arrows-mode
         (eaa--maybe-complain (quote ,cmd))))))

(add-evil-annoying-arrows-advice evil-next-line '(evil-search-forward evil-jumper/backward evil-snipe-s) next-line)
(add-evil-annoying-arrows-advice evil-previous-line '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward) previous-line)
(add-evil-annoying-arrows-advice evil-forward-char '(evil-search-forward evil-find-char evil-snipe-f evil-snipe-s))
(add-evil-annoying-arrows-advice evil-backward-char '(evil-search-backward evil-find-char-backward evil-snipe-F evil-snipe-S))
(add-evil-annoying-arrows-advice evil-delete-backward-char '(evil-delete-backward-word evil-delete-line evil-delete-whole-line))
(add-evil-annoying-arrows-advice evil-delete-char '(evil-delete-backward-word evil-delete-line evil-delete-whole-line))

###autoload
(define-minor-mode evil-annoying-arrows-mode
  "Evil-Annoying-Arrows emacs minor mode."
  nil "" nil)

###autoload
(define-globalized-minor-mode global-evil-annoying-arrows-mode
  evil-annoying-arrows-mode evil-annoying-arrows-mode)

(defun eaa-add-suggestion (cmd alternative)
  (let ((old-alts (or (get cmd 'eaa--alts)
                      ())))
    (unless (memq alternative old-alts)
      (put cmd 'eaa--alts (cons alternative old-alts)))))

(defun eaa-add-suggestions (cmd alternatives)
  (let ((old-alts (or (get cmd 'eaa--alts)
                      ())))
    (put cmd 'eaa--alts (append
                        (remove-if (lambda (cmd)
                                     (memq cmd old-alts)) alternatives)
                        old-alts))))

(provide 'evil-annoying-arrows)
evil-annoying-arrows.el ends here
