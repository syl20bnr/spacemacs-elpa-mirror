;;; jg-quicknav.el --- Quickly navigate the file system to find a file.

;; Copyright (C) 2013-2017 Jeff Gran

;; Author: Jeff Gran <jeff@jeffgran.com>
;; URL: https://github.com/jeffgran/jg-quicknav
;; Package-Version: 20170809.130
;; Created: 3 Mar 2013
;; Keywords: navigation
;; Version: 1.4.0
;; Package-Requires: ((s "1.9.0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This code is licensed under the Do What the Fuck You Want To Public License
;;
;;        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2004
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;;; Commentary:

;; A quick file-finder for emacs. Navigate up and down directories to find a file.
;;
;; Like ido-find-file, lusty-explorer, helm/anything, etc. But none of them
;; did quite what I wanted so I created this. The goal is to navigate the file
;; system as fast as possible. Like a much faster way of doing the...
;;
;;  1. cd <foo>
;;  2. ls
;;  3. goto 1
;;  4. <open file>
;;
;;  ...loop in the shell.
;;
;; Usage: - assign `jg-quicknav' to a key, and use it.
;;        - buffer will show you the directory listing for the current default directory
;;          (reminder: if you're in a file buffer, that will be the directory
;;          that file is in. If you're in shell-mode, eshell, ansi-term, dired, etc,
;;          that will be the correct current directory).
;;        - type some letters to filter down the results. C-n and C-p to change selection
;;        - RET on a file to open the file, or RET on a directory to `jg-quicknav' that dir.
;;        - C-, to go "up a directory", after which you can go "forward" with C-.
;;        - Drop into dired with C-/
;;        - I tried to make it easy to rebind the bindings. I rebind everything all the time
;;          so I was sure others would not like my key choices. see below.
;;
;;
;;
;; You'll likely want to change some of the key bindings. Just redefine the keys
;; on jg-quicknav-mode-map. For example, if you use C-n for something else and want
;; to move up and down lines while navigating with M-n instead, use this:
;;
;; (define-key jg-quicknav-mode-map (kbd "C-n") nil)
;; (define-key jg-quicknav-mode-map (kbd "M-n") 'jg-quicknav-next)
;;
;;
;; TODO/IDEAS:
;; - better support for dirs with many files in them
;;   - ensure-cursor-visible or something in case selection is off the screen
;;   - also page-down and page-up support for long lists
;;   - maybe add an option to toggle showing only files/only dirs?
;; - shell-mode plugin to "cd" to a directory chosen via jgqn?
;; - secondary-highlight for previous dir after going 'updir'
;; - different face/color for executables? or just the "*"?
;;   - and different face for file extensions? (just like dired+)
;; - add a jg-quick-buffer-switcher too?

;;; History:

;; 2013-03-03 Initial release v 1.0.0
;; 2013-03-03 v 1.0.1
;;            - use overriding-local-map in minibuffer to make sure
;;              the jgqn keys override other minor modes during navigation
;;            - bugfix: clear out "history" in between sessions
;; 2013-03-05 v 1.1.0
;;            - fixed weirdness when going "back" when already at the root directory
;;            - added `jg-quicknav-find-file' -- key command to open a buffer for a new file
;;              with the contents of the minibuffer as the filename, in the same directory
;;              currently being shown
;;            - added `jg-quicknav-dired' -- key command to drop into dired in the same directory
;;              currently being shown
;; 2013-03-09 v 1.1.1
;;            - fixed bug: opening multiple files of the same name doesn't work
;;            - fixed bug: doesn't show / when / is the jg-quicknav-default-dir
;;            - fixed bug: use save-window-excursion instead of delete-window when session
;;              ends. this ensures your window configuration will look the same before and
;;              after the jgqn session
;;            - fixed bug: when updating results, clamp the selection index to the number
;;               of results so it doesn't disappear off the end
;; 2013-03-23 v 1.2.0
;;            - added jg-quicknav-dired. use it to fuzzy-filter a dired buffer
;;            - added (and made default) different behavior for C-g. Now it will clear the
;;              minibuffer input (if any) the first time you press it, and then quit the
;;              quicknav session the second time. To go back to previous behavior (C-g
;;              always quits), set "C-g" to 'jg-quicknav-minibuffer-exit instead of
;;              'jg-quicknav-minibuffer-clear-then-exit in the jg-quicknav-mode-map
;; 2013-03-29 v 1.2.1
;;            - fixed small annoyance: if you narrow to zero results then delete,
;;              clamp selection index to 1 otherwise it disappears until you move it again.
;; 2013-04-14 v 1.3.0
;;            - Now works remotely! You must be using tramp. This means that if you are
;;              visiting a remote file, or if you are in an ssh shell backed by tramp,
;;              you can use jg-quicknav and it will show the listing of the corresponding
;;              remote directory.
;;            - fixed annoying blinking selection line when list is empty
;; 2013-04-28 v 1.3.1
;;            - fixed bug: dir with space in name results in error. wrap `cd` command in quotes.
;; 2014-09-12 v 1.3.2
;;            - fixed bug: use expand-file-name so we always start with a full absolute path
;;            - fixed bug: concat pwd when visiting file so two files in different directories
;;              with the same name don't get confused.
;; 2015-02-11 v 1.4.0
;;            - no new code, just renamed a bunch of functions for melpa compliance.
;;              - everything now prefixed with jg-quicknav- (previously some were jgqn-)
;;              - prefixed stolen functions too, to prevent conflicts.
;;              - internal-only functions prefixed with jg-quicknav--
;; 2015-02-16 v 1.4.1
;;            - add (require 'tramp) to make sure tramp is loaded. doesn't work without it.


;;; Code:

(require 's)
(require 'cl-lib)
(require 'tramp)

(defvar jg-quicknav-pwd nil
  "The current working directory, for jg-quicknav.
This is kept up-to-date while navigating in the quicknav buffer")

(defvar jg-quicknav-ls nil
  "This is a placeholder for the results of the `ls` command in the current `jg-quicknav-pwd'")

(defvar jg-quicknav-buffer nil
  "The buffer where directory listing is shown during navigation
Gets created on demand when using `jg-quicknav'")

(defvar jg-quicknav-selection-index 1
  "The (1-based) index of the currently selected line in the `jg-quicknav-buffer'")

(defvar jg-quicknav-mode-map (make-sparse-keymap)
  "The mode that is active in the minibuffer when navigating.
Commands generally control the `jg-quicknav-buffer'

The following keys are defined:
\\{jg-quicknav-mode-map}")

(defvar jg-quicknav-initialized nil
  "Whether the hooks and advice for `jg-quicknav' have been initialized.")

(defvar jg-quicknav-file-or-dir-to-visit nil
  "A string containing the resulting file name or directory name.
This is a single 'token', and needs to be combined with (`jg-quicknav-pwd') to be the
full path.")

(defvar jg-quicknav-history nil
  "A list holding the current history, like a browser. This enables you
to go `jg-quicknav-downdir' (forwards) after going `jg-quicknav-updir' (backwards)")

(set-keymap-parent jg-quicknav-mode-map minibuffer-local-map)
(define-key jg-quicknav-mode-map (kbd "C-n") 'jg-quicknav-next)
(define-key jg-quicknav-mode-map (kbd "<down>") 'jg-quicknav-next)
(define-key jg-quicknav-mode-map (kbd "C-p") 'jg-quicknav-prev)
(define-key jg-quicknav-mode-map (kbd "<up>") 'jg-quicknav-prev)
(define-key jg-quicknav-mode-map (kbd "M-<") 'jg-quicknav-first)
(define-key jg-quicknav-mode-map (kbd "M->") 'jg-quicknav-last)

(define-key jg-quicknav-mode-map (kbd "C-g") 'jg-quicknav-minibuffer-clear-then-exit)
(define-key jg-quicknav-mode-map (kbd "RET") 'jg-quicknav-visit-file-or-dir)
(define-key jg-quicknav-mode-map (kbd "TAB") 'jg-quicknav-visit-file-or-dir)
;;(define-key jg-quicknav-mode-map (kbd "C-j") 'jg-quicknav-visit-file-or-dir)
(define-key jg-quicknav-mode-map (kbd "C-,") 'jg-quicknav-updir)
(define-key jg-quicknav-mode-map (kbd "C-.") 'jg-quicknav-downdir)
(define-key jg-quicknav-mode-map (kbd "C-o") 'jg-quicknav-find-file)
(define-key jg-quicknav-mode-map (kbd "C-/") 'jg-quicknav-dired)

(define-key jg-quicknav-mode-map (kbd "C-d") 'jg-quicknav-shell-cd)

;;;###autoload
(define-minor-mode jg-quicknav-mode
  "Minor mode that is in effect when navigating using `jq-quicknav'

Associated mode map is `jg-quicknav-mode-map':
\\{jg-quicknav-mode-map}"
  :lighter " jgqn"
  :keymap jg-quicknav-mode-map
  :group 'jg-quicknav)

(defun jg-quicknav-ls ()
  "Get the result of `ls` for the current directory (`jg-quicknav-pwd') in a string."
  (or jg-quicknav-ls
      (setq jg-quicknav-ls
            (let ((the-pwd (if (tramp-tramp-file-p (jg-quicknav-pwd))
                               (with-parsed-tramp-file-name (jg-quicknav-pwd) jg-quicknav-pwd
                                 jg-quicknav-pwd-localname)
                             (jg-quicknav-pwd))))
              (let ((prev-shell-file-name shell-file-name))
                (prog2
                  (setq-default shell-file-name "/bin/bash")
                  (shell-command-to-string
                     (concat "cd '"
                             (concat the-pwd "/")
                             "' && ls -1AF"))
                  (setq-default shell-file-name prev-shell-file-name))
                )))))


(defun jg-quicknav-pwd ()
  "The current directory while navigating via `jg-quicknav'

Defaults to the value of `default-directory' if not set.

Must not have a trailing /."
  (or jg-quicknav-pwd (setq jg-quicknav-pwd (s-chop-suffix "/" (expand-file-name default-directory)))))


(defun jg-quicknavigating-p ()
  "Returns t if in a `jg-quicknav' session, nil otherwise."
  (and (minibufferp)
       (memq jg-quicknav-mode-map (current-minor-mode-maps))))

(defun jg-quicknav-initialize ()
  "Initialize the hooks and advice for `jg-quicknav' mode"
  (or jg-quicknav-initialized
      (progn
        (add-hook 'minibuffer-setup-hook 'jg-quicknav-minibuffer-setup)
        ;;(add-hook 'minibuffer-exit-hook 'jg-quicknav-minibuffer-teardown)

        ;; TODO make this optional? could be annoying
        (defadvice delete-backward-char (around jg-quicknav-delete-backward-char activate)
          "Go up a directory instead of backspacing when the minibuffer is empty during `jg-quicknav'"
          (if (and (jg-quicknavigating-p)
                   (eq 0 (length (jg-quicknav-get-minibuffer-string))))
              (jg-quicknav-updir)
            ad-do-it))
        (setq jg-quicknav-initialized t))))

;;;###autoload
(defun jg-quicknav ()
  "Main entry-point for jg-quicknav. Assign this to your preferred keybinding.

Opens a quicknav buffer with a directory listing, and turns on
`jg-quicknav-mode' in the minibuffer.

The initial directory is set to the variable `default-directory'

The following keys are in effect via `jg-quicknav-mode-map' (whose parent
is the standard `minibuffer-local-map') while navigating:

\\{jg-quicknav-mode-map}"
  (interactive)

  ;; get or create the buffer for showing the list
  (setq jg-quicknav-buffer (get-buffer-create "*quicknav*"))

  (with-current-buffer jg-quicknav-buffer
    (setq buffer-read-only t)
    (setq cursor-type nil))

  (save-window-excursion
    (display-buffer jg-quicknav-buffer)
    (jg-quicknav-show-results)

    (jg-quicknav-initialize)

    ;; apparently people override this sometimes so I have override it to fix it :/
    (setq this-command 'jg-quicknav)

    (read-string (concat "Current Directory: " (jg-quicknav-pwd) "/")))

  (if jg-quicknav-file-or-dir-to-visit
      (switch-to-buffer (get-file-buffer (concat (jg-quicknav-pwd) "/" jg-quicknav-file-or-dir-to-visit))))

  (jg-quicknav-cleanup)
  (setq jg-quicknav-history nil)

  (kill-buffer jg-quicknav-buffer))


(defun jg-quicknav-cleanup ()
  "Clean out the cached values set while navigating via `jg-quicknav',
in order to start anew with a new directory"
  (setq jg-quicknav-file-or-dir-to-visit  nil
        jg-quicknav-pwd                   nil
        jg-quicknav-ls                    nil
        jg-quicknav-selection-index       1))

(defun jg-quicknav-show-results ()
  "General purpose function to update the minibuffer prompt and update the
`jg-quicknav-buffer' with the latest (maybe filtered) results. Called when
starting a session, changing directories, or after changing the minibuffer text."

  (interactive)
  (jg-quicknav-update-minibuffer-prompt)

  (let ((query-string (jg-quicknav-get-minibuffer-string)))
    (with-current-buffer jg-quicknav-buffer

      (let ((new-lines (jg-quicknav-sort-and-filter
                        (s-split "\n" (jg-quicknav-ls) t)
                        query-string))
            (buffer-read-only nil))
        (erase-buffer)
        (jg-quicknav-insert-status-line query-string)
        (if new-lines
            (insert (mapconcat 'identity new-lines "\n")))
        (newline)
        ;; clamp the selected line to make sure it's still visible
        (if (> jg-quicknav-selection-index (length new-lines))
            (setq jg-quicknav-selection-index (length new-lines)))
        (when (< jg-quicknav-selection-index 1)
          (setq jg-quicknav-selection-index 1)))))

  ;; now update the faces.
  (jg-quicknav-update-faces))

(defun jg-quicknav-sort-and-filter (list query)
  "Filter LIST by using 'fuzzy-matching' against QUERY.
The filter matches using a regexp with  (.*) between each character.
Then sort ascending by length if the query is not empty.
Turns out this is my favorite fuzzy matching/sorting algorithm."
  (jg-quicknav--fuzzy-match query
                   (sort list
                         (lambda (s1 s2)
			   (if (> (length query) 0)
			       (< (length s1) (length s2))
			     (string< s1 s2))))))

(defun jg-quicknav-insert-status-line (query-string)
  (insert (jg-quicknav-pwd))
  (insert "/")
  (insert (or query-string ""))
  (insert "\n\n"))

(defadvice message (around dont-message activate disable)
  "Override `message' to not message anything."
  nil)

(defmacro suppress-messages (&rest body)
  "Don't display any messages in the mibuffer while evaluating BODY."
  `(progn
     (ad-enable-advice 'message 'around 'dont-message)
     (ad-activate 'message)

     ,@body

     (ad-disable-advice 'message 'around 'dont-message)
     (ad-activate 'message)))

(defun jg-quicknav-filter-dired ()
  "Do the actual fuzzy-filtering of a dired buffer. Called after each character
is typed."
  (interactive)
  (let ((query (jg-quicknav--explode-to-regexp (jg-quicknav-get-minibuffer-string))))
    (with-current-buffer (cdr (cl-first dired-buffers))
      (suppress-messages
       (revert-buffer)
       (dired-mark-files-regexp query)
       (dired-toggle-marks)
       (dired-do-kill-lines)))))

(defun jg-quicknav-minibuffer-setup ()
  "For assigning to the `minibuffer-setup-hook' to set up for a `jg-quicknav' session"
  (cond ((eq this-command 'jg-quicknav)
         (jg-quicknav-mode t)


         (setq overriding-local-map jg-quicknav-mode-map)
         (add-hook 'post-command-hook 'jg-quicknav-show-results nil t)	; t for local-only
         )
        ((eq this-command 'jg-quicknav-dired)
         (add-hook 'post-command-hook 'jg-quicknav-filter-dired nil t))))

(defun jg-quicknav-minibuffer-exit ()
  "Wrapper around `exit-minibuffer' in order to know if we just exited a
`jg-quicknav' session or not (via `this-command')"
  (interactive)
  ;;(ding)
  (exit-minibuffer))

(defun jg-quicknav-minibuffer-clear-then-exit ()
  "Clear the minibuffer input if any, and if not, quit the jg-quicknav session."
  (interactive)
  (let ((minibuffer-string (jg-quicknav-get-minibuffer-string)))
    (if (> (length minibuffer-string) 0)
        (delete-region (minibuffer-prompt-end) (point-max))
      (jg-quicknav-minibuffer-exit))))

(defun jg-quicknav-update-minibuffer-prompt ()
  "Updates the minibuffer prompt to show the updated current directory."
  (when (minibufferp)
    (let ((inhibit-read-only t))
      (put-text-property
       (point-min)
       (minibuffer-prompt-end)
       'display
       (concat "Current Directory: " (jg-quicknav-pwd) "/")))))

(defun jg-quicknav-get-minibuffer-string ()
  (and (minibufferp)
       (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))

(defun jg-quicknav-visit-file-or-dir ()
  "Get the current selection line and take action

Either visit it with `find-file' if it is a file, or reset `jg-quicknav-pwd' if it
is a directory."
  (interactive)
  ;; get the name of the thing we should go to, if possible
  (with-current-buffer jg-quicknav-buffer
    (let ((str (jg-quicknav-get-current-line)))
      (if (not (= 0 (length str)))
          (setq jg-quicknav-file-or-dir-to-visit str)
        (ding))))

  ;; if we got something, find-file it if it's a file,
  ;; or jg-quicknav in it if it's a directory

  (when jg-quicknav-file-or-dir-to-visit
    (let ((file-or-dir
           (concat (jg-quicknav-pwd) "/" jg-quicknav-file-or-dir-to-visit)))

      ;;(message (concat "file-or-dir: " file-or-dir))
      (if (file-directory-p file-or-dir)
          (progn
            (delete-minibuffer-contents)
            (jg-quicknav-cleanup)
            (setq jg-quicknav-pwd file-or-dir)
            (setq jg-quicknav-history nil)
            (jg-quicknav-show-results))

        (find-file file-or-dir)
        (exit-minibuffer)))))

(defun jg-quicknav-updir ()
  "Change directories up a level, like using `cd ..`"
  (interactive)
  (unless (s-equals-p "" (jg-quicknav-pwd))
    (let* ((tokens (s-split "/" (jg-quicknav-pwd)))
           (olddir (last tokens))
           (tokens (butlast tokens))
           (newdir (s-join "/" tokens)))
      (push olddir jg-quicknav-history)
      (jg-quicknav-cleanup)
      (setq jg-quicknav-pwd newdir))
    (delete-minibuffer-contents)
    (jg-quicknav-show-results)))

(defun jg-quicknav-downdir ()
  "Go back 'forward' after calling `jg-quicknav-updir'"
  (interactive)
  (if (not (null jg-quicknav-history))
      (let ((new-pwd (s-join "/" (cons (jg-quicknav-pwd) (pop jg-quicknav-history)))))
        (jg-quicknav-cleanup)
        (setq jg-quicknav-pwd new-pwd)
        (delete-minibuffer-contents)
        (jg-quicknav-show-results))
    (ding)))

(defun jg-quicknav-find-file ()
  "Open the file named by the minibuffer contents. If it doesn't exist, create it"
  (interactive)
  (let ((filename (minibuffer-contents)))
    (find-file (concat (jg-quicknav-pwd) "/" filename))
    (setq jg-quicknav-file-or-dir-to-visit filename)
    )
  (exit-minibuffer))

(defun jg-quicknav-dired ()
  "Open a dired buffer with the current directory, from inside a quicknav session."
  (interactive)
  (let ((dir (car (last (s-split "/" (jg-quicknav-pwd))))))
    (dired (jg-quicknav-pwd))
    (setq jg-quicknav-file-or-dir-to-visit dir)
    (exit-minibuffer)))

;; mostly lifted from (shell-resync-dirs) in shell.el
(defun jg-quicknav-shell-cd ()
  (interactive)
  ;; not (current-buffer) -- previous buffer...
  ;; no... most recent buffer that has a shell process, or
  ;; create a new shell buffer.
  (if (get-buffer-process (current-buffer))
      (let* ((proc (get-buffer-process (current-buffer)))
             (pmark (process-mark proc))
             (started-at-pmark (= (point) (marker-position pmark))))
        (save-excursion
          (goto-char pmark)
          ;; If the process echoes commands, don't insert a fake command in
          ;; the buffer or it will appear twice.
          (unless comint-process-echoes
            (insert (concat "cd " (jg-quicknav-pwd))) (insert "\n"))
          (sit-for 0)			; force redisplay
          (comint-send-string proc (concat "cd " (jg-quicknav-pwd)))
          (comint-send-string proc "\n"))
        (if started-at-pmark (goto-char (marker-position pmark))))
    ;; no process -- this is not a shell buffer.
    (ding)))

;;===============================
;; setting/changing the selection:
;;===============================
(defun jg-quicknav-set-selection-index (new-index)
  "Generic function to set the selection in `jg-quicknav-buffer' to NEW-INDEX"
  (interactive)
  (cond ((> 1 new-index)
         (jg-quicknav-last))
        ((< (- (jg-quicknav-count-lines) 2) new-index)
         (jg-quicknav-first))
        (t
         (setq jg-quicknav-selection-index new-index)))
  (jg-quicknav-update-faces))

(defun jg-quicknav-change-selection-index (offset)
  "Generic function to change the selection in `jg-quicknav-buffer' by OFFSET"
  (interactive)
  (jg-quicknav-set-selection-index (+ jg-quicknav-selection-index offset))
  )

(defun jg-quicknav-prev ()
  "Go to the previous selection in the `jg-quicknav-buffer'"
  (interactive)
  (jg-quicknav-change-selection-index -1))

(defun jg-quicknav-next ()
  "Go to the next selection in the `jg-quicknav-buffer'"
  (interactive)
  (jg-quicknav-change-selection-index +1))

(defun jg-quicknav-last ()
  "Go to the last selection in the `jg-quicknav-buffer'"
  (interactive)
  (jg-quicknav-set-selection-index (- (jg-quicknav-count-lines) 2)))

(defun jg-quicknav-first ()
  "Go to the last selection in the `jg-quicknav-buffer'"
  (interactive)
  (jg-quicknav-set-selection-index 1))

(defun jg-quicknav-get-current-line ()
  "Return the current line with no properties and no \n or ^J or whatever else at the end,
and without a trailing / if it was there"
  (goto-char (point-min))
  (forward-line (1- (+ 2 jg-quicknav-selection-index))) ;; + 2 to account for the status line and blank line
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    ;; remove the trailing executable or directory indicator
    (s-chop-suffixes '("@" "*" "/") line)))

(defun jg-quicknav-count-lines ()
  (with-current-buffer jg-quicknav-buffer
    (count-lines (point-min) (point-max))))

;; ===============================================================================
;; Faces
;; ===============================================================================
(defface jg-quicknav-directory-face
  '((t (:foreground "#FFEA77")))
  "This face is used to color directories in the quicknav buffer"
  :group 'jg-quicknav)

(defface jg-quicknav-selected-directory-face
  '((t (:foreground "#FFEA77" :background "#004083")))
  "This face is used to color a directory in the quicknav buffer if it is on the selection line"
  :group 'jg-quicknav)

(defface jg-quicknav-file-face
  '((t (:foreground "#6cb0f3")))
  "This face is used to color files in the quicknav buffer"
  :group 'jg-quicknav)

(defface jg-quicknav-selected-file-face
  '((t (:foreground "#ccffef" :background "#004083")))
  "This face is used to color a file in the quicknav buffer if it is on the selection line"
  :group 'jg-quicknav)

(defun jg-quicknav-update-faces ()
  "Updates the faces for the quicknav buffer"
  (with-current-buffer jg-quicknav-buffer
    (let ((buffer-read-only nil))
      (remove-list-of-text-properties (point-min) (point-max) '(face))

      ;; colors for files and dirs
      (goto-char (point-min))
      (forward-line (1- 3))

      (while (progn
               (cond (
                      ;; highlighted directories
                      (and (eq ?/ (char-before (line-end-position )))
                           ;; + 2 to account for the status line and blank line
                           (eq (line-number-at-pos (point)) (+ 2 jg-quicknav-selection-index)))
                      (add-text-properties
                       (line-beginning-position)
                       (+ 1 (line-end-position))
                       '(face jg-quicknav-selected-directory-face)))

                     ;; directories
                     ((eq ?/ (char-before (line-end-position)))
                      (add-text-properties
                       (line-beginning-position)
                       (line-end-position)
                       '(face jg-quicknav-directory-face)))

                     ;; highlighted file
                     ((eq (line-number-at-pos (point))
                          ;; + 2 to account for the status line and blank line
                          (+ 2 jg-quicknav-selection-index))
                      (add-text-properties
                       (line-beginning-position)
                       (+ 1 (line-end-position))
                       '(face jg-quicknav-selected-file-face))
                      )

                     ;; else -- just files
                     (t (add-text-properties
                         (line-beginning-position)
                         (line-end-position)
                         '(face jg-quicknav-file-face)))
                     )

               (not (and
                     (> (forward-line 1) 0)
                     (eq (point-max) (point))))
               ))

      )
    )
  )
;; ===============================================================================
;; /Faces
;; ===============================================================================




;; -------------------------------------------------------------------------------
;; Library functions I lifted
;; -------------------------------------------------------------------------------

;; ---------------------------------------------------------------------------------------
;; explosion functions (and regexp function, modified) from:
;; https://github.com/smtlaissezfaire/emacs-lisp-experiments/blob/master/fuzzy-matching.el
(defun jg-quicknav--fuzzy-match (string list)
  (jg-quicknav--match-with-regexp (jg-quicknav--explode-to-regexp string) list))

(defun jg-quicknav--match-with-regexp (regexp lst)
  "Return the elements of list lst which match the regular expression regexp"
  (cl-remove-if-not
   (lambda (str)
     (s-matches? regexp str))
   lst))

(defun jg-quicknav--explode-to-regexp (string)
  "Explode a string to a regular expression, where each char has a .* in front and back of it"
  (apply 'concat (jg-quicknav--explode-to-regexp-list string)))

(defun jg-quicknav--explode-to-regexp-list (string)
  "Explode a string to a list of chars, where each char has a .* in front and back of it"
  (cons ".*"
        (mapcar 'jg-quicknav--char-exploded-to-regexp string)))

(defun jg-quicknav--char-exploded-to-regexp (char)
  (concat (string char) ".*"))

;; ---------------------------------------------------------------------------------------
;; /Library
;; ---------------------------------------------------------------------------------------

(provide 'jg-quicknav)

;;; jg-quicknav.el ends here
