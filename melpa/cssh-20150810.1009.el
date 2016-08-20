;;; cssh.el --- clusterssh implementation for emacs
;;
;; Copyright (C) 2008 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://tapoueh.org/emacs/cssh.html
;; Package-Version: 20150810.1009
;; Version: 0.7
;; Created: 2008-09-26
;; Keywords: ClusterSSH ssh cssh
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Emacs Integration:
;; (require 'cssh)
;;
;; cssh bindings to open ClusterSSH controler in cssh-mode on some buffers:
;;
;;  C-=       asks remote hostname then opens a term and ssh to it
;;  C-=       from IBuffer mode opens ClusterSSH controler on marked buffers
;;  C-u C-=   asks for the name of the ClusterSSH controler buffer
;;  C-M-=     matches given regexp against ssh known_hosts and open
;;            buffers in which ssh <remote> is typed
;;  C-u C-M-= asks for a name before
;;
;; Special keys while in the *cssh* controller you might want to know about:
;;
;;  C-=   redraw buffer selection in windows
;;  C-!   reconnect the ssh, for when your ssh buffers outlive the ssh inside
;;
;; Dired integration:
;;
;;  C-=   in dired uses a `dsh' style configuration file to get the remote
;;        hosts list
;;
;; TODO
;;
;;  * add some more documentation
;;
;; WON'T FIX
;;
;; * the line and char modes as in term.el are not a good idea here, as it
;;   seems much better for the *cssh* controller buffer to behave as much as
;;   possible like a plain emacs buffer.
;;
;; CHANGES
;;
;; 0.9
;;  Add support for remote directory tracking with support for bash, thanks
;;  to Sébastien Gross
;;
;;  Base cssh-controler based comint mode, using a dummy special shell that
;;  about does while read line; echo $line; done, but with a prompt.
;;
;; 0.8
;;  Add `dsh' support from the main C-= function, using @group notation
;;
;; 0.7
;;  Add `dsh' configuration file support from dired
;;
;; 0.6
;;  Use tramp to complete the host names
;;

(require 'dired)
(require 'ibuffer)
(require 'term)
(require 'tramp)
(require 'cl)
(require 'shell)

(defgroup cssh nil "ClusterSSH mode customization group"
  :group 'convenience)

(defcustom split-horizontally-first t
  "Do we first split horizontally or vertically"
  :group 'cssh
  :options '(t nil))

(defcustom cssh-prompt "cssh> "
  "cssh buffer prompt"
  :group 'cssh)

(defcustom cssh-shell "/bin/bash"
  "cssh shell to use"
  :group 'cssh)

(defcustom cssh-term-type "screen"
  "cssh TERM environment variable to export at connection time"
  :group 'cssh)

(defcustom cssh-default-buffer-name "*cssh*"
  "cssh default buffer name, the one in cssh major mode"
  :group 'cssh)

(defcustom cssh-after-command "exit"
  "command to run when exiting from the remote (ssh) shell"
  :group 'cssh)

(defcustom cssh-dsh-path '("~/.dsh/group" "/etc/dsh/group")
  "Where to look for `dsh' configuration files (cssh groups)"
  :group 'cssh)

(defcustom cssh-remote-directory-track
  '((bash .
	  (lambda (remote)
	    (concat
	     ;; enable remote directory tracking
	     "function prompt_cmd { "
	     "echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)};"
	     "echo -e \"\\033AnSiTc\" $(pwd);"
	     (format "echo -e \"\\033AnSiTh\" ${TRAMP_HOSTNAME-%s}; };" remote)
	     "export PROMPT_COMMAND=prompt_cmd;"
	     ;; don't store these lines into shell history
	     "history -d $((HISTCMD - 1));"))))
  "ALIST defining how to track remote directory from the shell buffer.

It associates to shell symbols like 'bash a `lambda' taking a
single parameter, REMOTE.  This lambda must return a single
command line that will be sent to the remote host at the terminal
creation."
  :group 'cssh)


;;;###autoload
(defun cssh-turn-on-ibuffer-binding ()
  (local-set-key (kbd "C-=") 'cssh-ibuffer-start))

(defun cssh-define-global-bindings ()
  (interactive)
  (add-hook 'ibuffer-mode-hook 'cssh-turn-on-ibuffer-binding)
  (global-set-key (kbd "C-=") 'cssh-term-remote-open)
  (global-set-key (kbd "C-M-=") 'cssh-regexp-host-start)
  (define-key dired-mode-map (kbd "C-=") 'cssh-dired-find-file))

;; hostname completion, on C-=
(defun cssh-tramp-hosts ()
  "ask tramp for a list of hosts that we can reach through ssh"
  (reduce 'append (mapcar (lambda (x)
			    (remove* nil (mapcar 'cadr (apply (car x) (cdr x)))))
			  (tramp-get-completion-function "ssh"))))

(defun cssh-dsh-groups (&optional dsh-path)
  "Returns a list of the defined dsh groups"
  (let ((groups))
    (mapc
     (lambda (dsh-path)
       (when (file-directory-p dsh-path)
	 (let ((default-directory dsh-path))
	   (dolist (g (directory-files dsh-path))
	     (unless (file-directory-p g)
	       (add-to-list 'groups (concat "@" g)))))))
     (if dsh-path (list dsh-path) cssh-dsh-path))
    groups))

(defun cssh-get-hosts-list ()
  "Returns a list of both tramp known hosts and `dsh' groups from
the cssh-dsh-path"
  `(,@(cssh-tramp-hosts) ,@(cssh-dsh-groups)))

;;
;; support function for opening the remote terminal
(defun cssh-term-create (remote-host &optional dont-set-buffer)
  "Create a terminal and type in ssh remotehost with given hostname.

Return the buffer name where to find the terminal."
  (let* ((ssh-command (concat "ssh " remote-host))
	 (ssh-buffer-name (concat "*" ssh-command "*"))
	 (cssh-remote-open-command
	  (concat
	   (format "TERM=%s %s -t %s ;"
		   cssh-term-type
		   ssh-command
		   (file-name-nondirectory cssh-shell))
	   cssh-after-command))
	 (cssh-dir-track
	  (cdr (assoc (intern (file-name-nondirectory cssh-shell))
		      cssh-remote-directory-track)))
	 (ssh-buffer (get-buffer ssh-buffer-name))
	 (proc (when ssh-buffer (get-buffer-process ssh-buffer)))
	 (proc-status (when proc (process-status proc))))

    ;; if the buffer already exists but its proc has been killed, kill also
    ;; the buffer and start afresh
    (when (and ssh-buffer (or (not proc) (eq proc-status 'exit)))
      (kill-buffer ssh-buffer)
      (setq ssh-buffer nil))

    (if ssh-buffer
        (unless dont-set-buffer (switch-to-buffer ssh-buffer-name))

      (ansi-term cssh-shell ssh-command)
      (unless dont-set-buffer (set-buffer (get-buffer ssh-buffer-name)))
      (with-current-buffer ssh-buffer-name
	(insert cssh-remote-open-command)
	(term-send-input)
	;; enable directory tracking
	(when cssh-dir-track (insert (funcall cssh-dir-track remote-host)))
	(term-send-input)))
    ;; return the newly created buffer name
    ssh-buffer-name))

;;
;; This could be seen as recursion init step, opening a single remote host
;; shell
;;
;;;###autoload
(defun cssh-term-remote-open ()
  "Prompt for a remote host to connect to, and open a term there."
  (interactive)
  (let ((remote-host (completing-read "Remote host: " (cssh-get-hosts-list))))
    (if (string-match "^@" remote-host)
	(cssh-open-dsh-group remote-host)
    (cssh-term-create remote-host))))

;;;
;;; open cssh windows and create buffers from a regexp
;;; the regexp matches host names as in cssh-tramp-hosts
;;;
;;;###autoload
(defun cssh-regexp-host-start (&optional cssh-buffer-name)
  "start ClusterSSH for all mathing hosts in  known_hosts"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "ClusterSSH buffer name: "
		      (generate-new-buffer-name cssh-default-buffer-name)))))
  (setq cssh-buffer-name
	(or cssh-buffer-name cssh-default-buffer-name))

  (let* ((re (read-from-minibuffer "Host regexp: "))
	 (buffer-list))

    (dolist (elt (cssh-tramp-hosts))
      (when (string-match re elt)
	(add-to-list 'buffer-list (get-buffer (cssh-term-create elt t)))))

    (message "%S" buffer-list)

    (cond ((endp buffer-list)
	   (message "No match to %S" re))

	  ((eq 1 (length buffer-list))
	   (set-buffer (car buffer-list))
	   (term-send-input))

	  (t
	   (cssh-open cssh-buffer-name buffer-list)
	   (with-current-buffer cssh-buffer-name
	     (cssh-send-string ""))))))

;;;
;;; ibuffer interaction: open cssh mode for marked buffers
;;;
(defun cssh-ibuffer-start (&optional cssh-buffer-name)
  "start ClusterSSH from current iBuffer marked buffers list"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "ClusterSSH buffer name: "
		      (generate-new-buffer-name cssh-default-buffer-name)))))
  (setq cssh-buffer-name
	(or cssh-buffer-name cssh-default-buffer-name))

  (cssh-init-from-ibuffer-marked-buffers cssh-buffer-name))

(defun cssh-init-from-ibuffer-marked-buffers (cssh-buffer-name)
  "open cssh global input frame and the buffers windows from
marked ibuffers buffers"
  (let* ((buffers-all-in-term-mode t)
	 (marked-buffers (ibuffer-get-marked-buffers)))

    (dolist (elt marked-buffers)
      (progn
	(message (buffer-name elt))
	;;(select-window (get-buffer-window elt))
	(with-current-buffer elt
	  (when (not (eq major-mode 'term-mode))
	    (progn
	      (setq buffers-all-in-term-mode nil)
	      (message "ClusterSSH only supports Term mode buffers"))))))

    (when buffers-all-in-term-mode
      (cssh-open cssh-buffer-name marked-buffers))))

;;;
;;; Dired integration for opening cssh from a `dsh' config file
;;;
(defun cssh-parse-dsh-config-file (filename)
  "Given a filename, parse it as a dsh filename, return the
remote hosts list"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (let ((l     (split-string (buffer-string)))
	  (hosts))
      (dolist (elt l)
	(when (string-match (rx bol (opt "@") (one-or-more (char alnum ".-@\\")) eol) elt)
	  (if (string-match "^@" elt)
	      (mapc (lambda (x) (add-to-list 'hosts x))
		    (cssh-parse-dsh-config-file
		     (concat (file-name-directory filename) (substring elt 1))))
	    (add-to-list 'hosts elt))))
      ;; we return hosts
      hosts)))

(defun cssh-open-dsh-config-file (filename)
  "Given a filename, will parse it as a dsh filename and open
cssh on the hosts"
  (let ((hosts (cssh-parse-dsh-config-file filename))
	(buffer-list))
    (mapc (lambda (x)
	    (add-to-list 'buffer-list
			 (get-buffer (cssh-term-create x t))))
	  hosts)

    (if (endp buffer-list)
	(message "file %S is not a dsh config file, or empty" filename)

      (cssh-open cssh-default-buffer-name buffer-list)
      (with-current-buffer cssh-default-buffer-name
	(cssh-send-string "")))))

(defun cssh-open-dsh-group (group)
  "Given a `dsh' group name, find the file defining it and open cssh"
  (let ((name (substring group 1)))
    (cssh-open-dsh-config-file
     (loop with filename = nil
	   do (message filename)
	   until (and filename (file-exists-p filename))
	   for p in cssh-dsh-path
	   for filename = (concat (file-name-as-directory p) name)
	   finally return filename))))

;;;###autoload
(defun cssh-dired-find-file ()
  "In dired, have cssh connect to hosts in the `dsh' configuration file."
  (interactive)
  ;; dired-get-filename is defined in dired.el
  (cssh-open-dsh-config-file (dired-get-filename)))

;;;
;;; Entry point
;;;
(defun cssh-open (cssh-buffer-name buffer-list)
  "open the cssh global input frame then the ssh buffer windows"

  (cond ((endp buffer-list)
	 (cssh-term-remote-open))

	((eq 1 (length buffer-list))
	 (set-window-buffer (selected-window) (car buffer-list)))

	(t
	 (set-window-buffer
	  (selected-window) (get-buffer-create cssh-buffer-name))

	 ;; make the controler buffer then split the window
	 (let* ((cssh-controler (split-window-vertically -4)))
	   ;; switch to css-mode, which make-local-variable cssh-buffer-list
	   ;; which we overwrite
	   (set-buffer cssh-buffer-name)
	   (cssh-mode)
	   (setq cssh-buffer-list buffer-list)

	   ;; create the windows needed to host our buffer-list
	   (cssh-nsplit-window buffer-list)

	   ;; now place the user into the cssh-controler and prompt him
	   (select-window cssh-controler)
	   ;; (insert (concat "\n" cssh-prompt))

	   ;; return the buffer list
	   cssh-buffer-list))))

;;;
;;; cssh editing mode
;;;
(defun cssh-bol ()
  "plain C-a is annoying, better target end of prompt"
  (interactive)
  (beginning-of-line) (search-forward cssh-prompt))

(defvar cssh-buffer-list '()
  "cssh controller buffer (*cssh*) local buffer list")

;; we reuse shell-mode and its map
(defvar cssh-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map [tab]          'cssh-send-tab)
    (define-key map (kbd "C-l")    'cssh-clear)
    (define-key map (kbd "C-d")    'cssh-eof)
    (define-key map (kbd "C-=")    'cssh-reopen)
    (define-key map (kbd "C-!")    'cssh-reconnect-ssh)
    map)
  "Keymap for `cssh-mode'.")

;;;###autoload
(define-derived-mode cssh-mode comint-mode "ClusterSSH"
  "A major mode for controlling multiple terms at once."
  :group 'cssh
  (make-local-variable 'cssh-buffer-list)
  (make-local-variable 'comint-prompt-regexp)
  ;; start a process with this shell and embed it into a comint buffer
  (unless (comint-check-proc (current-buffer))
    (let ((name "cssh")
	  (program cssh-shell)
	  (startfile "/tmp/cssh.sh"))
      (with-temp-file startfile
	(insert
	 (format (concat "echo -n \"%s\"; "
			 "while read line; do "
			 "  echo $line;"
			 "  echo -n \"%s\";"
			 "done")
		 cssh-prompt cssh-prompt)))
      (setenv "PS1" cssh-prompt)
      (make-comint-in-buffer name (current-buffer) program startfile "-i")
      (insert "\n")
      (comint-send-input)
      (setq comint-input-sender
      	    (lambda (proc string)
      	      (message "cssh comint-input-sender %S %S" proc string)
	      (comint-simple-send proc string)
      	      (cssh-send-string string))))))

;;
;; Input functions
;;
(defun cssh-send-string (string)
  "generic function to send input to the terms"
  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (with-current-buffer elt
	(ignore-errors
	  (insert string)
	  (term-send-input))))))

(defun cssh-send-defun (term-fun)
  "generic function to apply term function to the terms"
  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (with-current-buffer elt
	(funcall term-fun)))))

(defun cssh-send-tab ()
  (interactive)
  (cssh-send-string
   (buffer-substring (+ (length cssh-prompt) (line-beginning-position))
		     (line-end-position)))
  (cssh-send-string "\C-i"))

(defun cssh-cancel-input ()
  (interactive)
  (cssh-send-string "\C-c")
  (comint-send-input))

(defun cssh-send-input ()
  "send current line content to all cssh-mode buffers"
  (interactive)
  ;; only consider when on last line
  (when (= 1 (forward-line 1))
    (save-excursion
      (let* ((input-beginning-position (+ (length cssh-prompt)
					  (search-backward cssh-prompt)))
	     (input (buffer-substring
		     input-beginning-position (point-max))))

	(cssh-send-string input)
	(save-excursion
	  (ring-insert cssh-input-ring input)
	  (setq cssh-input-ring-index 0))))
    (cssh-newline-and-prompt)))

(defun cssh-clear ()
  (interactive)
  (cssh-send-string "\C-l"))

(defun cssh-eof ()
  (interactive)
  (cssh-send-string "\C-d"))

(defun cssh-reopen ()
  (interactive)
  (cssh-open (buffer-name) cssh-buffer-list))

(defun cssh-reconnect-ssh (&optional clear)
  (interactive "P")
  (when (consp clear) (cssh-clear))

  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (let* ((elt-name (buffer-name elt))
	     (buffer-ssh-command (substring elt-name 1 -1)))
	(with-current-buffer elt
	  (insert (concat "TERM=" cssh-term-type " " buffer-ssh-command))
	  (term-send-input))))))

;;;
;;; Window splitting code
;;;
(defun cssh-split-window (&optional backward? &optional size)
  "split current window either vertically or horizontally
depending on split-preference value"
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (if size
	(if go-horizontal
	    (split-window-horizontally size)
	  (split-window-vertically size))

      (if go-horizontal
	  (split-window-horizontally)
	(split-window-vertically)))))

(defun cssh-get-third-size (backward? left top right bottom)
  "Given a window edges and a direction"
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (/ (+ 1 (if go-horizontal (- right left) (- bottom top))) 3)))

(defun cssh-nsplit-window (buffer-list &optional backward?)
  "split current window into n windows"
  (let* ((w (selected-window))
	 (n (length buffer-list)))

    (cond ((= n 2)
	   ;; if at least one of the list elements is a buffer, it's final
	   ;; recursion and we always prefer to maximize line length
	   (let* ((w1 (cssh-split-window (if (or (bufferp (car buffer-list))
						 (bufferp (cadr buffer-list)))
					     ;; force to split horizontally
					     split-horizontally-first
					   backward?))))

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

  	     (list w w1)))

	  ((= n 3)
	   ;; if at least one of the list elements is a buffer, it's final
	   ;; recursion and we always prefer to maximize line length
	   (let* ((edges (window-edges))
		  (direction (if (or (bufferp (car buffer-list))
				     (bufferp (cadr buffer-list))
				     (bufferp (cadr (cdr buffer-list))))
				 ;; force to split horizontally first
				 split-horizontally-first
			       backward?))
		  (size      (apply #'cssh-get-third-size
				    (cons direction edges)))
		  (w1        (cssh-split-window direction size))
		  (w2        (progn (select-window w1)
				    (cssh-split-window direction size))))

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

	     (when (bufferp (cadr (cdr buffer-list)))
	       (set-window-buffer w2 (cadr (cdr buffer-list))))

	     (list w w1 w2)))

	  ((= n 5)
	   ;; cut in half then split one half in 2 and the other in 3
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window '(1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l
		     (cssh-nsplit-window
		      (butlast buffer-list 3) (not backward?))))

	       (select-window w)
	       (append h1l
		       (cssh-nsplit-window
			(last buffer-list 3) (not backward?))))))

	  ((= 0 (% n 2))
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window '(1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l
		     (cssh-nsplit-window
		      (butlast buffer-list (/ n 2)) (not backward?))))

	       (select-window w)
	       (append h1l
		       (cssh-nsplit-window
			(last buffer-list (/ n 2)) (not backward?))))))

	  ((= 0 (% n 3))
	   ;; cut in three parts then re split
	   (let* ((thirds (cssh-nsplit-window '(1 2 3) backward?)))

	     (select-window (nth 1 thirds))

	     (let* ((t1l (cssh-nsplit-window
			  ;; take the first third of the list
			  (butlast (butlast buffer-list (/ n 3)) (/ n 3))
			  (not backward?))))

	       (select-window (nth 2 thirds))

	       (let* ((t2l (cssh-nsplit-window
			    ;; take the second third of the list
			    (last (butlast buffer-list (/ n 3)) (/ n 3))
			    (not backward?))))

		 (select-window w)
		 (append t1l
			 t2l
			 (cssh-nsplit-window
			  ;; take the last third of the list
			  (last buffer-list (/ n 3)) (not backward?)))))))

	  ;; n is not divisible by either 2 or 3, produce some more windows
	  ;; than necessary
	  ((= 0 (% (+ 1 n) 2))
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  ((= 0 (% (+ 1 n) 3))
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  (t (message "error: number of windows not a multiple of 2 or 3.")))))

(provide 'cssh)
;;; cssh.el ends here
