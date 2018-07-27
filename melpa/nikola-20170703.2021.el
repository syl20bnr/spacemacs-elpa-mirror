;;; nikola.el --- Simple wrapper for nikola -*- lexical-binding: t -*-

;; Copyright:: Copyright (c) 2017, drymer

;; Author:: drymer <drymer [ AT ] autistici.org>
;; Package-Requires: ((async "1.5") (emacs "24.3"))
;; Package-Version: 20170703.2021
;; Version: 0.1

;; Keywords:: nikola
;; X-URL:: https://git.daemons.it/drymer/nikola.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or (at
;; your option) any later version.
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; If you find a bug, you may send an e-mail or open an issue at
;; https://git.daemons.it/drymer/nikola.el/
;; ◊ Main commands:
;;   • `nikola-new-post': Creates a new post and opens it.
;;   • `nikola-new-page': Creates a new page and opens it.
;;   • `nikola-build': Builds the site.
;;   • `nikola-start-webserver': Starts nikola's webserver.
;;   • `nikola-stop-webserver': Stops nikola's webserver.
;;   • `nikola-deploy': Deploys the site.
;;   • `nikola-version': Shows nikola and nikola.el version.
;; ◊ Variables:
;;   • `nikola-output-root-directory': Nikola's site directory.
;;   • `nikola-verbose': If set to *t*, it will create a buffer called
;;     **Nikola** with the output of all commands.  Set to *nil* by default.
;;   • `nikola-webserver-auto': If set to *t*, it will use `nikola auto' to
;;     launch the webserver.  If set to *nil*, it will use `nikola
;;     serve'.  Set to *nil* by default.
;;   • `nikola-webserver-host': Set it to *0.0.0.0* if you want to make the
;;     webserver accesible from outside the machine.  Set to *127.0.0.1* by
;;     default.
;;   • `nikola-webserver-port': Nikola's webserver port.  Set to *8000* by
;;     default.
;;   • `nikola-deploy-input': If *nil*, just execute plain deploy, if *t*,
;;     asks for user input, *any string* is passed to the deploy string
;;     automatically.
;;     This variable is intended to use with a deploy script or command that
;;     uses git, thus needs a commit message.  It could be used for whatever
;;     other reason, also.  To use the message writed on Emacs on the deploy
;;     order, you have to use the variable **$COMMIT**.  For example, your
;;     deploy command could be:
;;   ┌────
;;   │ DEPLOY_COMMANDS = {
;;   │      'default': [
;;   │          "git add .", "git commit -m \"$COMMIT\"", "git push"
;;   │      ]
;;   │  }
;;   └────
;;   Set to *nil* by default.
;;   • `nikola-new-post-extension': The extension of new posts. If it's a
;;     list, ido completion will be offered. Set to *html* by default.
;;   • `nikola-new-page-extension': The extension of new pages. If it's a
;;     list, ido completion will be offered. Set to *html* by default.
;;   • `nikola-deploy-input-default': If `nikola-deploy-input' is *t*, this
;;     variable changes the default value so you can just press RET. Set to
;;     *New post* by default.
;; ◊ Hooks:
;;   Use them as you would usually do.
;;   • `nikola-build-before-hook'
;;   • `nikola-build-after-hook'
;;   • `nikola-deploy-before-hook'
;;   • `nikola-deploy-after-hook'
;;   If you only want to execute a simple script or command before or after
;;   building or deploying, you can set the next variables to that script's
;;   path or command:
;;   • `nikola-build-before-hook-script'
;;   • `nikola-build-after-hook-script'
;;   • `nikola-deploy-before-hook-script'
;;   • `nikola-deploy-after-hook-script'
;;   For example, to execute a script before deploying:
;;   ┌────
;;   │ (setq nikola-deploy-before-hook-script "~/scripts/pre-deploy.sh")
;;   └────
;;   For more complicated things, you should use create a function and add
;;   is a hook.

;;; Code:
(require 'async)
(require 'cl-lib)

(defgroup nikola nil
  "Wrappers around nikola command."
  :group 'tools)

(defcustom nikola-command "nikola"
  "The nikola command (no shit, Sherlock).  It shouldn't be necessary to chang\
e it if it's on the PATH."
  :type 'variable
  :group 'nikola)

(defcustom nikola-output-root-directory nil
  "Nikola's default directory."
  :type 'variable
  :group 'nikola)

(defcustom nikola-verbose nil
  "If set to t, it will create a buffer called *Nikola* with the output of a\
ll commands."
  :type 'variable
  :group 'nikola)

(defcustom nikola-webserver-auto nil
  "If set to t, it will use nikola auto to launch the webserver.  If set to ni\
l, it will use nikola serve."
  :type 'variable
  :group 'nikola)

(defcustom nikola-webserver-host "127.0.0.1"
  "Set it to 0.0.0.0 if you want to make the webserver accesible from outside \
the machine."
  :type 'variable
  :group 'nikola)

(defcustom nikola-webserver-port "8000"
  "Nikola's webserver port."
  :type 'variable
  :group 'nikola)

(defcustom nikola-webserver-open-browser-p nil
  "If set to t, opens xdg defined browser when executing `nikola-webserver-sta\
rt`'."
  :type 'variable
  :group 'nikola)

(defcustom nikola-deploy-input nil
  "If nil, just execute plain deploy, if t, asks for user input, any string is\
passed to the deploy string automatically."
  :type 'variable
  :group 'nikola)

(defcustom nikola-deploy-input-default "New post"
  "If `nikola-deploy-input' is t, this 'variable changes the default value so y\
can just press RET."
  :type 'variable
  :group 'nikola)

(defcustom nikola-new-post-extension "html"
  "The extension of new posts.  If it's a list, ido completion will be offered\
."
  :type 'variable
  :group 'nikola)

(defcustom nikola-new-page-extension "html"
  "The extension of new pages.  If it's a list, ido completion will be offered\
."
  :type 'variable
  :group 'nikola)

(defcustom nikola-build-before-hook-script nil
  "Path of a script to execute before building the site."
  :type 'hook
  :group 'nikola)

(defcustom nikola-build-after-hook-script nil
  "Path of the script to execute after building the site."
  :type 'hook
  :group 'nikola)

(defcustom nikola-deploy-before-hook-script nil
  "Path of the script to execute before deploying the site."
  :type 'hook
  :group 'nikola)

(defcustom nikola-deploy-after-hook-script nil
  "Path of the script to execute after deploying the site."
  :type 'hook
  :group 'nikola)

(defcustom nikola-build-before-hook nil
  "Hook executed before `nikola-build'."
  :type 'hook
  :group 'nikola)

(defcustom nikola-build-after-hook nil
  "Hook executed after `nikola-build'."
  :type 'hook
  :group 'nikola)

(defcustom nikola-deploy-before-hook nil
  "Hook executed before `nikola-deploy'."
  :type 'hook
  :group 'nikola)

(defcustom nikola-deploy-after-hook nil
  "Hook executed after `nikola-deploy'."
  :type 'hook
  :group 'nikola)

(defvar nikola-version-v nil
  "Nikola version.  Do not modify manually.")

(defun nikola-sentinel (process event)
  "React to nikola's PROCESS and EVENTs."
  (cond
   ;; It enters when nikola-stop-webserver
   ((and
     (string-match-p "(hangup)" event)
     (string-match-p "nikola-webserver" (format "%s" process)))
    (message "Webserver stopped."))))

(defun nikola-clean-slug (title)
  "Clean the TITLE to make the slug."
  (let* ((slug (downcase title))
	 (slug (replace-regexp-in-string "\\(á\\|à\\|â\\|ä\\)" "a" slug))
	 (slug (replace-regexp-in-string "\\(é\\|è\\|ê\\|ë\\)" "e" slug))
	 (slug (replace-regexp-in-string "\\(í\\|ì\\|î\\|ï\\)" "i" slug))
	 (slug (replace-regexp-in-string "\\(ó\\|ò\\|ô\\|ö\\)" "o" slug))
	 (slug (replace-regexp-in-string "\\(ú\\|ù\\|û\\|ü\\)" "u" slug))
	 (slug (replace-regexp-in-string "\\(,\\|\\.\\|\'\\|\"\\)" "" slug))
	 (slug (replace-regexp-in-string "\\(\\?\\|\\¿\\|\\!\\|\\¡\\)" ""
					 slug))
	 (slug (replace-regexp-in-string
		"\\(+\\|\\^\\|@\\|\\[\\|\\]\\|\{\\|\}\\|\\\\\\)" "" slug))
	 (slug (replace-regexp-in-string "\\( \\|_\\)" "-" slug))
	 (slug (replace-regexp-in-string "\\(--\\)" "-" slug))
	 (slug (replace-regexp-in-string "\\(--\\)" "-" slug)))
    slug))

;;;###autoload
(defun nikola-init ()
  "Create a default site and opens the file conf.py to edit it."
  (interactive)
  (message "Creating the site...")
  (if (file-exists-p (concat nikola-output-root-directory "conf.py"))
      (user-error "There's a site on that directory.  Delete it or change the \
nikola-output-root-directory variable.  "))
  (async-start
   `(lambda()
      ,(async-inject-variables "\\(nikola-\\)")
      (let ((output (shell-command-to-string
		    (concat "nikola init -q " nikola-output-root-directory))))
	output))
   (lambda (result)
     (find-file (concat nikola-output-root-directory "conf.py"))
     (if (cl-search "This command needs to run inside an existing Nikola site."
		 result)
	 (if (eq nikola-verbose t)
	     (message "Something went wrong. You may want to set nikola-verbos\
e to t and retry it.")
	   (message "Something went wrong. You may want to check the *Nikola* \
buffer."))
       (message "Site created correctly. Let's edit the configuration file!"))
     (if (eq nikola-verbose t)
	 (save-window-excursion
	   (switch-to-buffer "*Nikola*")
	   (let ((inhibit-read-only t))
	     (insert result)))))))

;;;###autoload
(defun nikola-new-post()
  "Creates a new post on nikola-output-root-directory/posts/ and opens it."
  (interactive)
  (let* ((title (read-string "Insert the title of the new post: "))
	(slug (nikola-clean-slug title))
	(extension (if (listp nikola-new-post-extension)
		       (concat "." (ido-completing-read "Which extension you w\
ant to use? " nikola-new-post-extension))
		     (concat "." nikola-new-post-extension))))
    (if (file-exists-p (concat nikola-output-root-directory "posts/" slug
			       extension))
	(if (not (y-or-n-p "This post exists.  You want to overwrite it? "))
	    (user-error "Exit.? ")))
    (with-temp-buffer
      (insert (concat ".. title: " title "\n"))
      (insert (concat ".. slug: " slug "\n"))
      (insert (concat ".. date: " (format-time-string "%Y-%m-%d %H:%M:%S")
		      "\n"))
      (insert ".. tags: \n")
      (write-file (concat nikola-output-root-directory "posts/" slug ".meta")))
    (with-temp-buffer
      (insert "Write your publication here.")
      (write-file (concat nikola-output-root-directory "posts/" slug
			  extension)))
    (find-file (concat nikola-output-root-directory "posts/" slug extension))
    (message "Write your publication.")))

;;;###autoload
(defun nikola-new-page()
  "Creates a new page on nikola-output-root-directory/stories/."
  (interactive)
  (let* ((title (read-string "Insert the title of the new page: "))
	 (slug (nikola-clean-slug title))
	 (extension
	  (if (listp nikola-new-post-extension)
	      (concat "." (ido-completing-read "Which extension you want to us\
e? " nikola-new-post-extension))
	   (concat "." nikola-new-post-extension))))
    (if (file-exists-p (concat nikola-output-root-directory "stories/" slug
			       extension))
	(if (not (y-or-n-p "This post exists.  You want to overwrite it? "))
	    (user-error "Exit")))
    (with-temp-buffer
      (insert (concat ".. title: " title "\n"))
      (insert (concat ".. slug: " slug "\n"))
      (insert (concat ".. date: " (format-time-string "%Y-%m-%d %H:%M:%S")
		      "\n"))
      (insert ".. tags: \n")
      (write-file (concat nikola-output-root-directory "stories/" slug
			  ".meta")))
    (with-temp-buffer
      (insert "Write your publication here.")
      (write-file (concat nikola-output-root-directory "stories/" slug
			  extension)))
    (find-file (concat nikola-output-root-directory "stories/" slug extension))
    (message "Write your publication.")))

;;;###autoload
(defun nikola-build ()
  "Build the site."
  (interactive)
  (message "Building the site...")
  (async-start
   `(lambda()
      ,(async-inject-variables "\\(nikola-\\)")
      (let ((default-directory nikola-output-root-directory)
	    (output))
	(run-hook-with-args 'nikola-build-before-hook "")
	(if (not (eq nikola-build-before-hook-script nil))
	    (setq output (shell-command-to-string
			  nikola-build-before-hook-script)))
	(setq output (concat output (shell-command-to-string
				     (concat nikola-command " build"))))
	(if (not (eq nikola-build-after-hook-script nil))
	    (setq output (concat output (shell-command-to-string
					 nikola-build-after-hook-script))))
	(run-hook-with-args 'nikola-build-after-hook "")
	output))
   (lambda (result)
     (if (cl-search "This command needs to run inside an existing Nikola site."
		 result)
	 (if (eq nikola-verbose t)
	     (message "Something went wrong. You may want to set nikola-verbos\
e to t and retry it.")
	   (message "Something went wrong. You may want to check the *Nikola* \
buffer."))
       (message "Site build correctly."))
     (if (eq nikola-verbose t)
	 (save-window-excursion
	   (switch-to-buffer "*Nikola*")
	   (let ((inhibit-read-only t))
	     (insert result)))))))

;;;###autoload
(defun nikola-webserver-start ()
  "Start webserver."
  (interactive)
  (let ((webserver)
	(browser)
	(buffer-name-p))
    (if (eq nikola-webserver-auto t)
	(setq webserver "auto")
      (setq webserver "serve"))
    (if (eq nikola-webserver-open-browser-p t)
	(setq browser " -b")
      (setq browser ""))
    (if (get-process "nikola-webserver")
	(if (y-or-n-p "There's a nikola-start-webserver process active.  Do yo\
u want to restart it? ")
	    (progn (nikola-webserver-stop)(sleep-for 1))
	  (user-error "Exit")))
    (message (concat "Serving Webserver on " nikola-webserver-host ":"
		     nikola-webserver-port "... " "(it can take a while):"))
    (if (eq nikola-verbose t)
	(setq buffer-name-p "*Nikola*")
      (setq buffer-name-p nil))
    (let ((default-directory nikola-output-root-directory))
      (set-process-sentinel
       (start-process-shell-command
	"nikola-webserver" buffer-name-p (concat "NIKOLA_MONO=1 "
						 nikola-command " " webserver
						 " -a " nikola-webserver-host
						 " -p " nikola-webserver-port
						 browser))
       'nikola-sentinel))))

;;;###autoload
(defun nikola-webserver-stop ()
  "Stops the webserver."
  (interactive)
  (if (get-process "nikola-webserver")
      (signal-process "nikola-webserver" 1)
    (message "There's no Webserver running.")))

;;;###autoload
(defun nikola-deploy ()
  "Deploys the site."
  (interactive)
  (let ((nikola-commit)
	(output))
    (if (eq nikola-deploy-input t)
	(progn
	  (setq nikola-commit (read-string
			       (concat "Enter the commit message (Default: "
				       nikola-deploy-input-default "): ")))
	  (if (string="" nikola-commit)
	      (setq nikola-commit nikola-deploy-input-default)))
      (if (eq nikola-deploy-input nil)
	  (setq nikola-commit nil)
	(setq nikola-commit nikola-deploy-input)))
    (message "Deploying the site...")
    (async-start
     `(lambda ()
	,(async-inject-variables "\\(nikola-\\)")
	(setq output nil)
	(let ((default-directory nikola-output-root-directory))
	  (run-hook-with-args 'nikola-deploy-before-hook "")
	  (if (not (eq nikola-deploy-before-hook-script nil))
	      (setq output (shell-command-to-string
			    nikola-deploy-before-hook-script)))
	  (setq output (shell-command-to-string (concat "COMMIT=\""
							nikola-commit "\" "
							nikola-command
							" deploy ")))
	  (if (not (eq nikola-deploy-after-hook-script nil))
	      (setq output (shell-command-to-string
			    nikola-deploy-after-hook-script)))
	  (run-hook-with-args 'nikola-deploy-before-hook ""))
	output)
     (lambda (result)
       (if (cl-search "This command needs to run inside an existing Nikola site."
		   result)
	   (if (eq nikola-verbose t)
	       (message "Something went wrong. You may want to set nikola-verbo\
se to t and retry it.")
	     (message "Something went wrong. You may want to check the *Nikola*\
buffer."))
	 (message "Site deployed correctly."))
       (if (eq nikola-verbose t)
	   (save-window-excursion
	     (switch-to-buffer "*Nikola*")
	     (let ((inhibit-read-only t))
	       (insert result))))))))

;;;###autoload
(defun nikola-version ()
  "Show nikola and nikola.el version."
  (interactive)
  (let ((output))
    (if (eq nikola-version-v nil)
	(async-start
	 `(lambda()
	    (setq output (shell-command-to-string "nikola version"))
	    (setq output (concat output "Nikola.el v0.1"))
	    output)
	 (lambda (result)
	   (setq nikola-version-v result)))
      (message nikola-version-v))))

;; Since the shell command `nikola version` is so slow, get its version async
;; when loading this mode
(nikola-version)

(provide 'nikola)

;;; nikola.el ends here
