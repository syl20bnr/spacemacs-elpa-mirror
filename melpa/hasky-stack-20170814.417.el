;;; hasky-stack.el --- Interface to the Stack Haskell development tool -*- lexical-binding: t; -*-
;;
;; Copyright © 2017 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/hasky-mode/hasky-stack
;; Package-Version: 20170814.417
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (f "0.18.0") (magit-popup "2.10"))
;; Keywords: tools, haskell
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Interface to the Stack Haskell development tool.  Bind just
;; two commands, like this:
;;
;;     (global-set-key (kbd "<next> h e") #'hasky-stack-execute)
;;     (global-set-key (kbd "<next> h i") #'hasky-stack-new)
;;
;; * `hasky-stack-execute' opens a popup with a collection of stack commands
;;   you can run.  Many commands have their own popups like in Magit.
;;
;; * `hasky-stack-new' allows to create a new project in current directory
;;   using a Stack template.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'magit-popup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & Variables

(defgroup hasky-stack nil
  "Interface to the Stack Haskell development tool."
  :group  'programming
  :tag    "Hasky Stack"
  :prefix "hasky-stack-"
  :link   '(url-link :tag "GitHub"
                     "https://github.com/hasky-mode/hasky-stack"))

(defface hasky-stack-project-name
  '((t (:inherit font-lock-function-name-face)))
  "Face used to display name of current project.")

(defface hasky-project-version
  '((t (:inherit font-lock-doc-face)))
  "Face used to display version of current project.")

(defvar hasky-stack--last-directory nil
  "Path to project's directory last time `hasky-stack--prepare' was called.

This is mainly used to check when we need to reload/re-parse
project-local settings that user might have.")

(defvar hasky-stack--cabal-mod-time nil
  "Time of last modification of \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-name nil
  "Name of current project extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-version nil
  "Version of current project extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-targets nil
  "List of build targets (strings) extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defcustom hasky-stack-executable nil
  "Path to Stack executable.

If it's not NIL, this value is used in invocation of Stack
commands instead of the standard \"stack\" string.  Set this
variable if your Stack is in a strange place where OS cannot find
it.

Note that the path is quoted with `shell-quote-argument' before
being used to compose command line."
  :tag "Path to Stack Executable"
  :type '(choice (file :must-match t)
                 (const :tag "Use Default" nil)))

(defcustom hasky-stack-read-function #'completing-read
  "Function to be called when user has to choose from list of alternatives."
  :tag  "Completing Function"
  :type '(radio (function-item completing-read)))

(defcustom hasky-stack-ghc-versions '("8.2.1" "8.0.2" "7.10.3" "7.8.4")
  "GHC versions to pick from (for commands like \"stack setup\")."
  :tag  "GHC versions"
  :type '(repeat (string :tag "Extension name")))

(defcustom hasky-stack-auto-target nil
  "Whether to automatically select the default build target."
  :tag  "Build auto-target"
  :type 'boolean)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various utilities

(defun hasky-stack--all-matches (regexp)
  "Return list of all stings matching REGEXP in current buffer."
  (let (matches)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (push (match-string-no-properties 1) matches))
    (reverse matches)))

(defun hasky-stack--parse-cabal-file (filename)
  "Parse \"*.cabal\" file with name FILENAME and set some variables.

The following variables are set:

  `hasky-stack--project-name'
  `hasky-stack--project-version'
  `hasky-stack--project-targets'

This is used by `hasky-stack--prepare'."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; project name
    (setq hasky-stack--project-name
          (car (hasky-stack--all-matches
                "^[[:blank:]]*name:[[:blank:]]+\\([[:word:]-]+\\)")))
    ;; project version
    (setq hasky-stack--project-version
          (car (hasky-stack--all-matches
                "^[[:blank:]]*version:[[:blank:]]+\\([[:digit:]\\.]+\\)")))
    ;; project targets
    (setq
     hasky-stack--project-targets
     (append
      ;; library
      (mapcar (lambda (_) (format "%s:lib" hasky-stack--project-name))
              (hasky-stack--all-matches
               "^[[:blank:]]*library[[:blank:]]*"))
      ;; executables
      (mapcar (lambda (x) (format "%s:exe:%s" hasky-stack--project-name x))
              (hasky-stack--all-matches
               "^[[:blank:]]*executable[[:blank:]]+\\([[:word:]-]+\\)"))
      ;; test suites
      (mapcar (lambda (x) (format "%s:test:%s" hasky-stack--project-name x))
              (hasky-stack--all-matches
               "^[[:blank:]]*test-suite[[:blank:]]+\\([[:word:]-]+\\)"))
      ;; benchmarks
      (mapcar (lambda (x) (format "%s:bench:%s" hasky-stack--project-name x))
              (hasky-stack--all-matches
               "^[[:blank:]]*benchmark[[:blank:]]+\\([[:word:]-]+\\)"))))))

(defun hasky-stack--find-dir-of-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.

Return absolute path to directory containing that file or NIL on
failure.  Returned path is guaranteed to have trailing slash."
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (directory-files path t regexp t))
              (f-full default-directory))))
    (when dir
      (f-slash dir))))

(defun hasky-stack--mod-time (filename)
  "Return time of last modification of file FILENAME."
  (nth 5 (file-attributes filename 'integer)))

(defun hasky-stack--executable ()
  "Return path to stack executable if it's available and NIL otherwise."
  (let ((default "stack")
        (custom  hasky-stack-executable))
    (cond ((executable-find default)     default)
          ((and custom (f-file? custom)) custom))))

(defun hasky-stack--initialized-p (dir)
  "Return non-NIL value if \"stack.yaml\" file exists in DIR."
  (f-file? (f-expand "stack.yaml" dir)))

(defun hasky-stack--templates ()
  "Return list of available Stack templates."
  (with-temp-buffer
    (shell-command
     (format "%s templates"
             (hasky-stack--executable))
     (current-buffer))
    (remove "Template"
            (hasky-stack--all-matches "^\\(\\([[:alnum:]]\\|-\\)+\\)"))))

(defun hasky-stack--completing-read (prompt &optional collection require-match)
  "Read user's input using `hasky-stack-read-function'.

PROMPT is the prompt to show and COLLECTION represents valid
choices.  If REQUIRE-MATCH is not NIL, don't let user input
something different from items in COLLECTION.

COLLECTION is allowed to be a string, in this case it's
automatically wrapped to make it one-element list.

If COLLECTION contains \"none\", and user selects it, interpret
it as NIL.  If user aborts entering of the input, return NIL.

Finally, if COLLECTION is nil, plain `read-string' is used."
  (let* ((collection
          (if (listp collection)
              collection
            (list collection)))
         (result
          (if collection
              (funcall hasky-stack-read-function
                       prompt
                       collection
                       nil
                       require-match
                       nil
                       nil
                       (car collection))
            (read-string prompt))))
    (unless (and (string= result "none")
                 (member result collection))
      result)))

(defun hasky-stack--select-target (prompt)
  "Present the user with a choice of build target using PROMPT."
  (if hasky-stack-auto-target
      hasky-stack--project-name
    (hasky-stack--completing-read
     prompt
     (cons hasky-stack--project-name
           hasky-stack--project-targets)
     t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation

(defun hasky-stack--prepare ()
  "Locate, read, and parse configuration files and set various variables.

This commands searches for first \"*.cabal\" files traversing
directories upwards beginning with `default-directory'.  When
Cabal file is found, the following variables are set:

  `hasky-stack--project-name'
  `hasky-stack--project-version'
  `hasky-stack--project-targets'

At the end, `hasky-stack--last-directory' and
`hasky-stack--cabal-mod-time' are set.  Note that this function
is smart enough to avoid re-parsing all the stuff every time.  It
can detect when we are in different project or when some files
have been changed since its last invocation.

Returned value is T on success and NIL on failure (when no
\"*.cabal\" files is found)."
  (let* ((project-directory
          (hasky-stack--find-dir-of-file "^.+\.cabal$"))
         (cabal-file
          (car (and project-directory
                    (f-glob "*.cabal" project-directory)))))
    (when cabal-file
      (if (or (not hasky-stack--last-directory)
              (not (f-same? hasky-stack--last-directory
                            project-directory)))
          (progn
            ;; We are in different directory (or it's the first
            ;; invocation). This means we should unconditionally parse
            ;; everything without checking of date of last modification.
            (hasky-stack--parse-cabal-file cabal-file)
            (setq hasky-stack--cabal-mod-time (hasky-stack--mod-time cabal-file))
            ;; Set last directory for future checks.
            (setq hasky-stack--last-directory project-directory)
            t) ;; Return T on success.
        ;; We are in an already visited directory, so we don't need to reset
        ;; `hasky-stack--last-directory' this time. We need to
        ;; reread/re-parse *.cabal file if it has been modified though.
        (when (time-less-p hasky-stack--cabal-mod-time
                           (hasky-stack--mod-time cabal-file))
          (hasky-stack--parse-cabal-file cabal-file)
          (setq hasky-stack--cabal-mod-time (hasky-stack--mod-time cabal-file)))
        t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level construction of individual commands

(defun hasky-stack--format-command (command &rest args)
  "Generate textual representation of a command.

COMMAND is the name of command and ARGS are arguments (strings).
Result is expected to be used as argument of `compile'."
  (mapconcat
   #'identity
   (append
    (list (shell-quote-argument (hasky-stack--executable))
          command)
    (mapcar #'shell-quote-argument
            (remove nil args)))
   " "))

(defun hasky-stack--exec-command (dir command &rest args)
  "Call target as if from DIR performing COMMAND with arguments ARGS.

Arguments are quoted if necessary and NIL arguments are ignored.
This uses `compile' internally."
  (let ((default-directory dir)
        (compilation-buffer-name-function
         (lambda (_major-mode)
           (format "*%s-%s*"
                   (downcase
                    (replace-regexp-in-string
                     "[[:space:]]"
                     "-"
                     hasky-stack--project-name))
                   "stack"))))
    (compile (apply #'hasky-stack--format-command command args))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popups

(magit-define-popup hasky-stack-build-popup
  "Show popup for the \"stack build\" command."
  'hasky-stack
  :switches '((?r "Dry run"           "--dry-run")
              (?t "Pedantic"          "--pedantic")
              (?f "Fast"              "--fast")
              (?s "Only snapshot"     "--only-snapshot")
              (?d "Only dependencies" "--only-dependencies")
              (?p "Profile"           "--profile")
              (?c "Coverage"          "--coverage")
              (?b "Copy bins"         "--copy-bins")
              (?l "Library profiling" "--library-profiling")
              (?e "Executable profiling" "--executable-profiling"))
  :options  '((?o "GHC options"         "--ghc-options=")
              (?b "Benchmark arguments" "--benchmark-arguments=")
              (?t "Test arguments"      "--test-arguments=")
              (?h "Haddock arguments"   "--haddock-arguments="))
  :actions  '((?b "Build"   hasky-stack-build)
              (?e "Bench"   hasky-stack-bench)
              (?t "Test"    hasky-stack-test)
              (?h "Haddock" hasky-stack-haddock))
  :default-action 'hasky-stack-build)

(defun hasky-stack-build (target &optional args)
  "Execute \"stack build\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--select-target "Build target: ")
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "build"
   target
   args))

(defun hasky-stack-bench (target &optional args)
  "Execute \"stack bench\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--select-target "Bench target: ")
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "bench"
   target
   args))

(defun hasky-stack-test (target &optional args)
  "Execute \"stack test\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--select-target "Test target: ")
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "test"
   target
   args))

(defun hasky-stack-haddock (&optional args)
  "Execute \"stack haddock\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "haddock"
   args))

(magit-define-popup hasky-stack-init-popup
  "Show popup for the \"stack init\" command."
  'hasky-stack
  :switches '((?s "Solver"         "--solver")
              (?o "Omit packages"  "--omit-packages")
              (?f "Force"          "--force")
              (?i "Ignore subdirs" "--ignore-subdirs"))
  :actions  '((?i "Init" hasky-stack-init))
  :default-action 'hasky-stack-init)

(defun hasky-stack-init (&optional args)
  "Execute \"stack init\" with ARGS."
  (interactive
   (list (hasky-stack-init-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "init"
   args))

(magit-define-popup hasky-stack-setup-popup
  "Show popup for the \"stack setup\" command."
  'hasky-stack
  :switches '((?r "Reinstall"     "--reinstall")
              (?c "Upgrade Cabal" "--upgrade-cabal"))
  :actions  '((?s "Setup" hasky-stack-setup))
  :default-action 'hasky-stack-setup)

(defun hasky-stack-setup (ghc-version &optional args)
  "Execute \"stack setup\" command to install GHC-VERSION with ARGS."
  (interactive
   (list (hasky-stack--completing-read
          "GHC version: "
          (cons "implied-by-resolver"
                hasky-stack-ghc-versions)
          t)
         (hasky-stack-setup-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "setup"
   (unless (string= ghc-version "implied-by-resolver")
     ghc-version)
   args))

(magit-define-popup hasky-stack-upgrade-popup
  "Show popup for the \"stack upgrade\" command."
  'hasky-stack
  :switches '((?s "Source only" "--source-only")
              (?b "Binary only" "--binary-only")
              (?f "Force download" "--force-download")
              (?g "Git" "--git"))
  :options  '((?p "Binary platform" "--binary-platform=")
              (?v "Binary version" "--binary-version=")
              (?r "Git repo" "--git-repo="))
  :actions  '((?g "Upgrade" hasky-stack-upgrade))
  :default-arguments '("--git-repo=https://github.com/commercialhaskell/stack")
  :default-action 'hasky-stack-upgrade)

(defun hasky-stack-upgrade (&optional args)
  "Execute \"stack upgrade\" command with ARGS."
  (interactive
   (list (hasky-stack-upgrade-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "upgrade"
   args))

(magit-define-popup hasky-stack-upload-popup
  "Show popup for the \"stack upload\" command."
  'hasky-stack
  :switches '((?i "Ignore check" "--ignore-check")
              (?n "No signature" "--no-signature")
              (?t "Test tarball" "--test-tarball"))
  :options  '((?s "Sig server" "--sig-server="))
  :actions  '((?p "Upload" hasky-stack-upload))
  :default-arguments '("--no-signature")
  :default-action 'hasky-stack-upload)

(defun hasky-stack-upload (&optional args)
  "Execute \"stack upload\" command with ARGS."
  (interactive
   (list (hasky-stack-upload-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "upload"
   "."
   args))

(magit-define-popup hasky-stack-sdist-popup
  "Show popup for the \"stack sdist\" command."
  'hasky-stack
  :switches '((?i "Ignore check" "--ignore-check")
              (?s "Sign"         "--sign")
              (?t "Test tarball" "--test-tarball"))
  :options  '((?s "Sig server"   "--sig-server="))
  :actions  '((?d "SDist" hasky-stack-sdist))
  :default-action 'hasky-stack-sdist)

(defun hasky-stack-sdist (&optional args)
  "Execute \"stack sdist\" command with ARGS."
  (interactive
   (list (hasky-stack-sdist-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "sdist"
   args))

(defun hasky-stack-exec (cmd)
  "Execute \"stack exec\" command running CMD."
  (interactive
   (list (read-string "Command to run: ")))
  (cl-destructuring-bind (app . args)
      (progn
        (string-match
         "^[[:blank:]]*\\(?1:[^[:blank:]]+\\)[[:blank:]]*\\(?2:.*\\)$"
         cmd)
        (cons (match-string 1 cmd)
              (match-string 2 cmd)))
    (hasky-stack--exec-command
     hasky-stack--last-directory
     (if (string= args "")
         (concat "exec " app)
       (concat "exec " app " -- " args)))))

(magit-define-popup hasky-stack-clean-popup
  "Show popup for the \"stack clean\" command."
  'hasky-stack
  :switches '((?f "Full"  "--full"))
  :actions  '((?c "Clean" hasky-stack-clean))
  :default-action 'hasky-stack-clean)

(defun hasky-stack-clean (&optional args)
  "Execute \"stack clean\" command with ARGS."
  (interactive
   (list (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "clean"
   hasky-stack--project-name
   args))

(magit-define-popup hasky-stack-root-popup
  "Show root popup with all the supported commands."
  'hasky-stack
  :actions  '((lambda ()
                (concat
                 (propertize hasky-stack--project-name
                             'face 'hasky-stack-project-name)
                 " "
                 (propertize hasky-stack--project-version
                             'face 'hasky-project-version)
                 "\n\n"
                 (propertize "Commands"
                             'face 'magit-popup-heading)
                 "\n"))
              (?b "Build"   hasky-stack-build-popup)
              (?i "Init"    hasky-stack-init-popup)
              (?s "Setup"   hasky-stack-setup-popup)
              (?u "Update"  hasky-stack-update)
              (?g "Upgrade" hasky-stack-upgrade-popup)
              (?p "Upload"  hasky-stack-upload-popup)
              (?d "SDist"   hasky-stack-sdist-popup)
              (?x "Exec"    hasky-stack-exec)
              (?c "Clean"   hasky-stack-clean-popup))
  :default-action 'hasky-stack-build-popup
  :max-action-columns 3)

(defun hasky-stack-update ()
  "Execute \"stack update\"."
  (interactive)
  (hasky-stack--exec-command
   hasky-stack--last-directory
   "update"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level interface

;;;###autoload
(defun hasky-stack-execute ()
  "Show the root-level popup allowing to choose and run a Stack command."
  (interactive)
  (if (hasky-stack--executable)
      (if (hasky-stack--prepare)
          (hasky-stack-root-popup)
        (message "Cannot locate ‘.cabal’ file"))
    (message "Cannot locate Stack executable on this system")))

;;;###autoload
(defun hasky-stack-new (project-name template)
  "Initialize the current directory by using a Stack template.

PROJECT-NAME is the name of project and TEMPLATE is quite
obviously template name."
  (interactive
   (list (hasky-stack--completing-read
          "Project name: "
          (file-name-nondirectory
           (directory-file-name
            default-directory)))
         (hasky-stack--completing-read
          "Use template: "
          (cons "none" (hasky-stack--templates))
          t)))
  (if (hasky-stack--prepare)
      (message "The directory is already initialized, it seems")
    (let ((hasky-stack--project-name project-name))
      (hasky-stack--exec-command
       default-directory
       "new"
       "--bare"
       project-name
       template))))

(provide 'hasky-stack)

;;; hasky-stack.el ends here
