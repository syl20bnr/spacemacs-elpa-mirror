;;; helm-grepint.el --- Generic helm interface to grep -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 19 Sep 2015
;; Keywords: grep, grepping, searching, helm
;; Package-Version: 20161001.713
;; Version: 1.2.0
;; URL: https://github.com/kopoli/helm-grepint
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ### Description

;; This package solves the following problems for me:
;; - A single function call interface to grep and therefore keybinding.
;; - Selects the grep based on context: Inside a git-repository, runs
;;   `git-grep', otherwise runs `ag'.
;; - Uses helm to select candidates and jumps to the given line with RET.

;; And the following additional problems (as of version 1.2.0):
;; - A second interactive function `helm-grepint-grep-root'.  This runs the
;;   grepping inside a root directory.  By default this has been defined for
;;   the git-grep where it greps from the git root directory.
;; - Inside a huge git repository one can create a file defined in the
;;   variable `helm-grepint-default-config-ag-presearch-marker-file' and it
;;   will set that directory as the root directory for grepping.  It uses `ag'
;;   instead of `git-grep' as the grep.
;; - The grepping is case-insensitive by default, but if an upper-case letter
;;   is given case-sensitive grepping is done.

;; The following enables the aforementioned:

;;         (require 'helm-grepint)
;;         (helm-grepint-set-default-config-latest)
;;         (global-set-key (kbd "C-c g") #'helm-grepint-grep)
;;         (global-set-key (kbd "C-c G") #'helm-grepint-grep-root)

;; The original configuration (i.e. without the above additional features) is
;; available with the following:

;;         (helm-grepint-set-default-config)

;; ### Key bindings within helm

;; - `RET'/`F1' selects an item and closes the helm session.
;; - `F2' displays the grep results in a `grep-mode' buffer.
;; - `Right-arrow' selects the item, but does not close the helm session.  This
;;   is similar as `helm-occur'.  Default helmkeybindings for this feature are
;;   also available (`C-j' and `C-z').
;; - `M-c' cycles case sensitiveness.

;; ### Customization

;; Look into the function `helm-grepint-set-default-config' to see how the default
;; cases are configured.  Also look into `helm-grepint-add-grep-config' for more
;; details on what is required for a new grep to be defined.

;; ### Changes

;; Version 1.2.0

;; - Obsoleted `helm-grepint-get-grep-config' in favor of
;;   `helm-grepint-grep-config'.
;; - Make the ignore-case a separate argument in the grep configuration.  This
;;   way it can be toggled on and off easily.
;; - Add case-fold-search support (case-(in)sensitiveness).  Add Helm
;;   keybinding `M-c' to control it.
;; - Add smart case-sensitiveness checking.
;; - Add a new configuration `helm-grepint-set-default-config-v1.2.0' which
;;   makes the smart cases-sensitiveness as the default.  The configuration is
;;   now the `helm-grepint-set-default-config-latest'.

;; Version 1.1.1

;; - Add `--ignore-case' argument for `git-grep' to make it consistent with
;;   `ag' in the `helm-grepint-set-default-config'.

;; Version 1.1.0

;; - Fix incompatibilities with recent helm versions.
;; - Add `helm-grepint-candidate-number-limit' variable to control the number
;;   of candidates instead of hard-coding 500.
;; - Create a new example configuration which adds the ag-presearch
;;   functionality.  The example configurations are now versioned:
;;   `helm-grepint-set-default-config-v1.0.0' and
;;   `helm-grepint-set-default-config-v1.1.0'.
;; - Change the `helm-grepint-set-default-config' function to an alias of
;;   `helm-grepint-set-default-config-v1.0.0'.  Add new alias
;;   `helm-grepint-set-default-config-latest' which points to
;;   `helm-grepint-set-default-config-v1.1.0'.

;; Version 1.0.0

;; - Add action to create a `grep-mode' buffer from the helm-buffer.
;; - Add universal-argument to manually ask the used grep configuration.

;; Version 0.5.5

;; - Fix swooping into multiple files within a helm session.  Previously it
;;   would change default-directory every swoop.
;; - Add action to open the helm buffer in grep-mode.  This enables the use of
;;   e.g. `wgrep'.
;; - Add `helm-grepint-grep-ask-root' and set it as default for ag.

;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'helm-grep)
(require 'thingatpt)

(defcustom helm-grepint-grep-list ()
  "List of grep commands.

These are the names in `helm-grepint-grep-configs'."
  :group 'helm-grepint)

(defcustom helm-grepint-pre-input-function
  (lambda ()
    (if (region-active-p)
	(buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'symbol)))
  "The function that supplies the pre-input for grep."
  :group 'helm-grepint)

(defcustom helm-grepint-candidate-number-limit 500
  "Number of candidates to display."
  :group 'helm-grepint)

(defconst helm-grepint-character-cases '(case-insensitive case-sensitive smart)
  "Possible character cases.
This is the order in which they are cycled with the
`helm-grepint-cycle-character-case' function.

Smart case here means that if user inputs only lower case
letters, the grepping should ignore character case.  If even a
single upper-case letter is given, character case is respected.
In Emacs nomenclature case sensitivity is called
`case-fold-search'.")

(defcustom helm-grepint-initial-case 'case-insensitive
  "Initial character case handling.
To be in effect, the `:ignore-case-argument' needs to be set in
the grep configuration."
  :type '(radio
          (const :tag "Case-insensitive" case-insensitive)
          (const :tag "Case-sensitive" case-sensitive)
          (const :tag "Smart" smart))
  :group 'helm-grepint)

(defvar helm-grepint-grep-configs ()
  "Manipulate this with `helm-grepint-add-grep-config'.")

(defvar helm-grepint-grep-jump-pre-hook '(push-mark)
  "Hook that is run before jumping to the target in `helm-grepint-grep-action-jump'.")

(defvar helm-grepint-grep-jump-post-hook nil
  "Hook that is run after jumping to the target in `helm-grepint-grep-action-jump'.")

(defmacro helm-grepint-add-grep-config (name &rest configuration)
  "Add configuration NAME with properties from CONFIGURATION.

The configuration can have the following items:

:command
 - A command string to run.

:arguments
 - Arguments provided for the command when it is run.  This
   and :command is provided for the `helm-grepint-run-command' function.

:enable-function
 - A function that returns non-nil if this grep can be used.  If
   this is nil, the grep can be used always.

:root-directory-function
 - Function that returns a string of a directory that is regarded
   as the root directory when running `helm-grepint-grep-root'.  If
   this is nil, `helm-grepint-grep-root' behaves exactly as `helm-grepint-grep'.

:ignore-case-argument
 - The argument for the grep command that makes grepping ignore
   character case.  Traditionally this is `--ignore-case' for a
   number of different greps.  This needs to be defined or the
   `helm-grepint-cycle-character-case' function has no effect."

  (declare (indent defun))
  `(helm-grepint-grep-config ',name ',configuration))

(defun helm-grepint-grep-config (name &optional new-config)
  "Get a grep configuration with NAME or set it to NEW-CONFIG."
  (if (null new-config)
      (assoc name helm-grepint-grep-configs)
    (assq-delete-all name helm-grepint-grep-configs)
    (push (cons name new-config) helm-grepint-grep-configs)))

(define-obsolete-function-alias 'helm-grepint-get-grep-config 'helm-grepint-grep-config
  "1.2.0" "Get the configuration associated with NAME.
This is superseded by the `helm-grepint-grep-config' that has
both get and set semantics.")

(defun helm-grepint-grep-config-property (name property &rest new-value)
  "Get a config NAME's PROPERTY or set it to NEW-VALUE.
The config NAME has been added with `helm-grepint-add-grep-config'.
Returns the current value of the property or nil if either name
or property was not found."
  (let ((cmd (assoc name helm-grepint-grep-configs)))
    (when cmd
      (if (null new-value)
	  (plist-get (cdr cmd) property)
	(plist-put (cdr cmd) property (car new-value))
	(car new-value)))))

(defvar helm-grepint-current-command nil
  "The current command that is being run.  It is available for actions.")

(defun helm-grepint--prepare-args(plist)
  "Prepare argument list for running the grep."
  (let ((igncasearg (plist-get plist :ignore-case-arg))
	(args (split-string (plist-get plist :arguments)))
	(searchstr (plist-get plist :extra-arguments)))

    (when igncasearg
      (with-helm-buffer
	(let ((ccase helm-grepint--character-case))
	  (when (equal ccase 'smart)
	    (setq ccase
		  (if (let ((case-fold-search nil))
			(string-match-p "[[:upper:]]" searchstr))
		      'case-sensitive
		    'case-insensitive)))
	  (setq args
		(progn
		  (delete igncasearg args)
		  (if (equal ccase 'case-insensitive)
		      (append args (list igncasearg))
		    args))))))
    (append args (list searchstr))))

(defun helm-grepint-run-command (&rest plist)
  "Run a grep command from PLIST.

The command line is constructed with the following PLIST items:

:command :arguments :extra-arguments.

The :arguments is split on whitespace, but :extra-arguments are
used as is."
  (let ((cmd (executable-find (plist-get plist :command)))
	(args (helm-grepint--prepare-args plist))
	proc)
    (when cmd
      (setq helm-grepint-current-command
	    (mapconcat #'identity (append (list cmd) args) " "))
      (setq proc (apply 'start-process "helm-grepint" nil
			(append (list cmd) args)))
      (set-process-sentinel proc
			    (lambda (process event)
			      (helm-process-deferred-sentinel-hook process event (helm-default-directory))))
      proc)))


(defun helm-grepint-select-grep (ask-grep)
  "Select the grep based on :enable-function from `helm-grepint-grep-configs'.

If ASK-GREP is non-nil, select the grep by asking with
`completing-read'.  The greps are compared in order of
`helm-grepint-grep-list'.  If the grep does not
have :enable-function property, select it automatically."

  (let (name enabler (greps helm-grepint-grep-list))
    (when ask-grep
      (setq greps (list (intern (completing-read "Select grep: "
    						  helm-grepint-grep-list nil t)))))
    (while greps
      (setq name (car greps))
      (setq enabler (or (helm-grepint-grep-config-property name :enable-function)
			#'(lambda () t)))
      (if (and (funcall enabler)
	       (executable-find (helm-grepint-grep-config-property name :command)))
	  (setq greps nil)
	(setq name nil)
	(pop greps)))
    (when (not name)
      (error "Helm-Grepint: No suitable grep found"))
    name))

(defun helm-grepint-grep-default-root ()
  "Get the default root directory if :root-directory-function isn't defined."
  default-directory)

(defun helm-grepint-grep-ask-root ()
  "Ask the root directory from user."
  (expand-file-name (read-directory-name "Root directory: ")))

;; Helm interface
(defun helm-grepint-grep-parse-line (line)
  "Parse a LINE of output from grep-compatible programs.

Returns a list of (file line contents) or nil if the line could not be parsed."
  ;; The regexp gotten from helm-grep.el
  (let ((ret (string-match "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)"
			   line)))
    (if ret
	(mapcar #'(lambda (x) (match-string x line)) '(1 2 3)))))

(defun helm-grepint-grep-action-jump (candidate)
  "Jump to line in a file described by a grep -line CANDIDATE."
  (run-hooks 'helm-grepint-grep-jump-pre-hook)
  (let ((items (helm-grepint-grep-parse-line candidate)))
    (with-helm-default-directory (helm-default-directory)
	(find-file (nth 0 items))
      (helm-goto-line (string-to-number (nth 1 items)))))
  (run-hooks 'helm-grepint-grep-jump-post-hook))

(defun helm-grepint-grep-action-mode (candidate)
  "Open a copy of the helm buffer in `grep-mode'.

CANDIDATE is ignored."
  (let ((newbuf (format "* grep-mode %s *" (buffer-name)))
	(oldparams
	 (with-helm-buffer
	   (list (current-buffer)
		 (save-excursion (goto-char (point-min)) (forward-line 1) (point))
		 (point-max)))))
    (with-current-buffer (get-buffer-create newbuf)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(apply #'insert-buffer-substring oldparams)
	(goto-char (point-min))
	(insert (format (concat "-*- mode: grep; default-directory: \"%s\" -*-\n"
				"\n\n%s\n")
			(helm-default-directory) helm-grepint-current-command))))
    (switch-to-buffer newbuf)
    (grep-mode)))

(defun helm-grepint-grep-process ()
  "This is the candidates-process for `helm-grepint-helm-source'."
  (let ((cfg (helm-grepint-grep-config helm-grepint--selected-grep)))
    (apply #'helm-grepint-run-command
	   :extra-arguments (replace-regexp-in-string "  *" ".*" helm-pattern)
	   (cdr cfg))))

(defun helm-grepint-grep-filter-one-by-one (candidate)
  "Propertize each CANDIDATE provided by `helm-grepint-helm-source'.

Uses `helm-grep-highlight-match' from helm-grep to provide line highlight."
  (let ((items (helm-grepint-grep-parse-line candidate)))
    (if items
	(format "%s:%s:%s"
		(propertize (nth 0 items) 'face compilation-info-face)
		(propertize (nth 1 items) 'face compilation-line-face)
		(helm-grep-highlight-match (nth 2 items) t))
      "")))

(defun helm-grepint--header-name (name)
  "Displays the helm header with given source NAME.

Additionally displays the used character case."
  (format "%s [%s]" name (symbol-name helm-grepint--character-case)))

(defun helm-grepint-cycle-character-case ()
  "Select the next one from the `helm-grepint-character-cases' list."
  (interactive)
  (with-helm-buffer
    (setq helm-grepint--character-case
	  (let ((cases helm-grepint-character-cases))
	    (cadr (member helm-grepint--character-case (append cases cases)))))
    (helm-force-update)))

(defvar helm-grepint-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<right>") 'helm-execute-persistent-action)
    (define-key map (kbd "M-c") #'helm-grepint-cycle-character-case)
    map))

(defvar helm-grepint-helm-source
  (helm-build-async-source "Generic grep interface"
      :volatile t
      :requires-pattern 3
      :candidates-process #'helm-grepint-grep-process
      :action '(("Jump to" . helm-grepint-grep-action-jump)
	       ("Open in grep-mode" . helm-grepint-grep-action-mode))
      :candidate-number-limit helm-grepint-candidate-number-limit
      :header-name #'helm-grepint--header-name
      :filter-one-by-one #'helm-grepint-grep-filter-one-by-one))

(defun helm-grepint--grep (in-root &optional arg)
  "Run grep either in current directory or if IN-ROOT, in a root directory.

ARG is the prefix argument and given \\[universal-argument] this
triggers manual selection of grep configuration,

The grep function is determined by the contents of
`helm-grepint-grep-configs' and the order of `helm-grepint-grep-list'.  The
root directory is determined by the :root-directory-function
property of an element of `helm-grepint-grep-configs'."
  (setq helm-grepint-current-command nil)
  (let ((name (helm-grepint-select-grep (and arg (> arg 1))))
	(default-directory default-directory))
    (when in-root
      (setq default-directory
	    (funcall (or (helm-grepint-grep-config-property name :root-directory-function)
			 #'helm-grepint-grep-default-root))))
    (helm :sources '(helm-grepint-helm-source)
	  :buffer (format "Grepint%s: %s" (if in-root "-root" "") name)
	  :keymap helm-grepint-helm-map
	  :input (funcall helm-grepint-pre-input-function)
	  :helm-grepint--selected-grep name
	  :helm-grepint--character-case helm-grepint-initial-case)))

;;;###autoload
(defun helm-grepint-grep (&optional arg)
  "Run grep in the current directory.

See the usage for ARG in `helm-grepint--grep'.

The grep function is determined by the contents of
`helm-grepint-grep-configs' and the order of `helm-grepint-grep-list'."
  (interactive "p")
  (helm-grepint--grep nil arg))

;;;###autoload
(defun helm-grepint-grep-root (&optional arg)
  "Function `helm-grepint-grep' is run in a root directory.

See the usage for ARG in `helm-grepint--grep'."
  (interactive "p")

  (helm-grepint--grep t arg))

;;;###autoload
(defun helm-grepint-set-default-config-v1.0.0 ()
  "Set the default grep configuration into `helm-grepint-grep-configs' and `helm-grepint-grep-list'."

  (setq helm-grepint-grep-configs nil)

  (defun helm-grepint-git-grep-locate-root ()
    (locate-dominating-file (file-name-as-directory
			     (expand-file-name (file-truename default-directory)))
			    ".git"))

  (helm-grepint-add-grep-config git-grep
    :command "git"
    :arguments "--no-pager grep --line-number --no-color"
    :ignore-case-arg "--ignore-case"
    :enable-function helm-grepint-git-grep-locate-root
    :root-directory-function helm-grepint-git-grep-locate-root)

  (helm-grepint-add-grep-config ag
    :command "ag"
    :arguments "--nocolor --search-zip --nogroup"
    :ignore-case-arg "--ignore-case"
    :root-directory-function helm-grepint-grep-ask-root)

  (setq helm-grepint-grep-list '(git-grep ag)))


(defvar helm-grepint-default-config-ag-presearch-marker-file ".projectile"
  "The file that is recognized to denote root when ag-presearch is used.")

;;;###autoload
(defun helm-grepint-set-default-config-v1.1.0 ()
  "Set default grep configuration.

Run `helm-grepint-set-default-config-v1.0.0' and then this function.

Adds configuration for running ag if file set in
`helm-grepint-default-config-ag-presearch-marker-file' is found
in a git repository before the git root.  The use case is running
this in huge git repositories and wanting to limit the searching
to a subdirectory."
  (helm-grepint-set-default-config-v1.0.0)

  (defun helm-grepint-ag-presearch-locate-root ()
    (let ((invalid nil)) ;; Creating a closure for the "invalid"
      (let ((hasfile
	     (lambda (dir)
	       (if invalid
		   nil
		 (if (file-exists-p (expand-file-name ".git" dir))
		     (progn
		       (setq invalid t)
		       nil)
		   (file-exists-p (expand-file-name
				   helm-grepint-default-config-ag-presearch-marker-file dir)))))))

	(locate-dominating-file (file-name-as-directory
				 (expand-file-name (file-truename default-directory)))
				hasfile))))

  (helm-grepint-grep-config 'ag-presearch
			    (cdr (append (helm-grepint-grep-config 'ag) nil)))
  (helm-grepint-grep-config-property 'ag-presearch
				     :enable-function #'helm-grepint-ag-presearch-locate-root)
  (helm-grepint-grep-config-property 'ag-presearch
				     :root-directory-function #'helm-grepint-ag-presearch-locate-root)
  (add-to-list 'helm-grepint-grep-list 'ag-presearch))

(defun helm-grepint-set-default-config-v1.2.0 ()
  "Set default grep configuration.

Run `helm-grepint-set-default-config-v1.1.0' and then this function.

Makes the `smart' character-case as the default.  Changes the
order of cycling the character-cases.  After the `smart' comes
case-sensitive."

  (helm-grepint-set-default-config-v1.1.0)

  ;; Make the smart case default and the case-sensitive next to it.
  (setq helm-grepint-character-cases '(smart case-sensitive case-insensitive)
  	helm-grepint-initial-case 'smart))

;;;###autoload
(fset 'helm-grepint-set-default-config #'helm-grepint-set-default-config-v1.0.0)

;;;###autoload
(fset 'helm-grepint-set-default-config-latest #'helm-grepint-set-default-config-v1.2.0)

(provide 'helm-grepint)
;;; helm-grepint.el ends here
