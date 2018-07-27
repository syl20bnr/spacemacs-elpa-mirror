;;; molecule.el --- Simple wrapper for molecule -*- lexical-binding: t -*-

;; Copyright:: Copyright (c) 2017, drymer

;; Author:: drymer <drymer [ AT ] autistici.org>
;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 20180527.743
;; Version: 0.1

;; Keywords:: languages terminals
;; Homepage: https://git.daemons.it/drymer/molecule.el

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
;; If you find a bug, you may send an e-mail to drymer [ AT ] autistici.org
;; or open an issue at https://git.daemons.it/drymer/molecule.el

;;; Code:
(defgroup molecule nil
  "Wrapper around molecule command."
  :group 'tools)

(defcustom molecule-command "molecule"
  "The molecule command (no shit, Sherlock).  It shouldn't be necessary to cha\
nge it if it's on the PATH."
  :type 'variable
  :group 'molecule)

(defvar molecule-buffer-name-v "*molecule*"
  "Molecule buffer name.  It shouldn't be necessary to modify it.")

(defcustom molecule-debug-v nil
  "If set to t, it will use the --debug flag."
  :type 'variable
  :group 'molecule)

(defvar molecule-version-v nil
  "Molecule version.  Do not modify manually.")

;;;###autoload
(defun molecule-init ()
  "Execute molecule init."
  (interactive)
  (let* (;; Choose scenario or role
	 (molecule-parameter (completing-read "Choose: " (list "scenario"
							       "role")))
	 ;; If scenario, choose scenario name and set role name automatically,
	 ;; if not ask
	 (scenario-name (if (string= molecule-parameter "scenario")
			    (concat " -s " (shell-quote-argument
					    (read-string "Scenario name: ")))))
	 (role-name (if (string= molecule-parameter "role")
	 		(shell-quote-argument (read-string "Role name: "))
		      (progn
			(if dired-directory
			    (shell-quote-argument (file-name-nondirectory
						   (directory-file-name
						    (file-name-directory
						     dired-directory))))
			  (shell-quote-argument (file-name-nondirectory
						 (directory-file-name
						  (file-name-directory
						   default-directory))))))))
	 ;; Set molecule driver
	 (molecule-driver (completing-read "Choose a driver: "
					   (list "azure" "docker" "ec2" "gce"
						 "lxc" "lxd" "openstack"
						 "vagrant")))
	 ;; Set molecule verifier
	 (molecule-verifier (completing-read "Choose a verifier: "
					     (list "testinfra" "goss")))
	 ;; Set debug
	 (debug (if molecule-debug-v " --debug" "")))
    (compile (concat molecule-command  debug " init " molecule-parameter " -r "
		     role-name " -d " molecule-driver " --verifier-name "
		     molecule-verifier scenario-name))))

(defun molecule-basedir (directory)
  "Molecule function which helps to manage directories names.
Argument DIRECTORY A directory path."
  (file-name-directory (replace-regexp-in-string
			(concat (file-name-nondirectory
				 (directory-file-name
				  (file-name-directory directory))) "/$") ""
				  directory)))

(defun molecule--wrapper (command)
  "Molecule generic `COMMAND' wrapper."
  (let (;; Set debug
	(debug (if (eq molecule-debug-v t) "--debug " ""))
	(old-dir)
	(scenario)
	(scenarios)
	(molecule-dir))
    ;; Search the molecule directory until two parent directories
    (if (string= (substring default-directory -6 -1) "tests")
	(progn
	  (message "1")
	  (setq old-dir default-directory)
	  (setq default-directory (substring default-directory 0 -23)))
      (progn
	(molecule-basedir default-directory)
	(if (not (file-directory-p "molecule"))
	    (progn
	      (setq molecule-dir (molecule-basedir default-directory))
	      (if (not (file-directory-p (concat molecule-dir "molecule")))
		  (progn
		    (setq molecule-dir (molecule-basedir molecule-dir))
		    (if (not (file-directory-p (concat molecule-dir
						       "molecule")))
			(user-error "There's no molecule directory! You should\
 execute M-x molecule-init")
		      (progn
			(setq old-dir default-directory)
			(setq default-dirrectory
			      (concat molecule-dir "molecule"))))))
	      (progn
		(setq old-dir default-directory)
		(setq default-directory molecule-dir)))
	  (setq old-dir default-directory))))
    ;; Exclude . and ..
    (setq scenarios (directory-files (expand-file-name
				      (concat default-directory "molecule"))
				     nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    ;; If there's more than one scenario, give an option to choose them
    (if (> (length scenarios) 1)
	(progn
	  (setq scenarios (cons "all" scenarios))
	  (setq scenario (concat " -s " (shell-quote-argument
					 (completing-read
					  "Choose a scenario: "
					  scenarios)))))
      (if (> (length scenarios) 1)
	  (user-error "There's no scenarios! You should execute M-x molecule-"\
		      "init")))

    (compile (concat molecule-command " " debug command scenario))
    (setq default-directory old-dir)))

;;;###autoload
(defun molecule-check ()
  "Execute molecule converge."
  (interactive)
  (funcall 'molecule--wrapper "check"))

;;;###autoload
(defun molecule-converge ()
  "Execute molecule converge."
  (interactive)
  (funcall 'molecule--wrapper "converge"))

;;;###autoload
(defun molecule-create ()
  "Execute molecule create."
  (interactive)
  (funcall 'molecule--wrapper "create"))

;;;###autoload
(defun molecule-dependency ()
  "Execute molecule dependency."
  (interactive)
  (funcall 'molecule--wrapper "dependency"))

;;;###autoload
(defun molecule-destroy ()
  "Execute molecule destroy."
  (interactive)
  (funcall 'molecule--wrapper "destroy"))

;;;###autoload
(defun molecule-idempotence ()
  "Execute molecule idempotence."
  (interactive)
  (funcall 'molecule--wrapper "idempotence"))

;;;###autoload
(defun molecule-lint ()
  "Execute molecule lint."
  (interactive)
  (funcall 'molecule--wrapper "lint"))

;;;###autoload
(defun molecule-list ()
  "Execute molecule list."
  (interactive)
  (funcall 'molecule--wrapper "list"))

;;;###autoload
(defun molecule-side-effect ()
  "Execute molecule side."
  (interactive)
  (funcall 'molecule--wrapper "side-effect"))

;;;###autoload
(defun molecule-syntax ()
  "Execute molecule syntax."
  (interactive)
  (funcall 'molecule--wrapper "syntax"))

;;;###autoload
(defun molecule-test ()
  "Execute molecule test."
  (interactive)
  (funcall 'molecule--wrapper "test"))

;;;###autoload
(defun molecule-verify ()
  "Execute molecule verify."
  (interactive)
  (funcall 'molecule--wrapper "verify"))


;;;###autoload
(defun molecule-version ()
  "Show molecule and molecule.el version."
  (interactive)
  (let ((output))
    (if (eq molecule-version-v nil)
	(progn
	  (setq output (shell-command-to-string
			(concat molecule-command " --version")))
	  (setq output (concat output "molecule.el v0.1"))
	  (setq molecule-version-v output)))
    (message molecule-version-v)))

;;;###autoload
(defun molecule-toggle-debug ()
  "Toggle molecule debug."
  (interactive)
  (if (eq molecule-debug-v nil)
      (setq molecule-debug-v t)
    (setq molecule-debug-v nil)))

;;;###autoload
(define-minor-mode molecule-mode
  "Minor mode for molecule wrapper.

When called interactively, toggle `molecule-mode'.  With
prefix ARG, enable `molecule-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `molecule-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`molecule-mode'.  Otherwise behave as if called interactively."

  :init-value nil
  :lighter " Mol"
  :group 'molecule
  :require 'molecule)

(provide 'molecule)

;;; molecule ends here

;;; molecule.el ends here
