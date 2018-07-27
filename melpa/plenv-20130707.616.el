;;; plenv.el --- A plenv wrapper for Emacs

;; Copyright (C) 2012 Kenta Sato

;; Author: Kenta Sato <karupa@cpan.org>
;; Version: 0.32
;; Package-Version: 20130707.616
;; Keywords: Emacs, Perl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Initialize
;; (require 'plenv)
;; (plenv-global "perl-5.16.2") ;; initialize perl version to use

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;; `plenv-dir'
;; your plenv directory
;; default = ~/.plenv

;;; Utility
;; (guess-plenv-perl-path)    ;; return current plenv perl path. (like "plenv which perl". but, implemented by elisp.)
;; (guess-plenv-perl-version) ;; return current plenv perl version. (like "plenv version". but, implemented by elisp.)

;;; Code:
(require 'cl)

(defgroup plenv nil
  "plenv"
  :group 'shell)

(defcustom plenv-dir (concat (getenv "HOME") "/.plenv")
  "your plenv directory"
  :group 'plenv)

(defvar plenv-versions-dir nil)
(defvar plenv-command-path nil)

(defvar plenv-current-perl-dir nil)
(defvar plenv-current-perl-path nil)

(defmacro plenv-trim (str)
  `(replace-regexp-in-string "\n+$" "" ,str))

(defvar plenv-global-perl-path (let ((curr-plenv-version-env (getenv "PLENV_VERSION"))
                                     (result))
                                 (setenv "PLENV_VERSION" "system")
                                 (setq result (plenv-trim (shell-command-to-string "plenv which perl")))
                                 (setenv "PLENV_VERSION" curr-plenv-version-env)
                                 result))

(defvar plenv-script-version
  (car (cdr (split-string (plenv-trim (shell-command-to-string "plenv --version")) " "))))

(defvar plenv-script-major-version
  (string-to-number (substring (replace-regexp-in-string "^v" "" plenv-script-version) 0 3)))

(defvar plenv-list-subcommand (if (<= 1.9 plenv-script-major-version) "versions" "list"))

(defmacro plenv-join (delimiter string-list)
  `(mapconcat 'identity ,string-list ,delimiter))

(defmacro plenv-command (args)
  `(plenv-join " " (cons "plenv" ,args)))

(defmacro plenv-basedir (curr-dir)
  `(plenv-join "/" (nreverse (cdr (nreverse (split-string ,curr-dir "/"))))))

(defmacro plenv-try-load-from-file (varname file)
  `(if (file-readable-p ,file)
       (setq ,varname (with-temp-buffer
                        (insert-file-contents ,file)
                        (plenv-trim (buffer-string))))))

(defun plenv-perls ()
  (let* ((perls (split-string (plenv plenv-list-subcommand)))
          (valid-perls (mapcar
                         (lambda (i)
                           (replace-regexp-in-string " (set by\.\*\$" "" i))
                       (remove-if-not
                         (lambda (i)
                           (string-match "^\\(perl\\|[0-9]\\)" i))
                       perls))))
    (if (string-equal plenv-list-subcommand "list")
        (setq valid-perls (append valid-perls (list "system"))))
    valid-perls))

(defun try-get-plenv-local-version (pwd)
  (unless (or (null pwd) (string= "" pwd) (string= "/" pwd) (string= "." pwd))
    (let ((plenv-version-file (concat pwd "/.perl-version"))
          (version nil))
      (plenv-try-load-from-file version plenv-version-file)
      (if (null version)
          (try-get-plenv-local-version (plenv-basedir pwd))
        version))))

(defun try-get-plenv-global-version ()
  (let ((plenv-version-file (concat plenv-dir "/version"))
        (version nil))
    (plenv-try-load-from-file version plenv-version-file)
    version))

(defun guess-plenv-version (&optional pwd)
  (if (null pwd)
      (setq pwd default-directory))
  (let ((version (getenv "PLENV_VERSION"))) ;; shell version
    (if (null version) ;; local version
        (setq version (try-get-plenv-local-version pwd)))
    (if (null version) ;; global version
        (setq version (try-get-plenv-global-version)))
    (if (null version) ;; fallback to command
        (setq version (plenv-trim (shell-command-to-string (plenv-command '("version"))))))
    version))

(defun guess-plenv-perl-path (&optional pwd)
  (if (null pwd)
      (setq pwd default-directory))
  (let ((version (guess-plenv-version pwd)))
    (cond ((string= "system" version) plenv-global-perl-path)
          (t (format "%s/versions/%s/bin/perl" plenv-dir version)))))

(defun plenv (args)
  (interactive "M$ plenv ")
  (let* ((command (plenv-command (list args)))
         (result (plenv-trim (shell-command-to-string command))))
    (if (called-interactively-p 'interactive)
        (unless (string-match "^\\s*$" result) (message result))
      result)))

(defun plenv-list ()
  (interactive)
  (shell-command (plenv-command (list plenv-list-subcommand))))

(defun plenv-shell (version)
  (interactive (list (completing-read "Version: " (plenv-perls) nil t)))
  (setq plenv-versions-dir (concat plenv-dir "/versions"))
  (unless (called-interactively-p 'interactive)
    (unless (member version (plenv-perls))
      (error "Not installed version: %s" version)))
  (setenv "PLENV_VERSION" version))

(defun plenv-local (version)
  (interactive (list (completing-read "Version: " (plenv-perls) nil t)))
  (setq plenv-versions-dir (concat plenv-dir "/versions"))
  (unless (called-interactively-p 'interactive)
    (unless (member version (plenv-perls))
      (error "Not installed version: %s" version)))
  (shell-command (plenv-command (list "local" version))))

(defun plenv-global (version)
  (interactive (list (completing-read "Version: " (plenv-perls) nil t)))
  (setq plenv-versions-dir (concat plenv-dir "/versions"))
  (unless (called-interactively-p 'interactive)
    (unless (member version (plenv-perls))
      (error "Not installed version: %s" version)))
  (shell-command (plenv-command (list "global" version))))

(defun plenv-version ()
  (interactive)
  (message "version: %s" (guess-plenv-version)))

(provide 'plenv)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; plenv.el ends here
