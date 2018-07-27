;;; perlbrew.el --- A perlbrew wrapper for Emacs

;; Copyright (C) 2011 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Version: 0.3
;; Package-Version: 20161109.709
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
;; (require 'perlbrew)
;; (perlbrew-use "perl-5.12.3") ;; initialize perl version to use

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;; `perlbrew-dir'
;; your perlbrew directory
;; default = ~/perl5/perlbrew

;;; Code:

(defgroup perlbrew nil
  "perlbrew"
  :group 'shell)

(defcustom perlbrew-dir (or
                         (getenv "PERLBREW_ROOT")
                         (concat (getenv "HOME") "/perl5/perlbrew"))
  "Your perlbrew directory (PERLBREW_ROOT)."
  :group 'perlbrew
  :set (lambda (sym val)
         (custom-set-default sym val)
         (setenv "PERLBREW_ROOT" val)))

(defvar perlbrew-perls-dir nil)
(defvar perlbrew-command-path nil)

(defvar perlbrew-current-perl-dir nil)
(defvar perlbrew-current-perl-path nil)

(eval-when-compile
  (require 'cl))

(defun perlbrew (args)
  (interactive "M$ perlbrew ")
  (let* ((command (perlbrew-command args))
         (result (perlbrew-trim (shell-command-to-string command))))
    (if (called-interactively-p 'interactive)
        (unless (string-match "^\\s*$" result) (message result))
      result)))

(defun perlbrew-use (version)
  (interactive (list (completing-read "Version: " (perlbrew-list) nil t)))
  (setq perlbrew-perls-dir (concat perlbrew-dir "/perls"))
  (unless (called-interactively-p 'interactive)
    (unless (member version (perlbrew-list))
      (error "Not installed version: %s" version)))
  (cond ((equal version "system")
         (perlbrew-clean-exec-path)
         (setq perlbrew-current-perl-path (executable-find "perl")))
        (t
         (perlbrew-set-current-perl-path version)
         (perlbrew-set-current-exec-path))))

(defun perlbrew-switch (version)
  (interactive (list (completing-read "Version: " (perlbrew-list) nil t)))
  (perlbrew-use version))

(defun perlbrew-command (args)
  (setq perlbrew-command-path (concat perlbrew-dir "/bin/perlbrew"))
  (perlbrew-join (list perlbrew-command-path args)))

(defun perlbrew-list ()
  (let* ((perls (split-string (perlbrew "list")))
         (valid-perls (remove-if-not
                       (lambda (i)
                         (string-match "^\\(perl\\|[0-9]\\)" i))
                       perls)))
    (append valid-perls '("system"))))

(defun perlbrew-set-current-perl-path (version)
  (setq perlbrew-current-perl-dir (concat perlbrew-perls-dir "/" version))
  (setq perlbrew-current-perl-path (concat perlbrew-current-perl-dir "/bin/perl")))

(defun perlbrew-set-current-exec-path ()
  (let* ((bin-dir (concat perlbrew-current-perl-dir "/bin")))
    (perlbrew-clean-exec-path)

    ;; setting for PATH
    (setenv "PATH" (concat bin-dir ":" (getenv "PATH")))

    ;; setting for exec-path
    (add-to-list 'exec-path bin-dir)))

(defun perlbrew-clean-exec-path ()
  (setenv "PATH"
          (mapconcat
           'identity
           (perlbrew-remove-all-perl-path (split-string (getenv "PATH") ":"))
           ":"))
  (setq exec-path (perlbrew-remove-all-perl-path exec-path)))

(defun perlbrew-join (lst)
  (mapconcat 'identity lst " "))

(defun perlbrew-trim (str)
  (replace-regexp-in-string "\n+$" "" str))

(defun perlbrew-remove-all-perl-path (path-list)
  (let ((perlbrew-perl-regexp (format "^%s" perlbrew-perls-dir)))
    (remove-if (lambda (path)
                 (string-match perlbrew-perl-regexp path))
               path-list)))

(provide 'perlbrew)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; perlbrew.el ends here
