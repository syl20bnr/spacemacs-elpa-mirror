;;; flycheck-coverity.el --- Integrate Coverity with flycheck

;; Copyright (c) 2016 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/flycheck-coverity
;; Package-Version: 20170703.1759
;; Version: 0.2
;; Package-Requires: ((flycheck "0.24") (dash "2.12.0") (emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This packages integrates the Coverity `cov-run-desktop' Fast Desktop
;; Analysis tool with flycheck to automatically detect any new defects in your
;; code on the fly.

;;;; Setup

;; (with-eval-after-load 'flycheck
;;    (require 'flycheck-coverity)
;;    (flycheck-coverity-setup)
;;    ;; chain after cppcheck since this is the last checker in the upstream
;;    ;; configuration
;;    (flycheck-add-next-checker 'c/c++-cppcheck '(warning . coverity)))

;; If you do not use cppcheck then chain after clang / gcc / other C checker
;; that you use

;; (flycheck-add-next-checker 'c/c++-clang '(warning . coverity))

;;; Code:
(require 'flycheck)
(require 'dash)
(require 'cl-lib)

(flycheck-def-args-var flycheck-coverity-args coverity)

(defun flycheck-coverity--get-default-directory (_checker)
  "Get the working directory."
  (-when-let (file (buffer-file-name))
    (locate-dominating-file file "coverity.conf")))

(defun flycheck-coverity--get-relative-buffer-file-name ()
  "Get the relative path name for the current (buffer-file-name)."
  (file-relative-name (buffer-file-name)
		      (flycheck-coverity--get-default-directory nil)))

(defun flycheck-coverity--locate-coverity-conf ()
  "Locate the coverity.conf file."
  (-when-let (root (flycheck-coverity--get-default-directory nil))
    (concat (file-name-as-directory root) "coverity.conf")))

(defun flycheck-coverity--locate-data-coverity ()
  "Locate the data-coverity directory."
  (-when-let (conf (flycheck-coverity--locate-coverity-conf))
    (let ((data-coverity (expand-file-name
			  (concat (file-name-as-directory
				   (file-name-directory conf))
				  "data-coverity"))))
      (when (file-exists-p data-coverity)
	data-coverity))))

(defun flycheck-coverity--locate-build-log ()
  "Locate the data-coverity directory."
  (-when-let (data-coverity (flycheck-coverity--locate-data-coverity))
    (cl-first (file-expand-wildcards (concat (file-name-as-directory data-coverity)
                                             (file-name-as-directory "*")
                                             (file-name-as-directory "idir")
                                             "build-log.txt")))))

(defun flycheck-coverity--setup-p ()
  "Determine if `cov-run-desktop --setup` has been run by the presence of data-coverity directory."
  (-when-let (build-log (flycheck-coverity--locate-build-log))
    (let ((file (flycheck-coverity--get-relative-buffer-file-name)))
      ;; use grep if available otherwise fall back to searching directly in
      ;; emacs-lisp - slower but available on all platforms
      (-if-let (grep (executable-find "grep"))
	  (= 0 (call-process grep nil nil nil "-q" file build-log))
	(let ((large-file-warning-threshold nil))
	  (with-current-buffer (find-file-noselect build-log)
	    (goto-char (point-min))
	    (search-forward file (point-max) t)))))))

(flycheck-define-checker coverity
  "A checker using coverity.

See `https://github.com/alexmurray/coverity/'."
  :command ("cov-run-desktop"
            "--text-output-style=oneline"
            (eval flycheck-coverity-args)
            source-original)
  :predicate (lambda () (and (flycheck-buffer-saved-p)
			(flycheck-coverity--setup-p)))
  :working-directory flycheck-coverity--get-default-directory
  :verify (lambda (_)
	    (let ((file (flycheck-coverity--get-relative-buffer-file-name))
		  (conf (flycheck-coverity--locate-coverity-conf))
		  (data-coverity (flycheck-coverity--locate-data-coverity))
		  (build-log (flycheck-coverity--locate-build-log))
		  (setup (flycheck-coverity--setup-p)))
	      (list
	       (flycheck-verification-result-new
		:label "coverity.conf"
		:message (if conf (format "Found at %s" conf) "no coverity.conf found")
		:face (if conf
			  'success
			'(bold error)))
	       (flycheck-verification-result-new
		:label "cov-run-desktop --setup"
		:message (if data-coverity "Yes" "No - please run `cov-run-desktop --setup`")
		:face (if data-coverity
			  'success
			'(bold error)))
	       (flycheck-verification-result-new
		:label "In Project"
		:message (if setup
			     "Yes"
			   (format "%s does not seem to mention this file (%s)"
				   build-log file))
		:face (if setup
			  'success
			'(bold error))))))

  :error-patterns ((warning line-start (file-name) ":" line ": CID"
                            (message (one-or-more not-newline)
                                     (zero-or-more "\n"
                                                   (one-or-more not-newline)))
                            line-end))
  :modes (c-mode c++-mode))

;;;###autoload
(defun flycheck-coverity-setup ()
  "Setup flycheck-coverity.

Add `coverity' to `flycheck-checkers'."
  (interactive)
  ;; append to list and chain after existing checkers
  (add-to-list 'flycheck-checkers 'coverity t))

(provide 'flycheck-coverity)

;;; flycheck-coverity.el ends here
