;;; python-test.el --- Python testing integration     -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/python-test.el
;; Package-Version: 20160924.1013
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

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
;;
;; `python-test.el' allows the execution of python tests from Emacs.
;;
;; Setup:
;;
;; Python objects:
;;
;; | objects      | function                 |
;; | -------------|--------------------------|
;; | class        | `python-test-class'      |
;; | method       | `python-test-method'     |
;; | function     | `python-test-function'   |
;; | file         | `python-test-file'       |
;; | project      | `python-test-project'    |
;;
;; Test frameworks:
;;
;; + [nose][]
;; + [pytest][]
;; + [unittest][]: Needs the command-line interface available since python >=2.7
;;
;; [nose]: https://nose.readthedocs.org/
;; [pytest]: https://pytest.org/
;; [unittest]: https://docs.python.org/library/unittest.html "Unit testing framework"

;;; Code:
(eval-when-compile (require 'cl-lib))

(require 'compile)
(require 'python)


(defgroup python-test nil
  "Python testing integration"
  :prefix "python-test-"
  :group 'applications)

(defcustom python-test-reuse-buffer t
  "Whether to reuse python test buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'python-test)

(defcustom python-test-project-root nil
  "Root of a python project."
  :type 'string
  :safe #'stringp
  :group 'python-test)

(defcustom python-test-project-root-files
  '("setup.py"                          ; Setuptools file
    "setup.cfg"                         ; Setuptools file
    "tox.ini"                           ; Tox file
    "pytest.ini"                        ; Py.test file
    "requirements.txt"                  ; Pip file
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence."
  :type '(repeat string)
  :group 'python-test)


;; Backends
(defvar python-test-backends
  '((unittest :command "python"
              :default-args ("-m" "unittest")
              :project-args ("discover")
              :file-args (lambda ()
                           (list (python-test-path-module (python-test-capture-path))))
              :defun-args (lambda ()
                            (error "Python unittest doesn't support testing functions"))
              :class-args (lambda ()
                            (list (format "%s.%s"
                                          (python-test-path-module (python-test-capture-path))
                                          (python-test-capture-class))))
              :method-args (lambda ()
                             (list (format "%s.%s.%s"
                                           (python-test-path-module (python-test-capture-path))
                                           (python-test-capture-class)
                                           (python-test-capture-defun)))))
    (pytest :command "py.test"
            :file-args (lambda ()
                         (list (python-test-capture-path)))
            :defun-args (lambda ()
                          (list (format "%s::%s"
                                        (python-test-capture-path)
                                        (python-test-capture-defun))))
            :class-args (lambda ()
                          (list (format "%s::%s"
                                        (python-test-capture-path)
                                        (python-test-capture-class))))
            :method-args (lambda ()
                           (list (format "%s::%s::%s"
                                         (python-test-capture-path)
                                         (python-test-capture-class)
                                         (python-test-capture-defun)))))
    (nose :command "nosetests"
          :file-args (lambda ()
                       (list (python-test-capture-path)))
          :defun-args (lambda ()
                        (list (format "%s:%s"
                                      (python-test-capture-path)
                                      (python-test-capture-defun))))
          :class-args (lambda ()
                        (list (format "%s:%s"
                                      (python-test-capture-path)
                                      (python-test-capture-class))))
          :method-args (lambda ()
                         (list (format "%s:%s.%s"
                                       (python-test-capture-path)
                                       (python-test-capture-class)
                                       (python-test-capture-defun)))))))

(defcustom python-test-backend 'unittest
  "Default backend for `python-test'."
  :type (append '(choice)
                (mapcar (lambda (x) (list 'const (car x))) python-test-backends))
  :safe #'symbolp
  :group 'python-test)

;; Faces
(defface python-test-description
  '((t :inherit warning))
  "Face for python-test error description lines."
  :group 'python-test)

(defface python-test-error
  '((t :inherit error))
  "Face for python-test error lines."
  :group 'python-test)

(defface python-test-info
  '((t :inherit success))
  "Face for test information."
  :group 'python-test)


(eval-and-compile
  (defconst python-test-rx-constituents
    `((defun       . ,(rx symbol-start "def" symbol-end))
      (class       . ,(rx symbol-start "class" symbol-end))
      (symbol-name . ,(rx (any letter ?_) (* (any word ?_)))))
    "Additional specific sexps for `python-test-rx'")

  (defmacro python-test-rx (&rest regexps)
    "Python Test specialized rx macro."
    (let ((rx-constituents (append python-test-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

(defconst python-test-beg-defun-regexp
  (python-test-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp for python function definition.")

(defconst python-test-beg-class-regexp
  (python-test-rx line-start (* space) class (+ space) (group symbol-name))
  "Regexp for python class definition.")

(defvar python-test-backends-history nil)
(defvar python-test-extra-args-history nil)
(defvar python-test-project-root-history nil)


;;; Internal functions
(defmacro with-python-test-project-root (directory &rest body)
  "Change from DIRECTORY and BODY."
  (declare (indent 1) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defmacro python-test-with-environment (&rest body)
  "Modify shell environment for python support of BODY."
  (declare (indent 0) (debug (body)))
  (if (fboundp 'python-shell-with-environment)
      `(python-shell-with-environment ,@body)
    `(let ((process-environment (python-shell-calculate-process-environment))
           (exec-path (python-shell-calculate-exec-path)))
       ,@body)))

(defun python-test-internal-capture ()
  "Capture python function internal.

It predates `python-nav-beginning-of-defun-regexp' to search a function definition."
  (save-excursion
    (when (or (python-info-looking-at-beginning-of-defun)
              (python-nav-beginning-of-defun 1))
      (re-search-forward python-nav-beginning-of-defun-regexp (point-at-eol) t)
      (match-string-no-properties 1))))

(defun python-test-capture-defun ()
  "Capture python function."
  (let ((python-nav-beginning-of-defun-regexp python-test-beg-defun-regexp))
    (or (python-test-internal-capture) (error "Python function name not found"))))

(defun python-test-capture-class ()
  "Capture python function."
  (let ((python-nav-beginning-of-defun-regexp python-test-beg-class-regexp))
    (or (python-test-internal-capture) (error "Python class name not found"))))

(defun python-test-locate-root-file (directory)
  "Locate project-root using from DIRECTORY."
  (cl-loop for file in python-test-project-root-files
           when (locate-dominating-file directory file)
           return it))

(defun python-test-calculate-project-root ()
  "Calculate project root."
  (or python-test-project-root (python-test-locate-root-file default-directory)))

(defun python-test-read-backend ()
  "Read path where python test will be executed."
  (let ((project-root (or (python-test-calculate-project-root) default-directory)))
    (if current-prefix-arg
        (list (read-directory-name "Project root: " project-root)
              (completing-read "Backend: " python-test-backends nil t nil 'python-test-backends-history)
              (read-string "Extra args: " nil 'python-test-extra-args-history))
      (list project-root python-test-backend ""))))

(defun python-test-capture-path (&optional project-root)
  "Return current file relative from PROJECT-ROOT."
  (file-relative-name (buffer-file-name) project-root))

(defun python-test-execute (command &rest args)
  "Internal execute COMMAND using ARGS."
  (python-test-with-environment
    (let ((command (mapconcat #'shell-quote-argument (cons command args) " ")))
      (save-some-buffers (not compilation-ask-about-save) nil)
      (compilation-start command #'python-test-mode
                         (lambda (_)
                           (if python-test-reuse-buffer
                               "*python-test*"
                             (format "*python-test: %s*" command)))))))

(defun python-test-path-module (file)
  "Convert a FILE path to a python module."
  (subst-char-in-string ?/ ?. (file-name-sans-extension file)))

(defun python-test-get-backend (backend-name)
  "Return for a BACKEND-NAME."
  (or (assoc-default (if (symbolp backend-name) backend-name (intern backend-name))
                     python-test-backends)
      (error "Backend not found")))


;;; python-test-mode
(defvar python-test-compilation-regexp-alist-alist
  `((python-tracebacks
     ;; XXX: "INTERNALERROR> " also make py.test error constituent errors
     ,(rx line-start (? "INTERNALERROR>") (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (pytest
     ,(rx line-start (group (+ any)) ":" (group (1+ digit)) ": " (? (group (+ not-newline))))
     1 2 3))
  "Specifications for matching errors in py.test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar python-test-compilation-regexp-alist (mapcar 'car python-test-compilation-regexp-alist-alist))

(defvar python-test-mode-font-lock-keywords
  `( ;; py.test error traces
    (,(rx line-start ">" (+ space) (+ not-newline) line-end)
     (0 'python-test-description))
    (,(rx line-start "E" (+ space) (+ not-newline) line-end)
     (0 'python-test-error))
    ;; py.test information separators
    (,(rx line-start (+ (in "-" "!" "=" "_")) (? space (+ not-newline) space) (+ (in "-" "!" "=" "_")) line-end)
     (0 'python-test-info))
    ;; nosetest information
    (,(rx line-start "Ran " (+ num) " tests in " (+ not-newline) line-end)
     (0 'python-test-info))
    (,(rx line-start (or "OK" (and "FAILED (failures=" (+ num) ")")) line-end)
     (0 'python-test-info))))

(defun python-test-ansi-color-filter ()
  "Handle match highlighting escape sequences.

This function is called from `compilation-filter-hook'."
  (ansi-color-apply-on-region compilation-filter-start (point)))

;; `python.el' variables introduced in Emacs 25.1
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

(defun python-test-track-pdb-prompt ()
  "Change compilation to `python-inferior-mode' when a pdb prompt is detected.

This function is a hack that enables `inferior-python-mode' when
a pdb prompt is detected in `compilation-mode' buffers, and to
work is meant to be added to `compilation-filter-hook'.  To go
back to `compilation-mode' you need to call
\\[python-test-back-to-compilation]."
  (let ((output (buffer-substring-no-properties compilation-filter-start (point-max))))
    (when (string-match-p python-shell-prompt-pdb-regexp output)
      (message "Entering pdb...")
      (setq buffer-read-only nil)
      (let ((python-shell--interpreter nil)
            (python-shell--interpreter-args nil))
        (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)
        (inferior-python-mode)
        (run-hook-with-args 'comint-output-filter-functions output)))))

(defun python-test-back-to-compilation ()
  "Go back to compilation mode.

See `python-test-track-pdb-prompt' documentation for more
information."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (message "Enabling compilation mode... ")
      (set-process-filter process 'compilation-filter)
      (compilation-mode))))

(define-compilation-mode python-test-mode "python-test"
  "Python test results compilation mode"
  (setq-local compilation-mode-font-lock-keywords
              python-test-mode-font-lock-keywords)
  (setq-local compilation-error-regexp-alist-alist
              python-test-compilation-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              python-test-compilation-regexp-alist)
  (add-hook 'compilation-filter-hook 'python-test-track-pdb-prompt nil t)
  (add-hook 'compilation-filter-hook 'python-test-ansi-color-filter nil t))

(define-key python-test-mode-map (kbd "p") #'compilation-previous-error)
(define-key python-test-mode-map (kbd "n") #'compilation-next-error)


;;;###autoload
(defun python-test-function (project-root backend-name extra-args)
  "Test a python function inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS."
  (interactive (python-test-read-backend))
  (with-python-test-project-root project-root
    (let* ((backend (python-test-get-backend backend-name))
           (command (or (plist-get backend :command) (error "Not command")))
           (defun-args (plist-get backend :defun-args))
           (arguments (append (plist-get backend :default-args)
                              (split-string-and-unquote extra-args)
                              (if (functionp defun-args)
                                  (funcall defun-args)
                                defun-args))))
      (apply #'python-test-execute command arguments))))

;;;###autoload
(defun python-test-method (project-root backend-name extra-args)
  "Test a single python method inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS."
  (interactive (python-test-read-backend))
  (with-python-test-project-root project-root
    (let* ((backend (python-test-get-backend backend-name))
           (command (or (plist-get backend :command) (error "Not command defined for %s backend" backend-name)))
           (method-args (plist-get backend :method-args))
           (arguments (append (plist-get backend :default-args)
                              (split-string-and-unquote extra-args)
                              (if (functionp method-args)
                                  (funcall method-args)
                                method-args))))
      (apply #'python-test-execute command arguments))))

;;;###autoload
(defun python-test-class (project-root backend-name extra-args)
  "Test a single python class inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS."
  (interactive (python-test-read-backend))
  (with-python-test-project-root project-root
    (let* ((backend (python-test-get-backend backend-name))
           (command (or (plist-get backend :command) (error "Not command defined for %s backend" backend-name)))
           (class-args (plist-get backend :class-args))
           (arguments (append (plist-get backend :default-args)
                              (split-string-and-unquote extra-args)
                              (if (functionp class-args)
                                  (funcall class-args)
                                class-args))))
      (apply #'python-test-execute command arguments))))

;;;###autoload
(defun python-test-file (project-root backend-name extra-args)
  "Test a single python file inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS."
  (interactive (python-test-read-backend))
  (with-python-test-project-root project-root
    (let* ((backend (python-test-get-backend backend-name))
           (command (or (plist-get backend :command) (error "Not command defined for %s backend" backend-name)))
           (file-args (plist-get backend :file-args))
           (arguments (append (plist-get backend :default-args)
                              (split-string-and-unquote extra-args)
                              (if (functionp file-args)
                                  (funcall file-args)
                                file-args))))
      (apply #'python-test-execute command arguments))))

;;;###autoload
(defalias 'python-test #'python-test-project)

;;;###autoload
(defun python-test-project (project-root backend-name extra-args)
  "Test a python project from PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS."
  (interactive (python-test-read-backend))
  (with-python-test-project-root project-root
    (let* ((backend (python-test-get-backend backend-name))
           (command (or (plist-get backend :command) (error "Not command defined for %s backend" backend-name)))
           (project-args (plist-get backend :project-args))
           (arguments (append (plist-get backend :default-args)
                              (split-string-and-unquote extra-args)
                              (if (functionp project-args)
                                  (funcall project-args)
                                project-args))))
      (apply #'python-test-execute command arguments))))

(provide 'python-test)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; python-test.el ends here
