;;; python-test.el --- Python testing integration     -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/python-test.el
;; Package-Version: 20161015.652
;; Keywords: convenience, tools, processes
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

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
(eval-when-compile
  (require 'cl-lib)
  (require 'cl-generic))

(require 'python)
(require 'compile)

(defgroup python-test nil
  "Python testing integration"
  :prefix "python-test-"
  :group 'compilation)

(defcustom python-test-reuse-buffer t
  "Whether to reuse python test buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'python-test)

(defcustom python-test-disable-warnings nil
  "Whether to disable warnings."
  :type 'boolean
  :safe #'booleanp
  :group 'python-test)

(defcustom python-test-command nil
  "Command to execute as python test."
  :type 'string
  :group 'python-test)
;;;###autoload(put 'python-test-command 'safe-local-variable (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command)) compilation-read-command))))

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
(defvar python-test-backends nil)

(defcustom python-test-backend 'unittest
  "Default backend for `python-test'."
  :type (append '(choice)
                (mapcar (lambda (x) (list 'const x)) python-test-backends))
  :safe #'python-test-registered-backend-p
  :group 'python-test)

(defvar python-test-command-history nil)


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


;;; Internal functions
(defun python-test-registered-backend-p (backend)
  "Determine whether `org-sync' BACKEND is registered."
  (memq backend python-test-backends))

(defun python-test-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defun python-test-read-command (command)
  "Read python test COMMAND."
  (read-shell-command "Test command: " command
                      (if (equal (car python-test-command-history) command)
                          '(python-test-command-history . 1)
                        'python-test-command-history)))

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

(defun python-test-project-root ()
  "Calculate project root."
  (or python-test-project-root
      (python-test-locate-root-file default-directory)))

(defun python-test-read-backend ()
  "Read the backend which will be used."
  (list (or (and (not current-prefix-arg) python-test-backend)
            (completing-read "Backend: " python-test-backends nil t nil))))

(defun python-test-capture-path (&optional project-root)
  "Return current file relative from PROJECT-ROOT."
  (if buffer-file-name
      (file-relative-name buffer-file-name project-root)
    (user-error "Not available here")))

(defun python-test-quote-command (command &rest args)
  "Quote COMMAND and ARGS."
  (mapconcat #'shell-quote-argument (cons command args) " "))

(defun python-test-resolve-executable (value)
  "Resolve executable VALUE."
  (cl-etypecase value
    (stringp value)
    (symbolp (symbol-value value))))

(defun python-test-path-module (file)
  "Convert a FILE path to a python module."
  (subst-char-in-string ?/ ?. (file-name-sans-extension file)))

(defun python-test-context-command (backend context)
  "Return BACKEND command from CONTEXT."
  (cl-assert (python-test-registered-backend-p backend) nil "Unregistered python-test backend: %s" backend)
  (let ((args (cl-ecase context
                (project   (python-test-args-project backend))
                (file      (python-test-args-file    backend))
                (class     (python-test-args-class   backend))
                (method    (python-test-args-method  backend))
                (defun     (python-test-args-defun   backend))))
        (executable (python-test-resolve-executable (python-test-executable backend))))
    (apply #'python-test-quote-command executable args)))

(defmacro python-test-with-project-root (&rest body)
  "Execute BODY in python project root."
  (declare (indent defun) (debug (body)))
  (let ((project-root (cl-gensym "project-root")))
    `(let ((,project-root (python-test-project-root)))
       (let ((default-directory (if ,project-root
                                    (file-name-as-directory ,project-root)
                                  (or python-test-disable-warnings (lwarn 'python-test :warning "Could not locate python project root: %s" default-directory))
                                  default-directory)))
         ,@body))))

(defmacro python-test-with-environment (&rest body)
  "Modify shell environment for python support of BODY."
  (declare (indent 0) (debug (body)))
  (if (fboundp 'python-shell-with-environment)
      `(python-shell-with-environment ,@body)
    `(let ((process-environment (python-shell-calculate-process-environment))
           (exec-path (python-shell-calculate-exec-path)))
       ,@body)))

;;; Backends
(cl-defgeneric python-test-executable (backend)
  "Backend args to test a project.")

(cl-defgeneric python-test-args-project (backend)
  "Backend args to test a project.")

(cl-defgeneric python-test-args-file (backend)
  "Backend args to test a file.")

(cl-defgeneric python-test-args-class (backend)
  "Backend args to test a class.")

(cl-defgeneric python-test-args-method (backend)
  "Backend args to test a class method.")

(cl-defgeneric python-test-args-defun (backend)
  "Backend args to test a function.")

;; unittest
(add-to-list 'python-test-backends 'unittest)

(cl-defmethod python-test-executable ((_backend (eql unittest)))
  "Python unitest executable is python itself, given that unittest is executed as module."
  python-shell-interpreter)

(cl-defmethod python-test-args-project ((_backend (eql unittest)))
  (list "-m" "unittest" "discover"))

(cl-defmethod python-test-args-file ((_backend (eql unittest)))
  (list "-m" "unitest" (python-test-path-module (python-test-capture-path))))

(cl-defmethod python-test-args-class ((_backend (eql unittest)))
  (list "-m" "unitest" (format "%s.%s"
                               (python-test-path-module (python-test-capture-path))
                               (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql unittest)))
  (list "-m" "unitest" (format "%s.%s.%s"
                               (python-test-path-module (python-test-capture-path))
                               (python-test-capture-class)
                               (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql unittest)))
  (user-error "Python unittest doesn't support testing functions"))


;; py.test
(add-to-list 'python-test-backends 'pytest)

(cl-defmethod python-test-executable ((_backend (eql pytest)))
  "Py.test executable name."
  "py.test")

(cl-defmethod python-test-args-project ((_backend (eql pytest)))
  (list))

(cl-defmethod python-test-args-file ((_backend (eql pytest)))
  (list (python-test-capture-path)))

(cl-defmethod python-test-args-class ((_backend (eql pytest)))
  (list (format "%s::%s" (python-test-capture-path) (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql pytest)))
  (list (format "%s::%s::%s" (python-test-capture-path) (python-test-capture-class) (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql pytest)))
  (list (format "%s::%s" (python-test-capture-path) (python-test-capture-defun))))


;; nosetests
(add-to-list 'python-test-backends 'nose)

(cl-defmethod python-test-executable ((_backend (eql nose)))
  "Nosetests executable."
  "nosetests")

(cl-defmethod python-test-args-project ((_backend (eql nose)))
  (list))

(cl-defmethod python-test-args-file ((_backend (eql nose)))
  (list (python-test-capture-path)))

(cl-defmethod python-test-args-class ((_backend (eql nose)))
  (list (format "%s:%s" (python-test-capture-path) (python-test-capture-class))))

(cl-defmethod python-test-args-method ((_backend (eql nose)))
  (list (format "%s:%s.%s" (python-test-capture-path) (python-test-capture-class) (python-test-capture-defun))))

(cl-defmethod python-test-args-defun ((_backend (eql nose)))
  (list (format "%s:%s" (python-test-capture-path) (python-test-capture-defun))))


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
(defun python-test-project (backend)
  "Execute python project test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'project))))

;;;###autoload
(defun python-test-file (backend)
  "Execute python file test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'file))))

;;;###autoload
(defun python-test-class (backend)
  "Execute python class test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'class))))

;;;###autoload
(defun python-test-method (backend)
  "Execute python method test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'method))))

;;;###autoload
(defun python-test-function (backend)
  "Execute python function test with BACKEND."
  (interactive (python-test-read-backend))
  (python-test-with-project-root
    (python-test (python-test-context-command (python-test-as-symbol backend) 'defun))))

;;;###autoload
(defun python-test (command)
  "Execute COMMAND with python test."
  (interactive (list (let ((command (eval python-test-command)))
                       (if (or compilation-read-command current-prefix-arg)
                           (python-test-read-command command)
                         command))))
  (python-test-with-environment
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compilation-start command #'python-test-mode
                       (lambda (mode)
                         (if python-test-reuse-buffer
                             (format "*%s*" (downcase mode))
                           (format "*%s: %s*" (downcase mode) command))))))

(provide 'python-test)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; python-test.el ends here
