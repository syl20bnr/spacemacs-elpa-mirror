;;; cinspect.el --- Use cinspect to look at the CPython source of builtins and other C objects!

;; Copyright (C) 2015 Ben Yelsey

;; Author: Ben Yelsey <ben.yelsey@gmail.com>
;; Version: 0.2.0
;; Package-Version: 20150715.1933
;; Keywords: python
;; Homepage: https://github.com/inlinestyle/cinspect-mode

;; Package-Requires: ((emacs "24") (cl-lib "0.5") (deferred "0.3.1") (python-environment "0.0.2"))

;;; Commentary:

;; Use cinspect to look at the CPython source of builtins and other C objects!
;; cinspect (the Emacs mode) can and optimally should be used in concert with Jedi.el.

;;; Code:

(require 'cl-lib)
(require 'deferred)
(require 'help-mode)
(require 'python-environment)


(defgroup cinspect nil
  "Inspect CPython builtins."
  :group 'completion
  :prefix "cinspect-")

(defcustom cinspect-use-with-jedi (featurep 'jedi)
  "Use jedi's epc server to get the qualified names of builtins."
  :group 'cinspect)

(defcustom cinspect-use-as-jedi-goto-fallback t
  "Automatically use as a fallback when jedi:goto-definition hits a python builtin."
  :group 'cinspect)

(defcustom cinspect-index-directory "~/.cinspect"
  "Location of cinspect's CPython indexes"
  :group 'cinspect)

(defcustom cinspect-tmp-directory "/tmp/cinspect"
  "Location for temporary download of cinspect repo"
  :group 'cinspect)

(defcustom cinspect-buffer-name "*cinspect*"
  "Name for cinspect's readonly C source code buffer"
  :group 'cinspect)

(declare-function jedi:goto-definition "jedi-core" ())
(declare-function jedi:call-deferred "jedi-core" (method-name))


(defun cinspect-getsource-with-jedi-as-jedi-fallback ()
  (interactive)
  (deferred:nextc (jedi:goto-definition)
    (lambda (message)
      (when (and message (or (string-match "builtin" message)
                             (string-match "Definition not found" message)
                             (string-match "File .+ does not exist" message)))
        (cinspect-getsource-with-jedi)))))

(defun cinspect-getsource-with-jedi ()
  (interactive)
  (deferred:nextc (cinspect--python-jedi-get-name-and-import-statement)
    (lambda (name-and-import-statement)
      (let ((name (car name-and-import-statement))
            (import-statement (cadr name-and-import-statement)))
        (message "Inspecting `%s'" name)
        (cinspect--python-cinspect name import-statement)))))

(defun cinspect-getsource ()
  (interactive)
  (let ((name (symbol-at-point)))
    (message "Inspecting `%s'" name)
    (cinspect--python-cinspect name)))

(defun cinspect--join-python-statements (&rest statements)
  (mapconcat (lambda (statement)
               (if (zerop (length statement))
                   ""
                 (concat statement "; ")))
             statements ""))

(defun cinspect--format-python-command (name import-statement)
  (list "python" "-c"
        (cinspect--join-python-statements
         "import cinspect"
         (if (equal name "NoneType")
             "from types import NoneType"
           import-statement)
         (format "print cinspect.getsource(%s)" name))))

(defun cinspect--python-cinspect (name &optional import-statement)
  (deferred:$
    (python-environment-run
     (cinspect--format-python-command name import-statement))
    (deferred:nextc it
      (lambda (response)
        (with-temp-buffer-window cinspect-buffer-name nil nil
                                 (with-current-buffer cinspect-buffer-name
                                   (c-mode)
                                   (use-local-map (copy-keymap help-mode-map)))
                                 (princ response))))
    (deferred:error it
      (lambda (err)
        (if (string-match "ImportError: No module named cinspect" (or (cadr err) ""))
            (message "Could not find cinspect in emacs python environment. Have you run `cinspect-install-cinspect'?")
          (message "Error running cinspect: %s" err))))))


;; Begin Jedi interface

(defun cinspect--python-builtin-p (module)
  (equal module "__builtin__"))

(defun cinspect--format-module (desc-with-module)
  "Expects a `:desc_with_module' field as returned by jediepcserver's 'get_definition' endpoint."
  (car (split-string desc-with-module ":")))

(defun cinspect--format-import-from-module (module name)
  (if (cinspect--python-builtin-p module)
      ""
    (format "from %s import %s" module name)))

(defun cinspect--format-name (module full-name name type)
  (pcase type
    ("class"    (if (cinspect--python-builtin-p module) full-name name))
    ("function" (if (cinspect--python-builtin-p module) full-name name))
    ("instance" (if (cinspect--python-builtin-p module) full-name name))
    ("module"   full-name)
    (_          full-name)))

(defun cinspect--format-import-statement (module full-name name type)
  (pcase type
    ("class"    (cinspect--format-import-from-module module name))
    ("function" (cinspect--format-import-from-module module name))
    ("instance" (cinspect--format-import-from-module module name))
    ("module"   (format "import %s" full-name))
    (_          "")))

(defun cinspect--python-jedi-get-name-and-import-statement ()
  (deferred:nextc (jedi:call-deferred 'get_definition)
    (lambda (response)
      (cl-destructuring-bind (&key desc_with_module full_name name type &allow-other-keys)
          (car response)
        (let* ((module (cinspect--format-module desc_with_module))
               (import-statement (cinspect--format-import-statement module full_name name type))
               (name (cinspect--format-name module full_name name type)))
          (list name import-statement))))))

;; End Jedi interface


;; Begin installation helpers

(defun cinspect--ensure-virtualenv ()
  (deferred:$
    (deferred:process "pip" "list")
    (deferred:nextc it
      (lambda (response)
        (if (string-match "virtualenv" response)
            (message "virtualenv executable found")
          (deferred:process "pip" "install" "--user" "virtualenv"))))))

(defun cinspect--ensure-indexes ()
  (if (file-exists-p cinspect-index-directory)
      (message "cinspect indexes found at %s, skipping index download" cinspect-index-directory)
    (python-environment-run '("cinspect-download"))))

(defun cinspect--ensure-cinspect-repo ()
  (if (file-exists-p cinspect-tmp-directory)
      (message "cinspect download found at %s, skipping download" cinspect-tmp-directory)
    (deferred:process "git" "clone" "https://github.com/punchagan/cinspect.git" cinspect-tmp-directory)))

(defun cinspect-install-cinspect ()
  (interactive)
  (lexical-let ((current-dir default-directory))
    (deferred:$
      (deferred:$
        (cinspect--ensure-cinspect-repo)
        (deferred:nextc it
          (lambda ()
            (cd cinspect-tmp-directory)
            (cinspect--ensure-virtualenv)))
        (deferred:nextc it
          (lambda ()
            (python-environment-run '("python" "setup.py" "install"))))
        (deferred:nextc it #'cinspect--ensure-indexes)
        (deferred:nextc it
          (lambda (reply)
            (message "Done installing cinspect"))))
      (deferred:error it
        (lambda (err) (message "Error installing cinspect: %s" err)))
      (deferred:nextc it
        (lambda ()
          (cd current-dir)
          (when (file-exists-p cinspect-tmp-directory)
            (delete-directory cinspect-tmp-directory t))
          (message "Done cleaning up"))))))

;; End installation helpers

;;;###autoload
(define-minor-mode cinspect-mode
  "CInspect Mode.
Uses `cinspect' (https://github.com/punchagan/cinspect) to show CPython source for Python builtins
Can be used as a fallback option for `jedi-mode' (https://github.com/tkf/emacs-jedi)."
  :lighter " cinspect"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") (if cinspect-use-with-jedi
                                              'cinspect-getsource-with-jedi
                                            'cinspect-getsource))
            (when cinspect-use-as-jedi-goto-fallback
              (define-key map (kbd "C-c .") 'cinspect-getsource-with-jedi-as-jedi-fallback))
            map))

(provide 'cinspect)

;;; cinspect.el ends here
