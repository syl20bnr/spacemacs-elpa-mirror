;;; ob-axiom.el --- An org-babel backend for the axiom-environment system -*- lexical-binding: t -*-

;; Copyright (C) 2014 - 2017 Paul Onions

;; Author: Paul Onions
;; Keywords: Axiom, OpenAxiom, FriCAS
;; Package-Version: 20171022.1246

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;; Package-Requires: ((emacs "24.2") (axiom-environment "20171021"))

;;; Commentary:

;; The ``ob-axiom'' package is an org-babel extension that integrates
;; the axiom-environment into org-mode, allowing a literate
;; development & presentation style with easy publishing to HTML, PDF,
;; etc.

;; To enable this extension put
;;
;;  (require 'ob-axiom)
;;
;; in your ~/.emacs initialisation file.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(require 'cl-lib)

(require 'axiom-environment)

;; Header arguments
(defconst org-babel-header-args:axiom '())

(defvar org-babel-default-header-args:axiom '((:session . "Axiom Org-Babel Session")))

;; File extension for Axiom Input files
(add-to-list 'org-babel-tangle-lang-exts '("axiom" . "input"))

(add-to-list 'org-babel-tangle-lang-exts '("spad" . "spad"))

;; Configure org editing options
(add-to-list 'org-src-lang-modes '("axiom" . axiom-input))

(add-to-list 'org-src-lang-modes '("spad" . axiom-spad))

;;; Org framework functions -- functions called by Org-mode
;;;
(defun org-babel-axiom-initiate-session (session params)
  "Start an Axiom session for use by org-babel."
  ;;(message "org-babel-axiom-initiate-session\n %S\n %S" session params)
  (unless (string= session "none")
    (let ((session-name (org-babel-axiom-starify-name session)))
      (let ((axiom-process-buffer-name session-name)) ; dynamic binding
        (if (org-babel-comint-buffer-livep session-name)
            session-name
          (axiom-process-start axiom-process-program))))))

(defun org-babel-axiom-var-to-axiom (val)
  "Convert an elisp var into a string of Axiom source code
specifying a var of the same value."
  ;;(message "org-babel-axiom-var-to-axiom\n %S" val)
  (if (listp val)
      (concat "[" (mapconcat #'org-babel-axiom-var-to-axiom val ", ") "]")
    (format "%S" val)))

(defun org-babel-variable-assignments:axiom (params)
  "Return a list of Axiom statements assigning the block's variables."
  ;;(message "org-babel-variable-assignments:axiom\n %S" params)
  (let ((vars (mapcan (lambda (param)
                        (and (eql :var (car param))
                             (list (cdr param))))
                      params)))
    (mapcar
     (lambda (pair)
       (format "%S := %s" (car pair) (org-babel-axiom-var-to-axiom (cdr pair))))
     vars)))

(defun org-babel-expand-body:axiom (body params)
  "Expand BODY with PARAMS."
  ;;(message "org-babel-expand-body:axiom\n %S\n %S" body params)
  (mapconcat #'identity (append (org-babel-variable-assignments:axiom params)
                                (list body))
             "\n"))

(defun org-babel-execute:axiom (body params)
  "Execute a block of Axiom code with org-babel.
This function is called by `org-babel-execute-src-block'."
  ;;(message "org-babel-execute:axiom\n %S\n %S" body params)
  (let* ((lines (split-string (org-babel-expand-body:axiom body params) "\n"))
         (session (org-babel-axiom-initiate-session (cdr (assoc :session params)) params)))
    (let ((axiom-process-buffer-name session))  ; dynamic binding
      (with-axiom-process-query-buffer
       (dolist (line lines)
         (beginning-of-line)
         (unless (string-match "^[[:space:]]*$" line)
           (axiom-process-redirect-send-command line (current-buffer) nil t t t t)))
       (buffer-substring (point-min) (point-max))))))

;;; Internal helper functions
;;;
(defun org-babel-axiom-starify-name (str)
  "Ensure valid process buffer name by wrapping with asterisks if necessary."
  (let ((name str))
    (unless (eql (aref str 0) ?*)
      (setq name (concat "*" name)))
    (unless (eql (aref str (1- (length str))) ?*)
      (setq name (concat name "*")))
    name))

(provide 'ob-axiom)

;;; ob-axiom.el ends here
