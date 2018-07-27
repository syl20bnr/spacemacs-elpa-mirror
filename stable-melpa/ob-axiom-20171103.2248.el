;;; ob-axiom.el --- An org-babel backend for the axiom-environment system -*- lexical-binding: t -*-

;; Copyright (C) 2014 - 2017 Paul Onions

;; Author: Paul Onions
;; Keywords: Axiom, OpenAxiom, FriCAS
;; Package-Version: 20171103.2248

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

;; There are two language names defined in this extension: ``axiom''
;; and ``spad''.  The former is for running arbitrary code in a
;; running axiom process, but both are also declared as tangle'able
;; languages.  They correspond to ``axiom-input-mode'' and
;; ``axiom-spad-mode'' in the axiom-environment system, respectively.

;; There are two extra header options (non-standard org-babel options)
;; for org-babel ``#+BEGIN_SRC axiom'' source code blocks:-
;;
;;   :block-read <yes/no>      (defaults to no)
;;   :show-prompt <yes/no>     (defaults to yes)
;;
;; The block-read option forces ob-axiom to send the entire code block
;; to the running axiom process via a temporary file.  This allows
;; ``pile mode'' axiom source code to be handled correctly.  Otherwise
;; ob-axiom sends each line of the code block individually to the
;; axiom process for interpretation.

;; The show-prompt option allows to enable or inhibit the display of
;; the axiom REPL prompt on a block-by-block basis.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(require 'cl-lib)

(require 'axiom-environment)

;; Header arguments
(defconst org-babel-header-args:axiom
  '((block-read (no yes))
    (show-prompt (no yes))))

(defvar org-babel-default-header-args:axiom
  '((:session . "Axiom Org-Babel Session")
    (:block-read . "no")
    (:show-prompt . "yes")))

;; File extension for Axiom Input files
(add-to-list 'org-babel-tangle-lang-exts '("axiom" . "input"))

(add-to-list 'org-babel-tangle-lang-exts '("spad" . "spad"))

;; Configure org editing options
(add-to-list 'org-src-lang-modes '("axiom" . axiom-input))

(add-to-list 'org-src-lang-modes '("spad" . axiom-spad))

;;; Internal helper functions
;;;
(defun org-babel-axiom--starify-name (str)
  "Ensure valid process buffer name by wrapping with asterisks if necessary."
  (let ((name str))
    (unless (eql (aref str 0) ?*)
      (setq name (concat "*" name)))
    (unless (eql (aref str (1- (length str))) ?*)
      (setq name (concat name "*")))
    name))

;;; Org framework functions -- functions called by Org-mode
;;;
(defun org-babel-axiom-initiate-session (session params)
  "Start an Axiom session for use by org-babel."
  ;;(message "org-babel-axiom-initiate-session\n %S\n %S" session params)
  (unless (string= session "none")
    (let ((session-name (org-babel-axiom--starify-name session)))
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
  (let ((vars (cl-mapcan (lambda (param)
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
  (let ((session (org-babel-axiom-initiate-session
                  (cdr (assoc :session params)) params))
        (block-read (cdr (assoc :block-read params))))
    (if (equal block-read "yes")
        (org-babel-axiom--execute-by-block-read session body params)
      (org-babel-axiom--execute-line-by-line session body params))))

(defun org-babel-axiom--execute-by-block-read (session body params)
  (let ((show-prompt (cdr (assoc :show-prompt params)))
        (tmp-filename (make-temp-file "axiom" nil ".input")))
    (with-temp-buffer
      (insert (org-babel-expand-body:axiom body params))
      (write-region (point-min) (point-max) tmp-filename))
    (let ((axiom-process-buffer-name session)) ; dynamic binding
      (with-axiom-process-query-buffer
       (axiom-process-redirect-send-command
        (format ")read %s" tmp-filename) (current-buffer)
        nil nil t nil (equal show-prompt "yes"))
       (let ((delete-trailing-lines t)) ; dynamic binding
         (delete-trailing-whitespace))
       (buffer-substring (point-min) (point-max))))))

(defun org-babel-axiom--execute-line-by-line (session body params)
  (let ((show-prompt (cdr (assoc :show-prompt params)))
        (lines (split-string (org-babel-expand-body:axiom body params) "\n"))
        (axiom-process-buffer-name session)) ; dynamic binding
    (with-axiom-process-query-buffer
     (dolist (line lines)
       (beginning-of-line)
       (unless (string-match "^[[:space:]]*$" line)
         (axiom-process-redirect-send-command
          line (current-buffer) nil t t t (equal show-prompt "yes"))))
     (let ((delete-trailing-lines t)) ; dynamic binding
       (delete-trailing-whitespace))
     (buffer-substring (point-min) (point-max)))))

(provide 'ob-axiom)

;;; ob-axiom.el ends here
