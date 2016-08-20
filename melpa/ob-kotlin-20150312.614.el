;;; ob-kotlin.el --- org-babel functions for kotlin evaluation

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-kotlin
;; Package-Version: 20150312.614
;; Keywords: org babel kotlin
;; Version: 0.0.1
;; Created: 12th Mar 2015
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; org-babel functions for kotlin evaluation
;;

;;; Code:
(require 'org)
(require 'ob)
(require 'comint)

(defvar org-babel-kotlin-eoe "org-babel-kotlin-eoe")

(defun org-babel-execute:kotlin (body params)
  (let* ((session (cdr (assoc :session params))))
    (ob-kotlin/eval session body)))

(defun ob-kotlin/eval (session body)
  (ob-kotlin/ensure-session session)
  (let* ((tmp (org-babel-temp-file "kotlin-"))
         (load (progn
                 (with-temp-file tmp (insert body))
                 (format ":load %s" tmp)))
         (result (ob-kotlin/eval-in-repl session load)))
    (message (prin1-to-string result))
    (org-babel-chomp (mapconcat 'identity (cdr (cdr (butlast result 2))) ""))))

(defun ob-kotlin/eval-in-repl (session body)
  (let ((buffer (format "*kotlin-%s*" session))
        (eoe (format "%S" org-babel-kotlin-eoe)))
    (org-babel-comint-with-output
        (buffer eoe t body)
      (mapc
       (lambda (line)
         (insert (org-babel-chomp line))
         (sleep-for 0 5)
         (comint-send-input nil t))
       (list body eoe)))))

(defun ob-kotlin/ensure-session (session)
  (unless (org-babel-comint-buffer-livep (format "*kotlin-%s*" session))
    (progn
      (make-comint (format "kotlin-%s" session) "env" nil "kotlinc")
      (ob-kotlin/eval-in-repl session "")
      (sleep-for 0 500))))

(provide 'ob-kotlin)
;;; ob-kotlin.el ends here
