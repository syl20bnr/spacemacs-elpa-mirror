;;; sly-company.el --- sly completion backend for company mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2009-2018  Ole Arndt, João Távora
;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, lisp, abbrev
;; Package-Version: 20180610.1052
;; Version: 1.0
;; Package-Requires: ((sly "1.0.0-alpha") (company "0.7") (emacs "24.3"))
;;
;;; Commentary:
;;;
;; Used to be company-mode Emacs completion backend for SLY, a Common
;; Lisp IDE.  Newer SLY doesn't need it and relies just on the
;; 'company-capf' built-in backend.

(require 'company)
(require 'company-capf)

(defalias 'sly-company #'company-capf)

;;;###autoload
(define-minor-mode sly-company-mode
  "Minor mode for using `company-mode' with SLY."
  nil nil nil
  (add-to-list 'company-backends 'sly-company nil
               (lambda (a b)
                 (and (symbolp b)
                      (equal (symbol-function a) b))))
  (if sly-company-mode
      (company-mode 1)))


;;; sly-company.el ends here
