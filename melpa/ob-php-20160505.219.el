;;; ob-php.el --- Execute PHP within org-mode blocks.
;; Copyright 2016 stardiviner

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org babel php
;; Package-Version: 20160505.219
;; URL: https://github.com/stardiviner/ob-php
;; Created: 04th May 2016
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute PHP within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-php nil
  "org-mode blocks for PHP."
  :group 'org)

;; todo
(defcustom ob-php:inf-php-buffer "*php*"
  "Default PHP inferior buffer."
  :group 'ob-php
  :type 'string)

;;;###autoload
(defun org-babel-execute:php (body params)
  "org-babel PHP hook."
  ;; todo
  (let* ((cmd (mapconcat 'identity (list "php") " -r ")))
    (org-babel-eval cmd body)
    ))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("php" . php)))

(provide 'ob-php)

;;; ob-php.el ends here
