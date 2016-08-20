;;; helm-pass.el --- helm interface of pass, the standard Unix password manager

;; Copyright (C) 2016 J. Alexander Branham

;; Author: J. Alexander Branham <branham@utexas.edu>
;; Maintainer: J. Alexander Branham <branham@utexas.edu>
;; URL: https://github.com/jabranham/helm-pass
;; Package-Version: 20160815.1758
;; Version: 0.1
;; Package-Requires: ((helm "0") (password-store "0"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; Emacs helm interface for pass, the standard Unix password manager

;; Usage:

;; (require 'helm-pass)
;;

;;; Code:

(require 'helm)
(require 'password-store)

(defgroup helm-pass nil
  "Emacs helm interface for helm-pass"
  :group 'helm)

(defvar helm-source-pass
  (helm-build-sync-source "pass functions with helm"
    :candidates #'password-store-list
    :action '(("Copy password to clipboard" . password-store-copy)
              ("Edit entry" . password-store-edit)
              ("Browse url of entry" . password-store-url))))

;;;###autoload
(defun helm-pass ()
  "Helm interface for pass"
  (interactive)
  (helm :sources 'helm-source-pass
        :buffer "*helm-pass*"))

(provide 'helm-pass)
;;; helm-pass.el ends here
