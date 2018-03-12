;;; flycheck-yang.el --- YANG flycheck checker
;; Copyright (c) 2016 Andrew Fort

;; Author: Andrew Fort (@andaru)
;; Version: 0.0.1
;; Package-Version: 20180312.1131
;; Package-Requires: ((yang-mode "0.9.4") (flycheck "0.18"))

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package configures provides YANG syntax checking via flycheck
;; in Emacs using the pyang YANG parser[1].
;;
;; [1] https://github.com/mbj4668/pyang

;;;; Setup
;;
;; Add this to your Emacs configuration:
;;
;;   ;; autoload yang-mode for .yang files
;;   (autoload 'yang-mode "yang-mode" "Major mode for editing YANG modules." t)
;;   (add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))
;;
;;   ;; enable the YANG checker after flycheck loads
;;   (eval-after-load 'flycheck '(require 'flycheck-yang))
;;
;;   ;; ensure flycheck-mode is enabled in yang mode
;;   (add-hook 'yang-mode-hook
;;     (lambda () (flycheck-mode)))

;;; Code:

(require 'yang-mode)
(require 'flycheck)

(defgroup flycheck-yang-pyang nil
  "Support for Flycheck in YANG via pyang"
  :group 'flycheck-yang)

(defcustom flycheck-yang-pyang-verbose nil
  "Validate the module(s) according to IETF rules."
  :type 'boolean
  :safe  #'booleanp
  :group 'flycheck-yang-pyang)

(defcustom flycheck-yang-pyang-ietf nil
  "Enable ietf output from pyang."
  :type 'boolean
  :safe #'booleanp
  :group 'flycheck-yang-pyang)

(defcustom flycheck-yang-path ""
  ":-separated search path for yin and yang modules"
  :type 'string
  :safe #'stringp
  :group 'flycheck-yang-pyang)

(defcustom flycheck-yang-lint nil
  "Enable --lint"
  :type 'bool
  :safe #'booleanp
  :group 'flycheck-yang-pyang)

(flycheck-define-checker yang-pyang
                         "A YANG syntax checker using the pyang parser."
                         :command ("pyang"
				   "--max-identifier-length=60"
				   (option-flag "-V" flycheck-yang-pyang-verbose)
				   (option-flag "--ietf" flycheck-yang-pyang-ietf)
				   (option-flag "--lint" flycheck-yang-lint)
				   (option "-p" flycheck-yang-path)
				   source)
                         :error-patterns ((error line-start (file-name) ":"
                                                 line ": " "error: " (message) line-end)
					  (warning line-start (file-name) ":"
                                                 line ": " "warning: " (message) line-end))
                         :modes yang-mode
			 :error-filter
			 (lambda (errors)
			   (-> errors
			       flycheck-dedent-error-messages
			       flycheck-sanitize-errors)))


(add-to-list 'flycheck-checkers 'yang-pyang)

(provide 'flycheck-yang)

;;; flycheck-yang.el ends here
