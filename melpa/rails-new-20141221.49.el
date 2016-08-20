;;; rails-new.el --- Handy emacs command for generating rails application.

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20141221.49
;; Keywords: rails, ruby
;; URL: https://github.com/cheunghy/rails-new

;;; Commentary:

;; This package provides a handy command:
;; M-x rails-new
;; for generating rails application.

;;; Code:

(defvar rails-new--last-rails-new-command nil
  "This variable hold last user rails new command.")

(defvar rails-new--last-rails-plugin-new-command nil
  "This variable hold last user rails plugin new command.")

(defvar rails-new--last-rails-dir nil
  "This variable hold the dir location of last user created rails app.")

(defvar rails-new--last-rails-plugin-dir nil
  "This variable hold the dir location of last user created rails plugin.")

;; TODO: bug exist.
(defvar rails-new--file-re
  "^\\s-+\\(?:create\\|exist\\|identical\\|conflict\\|new\\|skip\\)\\s-+\\(.+\\)$")

;;;###autoload
(defun rails-new-again ()
  "Retry last rails new command."
  (interactive)
  (rails-new--compile))

;;;###autoload
(defun rails-new (dir &optional ruby template
                      skip-gemfile skip-bundle skip-git
                      skip-keeps skip-active-record skip-turbolinks
                      skip-sprockets skip-spring database
                      js-library skip-js skip-test-unit
                      )
  "Create new rails app."
  (interactive (list (read-directory-name "Directory: ") ;; parameter 1
                     (if (y-or-n-p "Use default ruby executable?")
                         nil
                       (read-file-name "Ruby: ")) ;; parameter 2
                     (if (y-or-n-p "Use template file?")
                         (read-file-name "Template file: ")
                       nil) ;; parameter 3
                     (y-or-n-p "Skip Gemfile?") ;; parameter 4
                     (y-or-n-p "Skip bundle?") ;; parameter 5
                     (y-or-n-p "Skip git?") ;; parameter 6
                     (y-or-n-p "Skip keeps?") ;; parameter 7
                     (y-or-n-p "Skip active record?") ;; parameter 8
                     (y-or-n-p "Skip turbolinks?") ;; parameter 9
                     (y-or-n-p "Skip sprockets?") ;; parameter 10
                     (y-or-n-p "Skip spring?") ;; parameter 11
                     (if (y-or-n-p "Use default sqlite3 database?")
                         nil
                       (completing-read "Database name: "
                                        (list
                                         "mysql" "oracle" "postgresql"
                                         "frontbase" "ibm_db"
                                         "sqlserver" "jdbcmysql" "jdbcsqlite3"
                                         "jdbcpostgresql""jdbc")
                                        nil nil nil nil
                                        "sqlite3")) ;; param 12
                     (if (y-or-n-p "Use default js library(jQuery)?")
                         nil
                       (completing-read "js library: "
                                        (list "prototype")
                                        nil nil nil nil
                                        "jquery")
                       ) ;; parameter 13
                     (y-or-n-p "Skip javascript?") ;; parameter 14
                     (y-or-n-p "Skip test unit?") ;; parameter 15
                     ))

  (let ((rails-new-command
         (with-temp-buffer
           (insert "rails new " dir " ")
           (if ruby (insert "--ruby=" ruby " "))
           (if template (insert "--template=" template " "))
           (if skip-gemfile (insert "--skip-gemfile "))
           (if skip-bundle (insert "--skip-bundle "))
           (if skip-git (insert "--skip-git "))
           (if skip-keeps (insert "--skip-keeps "))
           (if skip-active-record (insert "--skip-active-record "))
           (if skip-turbolinks (insert "--skip-turbolinks "))
           (if skip-sprockets (insert "--skip-sprockets "))
           (if skip-spring (insert "--skip-spring "))
           (if database (insert "--database=" database " "))
           (if js-library (insert "--javascript=" js-library " "))
           (if skip-js (insert "--skip-javascript "))
           (if skip-test-unit (insert "--skip-test-unit"))
           (buffer-string)
           )))
    (setq rails-new--last-rails-dir dir)
    (setq rails-new--last-rails-new-command rails-new-command)
    (rails-new--compile)))

;;;###autoload
(defun rails-plugin-new (directory arguments)
  "Create new rails plugin."
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "rails plugin new ")))
  (let ((rails-plugin-new-command
         (with-temp-buffer
           (insert "rails plugin new " directory " ")
           (insert arguments)
           (buffer-string))))
    (setq rails-new--last-rails-plugin-dir directory)
    (setq rails-new--last-rails-plugin-new-command rails-plugin-new-command)
    (rails-new--plugin-compile)))

(defun rails-new--plugin-compile ()
  (if rails-new--last-rails-plugin-new-command
      (compile rails-new--last-rails-plugin-new-command 'rails-plugin-new-mode)
    (error "Last rails plugin new command is not exist.")))

(defun rails-new--compile ()
  (if rails-new--last-rails-new-command
      (compile rails-new--last-rails-new-command 'rails-new-mode)
    (error "Last rails new command is not exist.")))

(define-derived-mode rails-new-mode compilation-mode
  "Rails new compilation"
  "Mode for rails new command."
  (add-hook 'compilation-filter-hook
            (lambda ()
              (rails-new--apply-ansi-color-and-generate-link
               "rails new")) nil t))

(define-derived-mode rails-plugin-new-mode compilation-mode
  "Rails plugin new compilation"
  "Mode for rails plugin new command."
  (add-hook 'compilation-filter-hook
            (lambda ()
              (rails-new--apply-ansi-color-and-generate-link
               "rails plugin new")) nil t))

(defun rails-new--apply-ansi-color-and-generate-link (link-mode)
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (rails-new--generate-buffer-links (current-buffer) link-mode)
  (read-only-mode))

(defun rails-new--jump-to-file (button)
  (let ((the-file (rails-new--file-exists-p (button-label button))))
    (if (file-directory-p the-file)
        (dired the-file)
      (find-file the-file))))

(defun rails-plugin-new--jump-to-file (button)
  (let ((the-file (rails-plugin-new--file-exists-p (button-label button))))
    (if (file-directory-p the-file)
        (dired the-file)
      (find-file the-file))))

(defun rails-plugin-new--file-exists-p (file)
  (let ((file-name (format "%s/%s" rails-new--last-rails-plugin-dir file)))
    (if (file-exists-p file-name) file-name nil)))

(defun rails-new--file-exists-p (file)
  (let ((file-name (format "%s/%s" rails-new--last-rails-dir file)))
    (if (file-exists-p file-name) file-name nil)))

(defun rails-new--generate-buffer-links (buffer link-mode &optional exit-code)
  (let ((link-function
         (cond ((string= "rails new" link-mode) 'rails-new--file-exists-p)
               ((string= "rails plugin new" link-mode)
                'rails-plugin-new--file-exists-p)))
        (jump-function
         (cond ((string= "rails new" link-mode) 'rails-new--jump-to-file)
               ((string= "rails plugin new" link-mode)
                'rails-plugin-new--jump-to-file))))
    (with-current-buffer buffer
      ;; TODO: Remove button if the file not exist anymore.
      ;; This line doesn't work.
      ;;(remove-text-properties 0 (buffer-end) '(mouse-face nil))
      (goto-char 0)
      (while (re-search-forward rails-new--file-re (max-char) t)
        ;; TODO: Bug exists. this won't run.
        (and
         (funcall
          link-function
          (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
         (make-button
          (match-beginning 1)
          (match-end 1)
          'action
          jump-function
          'follow-link
          t))))))

(provide 'rails-new)
;;; rails-new.el ends here
