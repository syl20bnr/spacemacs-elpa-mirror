;;; projectile-hanami.el --- Minor mode for Hanami projects based on projectile

;; copyright (c) 2016 Arjan van der Gaag

;; Author:           Arjan van der Gaag
;; URL:              https://github.com/avdgaag/projectile-hanami
;; Package-Version: 20160505.1311
;; Version:          0.1.0
;; Keywords:         hanami, ruby, projectile
;; Package-Requires: ((emacs "24.3") (projectile "0.12.0") (rake "0.3.2") (inf-ruby "2.2.6"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Projectile Hanami is an Emacs minor mode, based on Projectile, for
;; navigating Hanami projects.  With Projectile Hanami, you can:
;;
;; * navigate through entities, repositories, actions, views and templates are
;;   different apps in your project;
;; * run `hanami server`
;; * run `hanami console`
;; * run Rake tasks
;;
;; Projectile Hanami is based on Projectile Rails, but is not a complete
;; port of all its functionality.

;;; Code:

(require 'projectile)
(require 'rake)
(require 'inf-ruby)
(require 'cl-lib)

(defgroup projectile-hanami nil
  "Hanami mode based on Projectile"
  :prefix "projectile-hanami-"
  :group 'projectile)

(defcustom projectile-hanami-keymap-prefix (kbd "C-c ;")
  "`projectile-hanami-mode' keymap prefix."
  :group 'projectile-hanami
  :type 'string)

(defcustom projectile-hanami-server-buffer-name "*projectile-hanami-server*"
  "Name for the buffer running `hanami server`."
  :group 'projectile-hanami
  :type 'string)

(defcustom projectile-hanami-cmd "bundle exec hanami"
  "The command to run Hanami."
  :group 'projectile-hanami
  :type 'string)

(defcustom projectile-hanami-generate-filepath-re
  "^\\s-+\\(?:create\\|identical\\|exists\\|conflict\\|skip\\|prepend\\)\\s-+\\(.+\\)$"
  "The regex used to find file paths in `projectile-hanami-generate-mode`."
  :group 'projectile-hanami
  :type 'regexp)

(define-derived-mode projectile-hanami-generate-mode compilation-mode "Projectile Hanami Generate"
  "Mode for output of hanami generate."
  (add-hook 'compilation-finish-functions 'projectile-hanami--generate-buffer-make-buttons nil t)
  (projectile-hanami-mode +1))

(defun projectile-hanami-generate-ff (button)
  "Open the file from BUTTON relative to the project path."
  (find-file (projectile-expand-root (button-label button))))

(defun projectile-hanami--generate-buffer-make-buttons (buffer exit-code)
  "Hook method to make buttons out of filepaths in BUFFER ignoring EXIT-CODE."
  (with-current-buffer buffer
    (goto-char 0)
    (while (re-search-forward projectile-hanami-generate-filepath-re (max-char) t)
      (make-button
       (match-beginning 1)
       (match-end 1)
       'action
       'projectile-hanami-generate-ff
       'follow-link
       t))))

(defun projectile-hanami-root ()
  "Find Hanami root directory or nil."
  (ignore-errors
    (let ((root (projectile-locate-dominating-file default-directory "Gemfile")))
      (when (file-exists-p (expand-file-name ".hanamirc" root))
        root))))

(defmacro projectile-hanami-with-root (body-form)
  "Execute BODY-FORM from the context of the Hanami project root directory."
  `(let ((default-directory (projectile-hanami-root)))
     ,body-form))

(defun projectile-hanami-number-of-string-matches ()
  "The total number of capture groups found in `match-data`."
  (/ (- (length (match-data)) 2) 2))

(defun projectile-hanami-goto-file (path)
  "Find PATH relative to the Hanami project root directory."
  (find-file (projectile-expand-root path)))

(defun projectile-hanami-concat-string-matches (re str sep)
  "Join together all captured groups in RE after matching STR using SEP."
  (mapconcat (function (lambda (n) (match-string n str)))
             (number-sequence 1 (projectile-hanami-number-of-string-matches))
             sep))

(defun projectile-hanami-choices (dirs)
  "Use `projectile-dir-files' function to find files in directories.
The DIRS is list of lists consisting of a directory path and regexp to filter
files from that directory.  Returns a hash table with keys being short names and
values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (cl-loop for (dir re) in dirs do
             (cl-loop for file in (projectile-dir-files (projectile-expand-root dir)) do
                      (when (string-match re file)
                        (puthash (projectile-hanami-concat-string-matches re file "/") file hash))))
    hash))

(defun projectile-hanami-hash-keys (hash)
  "Extract keys from HASH."
  (if (boundp 'hash-table-keys)
      (hash-table-keys hash)
    (let (keys)
      (maphash (lambda (key value) (setq keys (cons key keys))) hash)
      keys)))

(defmacro projectile-hanami-find-resource (prompt dirs)
  "Use PROMPT to present files from DIRS to the user using `projectile-completing-read'."
  `(let* ((choices (projectile-hanami-choices ,dirs))
          (filename (or
                     (projectile-completing-read ,prompt (projectile-hanami-hash-keys choices))
                     (user-error "The completion system you're using does not allow inputting arbitrary value")))
          (filepath (gethash filename choices)))
     (if filepath
         (projectile-hanami-goto-file filepath))))

(defun projectile-hanami-find-initializer ()
  "Find a Ruby file in the initializers directory."
  (interactive)
  (projectile-hanami-find-resource
   "initializer: "
   '(("config/initializers/" "/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-lib ()
  "Find a Ruby file in the project lib folder."
  (interactive)
  (projectile-hanami-find-resource
   "lib: "
   '(("lib/" "/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-controller ()
  "Find a controller file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "controller: "
   '(("apps/" "/\\(.+\\)/controllers/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-view ()
  "Find a view file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "view: "
   '(("apps/" "/\\(.+\\)/views/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-template ()
  "Find a template file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "template: "
   '(("apps/" "/\\(.+\\)/templates/\\(.+\\..+\\..+\\)$"))))

(defun projectile-hanami-find-presenter ()
  "Find a presenter file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "presenter: "
   '(("apps/" "/\\(.+\\)/presenters/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-javascript ()
  "Find a javascript file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "javascript: "
   '(("apps/" "/\\(.+\\)/assets/javascripts/\\(.+\\.js.*\\)$"))))

(defun projectile-hanami-find-stylesheet ()
  "Find a stylesheet file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "stylesheet: "
   '(("apps/" "/\\(.+\\)/assets/stylesheets/\\(.+\\.css.*\\)$"))))

(defun projectile-hanami-find-config ()
  "Find a config file in one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "config: "
   '(("apps/" "/\\(.+\\)/config/\\(.+\\)\\.rb$")
     ("lib/config/" "/\\(.+\\)\\.rb$")
     ("config/" "/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-routes ()
  "Find the routes file for one of the project's apps."
  (interactive)
  (projectile-hanami-find-resource
   "app: "
   '(("apps/" "/\\(.+\\)/config/routes.rb$"))))

(defun projectile-hanami-find-entity ()
  "Find an entity in the project."
  (interactive)
  (projectile-hanami-find-resource
   "entity: "
   '(("lib/" "/\\(.+\\)/entities/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-repository ()
  "Find a repository in the project."
  (interactive)
  (projectile-hanami-find-resource
   "repository: "
   '(("lib/" "/\\(.+\\)/repositories/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-mailer ()
  "Find a mailer in the project."
  (interactive)
  (projectile-hanami-find-resource
   "mailer: "
   '(("lib/" "/\\(.+\\)/mailers/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-mailer-template ()
  "Find a mailer template in the project."
  (interactive)
  (projectile-hanami-find-resource
   "mailer template: "
   '(("lib/" "/\\(.+\\)/mailers/template/\\(.+\\)\\.rb$"))))

(defun projectile-hanami-find-application ()
  "Find an application in the project."
  (interactive)
  (projectile-hanami-find-resource
   "application: "
   '(("apps/" "/\\(.+\\)/application.rb$"))))

(defun projectile-hanami-find-env ()
  "Find a .env file in the project root."
  (interactive)
  (projectile-hanami-find-resource
   ".env:"
   '(("./" "\\(\\.env.*\\)$"))))

(defun projectile-hanami-filter (func lst)
  "Use FUNC to filter out elements from LST."
  (delq nil (mapcar (lambda (x) (and (funcall func x) x)) lst)))

(defun projectile-hanami-find-related-file (filename next-type)
  "Find a file related to FILENAME in the NEXT-TYPE directory."
  (let* ((re "\\(.+\\)/apps/\\([^/]+\\)/\\([^/]+\\)/\\(.+\\)/\\([^./]+\\)\\.\\(.+\\)$")
         (_match (string-match re filename))
         (path (match-string 1 filename))
         (app (match-string 2 filename))
         (type (match-string 3 filename))
         (resource (match-string 4 filename))
         (name (match-string 5 filename))
         (new-path-re (concat "apps/" app "/" next-type "/" resource "/" name "\\..+"))
         (matching-files
          (projectile-hanami-filter
           (lambda (x) (string-match-p new-path-re x))
           (projectile-dir-files (projectile-project-root)))))
    (if (and app type resource name)
        (if (= 1 (length matching-files))
            (projectile-hanami-goto-file (concat path "/"  (car matching-files)))
          (user-error "Expected exactly one matching file")))))

(defun projectile-hanami-goto-related-view ()
  "Find the view for the current template or controller action."
  (interactive)
  (projectile-hanami-find-related-file (buffer-file-name) "views"))

(defun projectile-hanami-goto-related-controller ()
  "Find the controller for the current template or view."
  (interactive)
  (projectile-hanami-find-related-file (buffer-file-name) "controllers"))

(defun projectile-hanami-goto-related-template ()
  "Find the template for the current controller action or view."
  (interactive)
  (projectile-hanami-find-related-file (buffer-file-name) "templates"))

(defun projectile-hanami-goto-repository ()
  "Find the repository for the current entity."
  (interactive)
  (let* ((filename (buffer-file-name))
         (repository-filename
          (replace-regexp-in-string "entities/\\(.+\\).rb$"
                                    "repositories/\\1_repository.rb"
                                    filename)))
    (projectile-hanami-goto-file repository-filename)))

(defun projectile-hanami-goto-entity ()
  "Find the entity for the current repository."
  (interactive)
  (let* ((filename (buffer-file-name))
         (entity-filename (replace-regexp-in-string "repositories/\\(.+\\)_repository\\.rb$" "entities/\\1.rb" filename)))
    (projectile-hanami-goto-file entity-filename)))

(defun projectile-hanami-goto-gemfile ()
  "Find the project's Gemfile."
  (interactive)
  (projectile-hanami-goto-file "Gemfile"))

(defun projectile-hanami-goto-mapping ()
  "Find the project's mapping file."
  (interactive)
  (projectile-hanami-goto-file "lib/config/mapping.rb"))

(defun projectile-hanami-goto-rakefile ()
  "Find the project's Rakefile."
  (interactive)
  (projectile-hanami-goto-file "Rakefile"))

(defun projectile-hanami-rake (arg)
  "Run a project Rake task ARG."
  (interactive "P")
  (rake arg))

(defun projectile-hanami-server ()
  "Run hanami server command."
  (interactive)
  (projectile-hanami-with-root
   (if (member projectile-hanami-server-buffer-name (mapcar 'buffer-name (buffer-list)))
       (switch-to-buffer projectile-hanami-server-buffer-name)
     (compile (concat projectile-hanami-cmd " server")))))

(defun projectile-hanami-console ()
  "Run hanami console command."
  (interactive)
  (projectile-hanami-with-root
   (with-current-buffer
       (run-ruby (concat projectile-hanami-cmd " console"))
     (projectile-hanami-mode +1))))

(defun projectile-hanami-generate ()
  "Run hanami generate command."
  (interactive)
  (projectile-hanami-with-root
   (let ((cmd (concat projectile-hanami-cmd " generate ")))
     (compile
      (concat cmd
              (read-from-minibuffer cmd)) 'projectile-hanami-generate-mode))))

(defvar projectile-hanami-mode-run-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'projectile-hanami-rake)
    (define-key map (kbd "c") 'projectile-hanami-console)
    (define-key map (kbd "s") 'projectile-hanami-server)
    (define-key map (kbd "g") 'projectile-hanami-generate)
    map)
  "Keymap for run-commands in `projectile-hanami-mode`.")
(fset 'projectile-hanami-mode-run-map projectile-hanami-mode-run-map)

(defvar projectile-hanami-mode-goto-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'projectile-hanami-goto-rakefile)
    (define-key map (kbd "g") 'projectile-hanami-goto-gemfile)
    (define-key map (kbd "m") 'projectile-hanami-goto-mapping)
    map)
  "Keymap for goto-commands in `projectile-hanami-mode`.")
(fset 'projectile-hanami-mode-goto-map projectile-hanami-mode-goto-map)

(defvar projectile-hanami-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'projectile-hanami-find-initializer)
    (define-key map (kbd "l") 'projectile-hanami-find-lib)
    (define-key map (kbd "c") 'projectile-hanami-find-controller)
    (define-key map (kbd "v") 'projectile-hanami-find-view)
    (define-key map (kbd ".") 'projectile-hanami-find-env)
    (define-key map (kbd "t") 'projectile-hanami-find-template)
    (define-key map (kbd "p") 'projectile-hanami-find-presenter)
    (define-key map (kbd "s") 'projectile-hanami-find-stylesheet)
    (define-key map (kbd "j") 'projectile-hanami-find-javascript)
    (define-key map (kbd "o") 'projectile-hanami-find-config)
    (define-key map (kbd "u") 'projectile-hanami-find-routes)
    (define-key map (kbd "e") 'projectile-hanami-find-entity)
    (define-key map (kbd "r") 'projectile-hanami-find-repository)
    (define-key map (kbd "m") 'projectile-hanami-find-mailer)
    (define-key map (kbd "T") 'projectile-hanami-find-mailer-template)
    (define-key map (kbd "a") 'projectile-hanami-find-application)
    (define-key map (kbd "C") 'projectile-hanami-goto-related-controller)
    (define-key map (kbd "T") 'projectile-hanami-goto-related-template)
    (define-key map (kbd "V") 'projectile-hanami-goto-related-view)
    (define-key map (kbd "E") 'projectile-hanami-goto-entity)
    (define-key map (kbd "R") 'projectile-hanami-goto-repository)
    (define-key map (kbd "!") 'projectile-hanami-mode-run-map)
    (define-key map (kbd "g") 'projectile-hanami-mode-goto-map)
    map)
  "Keymap after `projectile-hanami-keymap-prefix`.")

(defvar projectile-hanami-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-hanami-keymap-prefix projectile-hanami-mode-command-map)
    map)
  "Keymap for `projectile-hanami-mode`.")

;;;###autoload
(defun projectile-hanami-applicable-p ()
  "Detect if the current `default-directory` is part of a Hanami project."
  (projectile-hanami-root))

;;;###autoload
(define-minor-mode projectile-hanami-mode
  "Hanami mode based on projectile"
  :init-value nil
  :lighter " Hanami")

(provide 'projectile-hanami)
;;; projectile-hanami.el ends here
