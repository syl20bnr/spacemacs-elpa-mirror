;;; latest-clojure-libraries.el --- Clojure dependency resolver

;; Copyright 2013 Adam Clements

;; Plugin:  Latest clojure libraries dependency resolver
;; Author:  Adam Clements <adam.clements@gmail.com>
;; URL:     http://github.com/AdamClements/latest-clojure-libraries/
;; Package-Version: 20140314.617
;; Version: 0.6
;; License: Eclipse Public License

;;; Commentary:

;; Looks up the latest version of clojure libraries on clojars/maven
;; and automatically populates the buffer with the appropriate
;; dependency vector. Optionally uses pomegranate to load the
;; dependency directly into your running repl.

;; Then, use M-x latest-clojure-libraries-insert-dependency

;;; Code:

(require 'thingatpt)

(setq lcl/nrepl-enabled? (or (require 'nrepl-client nil t)
                             (require 'nrepl nil t)
                             f))

(defun lcl/get-version-vec (s)
  (when (string-match "[[].+? \".+?\"[]]" s)
    (match-string 0 s)))

(defun lcl/get-latest-clojure-library (package)
  (lcl/get-version-vec
   (shell-command-to-string
    (format "lein ancient latest %s" package package))))

(defun lcl/add-clojure-dependency (spec)
  (when lcl/nrepl-enabled?
    (nrepl-send-string
     (concat "(require 'cemerick.pomegranate)"
             "(cemerick.pomegranate/add-dependencies"
             "  :coordinates '[" spec "]"
             "  :repositories (merge cemerick.pomegranate.aether/maven-central"
             "                       {\"clojars\" \"http://clojars.org/repo\"}))")
     (lambda (out) (message (print out))))))

(defun lcl/nrepl-available ()
  (and lcl/nrepl-enabled?
       (fboundp 'nrepl-connection-buffers)
       (nrepl-connection-buffers)))

;;;###autoload
(defun latest-clojure-libraries-insert-dependency (package inject)
  "Insert dependency for PACKAGE and optionally INJECT it into nrepl classpath."
  (interactive (list (read-string "Library name: ")
                     (when (lcl/nrepl-available)
                       (y-or-n-p "Add to running nrepl's classpath (requires cemerick.pomegranate)?"))))
  (message "Searching...")
  (let ((spec (lcl/get-latest-clojure-library package)))
    (if spec
        (progn (message (concat "Found " spec))
               (insert spec)
               (when inject (lcl/add-clojure-dependency spec)))
      (error "Can't find %s. Check that you have the correct spelling and you have leiningen and the lein-ancient plugin set up." package))))

;;;###autoload
(defun latest-clojure-libraries-update-dependency-version (inject)
  "Update dependency version number for package at point. Point
must be at the name of the package, not the version number."
  (interactive (list (when (lcl/nrepl-available)
                       (y-or-n-p "Add to running nrepl's classpath (requires cemerick.pomegranate)?"))))
  (let ((package-name (symbol-at-point)))
    (if package-name
        (save-excursion
          (backward-up-list)
          (latest-clojure-libraries-insert-dependency package-name inject)
          (kill-sexp)))))

(provide 'latest-clojure-libraries)
;;; latest-clojure-libraries.el ends here
