;;; flycheck-gradle.el --- Flycheck extension for Gradle. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Authors: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/flycheck-gradle
;; Package-Version: 20180121.2251
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (flycheck "0.25"))
;; Keywords: languages gradle

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck extension for Gradle.
;; (with-eval-after-load 'flycheck
;;   (flycheck-gradle-setup))

;;; Code:

(require 'flycheck)
(require 'cl-lib)

;; Compatibility
(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'flycheck-gradle-if-let* #'if-let)
          (defalias 'flycheck-gradle-when-let* #'when-let)
          (function-put #'flycheck-gradle-if-let* 'lisp-indent-function 2)
          (function-put #'flycheck-gradle-when-let* 'lisp-indent-function 1))
      (defalias 'flycheck-gradle-if-let* #'if-let*)
      (defalias 'flycheck-gradle-when-let* #'when-let*))))

;; Customization

(defcustom flycheck-gradle-java-log-level "quiet"
  "The log level gradle should use.

This log level should match an actual gradle log level.

e.g. warn, info, or a custom log level.

Warn should be used to check for warnings but isn't available in gradle
versions below 3 so it's safer choice to use error."
  :type 'string
  :group 'flycheck)

(defcustom flycheck-gradle-kotlin-log-level "quiet"
  "The log level gradle should use.

This log level should match an actual gradle log level.

e.g. warn, info, or a custom log level.

Warn should be used to check for warnings but isn't available in gradle
versions below 3 so it's safer choice to use error."
  :type 'string
  :group 'flycheck)

;;; Flycheck
(defvar flycheck-gradle-modes '(java-mode kotlin-mode)
  "A list of modes for use with `flycheck-gradle'.")

(flycheck-def-executable-var gradle "gradle")

(flycheck-def-option-var flycheck-gradle-extra-flags nil gradle
  "Extra flags prepended to arguments of gradle."
  :type '(repeat (string :tag "Flags"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker gradle-kotlin
  "Flycheck plugin for for Gradle."
  :command ("./gradlew"
            (eval (flycheck-gradle--warm-or-cold-build))
            (eval (flycheck-gradle--log-level))
            "--console"
            "plain"
            (eval flycheck-gradle-extra-flags))
  :error-patterns
  (;; e: /kotlin/MainActivity.kt: (10, 46): Expecting ')'
   (error line-start "e: " (file-name) ": (" line ", " column "): "
          (message) line-end)
   ;; w: /kotlin/MainActivity.kt: (12, 13): Variable 'a' is never used
   (warning line-start "w: " (file-name) ": (" line ", " column "): "
            (message) line-end))
  :modes (kotlin-mode)
  :predicate
  (lambda ()
    (funcall #'flycheck-gradle--gradle-available-p))
  :working-directory
  (lambda (checker)
    (flycheck-gradle--find-gradle-project-directory checker)))

(flycheck-define-checker gradle-java
  "Flycheck plugin for for Gradle."
  :command ("./gradlew"
            (eval (flycheck-gradle--warm-or-cold-build))
            (eval (flycheck-gradle--log-level))
            "--console"
            "plain"
            (eval flycheck-gradle-extra-flags))
  :error-patterns
  (;; /java/MainActivity.java:11: error: ';' expected setContentView(R.layout.activity_main)
   (error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes (java-mode)
  :predicate
  (lambda ()
    (funcall #'flycheck-gradle--gradle-available-p))
  :working-directory
  (lambda (checker)
    (flycheck-gradle--find-gradle-project-directory checker)))

;;;###autoload
(defun flycheck-gradle-setup ()
  "Setup Flycheck for Gradle."
  (interactive)
  (add-hook 'flycheck-before-syntax-check-hook
            #'flycheck-gradle--set-flychecker-executable)

  (unless (memq 'gradle-java flycheck-checkers)
    (add-to-list 'flycheck-checkers 'gradle-java)
    (if (memq 'meghanada-live flycheck-checkers)
        ;; `flycheck-gradle-java' checker will go first.
        (flycheck-add-next-checker 'gradle-java 'meghanada-live)
      (with-eval-after-load 'meghanada
        ;; `flycheck-java' will go first.
        (flycheck-add-next-checker 'meghanada-live 'gradle-java))))

  (unless (memq 'gradle-kotlin flycheck-checkers)
    (add-to-list 'flycheck-checkers 'gradle-kotlin)
    (if (memq 'kotlin-ktlint flycheck-checkers)
        ;; `flycheck-gradle-kotlin' checker will go first.
        (flycheck-add-next-checker 'gradle-kotlin 'kotlin-ktlint)
      (with-eval-after-load 'flycheck-kotlin
        ;; `flycheck-kotlin' will go first.
        (flycheck-add-next-checker 'kotlin-ktlint 'gradle-kotlin)))))

(defun flycheck-gradle--log-level ()
  "Return default LOG level for gradle."
  (if (eq major-mode 'java-mode)
      (format "-%s" flycheck-gradle-java-log-level)
    (format "-%s" flycheck-gradle-kotlin-log-level)))

(defun flycheck-gradle--warm-or-cold-build ()
  "Return whether or not gradle should be ran with clean."
  (if (flycheck-has-current-errors-p 'error)
      '("build")
    '("clean" "build")))

(defun flycheck-gradle--gradle-available-p ()
  "Return whether or not current buffer is part of a Gradle project."
  (flycheck-gradle--find-build-gradle-file))

(defun flycheck-gradle--find-gradle-project-directory (&optional _checker)
  "Return directory containing project-related gradle files or nil."
  (or
   (locate-dominating-file buffer-file-name "gradlew")
   (locate-dominating-file buffer-file-name "settings.gradle")
   (locate-dominating-file buffer-file-name "build.gradle")))

(defun flycheck-gradle--find-build-gradle-file ()
  "Return whether or not a build.gradle file can be found.

We use the presence of a build.gradle file to infer that this project is
a gradle project."
  (locate-dominating-file buffer-file-name "build.gradle"))

(defun flycheck-gradle--set-flychecker-executable ()
  "Set `flycheck-gradle' executable according to gradle location."
  (when (and (memq major-mode flycheck-gradle-modes)
             (flycheck-gradle--gradle-available-p))
    (flycheck-gradle-if-let*
        ((gradlew-path (flycheck-gradle--find-gradlew-executable))
         (gradlew-expanded-path (expand-file-name gradlew-path)))
        (progn
          (setq flycheck-gradle-java-executable gradlew-expanded-path)
          (setq flycheck-gradle-kotlin-executable gradlew-expanded-path))
      (setq flycheck-gradle-java-executable "gradle")
      (setq flycheck-gradle-kotlin-executable "gradle"))))

(defun flycheck-gradle--find-gradlew-executable ()
  "Return path containing gradlew, if it exists."
  (flycheck-gradle-when-let*
      ((path (locate-dominating-file buffer-file-name "gradlew")))
    (concat path "gradlew")))

(provide 'flycheck-gradle)
;;; flycheck-gradle.el ends here
