;;; lsp-javacomp.el --- Java support for lsp-mode

;; Version: 1.0
;; Package-Version: 20171005.659
;; Package-Requires: ((emacs "25.1") (lsp-mode "2.0"))
;; Keywords: java
;; URL: https://github.com/tigersoldier/lsp-javacomp

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

;; A lsp-mode client that provides Java code-completion and other IDE
;; features for Emacs. It's backed by JavaComp.

;; To use it, add the code below to your .emacs file:

;;   (with-eval-after-load 'lsp-mode
;;     (require 'lsp-javacomp))
;;   (add-hook 'java-mode-hook 'lsp-mode)

;;; Code:

(require 'cc-mode)
(require 'lsp-mode)

(defcustom lsp-javacomp-server-jar "~/bin/JavaComp_deploy.jar"
  "Name of JavaComp jar file to run."
  :type '(choice (const nil) string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-java-executable "java"
  "Name or path of the java executable binary file."
  :type '(choice (const nil) string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-java-options nil
  "List of command line options to be pased to java command."
  :type '(set string)
  :group 'lsp-javacomp)

(defun lsp-javacomp--command ()
  "Return a list of the command and arguments to launch the JavaComp server."
  `( ,lsp-javacomp-java-executable
     ,@lsp-javacomp-java-options
     "-jar"
     ,(expand-file-name lsp-javacomp-server-jar)))

(defun lsp-javacomp--get-root ()
  "Retrieves the root directory of the java project root if available.

The current directory is assumed to be the java project’s root otherwise."
  (cond
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("pom.xml" "build.gradle" ".project" "WORKSPACE")))
        (or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
            default-directory)))))

(lsp-define-stdio-client 'java-mode "javacomp" 'stdio #'lsp-javacomp--get-root
			 "JavaComp Server"
			 (lsp-javacomp--command)
			 :ignore-regexps '("^SLF4J: "
					   "^Listening for transport dt_socket at address: "))

(provide 'lsp-javacomp)
;;; lsp-javacomp.el ends here
