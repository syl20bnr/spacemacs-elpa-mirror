;;; package-safe-delete.el --- Safely delete package.el packages -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/package-safe-delete
;; Package-Version: 20150116.1607
;; Version: 0.1.7
;; Package-Requires: ((emacs "24") (epl "0.7-cvs"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Delete package.el packages safely, without leaving unresolved dependencies.
;;
;; - To delete a package:
;;     M-x package-safe-delete
;; - To delete a list of packages:
;;     (package-safe-delete-packages '(package1 package2 ...))
;; - To delete a package and all its dependencies not required by other
;;   installed packages:
;;     M-x package-safe-delete-recursively
;; - To delete a list of packages recursively:
;;     (package-safe-delete-packages-recursively '(package1 package2 ...))
;; - To delete all packages:
;;     M-x package-safe-delete-all
;;
;; To prevent a package from being deleted, even by `package-safe-delete-all',
;; add its name to `package-safe-delete-required-packages':
;;     (add-to-list 'package-safe-delete-required-packages 'package1)

;;; Code:
;; Ensure generalized variables are available.
(eval-when-compile
  (unless (require 'gv nil t)
    (require 'cl)))
(require 'epl)

(defgroup package-safe-delete nil
  "Safely remove package.el packages."
  :group 'package)

(defcustom package-safe-delete-required-packages '()
  "List of explicitly required packages.
Each element is a package name, as a symbol.

The packages in this list are treated as required by a dummy package, and thus
are never deleted."
  :type '(repeat symbol)
  :group 'package-safe-delete)

(defun package-safe-delete--installed-packages ()
  "Create a list of installed packages.
Elements are of the form (PACKAGE-NAME . PACKAGE-DESCRIPTOR)."
  (mapcar (lambda (package)
            (cons (epl-package-name package) package))
          (epl-installed-packages)))

(defun package-safe-delete--list-to-hashtable (list)
  "Convert a LIST based set to a hashtable based set."
  (let ((result (make-hash-table :test #'eq)))
    (dolist (elt list)
      (puthash elt t result))
    result))

(defun package-safe-delete--installed-package-dependencies (installed excluded)
  "Get a dependency tree of the installed packages.
INSTALLED is a list of installed packages as returned by
`package-safe-delete--installed-packages'.
Dependencies of EXCLUDED packages are ignored.

The returned value is a hash table of the form package => list of packages
requiring it."
  (let ((dependencies (make-hash-table :test #'eq)))
    (dolist (package-entry installed)
      (let ((package-name (car package-entry)))
        (unless (memq package-name excluded)
          (dolist (requirement (epl-package-requirements (cdr package-entry)))
            (let ((requirement-name (epl-requirement-name requirement)))
              (when (assq requirement-name installed)
                (push package-name (gethash requirement-name dependencies))))))))
    (let ((required-package-symbol (make-symbol "<required-package>")))
      (dolist (required-package package-safe-delete-required-packages)
        (push required-package-symbol (gethash required-package dependencies))))
    dependencies))

(defun package-safe-delete--delete (packages force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (cond
   ((null packages)
    (message "%s" "Nothing to delete"))
   ((or force
        (yes-or-no-p
         (pcase packages
           (`(,package)
            (format "Delete package `%s'? " package))
           (_
            (format "Delete these packages: %s? "
                    (mapconcat #'symbol-name packages ", "))))))
    (dolist (package packages)
      (mapc #'epl-package-delete (epl-find-installed-packages package))))))

(defun package-safe-delete--prompt-package-name (prompt)
  "Read a package name in the minibuffer.
PROMPT is a string to prompt with."
  (list
   (intern
    (completing-read prompt
                     (mapcar #'epl-package-name (epl-installed-packages))
                     nil
                     t))))

(defun package-safe-delete--ensure-installed (packages)
  "Ensure all PACKAGES are installed.
If at least one is not installed, an error is signaled."
  (dolist (package packages)
    (unless (epl-package-installed-p package)
      (error "Package `%S' is not installed" package))))

(defun package-safe-delete--ensure-no-dependencies (packages dependencies)
  "Ensure no installed package relies on the PACKAGES being deleted.
If any other installed package requires a package in PACKAGES, an error is
signaled.

DEPENDENCIES is a dependency tree as generated by
`package-safe-delete--installed-package-dependencies'."
  (dolist (package packages)
    (pcase (gethash package dependencies)
      (`nil)
      (`(,dependent-package)
       (error "Cannot delete `%S' because it's required by `%S'"
              package
              dependent-package))
      (dependent-packages
       (error "Cannot delete `%S' because it's required by: %s"
              package
              (mapconcat #'symbol-name dependent-packages ", "))))))

;;;###autoload
(defun package-safe-delete-packages (packages &optional force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
None of the PACKAGES are deleted when there's a package depending on one of
them, or if one of the PACKAGES is not installed.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (package-safe-delete--ensure-installed packages)
  (let* ((installed (package-safe-delete--installed-packages))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        packages)))
    (package-safe-delete--ensure-no-dependencies packages dependencies))
  (package-safe-delete--delete packages force))

;;;###autoload
(defun package-safe-delete (package)
  "Delete a PACKAGE.

PACKAGE is a package name symbol.
PACKAGE is not deleted when there are other packages requiring it.
Interactively, prompt for its name."
  (interactive (package-safe-delete--prompt-package-name "Delete package: "))
  (package-safe-delete-packages (list package)))

;;;###autoload
(defun package-safe-delete-packages-recursively (packages &optional force)
  "Delete PACKAGES.
Each of the PACKAGES and all packages required only by the PACKAGES are deleted.

PACKAGES is a list of package name symbols.
None of the PACKAGES are deleted when there's a package depending on one of
them, or if one of the PACKAGES is not installed.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (package-safe-delete--ensure-installed packages)
  (let* ((installed (package-safe-delete--installed-packages))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        packages)))
    (package-safe-delete--ensure-no-dependencies packages dependencies)
    ;; Convert those lists into hash tables because they're less of a hassle to
    ;; modify.
    (maphash (lambda (k v)
               (puthash k (package-safe-delete--list-to-hashtable v) dependencies))
             dependencies)
    (let ((total-packages '()))
      (while packages
        (let ((pending-dependencies '()))
          (dolist (package-name packages)
            (dolist (package (epl-find-installed-packages package-name))
              (dolist (requirement (epl-package-requirements package))
                (let* ((requirement-name (epl-requirement-name requirement))
                       (requirement-bucket (gethash requirement-name dependencies)))
                  (when (or (null requirement-bucket)
                            (progn
                              (remhash package-name requirement-bucket)
                              ;; Was `package' the last package requiring
                              ;; `requirement'?
                              (= 0 (hash-table-count requirement-bucket))))
                    (when (assq requirement-name installed)
                      (push requirement-name pending-dependencies)))))))
          ;; We're done with `packages', handle their direct dependencies now.
          (setq total-packages (append packages total-packages))
          (setq packages pending-dependencies)))
      (setq packages total-packages)))
  (package-safe-delete--delete packages force))

;;;###autoload
(defun package-safe-delete-recursively (package)
  "Recursively delete a PACKAGE.
PACKAGE and all packages required only by it are deleted.

PACKAGE is a package name symbol.
PACKAGE is not deleted when there are other packages requiring it.
Interactively, prompt for its name."
  (interactive (package-safe-delete--prompt-package-name
                "Recursively delete package: "))
  (package-safe-delete-packages-recursively (list package)))

;;;###autoload
(defun package-safe-delete-all (&optional force)
  "Delete all packages not explicitly required.

With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (interactive)
  (let* ((installed (package-safe-delete--installed-packages))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        '()))
         (packages-to-delete '()))
    ;; Collect only those packages not required by the user and not required by
    ;; other packages, `package-safe-delete-packages-recursively' will take care
    ;; of the rest.
    (dolist (package-entry installed)
      (let ((package-name (car package-entry)))
        (when (and (null (gethash package-name dependencies))
                   (null (memq package-name package-safe-delete-required-packages)))
          (push package-name packages-to-delete))))
    (package-safe-delete-packages-recursively packages-to-delete force)))

(provide 'package-safe-delete)
;;; package-safe-delete.el ends here
