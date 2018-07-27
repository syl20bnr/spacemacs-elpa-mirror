;;; easy-after-load.el --- eval-after-load for all files in a directory

;; Copyright (C) 2017  Kyle Hargraves

;; Author: Kyle Hargraves
;; URL: https://github.com/pd/easy-after-load
;; Package-Version: 20170817.1231
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

; This doesn't seem to fit in to any particular customize group.
; Maybe 'tools is a better option. Meh.
(defgroup easy-after-load nil
  "Easily manage eval-after-load statements."
  :group 'files)

(defcustom easy-after-load-directory
  (file-name-as-directory (expand-file-name "after-loads" user-emacs-directory))
  "The directory in which you keep your after-load files."
  :type 'directory
  :group 'easy-after-load)

(defcustom easy-after-load-pattern "^after-\\(.+\\)\\.el$"
  "The pattern used to extract the name of the feature to add the
`eval-after-load' statement for.  The first subexpression should
return the name of the feature. For the default value, the file
\"after-foo.el\" will be loaded after loading the feature \"foo\".

If you change `easy-after-load-function', this pattern will not
be used."
  :type 'regexp
  :group 'easy-after-load)

(defcustom easy-after-load-function 'easy-after-load-apply-pattern
  "The function used to extract the name of the feature to add the
`eval-after-load' statement for.  The function is given a single
argument, the `file-name-nondirectory' part of the filename of each
file in `easy-after-load-directory'. It should return the name of the
feature to add the `eval-after-load' statement for."
  :type 'function
  :group 'easy-after-load)

(defun easy-after-load-apply-pattern (filename)
  "Apply `easy-after-load-pattern' to FILENAME and return the
contents of the first matching subexpression."
  (if (string-match easy-after-load-pattern filename)
      (match-string 1 filename)
    (progn
      (message "easy-after-load-pattern failed to match against %s" filename)
      nil)))

(defun easy-after-load--files (dir)
  (condition-case err
      (directory-files dir t "\\.el$")
    (file-error (message "easy-after-load encountered an error reading from directory: %s" dir))))

(defun easy-after-load--get-feature (path)
  (let ((filename (file-name-nondirectory path)))
    (condition-case err
        (let ((feature (funcall easy-after-load-function filename)))
          (and feature (cons path (intern feature))))
      (error (message "easy-after-load-function failed on %s: %s" filename (cadr err))
             nil))))

(defun easy-after-load--eval-after-load (after-load)
  (when after-load
    (eval-after-load (cdr after-load)
      `(progn
         (ignore "added by easy-after-load")
         (load ,(car after-load))))))

(defun easy-after-load--alist-entries ()
  "Returns a list of (FEATURE . ENTRY) that were added to `after-load-alist'
by `easy-after-load'."
  (let (added)
    (dolist (entry after-load-alist)
      (let ((feature (car entry))
            (forms   (cdr entry)))
        (dolist (form forms)
          (with-temp-buffer
            (princ form (current-buffer))
            (when (string-match-p "added by easy-after-load" (buffer-string))
              (setq added (cons (cons feature form)
                                added)))))))
    added))

(defun easy-after-load-reset ()
  "Removes all entries in `after-load-alist' which were added by
`easy-after-load'."
  (dolist (added (easy-after-load--alist-entries))
    (let* ((feature   (car added))
           (rm-form   (cdr added))
           (full-form (assoc feature after-load-alist)))
      (delq rm-form full-form)
      (when (equal full-form (list feature))
        (setq after-load-alist
              (delq (assoc feature after-load-alist) after-load-alist))))))

;;;###autoload
(defun easy-after-load (&optional directory)
  "Add `eval-after-load' statements for all features with corresponding
files in DIRECTORY (or `easy-after-load-directory' if nil).

See also `easy-after-load-pattern', `easy-after-load-function'."
  (let* ((directory   (file-name-as-directory (or directory easy-after-load-directory)))
         (paths       (easy-after-load--files directory)))
    (easy-after-load-reset)
    (mapc 'easy-after-load--eval-after-load
          (mapcar 'easy-after-load--get-feature paths))))

;;;###autoload
(defun easy-auto-mode (modes)
  "Add entries to `auto-mode-alist' for each element in MODES.

Each element looks like (MODE REGEXP REGEXP ...).

Example:
    (easy-auto-mode
     '((ruby-mode \"\\\\.rake$\" \"^Rakefile$\")
       (markdown-mode \"\\\\.md$\")))"
  (dolist (entry modes)
    (mapc (lambda (regexp)
              (add-to-list 'auto-mode-alist (cons regexp (car entry))))
            (cdr entry))))

(provide 'easy-after-load)

;;; easy-after-load.el ends here
