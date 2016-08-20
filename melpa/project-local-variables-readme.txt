This file allows you to create .emacs-project files that are
evaluated when a file is opened. You may also create
.emacs-project-$MODE files that only get loaded when you open files
of a specific mode in the project. All files for which a
.emacs-project file exists in an ancestor directory will have it
loaded.

It has not been tested in versions of Emacs prior to 22.

(defvar plv-project-file ".emacs-project"
  "Name prefix for project files.
 Emacs appends name of major mode and looks for such a file in
 the current directory and its parents.")

(defmacro setl (sym val)
  "Like setq, but makes sym a local variable first."
  `(set (make-local-variable ',sym) ,val))

(defun plv-find-project-file (dir mode-name)
 (let ((f (expand-file-name (concat plv-project-file mode-name) dir))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) f)
         (t (plv-find-project-file parent mode-name)))))

(defadvice hack-local-variables (before project-local-variables activate)
  (let* ((full-name (symbol-name major-mode))
         (mode-name (if (string-match "\\(.*\\)-mode$" full-name)
                        (match-string 1 full-name)
                      full-name))
         (pfile (plv-find-project-file default-directory (concat "-" mode-name)))
         (gfile (plv-find-project-file default-directory "")))
    (save-excursion
      (when gfile (load gfile))
      (when pfile (load pfile)))))

(add-to-list 'auto-mode-alist '("^\.emacs-project" . emacs-lisp-mode))

(provide 'project-local-variables)

project-local-variables.el ends here
