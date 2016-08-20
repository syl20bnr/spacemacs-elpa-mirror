;;; sphinx-frontend.el --- Launch build process for rst documents via sphinx.

;; Copyright © 2012-2016 - Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/sphinx-frontend
;; Package-Version: 20160606.820
;; Keywords: compile, sphinx, reStructuredText
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

(defvar sphinx-build-command "sphinx-build -b "
  "Runs `sphinx-build-script.py'. 
-b <builder> -- builder to use; default is html")

(defvar sphinx-output-dir-html "build-html" 
  "Relative to current document's root output directory for html format.")

(defvar sphinx-output-dir-pdf "build-pdf" 
  "Relative to current document's root output directory for pdf format.")

(defvar sphinx-conf-file-name "conf.py"
  "Document's root configuration file.")

(defun sphinx-get-root-document-dir ()
  "Recursively searches current document's tree root.
The root sign is the location in the directory `sphinx-conf-file-name' file.
Returns current document's tree root directory."
  (or (locate-dominating-file default-directory sphinx-conf-file-name)
      (error (concat "Can't find file " sphinx-conf-file-name))))

(defun sphinx-get-build-command (output-format output-dir)
  "Returns sphinx build command according to `output-format'."
  (let ((current-dir (sphinx-get-root-document-dir)))
    (concat sphinx-build-command " " output-format " "
            ;; sourcedir
            (shell-quote-argument (file-name-as-directory current-dir)) " "
            ;; outdir
            (shell-quote-argument (file-name-as-directory
                                   (expand-file-name output-dir current-dir))))))

(defun sphinx-get-build-command-html ()
  (sphinx-get-build-command "html" sphinx-output-dir-html))

(defun sphinx-get-build-command-latex ()
  (sphinx-get-build-command "latex" sphinx-output-dir-pdf))

(defun sphinx-build (save-without-query command)
  "Compiles the rst file via sphinx and shows the output in a buffer.
`command' is a full sphinx build command with params, input and output 
directories.
If `save-without-query' is t, saves current file without query."  
  (let (rst-buffer (current-buffer))
    (progn
      (if (and save-without-query (buffer-modified-p rst-buffer))
          (save-buffer rst-buffer))
      (compile command))))

;;;###autoload
(defun sphinx-build-html (arg)
  "Compiles the rst file to html via sphinx and shows the output in a buffer.
Without `arg' saves current file."
  (interactive "P")
  (sphinx-build (not arg) (sphinx-get-build-command-html)))

;;;###autoload
(defun sphinx-build-latex (arg)
  "Compiles the rst file to latex via sphinx and shows the output in a buffer.
Without `arg' saves current file."
  (interactive "P")
  (sphinx-build (not arg) (sphinx-get-build-command-latex)))

;;;###autoload
(defun sphinx-run-pdflatex ()
  "Ad-hoc call pdflatex for LaTeX-builded documentation."
  (interactive)
  (cd (expand-file-name sphinx-output-dir-pdf (sphinx-get-root-document-dir)))
  (compile "pdflatex -interaction=nonstopmode *.tex"))

(defun sphinx--clean (sphinx-output-dir)
  "Removes directory `sphinx-output-dir' in the documentation root path."
  (delete-directory
   (file-name-as-directory 
    (expand-file-name sphinx-output-dir
                      (sphinx-get-root-document-dir))) t t))

;;;###autoload
(defun sphinx-clean-html ()
  "Removes `sphinx-output-dir-html' dir."
  (interactive)
  (sphinx--clean sphinx-output-dir-html))

;;;###autoload
(defun sphinx-clean-pdf ()
  "Removes `sphinx-output-dir-pdf' dir."
  (interactive)
  (sphinx--clean sphinx-output-dir-pdf))

(require 'rst)

(define-key rst-mode-map (kbd "C-c h") 'sphinx-build-html)
(define-key rst-mode-map (kbd "C-c l") 'sphinx-build-latex)
(define-key rst-mode-map (kbd "C-c p") 'sphinx-run-pdflatex)


(provide 'sphinx-frontend)

;;; sphinx-frontend.el ends here
