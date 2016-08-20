;;; grin.el --- run grin and grind (python replacements for grep and find) putting hits in a grep buffer

;; Copyright (C) 2011 Darius Powell

;; Author: Darius Powell <dariusp686@gmail.com>
;; Version: 1.0
;; Package-Version: 20110805.2358
;; URL: http://bitbucket.org/dariusp686/emacs-grin
;; Keywords: python, grin, grind, grep, find

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

;;; Code:

(defvar grin-hist nil)

(defvar grind-hist nil)

(defvar grind-find-hist nil)

(defgroup grin nil
  "Run grin and grind (python replacements for grep and find) putting hits in a grep buffer."
  :group 'tools
  :group 'processes)

(defcustom grin-cmd "grin --emacs -i"
  "The grin command."
  :type 'string
  :group 'grin)

(defcustom grind-cmd "grind"
  "The grind command."
  :type 'string
  :group 'grin)

;;;###autoload
(defun grin ()
  (interactive)
  (let* ((c (concat grin-cmd " \"\""))
         (l (length c))
         (cmd (read-shell-command "Command: " (cons c l) 'grin-hist))
         (null-device nil))
    (grep cmd)))

;;;###autoload
(defun grind ()
  (interactive)
  (let* ((c (concat grind-cmd " \"\""))
         (l (length c))
         (args (read-shell-command "Command: " (cons c l) 'grind-hist))
         (cmd (concat args " | sed s/$/\\:1\\:/"))
         (null-device nil))
    (grep cmd)))

;;;###autoload
(defun grind-find ()
  (interactive)
  (let* ((c "find . -name \"CVS\" -prune -o -name \"RCS\" -prune -o -name \".svn\" -prune -o -name \".hg\" -prune -o -name \".bzr\" -prune -o -name \".git\" -o -name \"build\" -prune -o -name \"dist\" -prune -o -name \"*.pyc\" -prune -o -name \"*.pyo\" -prune -o -name \"*.so\" -prune -o -name \"*.o\" -prune -o -name \"*.a\" -prune -o -name \"*.tgz\" -prune -o -name \"*.tar.gz\" -prune -o -name \"*.rar\" -prune -o -name \"*.zip\" -prune -o -name \"*~\" -prune -o -name \"#\" -prune -o -name \"*.bak\" -prune -o -name \"*.png\" -prune -o -name \"*.jpg\" -prune -o -name \"*.gif\" -prune -o -name \"*.bmp\" -prune -o -name \"*.tif\" -prune -o -name \"*.tiff\" -prune -o -name \"*.pyd\" -prune -o -name \"*.dll\" -prune -o -name \"*.exe\" -prune -o -name \"*.obj\" -prune -o -name \"*.lib\" -prune -o -iname \"\" -print")
         (l (- (length c) 7))
         (args (read-shell-command "Command: " (cons c l) 'grind-find-hist))
         (cmd (concat args " | sed s/$/\\:1\\:/"))
         (null-device nil))
    (grep cmd)))

(provide 'grin)

;;; grin.el ends here
