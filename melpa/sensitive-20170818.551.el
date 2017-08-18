;;; sensitive.el --- A dead simple way to load sensitive information

;; Copyright (C) 2013 Tim Visher <tim.visher@gmail.com>

;; Author: Tim Visher <tim.visher@gmail.com>
;; Keywords: convenience
;; Package-Version: 20170818.551
;; Package-Requires: ((emacs "24") (sequences "0.1.0"))
;; Version: 1.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is intended to make it dead simple to include
;; sensitive information in your public .emacs file. More people
;; publishing their .emacs file is a good thing for the world of Emacs
;; users and the more facilities people have for hiding the
;; information they need to load without having to jump through hoops
;; to load it should make that easier.

;;; Usage:

;; To use sensitive, place directories and files in `sensitive-root`
;; that correspond to modes and variables.
;;
;; For instance. If the contents of `sensitive-root` (by default
;; `~/sensitive`) are as follows:
;;
;;     {sensitive-root}
;;     ├── erc
;;     │   └── erc-password -> "password as a string"
;;     └── pivotal-tracker
;;         └── pivotal-api-token -> "api token as a string"
;;
;; `sensitive` will cause the forms
;;
;;     (eval-after-load 'erc
;;       '(setq 'erc-password "password as a string"))
;;     
;;     (eval-after-load 'pivotal-api-token
;;       '(setq pivotal-api-token "api token as string"))
;;
;; to be evaled.
;;
;; It's important to note that the contents of the variable files
;; should be exactly what you would have used if you were to write the
;; `eval-after-load` form yourself. In other words, there is no
;; defaulting to strings or something, the form is actually read via
;; the elisp reader and inserted into the `eval-after-load` form.
;;
;; That's all there is to it. Happy hunting!

;;; Code:

(require 'cl-lib)
(require 'sequences)

(defgroup sensitive '()
  "Customization group for `sensitive`"
  :link '(url-link "http://github.com/timvisher/sensitive.el"))

(defcustom sensitive-root "~/sensitive"
  "Root directory to set sensitive vars from."
  :type  'directory
  :group 'sensitive)

;;;###autoload
(defun load-sensitive-files ()
  (if (file-directory-p sensitive-root)
      (cl-dolist (setting-file (cl-remove-if 'file-directory-p (sequences-file-seq sensitive-root)))
        (with-temp-buffer
          (insert-file-contents setting-file)
          (goto-char (point-min))
          (let ((package-name (intern (file-name-base (substring (file-name-directory setting-file) 0 -1))))
                (var-name     (intern (file-name-base setting-file)))
                (value        (read (current-buffer))))
            (message (format "Setting %s to %s after %s is loaded." (symbol-name var-name) value (symbol-name package-name) ))
            (eval-after-load package-name
              (set var-name value)))))
    (message (format "%s is not a directory." sensitive-root))))

(provide 'sensitive)

;;; Local Variables:
;;; tab-width:2
;;; indent-tabs-mode:nil
;;; lexical-binding:t
;;; End:
;;; sensitive.el ends here
