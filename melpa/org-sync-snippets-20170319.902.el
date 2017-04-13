;;; org-sync-snippets.el --- Export snippets to org-mode and vice versa

;; Copyright (C) 2017, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Package-Version: 20170319.902
;; Author: Adrien Brochard
;; Keywords: snippet org-mode yasnippet tools
;; URL: https://github.com/abrochard/org-sync-snippets
;; License: GNU General Public License >= 3
;; Package-Requires: ((org "8.3.5") (emacs "24.3") (f "0.17.3"))

;;; Commentary:

;; Simple extension to export snippets to org-mode and vice versa.
;; It was designed with Yasnippet in mind.

;;; Install:

;; Install from MELPA with
;;
;; M-x package-install org-sync-snippets
;;
;; or load the present file.

;;; Usage:

;; Load with
;;
;; (require 'org-sync-snippets)
;;
;; To export your snippets to an org-mode file, use
;;
;; M-x org-sync-snippets-snippets-to-org
;;
;; Alternatively, to turn your org-mode file into snippets
;;
;; M-x org-sync-snippets-org-to-snippets
;;
;; Notice: you can prevent certain snippets from being exported to org by adding the `tangle: no` tag in them.

;;; Customize:

;; By default, snippets are taken from the 'user-emacs-directory' (typically '~/.emacs.d/snippets/') folder.
;; You can change this with
;;
;; (setq org-sync-snippets-snippets-dir "~/your/path/to/snippets")
;;
;; Similarly, the org file compiled goes to your 'org-directory' (usually '~/org/snippets.org').
;; You can define a different one with
;;
;; (setq org-sync-snippets-org-snippets-file "~/your/path/to/snippet/file")
;;
;; Finally, if you want to save your snippets regularly, I recommend using a hook like
;;
;; (add-hook 'yas-after-reload-hook 'snippets-to-org)

;;; Code:
(require 'org)
(require 'f)

(defgroup org-sync-snippets nil
  "Export snippets to org-mode and vice versa.")

(defcustom org-sync-snippets-org-snippets-file (concat (file-name-as-directory org-directory) "snippets.org")
  "Location of the snippets.org file."
  :type 'file
  :group 'org-sync-snippets)

(defcustom org-sync-snippets-snippets-dir (locate-user-emacs-file "snippets")
  "Location the snippets folder."
  :type 'directory
  :group 'org-sync-snippets)

(defcustom org-sync-snippets-collection-title "Snippets Collection"
  "Title of the snippets.org collection."
  :type 'string
  :group 'org-sync-snippets)

(defun org-sync-snippets--to-org (snippets-dir org-file)
  "Write snippets to org file.

SNIPPETS-DIR is the location of the snippet files.
ORG-FILE the location of the compiled org file."
  (with-temp-file org-file
    (insert "#+TITLE: " org-sync-snippets-collection-title "\n")
    (insert "#+AUTHOR: org-sync-snippets\n\n")
    (dolist (mode (f-directories snippets-dir))
      (insert "* " (file-name-base mode) "\n")
      (dolist (snippet-file (f-files mode))
        (let ((content (f-read-text snippet-file 'utf-8)))
          (unless (string-match "^# tangle: no" content)
            (insert "** " (file-name-base snippet-file) "\n"
                    "#+BEGIN_SRC snippet "
                    ":tangle " snippet-file
                    "\n"
                    (replace-regexp-in-string "^" "  " content) "\n"
                    "#+END_SRC\n")))))))

(defun org-sync-snippets--to-snippets (org-file snippets-dir)
  "Tangle org file to snippets.

ORG-FILE the location of the compiled org file
SNIPPETS-DIR is the location of the snippet files."
  (unless (f-dir? snippets-dir)
    (f-mkdir snippets-dir))
  (with-temp-buffer
    (insert-file-contents org-file)
    (while (re-search-forward "^* \\(.+-mode\\)" (point-max) t)
      (let ((path (concat (file-name-as-directory snippets-dir) (match-string 1))))
        (unless (f-dir? path)
          (f-mkdir path)))))
  (org-babel-tangle-file org-file))

;;;###autoload
(defun org-sync-snippets-snippets-to-org ()
  "Compile snippet files to an 'org-mode' file."
  (interactive)
  (org-sync-snippets--to-org org-sync-snippets-snippets-dir org-sync-snippets-org-snippets-file)
  (message "Done"))

(defun org-sync-snippets-org-to-snippets ()
  "Export the 'org-mode' file back to snippet files."
  (interactive)
  (org-sync-snippets--to-snippets org-sync-snippets-org-snippets-file org-sync-snippets-snippets-dir)
  (message "Done"))

(provide 'org-sync-snippets)
;;; org-sync-snippets.el ends here
