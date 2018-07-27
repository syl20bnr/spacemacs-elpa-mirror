;;; openstack-cgit-browse-file.el --- Browse the current file in OpenStack cgit

;; Copyright (C) 2013 Chmouel Boudjnah <chmouel@chmouel.com>

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Homepage: https://github.com/chmouel/openstack-cgit-browse-file
;; Version: 0.1
;; Package-Version: 20130819.927
;; Keywords: convenience vc git cgit gerrit openstack

;;; Installation:

;;; Commentary:

;; Call `openstack-cgit-browse-file' (for the git blob) to browse
;; current file on OpenStack cgit.

;; Inspired by github-browse-file for OpenStack and cgit
;; from Ozan Sener <ozan@ozansener.com> See:
;; https://github.com/osener/github-browse-file

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

(require 'vc-git)

(defvar openstack-cgit-url "https://git.openstack.org/cgit")

(defun openstack-cgit-file--repo-relative-path ()
  "Return the path to the current file relative to the repository root.
Imported from github-browse-library"
  (let* ((root (ignore-errors (vc-git-root buffer-file-name))))
    (and root (file-relative-name buffer-file-name root))))


(defun openstack-cgit-get-gitreview-info()
  (let ((review-branch "master")
        (review-project)
        (review-file (locate-dominating-file default-directory ".gitreview")))
    (with-temp-buffer
      (setq review-file (concat review-file ".gitreview" ))
      (buffer-disable-undo)
      (cond ((get-file-buffer review-file)
             (insert (with-current-buffer (get-file-buffer review-file)
                       (buffer-substring (point-min) (point-max)))))
            ((not (file-exists-p review-file)))
            (t (insert-file-contents review-file)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n) (newline))
      (goto-char (point-min))
      (while (re-search-forward
              "^defaultbranch=\\([^ \t\n]+\\)" nil t)
        (setq review-branch (buffer-substring (match-beginning 1) (match-end 1))))
      (while
          (re-search-forward
           "^project=\\([^ \t\n]+\\)" nil t)
        (setq review-project
              (replace-regexp-in-string "\.git$" ""
                                        (buffer-substring (match-beginning 1) (match-end 1))))))
    (if review-project
        (list review-project review-branch)
        )))

;;;###autoload
(defun openstack-cgit-browse-file()
  (interactive)
  (let ((n (line-number-at-pos))
        (gitreview-info (openstack-cgit-get-gitreview-info)))
    (if gitreview-info
        (browse-url (format "%s/%s/tree/%s?h=%s#n%d"
                            openstack-cgit-url
                            (car gitreview-info)
                            (openstack-cgit-file--repo-relative-path)
                            (nth 1 gitreview-info)
                            n)))))

;;;###autoload
(defun openstack-cgit-repo-log()
  (interactive)
  (let ((gitreview-info (openstack-cgit-get-gitreview-info)))
    (if gitreview-info
        (browse-url (format "%s/%s/log/?h=%s"
                            openstack-cgit-url
                            (car gitreview-info)
                            (nth 1 gitreview-info))))))


(provide 'openstack-cgit-browse-file)

;;; openstack-cgit-browse-file.el ends here
