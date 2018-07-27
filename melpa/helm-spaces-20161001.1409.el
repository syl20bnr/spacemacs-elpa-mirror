;;; helm-spaces.el --- helm sources for spaces

;; Copyright (C) 2013-2016 Yasuyuki Oka

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.4
;; Package-Version: 20161001.1409
;; URL: https://github.com/yasuyk/helm-spaces
;; Package-Requires: ((helm-core "2.2") (spaces "0.1.0"))
;; Keywords: helm frames convenience

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

;;; Commentary:


;; Installation:

;; Add the following to your Emacs init file:
;;
;; (require 'helm-spaces)  ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c s") 'helm-spaces)

;; That's all.

;;; Code:

(require 'helm)
(require 'spaces)

(defgroup helm-spaces nil
  "Spaces related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-spaces-new-space-query t
  "Non-nil means ask user before `sp-new-space', If a space with same name already exists."
  :type '(choice (const :tag "Don't ask" nil)
                 (other :tag "Ask" t))
  :group 'helm-spaces)

;;; Faces
(defface helm-spaces-current-space nil
  "Face used for current space."
  :group 'helm-spaces)

(defun helm-spaces-new-space (name)
  "Create a new space with the given NAME.

If There is already NAME in `sp-spaces' and `helm-spaces-new-space-query' is t,
ask user replacing old space."
  (catch 'return
    (when (and helm-spaces-new-space-query
               (cl-find-if
                (lambda (space) (equal name (car space)))
                sp-spaces))
      (unless (y-or-n-p (format "Replace old %s? " name))
        (throw 'return nil)))
    (sp-new-space name)))

(defvar helm-source-space-create
  (helm-build-dummy-source
      "Creates a new space"
    :action (helm-make-actions
             "Create a new space"
             (lambda (candidate)
               (if (equal candidate "")
                   (sp-new-space)
                 (helm-spaces-new-space candidate))))))


(defun helm-spaces-candidates ()
  "Return a list of all spaces.
The currently selected space is colored by `helm-spaces-current-space'."
  (when sp-spaces
    (loop for sname in sp-spaces
          if (equal (car sname) sp-current-space)
          collect (propertize (car sname) 'face 'helm-spaces-current-space)
          else
          collect (car sname))))

(defvar helm-source-spaces
  (helm-build-sync-source "Show all spaces"
    :candidates 'helm-spaces-candidates
    :action '(("Switch space" . sp-switch-space)
               ("Kill space" . sp-kill-space)
               ("Replace space" . helm-spaces-new-space)
               ("Kill all spaces" .
                (lambda (dummy) (sp-clear-spaces))))))

;;;###autoload
(defun helm-spaces ()
  "Helm to list spaces and to create a new space."
  (interactive)
  (helm :sources'(helm-source-spaces
                  helm-source-space-create)
        :buffer "*helm spaces*"))

(provide 'helm-spaces)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; helm-spaces.el ends here
