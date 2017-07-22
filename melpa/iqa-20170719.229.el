;;; iqa.el --- Init file(and directory) Quick Access. -*- lexical-binding: t -*-

;; Homepage: https://github.com/a13/iqa.el
;; Version: 0.0.1
;; Package-Version: 20170719.229
;; Package-Requires: ((emacs "24.3"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Sadly, Emacs (unlike Spacemacs, which has `spacemacs/find-dotfile') doesn't have
;; a function to open its own init file, so thousands of users have to write their owns.
;; I'm not different :)
;;
;; `iqa-find-user-init-file' is a shorthand to open user init file.
;; By default `user-init-file' is used.  If your configuration is generated
;; from org-mode source you may want to point it to your org file.
;;
;; (setq iqa-user-init-file (concat user-emacs-directory "init.org"))
;;
;; File is opened by `find-file', but you can redefine it by e.g.
;;
;; (setq iqa-find-file-function #'find-file-other-window)
;;
;; `iqa-reload-user-init-file' reloads `user-init-file' (not `iqa-user-init-file')
;; For a full restart take a look at `restart-emacs' package.
;;
;; `iqa-find-user-init-directory' opens init file directory
;;
;; `iqa-setup-default' defines keybindings:
;; "C-x M-f" — `iqa-find-user-init-file'
;; "C-x M-r" — `iqa-reload-user-init-file'
;; "C-x M-d" — `iqa-find-user-init-directory'
;;
;;
;; Installation with `quelpa-use-package':
;;
;; (use-package iqa
;;   :ensure nil
;;   :quelpa
;;   (iqa :repo "a13/iqa.el" :fetcher github :version original)
;;   ;; for generated files only
;;   ;; :init
;;   ;; (setq iqa-user-init-file (concat user-emacs-directory "init.org"))
;;   :config
;;   (iqa-setup-default))

;;; Code:

(defgroup iqa nil
  "Init file quick access."
  :group 'startup)

(defcustom iqa-user-init-file
  nil
  "Default init file to open instead of `user-init-file'."
  :group 'iqa
  :type 'file)

(defcustom iqa-find-file-function
  #'find-file
  "Find file function.  Should take one required FILENAME argument."
  :group 'iqa
  :type 'function)

(defun iqa--init-file ()
  "Return init file name."
  (or iqa-user-init-file user-init-file))

;;;###autoload
(defun iqa-find-user-init-file ()
  "Open user init file using `iqa-find-file-function'."
  (interactive)
  (funcall iqa-find-file-function (iqa--init-file)))

;;;###autoload
(defun iqa-reload-user-init-file (save-all)
  "Load user init file.  Call `save-some-buffers' if prefix SAVE-ALL is set.
Ask for saving only `user-init-file' otherwise."
  (interactive "P")
  (if save-all
      (save-some-buffers)
    (let* ((init-file (iqa--init-file))
           (init-file-buffer (get-file-buffer init-file)))
      (when (and init-file-buffer
                 (buffer-modified-p init-file-buffer)
                 (y-or-n-p (format "Save file %s? " init-file)))
        (with-current-buffer init-file-buffer
          (save-buffer)))))
  (load-file user-init-file))

;;;###autoload
(defun iqa-find-user-init-directory ()
  "Open user init file directory using `iqa-find-file-function'."
  (interactive)
  (funcall iqa-find-file-function (file-name-directory (iqa--init-file))))

;;;###autoload
(defun iqa-setup-default ()
  "Setup default shortcuts for iqa."
  (interactive)
  (define-key ctl-x-map "\M-f" #'iqa-find-user-init-file)
  (define-key ctl-x-map "\M-r" #'iqa-reload-user-init-file)
  (define-key ctl-x-map "\M-d" #'iqa-find-user-init-directory))

(provide 'iqa)

;;; iqa.el ends here
