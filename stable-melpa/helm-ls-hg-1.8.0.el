;;; helm-ls-hg.el --- List hg files in hg project. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Package-Requires: ((helm "1.7.8"))
;; Package-Version: 1.8.0

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

;;; Code

(require 'cl-lib)
(require 'vc)
(require 'vc-hg)
(require 'helm-locate)
(require 'helm-files)

(defvaralias 'helm-c-source-hg-list-files 'helm-source-hg-list-files)
(make-obsolete-variable 'helm-c-source-hg-list-files 'helm-source-hg-list-files "1.5.1")
(defvaralias 'helm-c-source-ls-hg-status 'helm-source-ls-hg-status)
(make-obsolete-variable 'helm-c-source-ls-hg-status 'helm-source-ls-hg-status "1.5.1")

(defvar helm-ls-hg-default-directory nil)
(defvar helm-ls-hg-status-command 'vc-dir)

(cl-defun helm-hg-root (&optional (directory default-directory))
  (let ((root (locate-dominating-file directory ".hg")))
    (and root (file-name-as-directory root))))

(defun helm-hg-root-p (candidate)
  ;; Check for file existence in case of creation
  ;; of file or directory.
  (when (or (file-exists-p candidate)
            (file-directory-p candidate))
  (let ((default-directory (if (file-directory-p candidate)
                               (file-name-as-directory candidate)
                               (file-name-as-directory
                                helm-ff-default-directory))))
    (stringp (helm-hg-root)))))

(defun helm-hg-list-files ()
  (let ((dir (helm-hg-root)))
    (if (and dir (file-directory-p dir))
        (with-temp-buffer
          (process-file "hg" nil t nil "manifest")
          (cl-loop with ls = (split-string
                              (replace-regexp-in-string
                               "^\\([0-9]\\{1,\\}\\) +\\(\\* +\\|\\)"
                               ""
                               (buffer-string)) "\n" t)
                   for f in ls
                   collect (concat dir f)))
        (error "Error: Not an hg repo (no .hg found)"))))

(defun helm-ls-hg-header-name (name)
  (format "%s (%s)"
          name
          (with-temp-buffer
            (call-process-shell-command "hg branch" nil t)
            (buffer-substring-no-properties (goto-char (point-min))
                                            (line-end-position)))))

(defvar helm-source-ls-hg-buffers nil)

(defvar helm-source-hg-list-files
  (helm-build-in-buffer-source "Hg files list"
    :data (lambda () (helm-hg-list-files))
    :keymap helm-generic-files-map
    :header-name 'helm-ls-hg-header-name
    :filtered-candidate-transformer 'helm-ls-hg-transformer
    :action 'helm-type-file-actions))

(defun helm-ls-hg-transformer (candidates _source)
  (cl-loop for i in candidates
           for abs = (expand-file-name i)
           for disp = (if (and helm-ff-transformer-show-only-basename
                               (not (string-match "[.]\\{1,2\\}$" i)))
                          (helm-basename i) abs)
           collect
           (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-ff-hg-find-files (_candidate)
  (with-helm-default-directory (helm-default-directory)
      (helm-run-after-exit
       #'(lambda (d)
           (let ((default-directory d))
             (helm-hg-find-files-in-project)))
       default-directory)))

(defun helm-ls-hg-status ()
  (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file
               "hg"
               nil t nil
               (list "status")))))

(defvar helm-source-ls-hg-status
  (helm-build-in-buffer-source "Hg status"
    :data (lambda () (helm-ls-hg-status))
    :filtered-candidate-transformer 'helm-ls-hg-status-transformer
    :action-transformer 'helm-ls-hg-status-action-transformer
    :persistent-action 'helm-ls-hg-diff
    :header-name 'helm-ls-hg-header-name
    :persistent-help "Diff"
    :action '(("Find file" . helm-find-many-files)
              ("Hg status" . (lambda (_candidate)
                               (funcall helm-ls-hg-status-command
                                        (helm-hg-root)))))))

(defun helm-ls-hg-status-transformer (candidates _source)
  (cl-loop with root = (helm-hg-root (helm-default-directory))
           for i in candidates
           collect
           (cond ((string-match "^\\(M \\)\\(.*\\)" i)
                  (cons (propertize i 'face '((:foreground "yellow")))
                        (expand-file-name (match-string 2 i) root)))
                 ((string-match "^\\([?] \\{1\\}\\)\\(.*\\)" i)
                  (cons (propertize i 'face '((:foreground "red")))
                        (expand-file-name (match-string 2 i) root)))
                 ((string-match "^\\([ARC] ?+\\)\\(.*\\)" i)
                  (cons (propertize i 'face '((:foreground "green")))
                        (expand-file-name (match-string 2 i) root)))
                 ((string-match "^\\([!] \\)\\(.*\\)" i)
                  (cons (propertize i 'face '((:foreground "Darkgoldenrod3")))
                        (expand-file-name (match-string 2 i) root)))
                 (t i))))

(defvar helm-ls-vc-delete-buffers-list nil)
(defun helm-ls-vc-commit (_candidate backend)
  (let* ((marked (helm-marked-candidates))
         (default-directory
          (file-name-directory (car marked))))
    (cl-loop for f in marked
             unless (or (find-buffer-visiting f)
                        (not (file-exists-p f)))
             do (push (find-file-noselect f)
                      helm-ls-vc-delete-buffers-list))
    (add-hook 'vc-checkin-hook 'helm-vc-checkin-hook)
    (vc-checkin marked backend)))

(defun helm-vc-checkin-hook ()
  (when helm-ls-vc-delete-buffers-list
    (cl-loop for b in helm-ls-vc-delete-buffers-list
             do (kill-buffer b)
             finally (setq helm-ls-vc-delete-buffers-list nil))))

(defun helm-ls-hg-commit (candidate)
  (helm-ls-vc-commit candidate 'Hg))

(defun helm-ls-hg-status-action-transformer (actions _candidate)
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^[?]\\{1\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-hg-register marked)))))))
          ((string-match "^M" disp)
           (append actions (list '("Diff file" . helm-ls-hg-diff)
                                 '("Commit file(s)" . helm-ls-hg-commit)
                                 '("Revert file(s)" . (lambda (_candidate)
                                                        (let ((marked (helm-marked-candidates)))
                                                          (cl-loop for f in marked do
                                                                   (progn
                                                                     (vc-hg-revert f)
                                                                     (helm-aif (get-file-buffer f)
                                                                         (with-current-buffer it
                                                                           (revert-buffer t t)))))))))))
          ((string-match "^[!]" disp)
           (append actions (list '("Hg delete"
                                   . (lambda (candidate)
                                       (let ((default-directory
                                              (file-name-directory candidate))
                                             (marked (helm-marked-candidates)))
                                         (cl-loop for f in marked
                                                  do (vc-hg-delete-file f))))))))
          (t actions))))

(defun helm-ls-hg-diff (candidate)
  (with-current-buffer (find-file-noselect candidate)
    (when (buffer-live-p (get-buffer "*vc-diff*"))
      (kill-buffer "*vc-diff*")
      (balance-windows))
    (vc-hg-diff (list candidate))
    (pop-to-buffer "*vc-diff*")
    (diff-mode)))

;;;###autoload
(defun helm-hg-find-files-in-project ()
  (interactive)
  (setq helm-ls-hg-default-directory default-directory)
  (unless helm-source-ls-hg-buffers
    (setq helm-source-ls-hg-buffers
          (helm-make-source "Buffers in hg project" 'helm-source-buffers
            :header-name 'helm-ls-hg-header-name
            :buffer-list (lambda () (helm-browse-project-get-buffers
                                     (helm-hg-root))))))
  (unwind-protect
       (helm :sources '(helm-source-ls-hg-status
                        helm-source-ls-hg-buffers
                        helm-source-hg-list-files)
             :ff-transformer-show-only-basename nil
             :buffer "*helm hg files*")
    (setq helm-ls-hg-default-directory nil)))

(provide 'helm-ls-hg)

;;; helm-ls-hg.el ends here
