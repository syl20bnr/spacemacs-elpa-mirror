;;; helm-ls-svn.el --- helm extension to list svn files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <chunyang@macports.org>
;; Created: Wed Jun 10 20:58:26 CST 2015
;; Version: 0.2.3
;; Package-Version: 20150717.39
;; URL: https://svn.macports.org/repository/macports/users/chunyang/helm-ls-svn.el/helm-ls-svn.el
;; Package-Requires: ((emacs "24.1") (helm "1.7.0") (cl-lib "0.5"))
;; Keywords: helm svn

;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Chunyang Xu "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Chunyang Xu or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Chunyang Xu.

;;; Commentary:
;;
;; `helm-ls-svn.el' is a helm extension for listing files in svn project.
;;
;; This package is on MELPA (http://melpa.org/#/helm-ls-svn), you can always get
;; the latest version from MELPA.
;;
;;
;; Installation
;; ============
;;
;; To install, make sure this file is saved in a directory in your `load-path',
;; and add the line:
;;
;;   (require 'helm-ls-svn)
;;
;; to your Emacs initialization file.
;;
;;
;; Usage
;; =====
;;
;; By calling helm-ls-svn-ls in any buffer that is a part of a svn repo, you
;; will be presented with a corresponding helm buffer containing a list of all
;; the buffers/files currently in that same repository,
;;
;;
;; Reporting Bugs
;; ==============
;;
;; Bug report, suggestion and patch are welcome. Please email me (see above to
;; get my email address).
;;
;;
;; Note
;; ====
;;
;; The code of `helm-ls-svn.el' is based on `helm-ls-git.el'
;; <https://github.com/emacs-helm/helm-ls-git>
;;
;;
;; TODO
;; ====
;;
;; - Handle conflict status, think about it, may or may not needed.


;;; Code:

(require 'cl-lib)
(require 'vc)
(require 'vc-svn)
(require 'helm-files)
(require 'helm-types)

;; Define the sources.
(defvar helm-source-ls-svn-status nil)
(defvar helm-source-ls-svn nil)
(defvar helm-source-ls-svn-buffers nil)


(defgroup helm-ls-svn nil
  "Helm completion for svn repos."
  :group 'helm
  :link '(emacs-commentary-link :tag "commentary" "helm-ls-svn.el")
  :link '(emacs-library-link :tag "lisp file" "helm-ls-svn.el")
  :link '(url-link :tag "MELPA" "http://melpa.org/#/helm-ls-svn"))

(defcustom helm-ls-svn-show-abs-or-relative 'absolute
  "Show full path or relative path to repo when using `helm-ff-toggle-basename'.
Valid values are symbol 'abs (default) or 'relative."
  :group 'helm-ls-svn
  :type  '(radio :tag "Show full path or relative path to Svn repo when toggling"
                 (const :tag "Show full path" absolute)
                 (const :tag "Show relative path" relative)))

(defcustom helm-ls-svn-status-command 'vc-dir
  "Favorite svn-status command for Emacs."
  :group 'helm-ls-svn
  :type 'symbol)

(defcustom helm-ls-svn-default-sources '(helm-source-ls-svn-status
                                         helm-source-ls-svn-buffers
                                         helm-source-ls-svn)
  "Default sources list used in the `helm-ls-svn-ls' command."
  :group 'helm-ls-svn
  :type '(repeat (choice symbol)))

(defvar helm-ls-svn-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    map))

(defun helm-ls-svn-root-dir (&optional directory)
  "Return svn root directory or DIRECTORY.
Return nil if not found."
  (locate-dominating-file (or directory default-directory) ".svn"))

(defun helm-ls-svn-not-inside-svn-repo ()
  "Not inside a svn repository."
  (not (helm-ls-svn-root-dir)))

(defun helm-ls-svn-branch ()
  "Return svn branch or trunk name."
  (shell-command-to-string
   "svn info | grep '^URL:' | egrep -o '(tags|branches)/[^/]+|trunk' | egrep -o '[^/]+$' | tr -d '\n'"))

(defun helm-ls-svn-header-name (name)
  "Compute `helm-ls-svn-ls' header name by using NAME."
  ;; Don't call `helm-ls-svn-branch'' because it's very slow
  ;; (let ((branch (helm-ls-svn-branch)))
  ;;   (format "%s (%s)"
  ;;           name (if (string= branch "")
  ;;                    (helm-ls-svn-root-dir) branch)))
  (format "%s (%s)" name (helm-ls-svn-root-dir)))

(defun helm-ls-svn-collect-data ()
  (let ((root (helm-ls-svn-root-dir)))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory root))
        (cl-remove-if
         (lambda (item) (or (null item)
                            (file-directory-p item)))
         (mapcar (lambda (item) (let ((file (car (last (split-string item)))))
                                  (when (stringp file) (expand-file-name file))))
                 (split-string
                  (shell-command-to-string
                   "svn status --non-interactive --quiet --verbose")
                  "\n")))))))

(defun helm-ls-svn-status ()
  "Run svn status."
  (helm-aif (helm-ls-svn-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
            (with-current-buffer standard-output
              (apply #'process-file
                     "svn" nil t nil
                     (list "status")))))))

(defun helm-ls-svn-status-transformer (candidates _source)
  "`helm-ls-svn-status-source' CANDIDATES transformer."
  (let ((root (helm-ls-svn-root-dir)))
    (sort
     (mapcar (lambda (candidate)
               (cons (cond ((string-match "^?" candidate)
                            (propertize candidate 'face 'font-lock-variable-name-face))
                           ((string-match "^M" candidate)
                            (propertize candidate 'face 'font-lock-constant-face))
                           ((string-match "^A" candidate)
                            (propertize candidate 'face 'font-lock-variable-name-face))
                           ((string-match "^C" candidate)
                            (propertize candidate 'face 'font-lock-warning-face))
                           (t candidate))
                     (expand-file-name (cadr (split-string candidate)) root)))
             candidates)
     (lambda (a b) (string> (substring (car a) 0 2)
                            (substring (car b) 0 2))))))

(defun helm-ls-svn-diff (candidate)
  "Diff action on CANDIDATE."
  (let (helm-persistent-action-use-special-display)
    (with-current-buffer (find-file-noselect candidate)
      (when (buffer-live-p (get-buffer "*vc-diff*"))
        (kill-buffer "*vc-diff*")
        (balance-windows))
      (vc-svn-diff (list candidate) nil nil "*vc-diff*")
      (pop-to-buffer "*vc-diff*")
      (diff-mode)
      (view-mode))))

(defun helm-ls-svn-revert (_candidate)
  "Revert action on marked candidate(s)."
  (let ((marked (helm-marked-candidates)))
    (cl-loop for f in marked do
             (progn
               (vc-svn-revert f)
               (helm-aif (get-file-buffer f)
                   (with-current-buffer it
                     (revert-buffer t t)))))))

(defun helm-ls-svn-status-action-transformer (actions _candidate)
  "Setup ACTIONS according to different candidates."
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^?" disp)
           (append actions
                   (helm-make-actions
                    "Add files(s)"
                    (lambda (candidate)
                      (let ((default-directory
                              (file-name-directory candidate))
                            (marked (helm-marked-candidates)))
                        (vc-call-backend 'SVN 'register marked)))
                    "Delete file(s)"
                    #'helm-delete-marked-files)))
          ((string-match "^M" disp)
           (append actions
                   (helm-make-actions
                    "Diff file" #'helm-ls-svn-diff
                    "Commit file(s)"
                    (lambda (_candidate)
                      (let* ((marked (helm-marked-candidates))
                             (default-directory
                               (file-name-directory (car marked))))
                        (vc-checkin marked 'SVN)))
                    "Revert file(s)" #'helm-ls-svn-revert
                    "Copy file(s) `C-u to follow'" #'helm-find-files-copy
                    "Rename file(s) `C-u to follow'" #'helm-find-files-rename)))
          ((string-match "^A" disp)
           (append actions
                   (helm-make-actions
                    "svn delete" #'vc-svn-delete-file
                    "Revert file(s)" #'helm-ls-svn-revert)))
          (t actions))))

(defun helm-ls-svn-transformer (candidates)
  "Toggle abs and relative path for CANDIDATES."
  (cl-loop with root = (helm-ls-svn-root-dir)
           for i in candidates
           for abs = (expand-file-name i root)
           for disp = (if (and helm-ff-transformer-show-only-basename
                               (not (string-match "[.]\\{1,2\\}$" i)))
                          (helm-basename i)
                        (cl-case helm-ls-svn-show-abs-or-relative
                          (absolute abs)
                          (relative (file-relative-name i root))))
           collect
           (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-ls-svn-sort-fn (candidates)
  "Transformer for sorting CANDIDATES."
  (helm-ff-sort-candidates candidates nil))

(defclass helm-ls-svn-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-svn-header-name)
   (data :initform 'helm-ls-svn-collect-data)
   (keymap :initform helm-ls-svn-map)
   (help-message :initform helm-generic-file-help-message)
   (candidate-transformer :initform '(helm-ls-svn-transformer
                                      helm-ls-svn-sort-fn))
   (candidate-number-limit :initform 9999)
   (action :initform helm-type-file-actions)))

(defclass helm-ls-svn-status-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-svn-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-ls-svn-status))))
   (keymap :initform helm-ls-svn-map)
   (filtered-candidate-transformer :initform 'helm-ls-svn-status-transformer)
   (persistent-action :initform 'helm-ls-svn-diff)
   (persistent-help :initform "Diff")
   (action-transformer :initform 'helm-ls-svn-status-action-transformer)
   (action :initform
           (helm-make-actions
            "Find file" 'helm-find-many-files
            "svn status" (lambda (_candidate)
                           (funcall helm-ls-svn-status-command
                                    (helm-default-directory)))))))

;;;###autoload
(defun helm-ls-svn-ls (&optional arg)
  "Use helm to list buffers/files in svn project.
If ARG is nil, only possible when called from Lisp code, don't check if current
buffer is under a svn project."
  (interactive "p")
  (and arg (helm-ls-svn-not-inside-svn-repo)
       (user-error "Not under a svn repository"))
  (unless helm-source-ls-svn-buffers
    (setq helm-source-ls-svn-buffers
          (helm-make-source "Buffers in svn project" 'helm-source-buffers
            :header-name #'helm-ls-svn-header-name
            :buffer-list (lambda () (helm-browse-project-get-buffers
                                     (helm-ls-svn-root-dir))))))
  (unless helm-source-ls-svn
    (setq helm-source-ls-svn
          (helm-make-source "svn files" 'helm-ls-svn-source)))
  (unless helm-source-ls-svn-status
    (setq helm-source-ls-svn-status
          (helm-make-source "svn status" 'helm-ls-svn-status-source)))
  (helm :sources helm-ls-svn-default-sources
        :buffer "*helm ls svn*"))

(provide 'helm-ls-svn)
;;; helm-ls-svn.el ends here
