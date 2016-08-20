;;; helm-codesearch.el --- helm interface for codesearch

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.3.0
;; Package-Version: 20160809.803
;; Keywords: tools
;; Package-Requires: ((s "1.10.0") (dash "2.12.0") (helm "1.7.7") (cl-lib "0.5"))

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

;; helm interface for codesearch
;;
;; See documentation on https://github.com/youngker/helm-codesearch.el

;;; Code:
(require 's)
(require 'dash)
(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'cl-lib)

(defgroup helm-codesearch nil
  "Helm interface for codesearch."
  :prefix "helm-codesearch-"
  :group 'helm)

(defface helm-codesearch-file-face
  '((t :inherit font-lock-function-name-face))
  "Face for file."
  :group 'helm-codesearch)

(defface helm-codesearch-lineno-face
  '((t :inherit font-lock-constant-face))
  "Face for lineno."
  :group 'helm-codesearch)

(defface helm-codesearch-source-face
  '((t :inherit font-lock-doc-face))
  "Face for source."
  :group 'helm-codesearch)

(defcustom helm-codesearch-csearchindex ".csearchindex"
  "Index file for each projects."
  :type 'string
  :group 'helm-codesearch)

(defcustom helm-codesearch-multi-csearchindex nil
  "Index file for multiple projects."
  :type 'boolean
  :group 'helm-codesearch)

(defcustom helm-codesearch-abbreviate-filename 80
  "Abbreviate filename length."
  :type 'number
  :group 'helm-codesearch)

(defcustom helm-codesearch-action
  '(("Find File" . helm-grep-action)
    ("Find file other frame" . helm-grep-other-frame)
    ("Save results in grep buffer" . helm-grep-save-results)
    ("Find file other window" . helm-grep-other-window))
  "Actions for helm-codesearch."
  :group 'helm-codesearch
  :type '(alist :key-type string :value-type function))

(defvar helm-codesearch-buffer "*helm codesearch*")
(defvar helm-codesearch-indexing-buffer "*helm codesearch indexing*")
(defvar helm-codesearch-file nil)
(defvar helm-codesearch-process nil)

(defvar helm-codesearch-source-pattern
  (helm-build-async-source "Codesearch: Find pattern"
    :header-name #'helm-codesearch-header-name
    :init #'helm-codesearch-init
    :cleanup #'helm-codesearch-cleanup
    :candidates-process #'helm-codesearch-find-pattern-process
    :filtered-candidate-transformer #'helm-codesearch-find-pattern-transformer
    :action 'helm-codesearch-action
    :persistent-action 'helm-grep-persistent-action
    :help-message 'helm-grep-help-message
    :keymap helm-grep-map
    :candidate-number-limit 99999
    :requires-pattern 3))

(defvar helm-codesearch-source-file
  (helm-build-async-source "Codesearch: Find file"
    :header-name #'helm-codesearch-header-name
    :init #'helm-codesearch-init
    :cleanup #'helm-codesearch-cleanup
    :candidates-process #'helm-codesearch-find-file-process
    :filtered-candidate-transformer #'helm-codesearch-find-file-transformer
    :action 'helm-type-file-actions
    :keymap helm-generic-files-map
    :candidate-number-limit 99999
    :requires-pattern 3))

(defun helm-codesearch-header-name (name)
  "Display Header NAME."
  (concat name " [" (getenv "CSEARCHINDEX") "]"))

(defun helm-codesearch-search-single-csearchindex ()
  "Search for single project index file."
  (let* ((start-dir (expand-file-name default-directory))
         (index-dir (locate-dominating-file start-dir helm-codesearch-csearchindex)))
    (if index-dir
        (concat index-dir helm-codesearch-csearchindex)
      (error "Can't find csearchindex"))))

(defun helm-codesearch-search-csearchindex ()
  "Search for project index file."
  (setenv "CSEARCHINDEX"
          (expand-file-name (or helm-codesearch-multi-csearchindex
                                (helm-codesearch-search-single-csearchindex)))))

(defun helm-codesearch-abbreviate-file (file)
  "FILE."
  (with-temp-buffer
    (insert file)
    (let* ((start (- (point) (length file)))
           (end (point))
           (amount (if (numberp helm-codesearch-abbreviate-filename)
                       (- (- end start) helm-codesearch-abbreviate-filename)
                     999))
           (advance-word (lambda ()
                           "Return the length of the text made invisible."
                           (let ((wend (min end (progn (forward-word 1) (point))))
                                 (wbeg (max start (progn (backward-word 1) (point)))))
                             (goto-char wend)
                             (if (<= (- wend wbeg) 1)
                                 0
                               (put-text-property (1+ wbeg) wend 'invisible t)
                               (1- (- wend wbeg)))))))
      (goto-char start)
      (while (and (> amount 0) (> end (point)))
        (cl-decf amount (funcall advance-word)))
      (goto-char end))
    (buffer-substring (point-min) (point-max))))

(defconst helm-codesearch-pattern-regexp
  "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")

(defun helm-codesearch-make-pattern-format (candidate)
  "Make pattern format from CANDIDATE."
  (-when-let* (((_ file lineno source)
                (s-match helm-codesearch-pattern-regexp candidate))
               (file (propertize file 'face 'helm-codesearch-file-face))
               (lineno (propertize lineno 'face 'helm-codesearch-lineno-face))
               (source (propertize source 'face 'helm-codesearch-source-face))
               (display-line (format "%08s %s" lineno source))
               (abbrev-file (format "\n%s" (helm-codesearch-abbreviate-file file)))
               (fake-file (propertize abbrev-file 'helm-candidate-separator t)))
    (if (string= file helm-codesearch-file)
        (list (cons display-line candidate))
      (progn
        (setq helm-codesearch-file file)
        (list (cons (format "%s\n%s" fake-file display-line) candidate))))))

(defun helm-codesearch-find-pattern-transformer (candidates source)
  "Transformer is run on the CANDIDATES and not use the SOURCE."
  (-mapcat 'helm-codesearch-make-pattern-format candidates))

(defun helm-codesearch-make-file-format (candidate)
  "Make file format from CANDIDATE."
  (-when-let* ((file-p (and (file-exists-p candidate) (> (length candidate) 0)))
               (file (propertize candidate 'face 'helm-codesearch-file-face))
               (lineno (propertize "1" 'face 'helm-codesearch-lineno-face))
               (source (propertize "..." 'face 'helm-codesearch-source-face))
               (display-line (format "%08s %s" lineno source))
               (abbrev-file (format "\n%s" (helm-codesearch-abbreviate-file file)))
               (fake-file (propertize abbrev-file 'helm-candidate-separator t)))
    (list (cons (format "%s\n%s" fake-file display-line) candidate))))

(defun helm-codesearch-find-file-transformer (candidates source)
  "Transformer is run on the CANDIDATES and not use the SOURCE."
  (-mapcat 'helm-codesearch-make-file-format candidates))

(defun helm-codesearch-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line, not used NAME."
  (let ((source (cdr (assoc helm-codesearch-process helm-async-processes))))
    (propertize
     (format "[%s Candidate(s)]" (or (cdr (assoc 'item-count source)) 0))
     'face 'helm-candidate-number)))

(defun helm-codesearch-set-process-sentinel (proc)
  "Set process sentinel to PROC."
  (prog1 proc
    (set-process-sentinel
     proc
     (lambda (process event)
       (helm-process-deferred-sentinel-hook
        process event (helm-default-directory))
       (unless (string= event "finished\n")
         (with-helm-window
           (forward-line 1)
           (insert " No match found.")))))))

(defun helm-codesearch-find-pattern-process ()
  "Execute the csearch for a pattern."
  (let ((proc (apply 'start-process
                     "codesearch"
                     nil
                     "csearch"
                     (cons "-n" (split-string helm-pattern " " t)))))
    (setq helm-codesearch-file nil)
    (setq helm-codesearch-process proc)
    (helm-codesearch-set-process-sentinel proc)))

(defun helm-codesearch-find-file-process ()
  "Execute the csearch for a file."
  (let ((proc (apply 'start-process
                     "codesearch"
                     nil
                     "csearch"
                     (list "-l" "-f" helm-pattern "$"))))
    (setq helm-codesearch-process proc)
    (helm-codesearch-set-process-sentinel proc)))

(defun helm-codesearch-create-csearchindex-process (dir)
  "Execute the cindex from a DIR."
  (let ((proc (apply 'start-process "codesearch"
                     helm-codesearch-indexing-buffer
                     "cindex" (list dir))))
    (with-current-buffer helm-codesearch-indexing-buffer
      (let ((buffer-read-only nil))
        (erase-buffer))
      (pop-to-buffer helm-codesearch-indexing-buffer)
      (setq buffer-read-only t)
      (goto-char (point-max)))))

(defun helm-codesearch-init ()
  "Initialize."
  (advice-add 'helm-show-candidate-number :override
              #'helm-codesearch-show-candidate-number))

(defun helm-codesearch-cleanup ()
  "Cleanup Function."
  (advice-remove 'helm-show-candidate-number
                 #'helm-codesearch-show-candidate-number))

;;;###autoload
(defun helm-codesearch-find-pattern ()
  "Find pattern."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (helm-codesearch-search-csearchindex)
    (helm :sources 'helm-codesearch-source-pattern
          :buffer helm-codesearch-buffer
          :input symbol
          :keymap helm-grep-map
          :prompt "Find pattern: "
          :truncate-lines t)))

;;;###autoload
(defun helm-codesearch-find-file ()
  "Find file."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (helm-codesearch-search-csearchindex)
    (helm :sources 'helm-codesearch-source-file
          :buffer helm-codesearch-buffer
          :input symbol
          :keymap helm-generic-files-map
          :prompt "Find file: "
          :truncate-lines t)))

;;;###autoload
(defun helm-codesearch-create-csearchindex (dir)
  "Create index file at DIR."
  (interactive "DIndex files in directory: ")
  (setenv "CSEARCHINDEX"
          (expand-file-name (or helm-codesearch-multi-csearchindex
                                (concat dir helm-codesearch-csearchindex))))
  (helm-codesearch-create-csearchindex-process (expand-file-name dir)))

(provide 'helm-codesearch)
;;; helm-codesearch.el ends here
