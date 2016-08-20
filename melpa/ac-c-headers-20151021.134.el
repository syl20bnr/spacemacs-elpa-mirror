;;; ac-c-headers.el --- auto-complete source for C headers

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Version: 20151021.134
;; Version: 1.0.0
;; Package-Requires: ((auto-complete "1.3.1"))

;;; Commentary:

;; Require this script (and auto-complete) then add to ac-sources.
;;
;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (add-to-list 'ac-sources 'ac-source-c-headers)
;;               (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
;;
;; then header filenames and symbols in imported headers are completed.
;;
;;   #include <s[tdio.h>]   <- ac-source-c-headers
;;   pr[intf]               <- ac-source-c-header-symbols

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(require 'find-file)

;; + constants

(defconst ac-c-headers-version "1.0.0")

;; + filenames

(defvar ac-c-headers--files-cache nil
  "list of (PREFIX . FILE-OR-DIRECTORY ...)")

(defun ac-c-headers--files-update (&optional prefix)
  (setq prefix (or prefix ""))
  (unless (assoc prefix ac-c-headers--files-cache)
    (setq ac-c-headers--files-cache
          (cons (cons prefix
                      (apply 'append
                             (mapcar
                              (lambda (dir)
                                (let ((path (concat (file-name-as-directory dir) prefix)))
                                  (when (file-accessible-directory-p path)
                                    (delq nil
                                          (mapcar
                                           (lambda (file)
                                             (cond ((file-directory-p (concat path file))
                                                    (concat file "/"))
                                                   ((string-match "\\h$" file)
                                                    file)
                                                   (t
                                                    nil)))
                                           (directory-files path nil))))))
                              cc-search-directories)))
                ac-c-headers--files-cache))))

(defun ac-c-headers--files-list (&optional point)
  "returns possible completions at the point"
  (save-excursion
    (when point (goto-char point))
    (when (looking-back "[<\"]\\([^<>\"]*?\\)\\([^<>\"/]*\\)")
      (let ((prefix (match-string 1)))
        (unless (assoc prefix ac-c-headers--files-cache)
          (ac-c-headers--files-update prefix))
        (cdr (assoc prefix ac-c-headers--files-cache))))))

;; + symbols in headers

(defvar ac-c-headers--symbols-cache nil
  "list of (HEADER . SYMBOL ...)")

(defun ac-c-headers--search-header-file (header)
  (catch 'found
    (dolist (prefix cc-search-directories)
      (let ((file (concat prefix
                          (unless (string-match "/$" prefix) "/")
                          header)))
        (when (file-exists-p file)
          (throw 'found file))))))

(defun ac-c-headers--symbols-update (header)
  (unless (assoc header ac-c-headers--symbols-cache)
    (let ((file (ac-c-headers--search-header-file header)))
      (when (and file (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          ;; delete /* comments */
          (goto-char (point-min))
          (while (search-forward-regexp
                  "/\\*\\([^*]\\|\\*[^/]\\)*\\*/" nil t)
            (replace-match ""))
          ;; delete // comments
          (goto-char (point-min))
          (while (search-forward-regexp "//.*$" nil t)
            (replace-match ""))
          ;; search symbols
          (setq ac-c-headers--symbols-cache
                (cons (cons header
                            (delete-dups
                             (let ((res nil))
                               (goto-char (point-min))
                               (while (search-forward-regexp
                                       "\\_<[a-zA-Z_]*\\_>" nil t)
                                 (setq res (cons (match-string 0) res)))
                               res)))
                      ac-c-headers--symbols-cache)))))))

(defun ac-c-headers--symbols-list (&optional buffer)
  "returns possible completions for the buffer"
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((res nil) header)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                "^#include *[<\"]\\([^>\"]*\\)[>\"]" nil t)
          (setq header (match-string 1))
          (unless (assoc header ac-c-headers--symbols-cache)
            (ac-c-headers--symbols-update header))
          (setq res (append (cdr (assoc header ac-c-headers--symbols-cache))
                            res))))
      res)))

;; + ac-sources

(defvar ac-source-c-headers
  '((prefix . "#include *[<\"][^<>\"]*?\\([^<>\"/]*\\)")
    (candidates . ac-c-headers--files-list)
    (action . (lambda ()
                (when (string-match "\\.h$" candidate)
                  (ac-c-headers--symbols-update candidate)
                  (cond ((looking-at "[>\"]")
                         (forward-char 1)
                         (newline-and-indent))
                        ((looking-back "#include *<\\([^<]*\\)")
                         (insert ">\n"))
                        (t
                         (insert "\"\n"))))))
    (symbol . "h")
    (requires . 0)
    (cache)))

(defvar ac-source-c-header-symbols
  '((candidates . ac-c-headers--symbols-list)
    (symbol . "h")
    (cache)))

;; + provide

(provide 'ac-c-headers)

;;; ac-c-headers.el ends here
