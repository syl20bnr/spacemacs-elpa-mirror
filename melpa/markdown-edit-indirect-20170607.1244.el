;;; markdown-edit-indirect.el --- Edit markdown code block in a separate buffer -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/markdown-edit-indirect.el
;; Package-Version: 20170607.1244
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (markdown-mode "2.2") (edit-indirect "0.1.4"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Commentary:

;; Edit markdown code block in a separate buffer like `org-edit-src-code'.
;;
;; Usage:
;;
;; You need to place the cursor inside a code block i.e. surrounded by triple
;; backticks (```), and call \\[markdown-edit-indirect]. Alternatively you can
;; add the following key binding:
;;
;;     (eval-after-load 'markdown-mode
;;       '(define-key markdown-mode-map (kbd "C-c '") 'markdown-edit-indirect))
;;
;; Or in case you use \\[use-package] you can install and configure everything
;; in one step:
;;
;;     (use-package markdown-mode
;;       :ensure t
;;       :demand markdown-edit-indirect
;;       :mode (("\\.md" . markdown-mode)
;;              ("\\.markdown" . markdown-mode))
;;       :bind (:markdown-mode-map
;;              ("C-c '" . markdown-edit-indirect)))

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'edit-indirect)
(require 'markdown-mode)

(defgroup markdown-edit-indirect nil
  "Edit markdown code blocks in a separate buffer."
  :prefix "markdown-edit-indirect-"
  :group 'markdown)

(defcustom markdown-edit-indirect-lang-alist
  '(("Shell"            . sh-mode)
    ("JavaScript"       . js-mode)
    ("reStructuredText" . rst-mode))
  "Alist for languages used to edit a code block.

The key part of this alist is case insensitive."
  :type '(alist :key-type string :value-type symbol)
  :group 'markdown-edit-indirect)

(defun markdown-edit-indirect-guess-mode (lang)
  "Guess a major mode for LANG."
  (and lang (let ((mode (intern (concat (downcase lang) "-mode"))))
              (and (fboundp mode) mode))))

;;;###autoload
(defun markdown-edit-indirect ()
  "Edit markdown code block indirect."
  (interactive)
  (cl-multiple-value-bind (begin end)
      (plist-get (text-properties-at (point)) 'markdown-gfm-code)
    (if (and begin end)
        (let* ((lang (save-excursion
                       (goto-char begin)
                       (cond
                        ((looking-back markdown-regex-gfm-code-block-open (line-beginning-position))
                         (setq begin (1+ begin))
                         (match-string-no-properties 2))
                        (t
                         (beginning-of-line 0)
                         (and (looking-at markdown-regex-gfm-code-block-open) (match-string-no-properties 3))))))
               (mode (or (cdr (assoc-string lang markdown-edit-indirect-lang-alist t))
                         (markdown-edit-indirect-guess-mode lang)
                         #'normal-mode))
               (edit-indirect-guess-mode-function (lambda (_parent-buffer _beg _end)
                                                    (funcall mode))))
          (edit-indirect-region begin (1- end) 'display-buffer))
      (user-error "Not inside a code block"))))

(provide 'markdown-edit-indirect)
;;; markdown-edit-indirect.el ends here
