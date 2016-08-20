;;; compact-docstrings.el --- Shrink blank lines in docstrings and doc comments

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/compact-docstrings
;; Package-Version: 0.1
;; Keywords: convenience, faces, lisp, maint, c

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

;; Shrink blank lines in docstrings and doc comments
;;
;; Enable locally with `compact-docstrings-mode':
;;   (add-hook 'some-mode-hook #'compact-docstrings-mode)
;;
;; Enable globally (in all programming modes) with
;;   (add-hook 'after-init-hook #'global-compact-docstrings--mode)

;;; Code:

(defgroup compact-docstrings nil
  "Shrink empty lines in docstrings and doc comments."
  :group 'faces)

(defface compact-docstrings-face
  '((t :height 0.5))
  "Face applied to blank lines in docstrings."
  :group 'compact-docstrings)

(defcustom compact-docstrings-only-doc-blocks t
  "When nil, also shrink blank lines in regular strings and comments."
  :group 'compact-docstrings
  :type 'boolean)

(defun compact-docstrings--matcher (bound)
  "Find blank line in docstring, looking in point .. BOUND."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^\\s-*\n" bound t))
      (let ((syntax (syntax-ppss)))
        (when (and (or (nth 3 syntax)  ;; In string
                       (nth 4 syntax)) ;; In comment
                   (or (not compact-docstrings-only-doc-blocks)
                       (let ((face (get-text-property (point) 'face)))
                         (or (eq face 'font-lock-doc-face)
                             (and (listp face) (memq 'font-lock-doc-face face))))))
          (setq found t))))
    found))

(defconst compact-docstrings--keywords
  '((compact-docstrings--matcher 0 'compact-docstrings-face prepend)) 'append)

;;;###autoload
(define-minor-mode compact-docstrings-mode
  "Shrink empty lines in docstrings and doc comments."
  :lighter " compact"
  (if compact-docstrings-mode
      (font-lock-add-keywords nil compact-docstrings--keywords 'append)
    (font-lock-remove-keywords nil compact-docstrings--keywords))
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer))))

(defun compact-docstrings--mode-on ()
  "Turn on `compact-docstrings-mode', if appropriate."
  (when (derived-mode-p major-mode #'prog-mode)
    (compact-docstrings-mode)))

;;;###autoload
(defalias 'shrink-docstrings #'compact-docstrings--mode-on)

;;;###autoload
(define-globalized-minor-mode global-compact-docstrings-mode compact-docstrings-mode
  compact-docstrings--mode-on
  :init-value nil)

;;;; ChangeLog:

;; 2016-06-30  Clément Pit--Claudel	 <clement.pitclaudel@live.com>
;; 
;; 	Merge commit '421e26058a6b7131f144bce96c6b0ac902a2b420'
;; 
;; 2016-06-26  Clément Pit--Claudel	 <clement.pitclaudel@live.com>
;; 
;; 	Add 'packages/compact-docstrings/' from commit
;; 	'7ada669605c4e2a9a00fa6d03da7176f2c6e3297'
;; 
;; 	git-subtree-dir: packages/compact-docstrings git-subtree-mainline:
;; 	3ad3bf290c5527930de97589a89e2891ac023895 git-subtree-split:
;; 	7ada669605c4e2a9a00fa6d03da7176f2c6e3297
;; 


(provide 'compact-docstrings)
;;; compact-docstrings.el ends here
