;;; blockdiag-mode.el --- Major mode for editing blockdiag files

;; Copyright (C) 2016 xcezx

;; Author: xcezx <main.xcezx@gmail.com>
;; URL: https://github.com/xcezx/xdiag-mode
;; Package-Version: 20160426.2224
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

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

;; Major mode for editing blockdiag files.

;;; Code:

(require 'smie)

(defgroup blockdiag nil
  "Major mode for editing blockdiag files."
  :group 'language)

(defcustom blockdiag-indent-level 4
  "Basic indentation step for blockdiag-mode."
  :type 'integer
  :group 'blockdiag)

(defconst blockdiag-diagram-attribute-keywords
  '(
    "default_fontsize"
    "default_group_color"
    "default_linecolor"
    "default_node_color"
    "default_shape"
    "default_textcolor"
    "edge_layout"
    "node_height"
    "node_width"
    "orientation"
    "span_height"
    "span_width"
    ))

(defconst blockdiag-node-attribute-keywords
  '(
    "background"
    "color"
    "description"
    "fontsize"
    "height"
    "icon"
    "label"
    "numbered"
    "rotate"
    "shape"
    "stacked"
    "style"
    "textcolor"
    "width"
    ))

(defconst blockdiag-edge-attribute-keywords
  '(
    "color"
    "dir"
    "folded"
    "fontsize"
    "hstyle"
    "label"
    "style"
    "textcolor"
    "thick"
    ))

(defconst blockdiag-font-lock-keywords
  `((,(regexp-opt
       (append
        blockdiag-diagram-attribute-keywords
        blockdiag-node-attribute-keywords
        blockdiag-edge-attribute-keywords) 'words) . font-lock-keyword-face)
    (,(regexp-opt '("blockdiag") 'words) . font-lock-constant-face)))

(defvar blockdiag-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for blockdiag-mode.")

(defconst blockdiag-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ";") (assoc ",")))))

(defun blockdiag-smie-rules (kind token)
  "Provide indentation rules for KIND given TOKEN.
See the documentation of `smie-rules-function' for further
information."
  (pcase (cons kind token)
    (`(:elem . basic) blockdiag-indent-level)
    (`(:elem . arg) 0)
    (`(:list-intro . ,(or `";" `"")) t)
    (`(:before . "{") (if (smie-rule-hanging-p)
                          (smie-rule-parent 0)))))

;;;###autoload
(define-derived-mode blockdiag-mode prog-mode "blockdiag"
  "Major mode for editing blockdiag file in Emacs"

  (setq font-lock-defaults '((blockdiag-font-lock-keywords)))

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (smie-setup blockdiag-smie-grammar #'blockdiag-smie-rules))

;;;###autoload
(add-to-list 'magic-mode-alist '("^blockdiag" . blockdiag-mode))

(provide 'blockdiag-mode)

;;; blockdiag-mode.el ends here
