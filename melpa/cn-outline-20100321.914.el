;;; cn-outline.el -- minor mode for column-number-base line folding

;; Copyright (C) 2009,2010  kitokitoki

;; Author: kitokitoki <morihenotegami@gmail.com>
;; Keywords: outliner
;; Package-Version: 20100321.914
;; Prefix: cn-

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

;;; Setting Sample

;; (require 'cn-outline)
;; (setq-default cn-outline-mode t)
;; (global-set-key (kbd "C-c C-c C-c") 'cn-outline-mode)

;; Change Log
;; 1.0.1: キーバインドを変更
;; 1.0.0: 新規作成

;;; Commentary:

;; C-u 3 C-x $ 相当。

;;; Code:

(defvar cn-outline-mode nil
  "ポイント位置のカラム値を基準に折りたたみ表示するマイナーモード")
(defvar cn-outline-mode-hook nil)

(make-variable-buffer-local 'cn-outline-mode)
(make-variable-buffer-local 'cn-outline-mode-hook)

(unless (assq 'cn-outline-mode minor-mode-alist)
  (add-to-list 'minor-mode-alist '(cn-outline-mode " CN")))

(defun cn-outline-mode (&optional arg)
  "Indent Outline minor mode"
  (interactive "P")
  (setq cn-outline-mode
        (if (null arg)
            (not cn-outline-mode)
          (> (prefix-numeric-value arg) 0)))
  (unless cn-outline-mode
    (let ((cn-outline-mode t))
      (cn-clear-folding))))

(setq cn-outline-mode-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-9") 'cn-fold-at-point)    
    (define-key map (kbd "C-c C-0") 'cn-clear-folding)
    map))

(add-to-list 'minor-mode-map-alist (cons 'cn-outline-mode cn-outline-mode-key-map))

(defun cn-fold-at-point ()
  "ポイント位置を基準に折り畳み表示する"
  (interactive)
  (set-selective-display (+ 1 (current-column))))

(defun cn-clear-folding ()
  "折り畳み表示を解除する"
  (interactive)
  (set-selective-display 0))

(run-hooks 'cn-outline-mode-hook)

(provide 'cn-outline)
;; cn-outline.el ends here

;;; cn-outline.el ends here
