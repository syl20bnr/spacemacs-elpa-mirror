;;; wavefront-obj-mode.el --- Major mode for Wavefront obj files
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Version: 0.6
;; Package-Version: 20170808.1716
;; Author: Sasha Kovar <sasha-emacs@arcocene.org>
;; Url: http://github.com/abend/wavefront-obj-mode
;;
;;; Commentary:
;;
;; A hodepodge of quick random hacks for editing Wavefront obj files.
;; Use as you wish.
;;

;;; Code:

(defvar wavefront-obj-mode-builtins-re (concat "^" (regexp-opt '("d" "f" "s"
                                                                 "v" "vn" "vp" "vt"
                                                                 "Ka" "Ke" "Kd" "Ks"
                                                                 "Ni" "Ns"
                                                                 "Tf" "Tr"
                                                                 "illum" "bump" "disp" "decal" "refl"
                                                                 "map_d"
                                                                 "map_Ka" "map_Ke" "map_Kd" "map_Ks"
                                                                 "map_Ni" "map_Ns"
                                                                 "map_bump"))))

(defvar wavefront-obj-mode-keywords
  `((,wavefront-obj-mode-builtins-re . font-lock-builtin-face)
    ("^\\(usemtl\\|mtllib\\|newmtl\\).*" . font-lock-string-face)
    ("^\\(o\\|g\\) .*" . font-lock-function-name-face)
    ("^#.*" . font-lock-comment-face)))

(defvar wavefront-obj-mode-defun-regex
  "^\\(g\\|o\\)\\s-+\\(.+\\)")

;; if we want separate submenus for objects and groups
;; (defvar wavefront-obj-imenu-generic-expression
;;   '(("Objects"  "^o\\s-+\\(.*\\)" 1)
;;     ("Groups"  "^g\\s-+\\(.*\\)" 1)))

(defvar wavefront-obj-imenu-generic-expression
  `((nil ,wavefront-obj-mode-defun-regex 2)))

;;;###autoload
(define-derived-mode wavefront-obj-mode fundamental-mode "Obj"
  "Major mode for editing Wavefront obj ascii files.

\\{wavefront-obj-mode-map}"
  :group 'wavefront-obj
  (setq font-lock-defaults '(wavefront-obj-mode-keywords))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'paragraph-start) wavefront-obj-mode-defun-regex)
  (set (make-local-variable 'beginning-of-defun-function)
       'wavefront-obj-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'wavefront-obj-end-of-defun)
  (set (make-local-variable 'add-log-current-defun-function)
       'wavefront-obj-current-defun)
  (set (make-local-variable 'imenu-generic-expression)
       wavefront-obj-imenu-generic-expression))

(defun wavefront-obj-try-to-add-imenu ()
  (ignore-errors (imenu-add-to-menubar "Imenu")))
(add-hook 'wavefront-obj-mode-hook 'wavefront-obj-try-to-add-imenu)


(defun wavefront-obj-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Every 'g' or 'o' block is considered to be a defun
 (see `wavefront-obj-mode-defun-regex').  Return t unless
search stops due to beginning or end of buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (wavefront-obj-end-of-defun (- arg)))

(defun wavefront-obj-end-of-defun (&optional arg)
  "Move forward to the end of the current defun.
Every 'g' or 'o' block is considered to be a defun
 (see `wavefront-obj-mode-defun-regex').
Return t unless search stops due to beginning or end of buffer."
  (interactive "p")
  (or arg (setq arg 1))

  (or (not (eq this-command 'wavefront-obj-beginning-of-defun))
      (eq last-command 'wavefront-obj-end-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))

  (if (< arg 0)
      (re-search-backward wavefront-obj-mode-defun-regex nil t)
      (re-search-forward wavefront-obj-mode-defun-regex nil t)))

(defun wavefront-obj-current-defun ()
  "`add-log-current-defun-function' for Wavefront Obj mode."
  (save-excursion
    (when (re-search-backward wavefront-obj-mode-defun-regex nil t)
      (match-string 2))))


(provide 'wavefront-obj-mode)

;;; wavefront-obj-mode.el ends here
