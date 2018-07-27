;;; ac-skk.el --- auto-complete-mode source for DDSKK a.k.a Japanese input method

;; Filename: ac-skk.el
;; Description: auto-complete-mode source for Japanese
;; Author: lugecy <https://twitter.com/lugecy>
;; Maintainer: myuhe
;; Copyright (C)  2014, lugecy,myuhe all rights reserved.
;; Created: 2014-11-24
;; Version: 0.1
;; Package-Version: 20141230.119
;; Keywords: convenience, auto-complete
;; URL: https://github.com/myuhe/ac-skk.el
;; Package-Requires: ((auto-complete "1.3.1")(ddskk "16.0.50")(tinysegmenter "0")(cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:

;; It is necessary to auto-complete.el Configurations.
;; heavily borrowed from dabbrev.el and skk-kakutei-extra.el.
;; Original Author is lugecy.  Thanks!!
;; <https://github.com/lugecy/dot-emacs/blob/master/local-elisp/ac-skk.el>

;; Installation:
;; ============================================= 

;; Put the ac-skk.el to your
;; load-path.
;; Add to init file :
;; (require 'ac-ja.el)

;;
;;; Changelog:
;;

;;; Code:

(require 'cl-lib)
(require 'tinysegmenter)
(require 'auto-complete)
(require 'skk)
(require 'context-skk)
(require 'skk-comp)


;; Customization
(defgroup ac-skk nil "Auto complete source for DDSKK"
  :group 'auto-complete)

(defcustom ac-skk-special-sources '(ac-source-skk ac-source-skk-hiracomp)
  "When non-nil, show completion result flags during fuzzy completion."
  :type '(repeat symbol)
  :group 'ac-skk)

(defvar ac-source-skk
  '((prefix . ac-skk-prefix)
    (candidates . ac-skk-candidates)
    (match . (lambda (prefix cands) cands))
    (requires . 1)
    (symbol . "SKK")))

(defun ac-skk-prefix ()
  (when (and skk-mode
             (eq skk-henkan-mode 'on))
    skk-henkan-start-point))

(defun ac-skk-make-cand (cand action midasi count)
  (propertize cand 'action action 'henkan-key midasi 'skk-count count))

(defun ac-skk-make-cand-list (midasi prog-list)
  (let* ((henkan-list (skk-search-progs midasi prog-list 'remove-note))
         (candidates (cl-loop for cand in henkan-list
                           for i from 0
                           collect (ac-skk-make-cand cand 'ac-skk-kakutei midasi i)))
         (forward-cand (ac-skk-make-cand midasi 'ac-skk-henkan-forward midasi (length candidates))))
    candidates))

(defvar ac-skk-selected-candidate nil)
(defvar ac-skk-ac-trigger-commands-orig nil)

(defun ac-skk-candidates ()
  (when (eq skk-henkan-mode 'on)
    (let ((henkan-prog-list (append
                             (cl-subseq skk-search-prog-list
                                     0 (cl-position '(skk-okuri-search) skk-search-prog-list :test #'equal))
                             (when skk-auto-okuri-process
                               (list '(skk-okuri-search-1)))))
          (midasi-prog-list '((skk-comp-from-jisyo skk-jisyo))))
      (cl-loop for midasi in (cons ac-prefix (skk-comp-get-all-candidates ac-prefix nil midasi-prog-list))
               nconc (ac-skk-make-cand-list midasi henkan-prog-list)))))
;; 順番が(Recent headに)変わらない時があるのはskk-studyの影響

(defun ac-skk-kakutei ()
  (when skk-katakana
    (error "No Support skk-katakana mode."))
  (delete-region skk-henkan-start-point (point))
  (insert (get-text-property 0 'henkan-key ac-skk-selected-candidate)) ;only support execute in ac-complete function
  (ac-skk-start-henkan (1+ (get-text-property 0 'skk-count ac-skk-selected-candidate)))
  (skk-kakutei))

(defun ac-skk-henkan-forward ()
  (ac-skk-start-henkan (get-text-property 0 'skk-count (buffer-substring ac-point (point))))
  (skk-start-henkan 1))

(defun ac-skk-start-henkan (count)
  (let ((skk-show-annotation nil))         ;darty hack??
    (dotimes (i count)
      (skk-start-henkan 1))))

;;;; hiracomp
(defvar ac-source-skk-hiracomp
  '((prefix . ac-skk-prefix-hiracomp)
    (candidates . ac-skk-hiracomp-candidates)
    (match . (lambda (prefix cands) cands))
    (requires . 2)
    (symbol . "SKKH")))

(defun ac-skk-prefix-hiracomp ()
  (when (and skk-mode
             skk-j-mode
             (not skk-henkan-mode)
             (fboundp 'tseg-segment))
    (save-match-data
      (when (looking-back "\\(?:\\cH\\|\\cK\\|\\cC\\)\\{1,10\\}" (max (- (point) 10) 0) t)
        (let* ((segs (tseg-segment (substring-no-properties (match-string 0) 0)))
               (lst (last segs 2)))
          (- (point)
             (if (null (cdr lst))
                 (length (car lst))
               (if (= (length (cadr lst)) 1)
                   (cl-loop for c in lst sum (length c))
                 (length (cadr lst))))))))))

(defun ac-skk-hiracomp-candidates ()
  (let ((prog-list '((skk-search-jisyo-file skk-jisyo 0 t)
                     (skk-okuri-search-1)
                     ;(skk-search-server skk-aux-large-jisyo 10000)
                     (skk-search-katakana))))
    (append
     (skk-search-progs ac-prefix prog-list 'remove-note)
     (cl-loop for i from 0 below (length ac-prefix)
           collect (propertize (concat (substring ac-prefix 0 i) "▽" (substring ac-prefix i))
                               'action 'ac-skk-hiracomp-mes)))))

(defun ac-skk-hiracomp-mes ()
  (let ((midasi ""))
    (save-match-data
      (when (looking-back "▽\\(\\cH+\\)" nil t)
        (setq midasi (match-string 1))
        (delete-char (- (length (match-string 0))))))
    (skk-set-henkan-point-subr)
    (insert midasi)
    (ac-start :force-init t)))          ;これでいいのか？

;;;; Enable/Disable mode functions
(defvar ac-skk-enable nil)
(defvar ac-skk-ac-sources-orig nil)
(defvar ac-skk-save-variable '(ac-trigger-commands skk-dcomp-activate skk-dcomp-multiple-activate))

;;;###autoload
(defun ac-skk-enable ()
  (interactive)
  (setq ac-skk-enable t)
  (message "enabled ac-skk."))

;;;###autoload
(defun ac-skk-disable ()
  (interactive)
  (setq ac-skk-enable nil)
  (message "disabled ac-skk."))

;;;###autoload
(defun ac-skk-toggle ()
  (interactive)
  (if (not ac-skk-enable)
      (ac-skk-enable)
    (ac-skk-disable)))

(defun ac-skk-setup ()
  (when ac-skk-enable
    (set (make-local-variable 'ac-skk-ac-sources-orig) ac-sources)
    (setq ac-sources ac-skk-special-sources)
    (dolist (sym ac-skk-save-variable)
      (let ((store-sym (intern (format "ac-skk-%s-orig" sym))))
        (set (make-local-variable store-sym) (symbol-value sym))
        (set (make-local-variable sym) nil)))
    (setq ac-trigger-commands (append '(skk-insert skk-previous-candidate) ac-skk-ac-trigger-commands-orig))))

(defun ac-skk-cleanup ()
  (when (local-variable-p 'ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-ac-sources-orig)
    (kill-local-variable 'ac-skk-ac-sources-orig)
    (dolist (sym ac-skk-save-variable)
      (let ((store-sym (intern (format "ac-skk-%s-orig" sym))))
        (kill-local-variable sym)
        (kill-local-variable store-sym)))))

(add-hook 'skk-mode-hook 'ac-skk-setup)
(defadvice skk-mode-exit (after ac-skk activate)
  (ac-skk-cleanup))
(defadvice skk-j-mode-on (after ac-skk activate)
  (when (and ac-skk-enable ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-special-sources)
    (dolist (com '(skk-insert skk-previous-candidate))
      (add-to-list 'ac-trigger-commands com))))
(defadvice skk-latin-mode (after ac-skk activate)
  (when (and ac-skk-enable ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-ac-sources-orig
          ac-trigger-commands ac-skk-ac-trigger-commands-orig)))

(defadvice ac-trigger-command-p (after ac-trigger-command-p-for-viper activate)
  "Return non-nil if `this-command' is a trigger command for viper-mode."
  (setq ad-return-value
        (if (or ;(and skk-j-mode (not skk-henkan-mode))
                (and skk-henkan-mode
                     (not (memq 'skk-insert ac-trigger-commands))))
            nil
          ad-return-value)))

(defadvice ac-expand-string (after ad-ac-expand-string activate)
       (setq ac-skk-selected-candidate ac-selected-candidate))

(provide 'ac-skk)

;;; ac-skk.el ends here
