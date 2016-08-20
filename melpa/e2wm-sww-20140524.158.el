;;; e2wm-sww.el --- Plugin of e2wm.el to switch plugin quickly

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager
;; Package-Version: 20140524.158
;; URL: https://github.com/aki2o/e2wm-sww
;; Version: 0.0.2
;; Package-Requires: ((e2wm "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; see <https://github.com/aki2o/e2wm-sww/blob/master/README.md>

;;; Dependency:
;; 
;; - e2wm.el ( see <https://github.com/kiwanami/emacs-window-manager> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'e2wm-sww)

;;; Configuration:
;; 
;; (setq e2wm:c-code-recipe
;;       '(- (:upper-size-ratio 0.5)
;;           (| (:right-max-size 70)
;;              main
;;              (- sw-helper helper))
;;           sub))
;; 
;; (setq e2wm:c-code-winfo
;;       '((:name main)
;;         (:name helper    :plugin files        :sww sw-helper)
;;         (:name helper    :plugin imenu        :sww sw-helper :sww-default t)
;;         (:name helper    :plugin history-list :sww sw-helper :sww-label "Hist")
;;         (:name sw-helper :plugin sww)
;;         (:name sub       :buffer "*info*" :default-hide t)))
;; 
;; ;; For more information, see <https://github.com/aki2o/e2wm-sww/blob/master/README.md>
;; 

;;; Customization:
;; 
;; Nothing

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "e2wm-sww:[^:]" :docstring t)
;; `e2wm-sww:click'
;; Click the button under cursor.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - e2wm.el ... Version 1.2


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'wid-edit)

(defgroup e2wm-sww nil
  "Plugin of e2wm.el to switch plugin quickly"
  :group 'e2wm
  :prefix "e2wm-sww:")

(defface e2wm-sww:button-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face of the button to switch plugin."
  :group 'e2wm-sww)

(defface e2wm-sww:button-pressed-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style pressed-button)
     :background "lightgrey" :foreground "black")
    (t
     :inverse-video t))
  "Face of the button to switch plugin when that pressed."
  :group 'e2wm-sww)

(defface e2wm-sww:button-mouse-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t
     :inverse-video t))
  "Face of the button to switch plugin when mouse is on that."
  :group 'e2wm-sww)


(defvar e2wm-sww:default-plugins nil
  "List of the symbol of the plugin that is activated in the buffer initially.")
(make-variable-buffer-local 'e2wm-sww:default-plugins)

(defvar e2wm-sww::window-margin 3)


;;;;;;;;;;;;;
;; Utility

(defsubst e2wm-sww::get-current-line-width ()
  (- (point-at-eol) (point-at-bol)))

(defsubst e2wm-sww::get-current-buffer-height ()
  (count-lines (point-min) (point-max)))

(defsubst e2wm-sww::get-current-buffer-width ()
  (save-excursion
    (loop initially (goto-char (point-min))
          while (not (eobp))
          maximize (e2wm-sww::get-current-line-width)
          do (forward-line 1))))

(defsubst e2wm-sww::managed-winfo-p (self-wname winfo)
  (eq self-wname (wlf:window-option-get winfo :sww)))


;;;;;;;;;;;;;;
;; For E2WM

(e2wm:plugin-register 'sww "SWW" 'e2wm-sww:def-plugin)

(defun e2wm-sww:def-plugin (frame wm winfo)
  (let* ((wname (wlf:window-name winfo))
         (pinfos (loop for w in (wlf:wset-winfo-list wm)
                       if (e2wm-sww::managed-winfo-p wname w)
                       collect `(,(wlf:window-option-get w :plugin) . ,(wlf:window-option-get w :sww-label))))
         (vorder (not (wlf:window-vertical winfo)))
         (buf (e2wm-sww::get-buffer wname vorder))
         (wnd (progn (wlf:set-buffer wm wname buf)
                     (wlf:get-window wm wname)))
         (wndlength (if vorder (window-height wnd) (window-width wnd))))
    (e2wm:message "SWW setup buffer of %s. plugin:%s vorder:%s size:%s" wname pinfos vorder wndlength)
    (with-current-buffer buf
      (when (e2wm-sww::condition-updated-p pinfos vorder wndlength)
        (e2wm:message "SWW remake contents")
        (e2wm-sww::remake-contents pinfos vorder wndlength))
      (let ((size (e2wm-sww::get-fit-length vorder)))
        (e2wm:message "SWW window resize %s to %s" (if vorder "width" "height") size)
        (wlf:window-resize wnd (not vorder) size))
      (goto-char (point-min))
      (set-window-point wnd (point)))
    (e2wm-sww::set-default-plugin wname wm)))


;;;;;;;;;;
;; Mode

(defvar e2wm-sww:sww-mode-map
  (let ((map (e2wm:define-keymap
              '(("C-m" . e2wm-sww:click)
                ) e2wm:prefix-key)))
    (set-keymap-parent map widget-keymap)
    map))

(define-derived-mode e2wm-sww:sww-mode special-mode "SWW"
  "Plugin of e2wm.el to switch plugin quickly"
  (set (make-local-variable 'widget-button-face) 'e2wm-sww:button-face)
  (set (make-local-variable 'widget-button-pressed-face) 'e2wm-sww:button-pressed-face)
  (set (make-local-variable 'widget-mouse-face) 'e2wm-sww:button-mouse-face)
  (set (make-local-variable 'widget-push-button-prefix) "")
  (set (make-local-variable 'widget-push-button-suffix) "")
  (setq buffer-read-only t))


;;;;;;;;;;;;;;;;;;
;; Setup Buffer

(defvar e2wm-sww::manage-plugin-names nil)
(defvar e2wm-sww::vertical-order-p nil)
(defvar e2wm-sww::last-window-length nil)

(defun e2wm-sww::get-buffer (wname vertical-order)
  (let ((bufnm (format " *WM:SWW-%s*" wname)))
    (or (get-buffer bufnm)
        (with-current-buffer (generate-new-buffer bufnm)
          (e2wm-sww:sww-mode)
          (set (make-local-variable 'e2wm-sww::manage-plugin-names) nil)
          (set (make-local-variable 'e2wm-sww::vertical-order-p) vertical-order)
          (set (make-local-variable 'e2wm-sww::last-window-length) 0)
          (current-buffer)))))

(defun e2wm-sww::condition-updated-p (plugin-infos vertical-order wndlength)
  (or (not (eq (length plugin-infos)
               (length e2wm-sww::manage-plugin-names)))
      (loop for i in plugin-infos
            for p = (car i)
            if (not (memq p e2wm-sww::manage-plugin-names))
            return t)
      (not (eq vertical-order e2wm-sww::vertical-order-p))
      (not (eq wndlength e2wm-sww::last-window-length))))

(defsubst e2wm-sww::layout-button-location (btnnm vertical-order wndlength idx)
  (if (= idx 1)
      (widget-insert "\n")
    (cond (vertical-order
           (let* ((maxheight (- wndlength e2wm-sww::window-margin))
                  (ventrycount (if (<= maxheight 1) 1 (/ maxheight 2))))
             (cond ((<= idx ventrycount)
                    (widget-insert "\n")
                    (widget-insert "\n"))
                   (t
                    (goto-char (point-min))
                    (forward-line 1)
                    (when (> ventrycount 1)
                      (forward-line (* (mod (- idx 1) ventrycount) 2)))
                    (goto-char (point-at-eol))))))
          (t
           (let ((threshold (- wndlength e2wm-sww::window-margin (length btnnm) 1))
                 (currlength (e2wm-sww::get-current-line-width)))
             (when (> currlength threshold)
               (widget-insert "\n")
               (widget-insert "\n"))))))
  (widget-insert " "))

(defun e2wm-sww::remake-contents (plugin-infos vertical-order wndlength)
  (let ((buffer-read-only nil)
        (plugin-names (loop for i in plugin-infos collect (car i))))
    (erase-buffer)
    (setq e2wm-sww::manage-plugin-names plugin-names)
    (setq e2wm-sww::vertical-order-p vertical-order)
    (setq e2wm-sww::last-window-length wndlength)
    (setq header-line-format (format "SWW [%s]" (length plugin-names)))
    (loop initially (goto-char (point-min))
          with idx = 1
          for p in plugin-names
          for lblnm = (assoc-default p plugin-infos)
          for btnnm = (or (when (and lblnm (not (string= lblnm "")))
                            lblnm)
                          (e2wm:$plugin-title (e2wm:plugin-get p)))
          do (e2wm-sww::layout-button-location btnnm vertical-order wndlength idx)
          do (widget-create 'push-button
                            :tag (format " %s " btnnm)
                            :action `(lambda (widget &optional event)
                                       (e2wm-sww::show ',p)))
          do (incf idx))))

(defun e2wm-sww::get-fit-length (vertical-order)
  (+ (if vertical-order
         (e2wm-sww::get-current-buffer-width)
       (e2wm-sww::get-current-buffer-height))
     e2wm-sww::window-margin))


;;;;;;;;;;;;;;;;;;;
;; Switch Plugin

(defvar e2wm-sww::active-plugin-alist nil)
(make-variable-buffer-local 'e2wm-sww::active-plugin-alist)

(defun e2wm-sww::set-active-plugin (wname plugin-name)
  (with-current-buffer (e2wm:history-get-main-buffer)
    (let ((curre (assq wname e2wm-sww::active-plugin-alist)))
      (when curre
        (setq e2wm-sww::active-plugin-alist (delq curre e2wm-sww::active-plugin-alist)))
      (push `(,wname . ,plugin-name) e2wm-sww::active-plugin-alist))))

(defun e2wm-sww::set-default-plugin (self-wname &optional wm)
  (let* ((wm (or wm (e2wm:pst-get-wm)))
         (mconfs (with-current-buffer (e2wm:history-get-main-buffer)
                   (list e2wm-sww:default-plugins
                         e2wm-sww::active-plugin-alist)))
         (defplugs (when mconfs (pop mconfs)))
         (act-pinfos (when mconfs (pop mconfs)))
         (local-pinfos (loop for w in (wlf:wset-winfo-list wm)
                             for wname = (wlf:window-name w)
                             for p = (wlf:window-option-get w :plugin)
                             if (and (e2wm-sww::managed-winfo-p self-wname w)
                                     (memq p defplugs)
                                     (not (assoc-default wname act-pinfos)))
                             collect `(,wname . ,p)))
         (global-pinfos (loop for w in (wlf:wset-winfo-list wm)
                              for wname = (wlf:window-name w)
                              if (and (e2wm-sww::managed-winfo-p self-wname w)
                                      (wlf:window-option-get w :sww-default)
                                      (not (assoc-default wname act-pinfos))
                                      (not (assoc-default wname local-pinfos)))
                              collect `(,wname . ,(wlf:window-option-get w :plugin)))))
    (e2wm:message "SWW set default plugin. currdef:%s active:%s local:%s global:%s"
                  defplugs act-pinfos local-pinfos global-pinfos)
    (loop for i in (append act-pinfos local-pinfos global-pinfos)
          for plugin-name = (cdr-safe i)
          if plugin-name
          do (e2wm-sww::show plugin-name wm))))

(defun e2wm-sww::reorder-winfo-list (wm winfo)
  (setf (wlf:wset-winfo-list wm)
        (append (list winfo)
                (loop with plugin-name = (wlf:window-option-get winfo :plugin)
                      for w in (wlf:wset-winfo-list wm)
                      if (not (eq (wlf:window-option-get w :plugin) plugin-name))
                      collect w))))

(defun e2wm-sww::switch-winfo-prop (from-winfo to-winfo)
  (setf (wlf:window-window to-winfo) (wlf:window-window from-winfo))
  (setf (wlf:window-window from-winfo) nil)
  (setf (wlf:window-edges to-winfo) (wlf:window-edges from-winfo))
  (setf (wlf:window-edges from-winfo) nil)
  (dolist (p '(:buffer :window-point :window-first-line-point))
    (plist-put (wlf:window-options from-winfo) p nil))
  (wlf:window-shown-set from-winfo nil)
  (wlf:window-shown-set to-winfo t))

(defun e2wm-sww::show (plugin-name &optional wm)
  (e2wm:message "SWW check to show %s" plugin-name)
  (let* ((wm (or wm (e2wm:pst-get-wm)))
         (winfo (loop for w in (wlf:wset-winfo-list wm)
                      if (eq (wlf:window-option-get w :plugin) plugin-name)
                      return w))
         (wname (when winfo (wlf:window-name winfo)))
         (currwinfo (when (and wname
                               (not (eq plugin-name (e2wm:pst-window-plugin-get wm wname))))
                      (wlf:get-winfo wname (wlf:wset-winfo-list wm)))))
    (when (and winfo currwinfo)
      (e2wm:message "SWW switch plugin of %s. %s -> %s"
                    wname
                    (wlf:window-option-get currwinfo :plugin)
                    (wlf:window-option-get winfo     :plugin))
      (e2wm-sww::switch-winfo-prop currwinfo winfo)
      (e2wm-sww::reorder-winfo-list wm winfo)
      (funcall (e2wm:$plugin-update (e2wm:plugin-get plugin-name)) (selected-frame) wm winfo)
      (e2wm-sww::set-active-plugin wname plugin-name))))


;;;;;;;;;;;;;;;;;;
;; User Command

(defun e2wm-sww:click (pos &optional event)
  "Click the button under cursor."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    (when button
      (widget-apply-action button event))))


(provide 'e2wm-sww)
;;; e2wm-sww.el ends here
