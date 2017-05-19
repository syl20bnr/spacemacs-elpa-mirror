;;; e2wm-direx.el --- Plugin of e2wm.el for direx.el

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager, convenience
;; Package-Version: 20170509.601
;; URL: https://github.com/aki2o/e2wm-direx
;; Version: 0.0.5
;; Package-Requires: ((e2wm "1.2") (direx "0.1alpha"))

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
;; see <https://github.com/aki2o/e2wm-direx/blob/master/README.md>

;;; Dependency:
;; 
;; - e2wm.el ( see <https://github.com/kiwanami/emacs-window-manager> )
;; - direx.el ( see <https://github.com/m2ym/direx-el> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'e2wm-direx)

;;; Configuration:
;; 
;; (setq e2wm:c-code-recipe
;;       '(| (:left-max-size 40)
;;           (- (:upper-size-ratio 0.6)
;;              tree history)
;;           (- (:lower-max-size 150)
;;              (| (:right-max-size 40)
;;                 main imenu)
;;              sub)))
;; 
;; (setq e2wm:c-code-winfo
;;       '((:name main)
;;         (:name tree    :plugin direx)
;;         (:name history :plugin history-list)
;;         (:name imenu   :plugin imenu :default-hide nil)
;;         (:name sub     :buffer "*info*" :default-hide t)))

;;; Customization:
;; 
;; Nothing

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "e2wm-direx:[^:]" :docstring t)
;; `e2wm-direx:start-sync-timer'
;; Not documented.
;; `e2wm-direx:stop-sync-timer'
;; Not documented.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - e2wm.el ... Version 1.2
;; - direx.el ... Version 0.1alpha


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'direx)
(require 'direx-project)
(require 'hl-line)

(defvar e2wm-direx::winfo nil)
(defvar e2wm-direx::sync-timer nil)
(defvar e2wm-direx::current-path nil)

(defun e2wm-direx:def-plugin (frame wm winfo)
  (setq e2wm-direx::winfo winfo)
  (setq e2wm-direx::current-path nil)
  (e2wm-direx:start-sync-timer))

(defun e2wm-direx:start-sync-timer ()
  (interactive)
  (when (not e2wm-direx::sync-timer)
    (setq e2wm-direx::sync-timer
          (run-with-idle-timer idle-update-delay
                               t
                               'e2wm-direx:do-sync))
    (e2wm:message "DirEX timer started.")))

(defun e2wm-direx:stop-sync-timer ()
  (interactive)
  (when (timerp e2wm-direx::sync-timer)
    (cancel-timer e2wm-direx::sync-timer))
  (setq e2wm-direx::sync-timer nil)
  (e2wm:message "DirEX timer stopped."))

(defun e2wm-direx:do-sync ()
  (if (not (e2wm-direx::active-p))
      (e2wm-direx:stop-sync-timer)
    (let* ((wm (e2wm:pst-get-wm))
           (wname (wlf:window-name e2wm-direx::winfo))
           (mbuf (e2wm:history-get-main-buffer))
           (mpath (buffer-file-name mbuf))
           (sync-point (lambda (buf)
                         (with-current-buffer mbuf
                           (direx:maybe-goto-current-buffer-item buf)))))
      (cond ((not mpath)
             ;; Main buffer is not target.
             (wlf:set-buffer wm wname (e2wm-direx::get-err-buffer)))
            ((and e2wm-direx::current-path
                  (string= mpath e2wm-direx::current-path))
             ;; Already sync is done.
             t)
            (t
             ;; Try sync.
             (e2wm:message "DirEX update current path : %s" mpath)
             (setq e2wm-direx::current-path mpath)
             (with-current-buffer mbuf
               (direx:aif (or (ignore-errors
                                (direx-project:find-project-root-noselect (or buffer-file-name
                                                                              default-directory)))
                              (direx:find-directory-reuse-noselect default-directory))
                   (with-current-buffer it
                     (e2wm:message "DirEX update active buffer")
                     (direx:awhen (direx:item-at-point)
                       (when (not (string= mpath
                                           (direx:file-full-name (direx:item-tree it))))
                         (e2wm:message "DirEX move point to %s" mpath)
                         (or (funcall sync-point (current-buffer))
                             (progn (direx:refresh-whole-tree)
                                    (funcall sync-point (current-buffer))))))
                     (hl-line-mode 1)
                     (hl-line-highlight)
                     (set-window-point (wlf:get-window wm wname) (point))
                     (wlf:set-buffer wm wname (current-buffer)))
                 ;; Failed to sync
                 (wlf:set-buffer wm wname (e2wm-direx::get-err-buffer)))))))))

(e2wm:plugin-register 'direx "DirEX" 'e2wm-direx:def-plugin)


(defun e2wm-direx::active-p ()
  (and (e2wm:managed-p)
       (eq (e2wm:pst-window-plugin-get (e2wm:pst-get-wm) (wlf:window-name e2wm-direx::winfo))
           'direx)
       (wlf:get-window (e2wm:pst-get-wm) (wlf:window-name e2wm-direx::winfo))
       t))

(defvar e2wm-direx::err-buffer-name " *WM:DirEX-Err*")

(defun e2wm-direx::get-err-buffer ()
  (or (get-buffer e2wm-direx::err-buffer-name)
      (with-current-buffer (get-buffer-create e2wm-direx::err-buffer-name)
        (insert "Available node is nothing.\n")
        (setq buffer-read-only t)
        (current-buffer))))


(provide 'e2wm-direx)
;;; e2wm-direx.el ends here
