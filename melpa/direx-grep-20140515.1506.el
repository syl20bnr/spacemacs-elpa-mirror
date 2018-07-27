;;; direx-grep.el --- Grep node of direx.el using incremental search like anything.el/helm.el

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; Package-Version: 20140515.1506
;; URL: https://github.com/aki2o/direx-grep
;; Version: 0.1.0
;; Package-Requires: ((direx "0.1alpha"))

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
;; see <https://github.com/aki2o/direx-grep/blob/master/README.md>

;;; Dependency:
;; 
;; - direx.el ( see <https://github.com/m2ym/direx-el> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'direx-grep)

;;; Configuration:
;; 
;; ;; Key Binding
;; (define-key direx:direx-mode-map (kbd "s") 'direx-grep:grep-item)
;; (define-key direx:direx-mode-map (kbd "S") 'direx-grep:grep-item-from-root)
;; (define-key direx:direx-mode-map (kbd "a") 'direx-grep:show-all-item-at-point)
;; (define-key direx:direx-mode-map (kbd "A") 'direx-grep:show-all-item)
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "direx-grep")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "direx-grep:[^:]" :docstring t)
;; `direx-grep:use-migemo'
;; Whether to use migemo.el in `direx-grep:grep-item'.
;; `direx-grep:toggle-use-migemo-key'
;; Keystroke for `direx-grep:toggle-use-migemo' in `direx-grep:grep-item'.
;; `direx-grep:ensure-greped-tree'
;; Whether to ensure the children of the target node before grep.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "direx-grep:[^:]" :docstring t)
;; `direx-grep:abort-grep'
;; Abort `direx-grep:grep-item'.
;; `direx-grep:toggle-use-migemo'
;; Toggle the value of `direx-grep:use-migemo'.
;; `direx-grep:grep-item'
;; Grep node of direx.el using incremental search like anything.el/helm.el.
;; `direx-grep:grep-item-from-root'
;; Do `direx-grep:grep-item' using the root of pointed item.
;; `direx-grep:show-all-item-at-point'
;; Show all item under pointed item.
;; `direx-grep:show-all-item'
;; Show all item in current buffer.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - direx.el ... Version 0.1alpha


;; Enjoy!!!


(require 'direx)
(require 'migemo nil t)

(defgroup direx-grep nil
  "Grep node of direx.el using incremental search like anything.el/helm.el"
  :group 'convenience
  :prefix "direx-grep:")

(defcustom direx-grep:use-migemo nil
  "Whether to use migemo.el in `direx-grep:grep-item'.

Also, you are able to toggle this value by pushing `direx-grep:toggle-use-migemo-key' in `direx-grep:grep-item'."
  :type 'boolean
  :group 'direx-grep)

(defcustom direx-grep:toggle-use-migemo-key "C-t"
  "Keystroke for `direx-grep:toggle-use-migemo' in `direx-grep:grep-item'."
  :type 'string
  :group 'direx-grep)

(defcustom direx-grep:ensure-greped-tree t
  "Whether to ensure the children of the target node before grep."
  :type 'boolean
  :group 'direx-grep)


(defvar direx-grep--timer nil)
(defvar direx-grep--last-input-value "")
(defvar direx-grep--current-use-migemo nil)
(defvar direx-grep--current-item nil)

(defun direx-grep--start-grep ()
  (setq direx-grep--last-input-value "")
  (unless direx-grep--timer
    (setq direx-grep--timer
          (run-with-idle-timer 0.5 t 'direx-grep--do-grep))))

(defun direx-grep--stop-grep ()
  (let ((timer (symbol-value 'direx-grep--timer)))
    (when timer
      (cancel-timer timer))
    (setq direx-grep--timer nil)))

(defun direx-grep--do-grep ()
  (let* ((iptvalue (with-selected-window (or (active-minibuffer-window)
                                             (minibuffer-window))
                     (minibuffer-contents)))
         (iptvalue (replace-regexp-in-string "^\\s-+" "" iptvalue))
         (iptvalue (replace-regexp-in-string "\\s-+$" "" iptvalue))
         (re-maker (or (when (and direx-grep--current-use-migemo
                                  (featurep 'migemo))
                         'migemo-search-pattern-get)
                       'regexp-quote))
         (do-update (not (string= iptvalue direx-grep--last-input-value)))
         (re-list (when (and do-update
                             (not (string= iptvalue "")))
                    (mapcar (lambda (s) (funcall re-maker s))
                            (split-string iptvalue " +"))))
         (buffer-read-only nil))
    (when (and do-update
               direx-grep--current-item)
      (setq direx-grep--last-input-value iptvalue)
      (direx-grep--turn-back-visibility direx-grep--current-item t)
      (when re-list
        (direx-grep--do-grep-recursively direx-grep--current-item re-list)))))

(defun direx-grep--do-grep-recursively (item re-list &optional grep-myself)
  (let* ((isnode (not (direx:item-leaf-p item)))
         (openednode (and isnode (direx:item-open item)))
         (leaffound (when isnode
                      (loop with found = nil
                            for child in (direx:item-children item)
                            if (direx-grep--do-grep-recursively child re-list t)
                            do (setq found t)
                            finally return found)))
         (ret (cond
               ((not grep-myself) t)
               (leaffound         (direx:item-show item)
                                  t)
               (t                 (loop with selfnm = (direx:tree-name (direx:item-tree item))
                                        for re in re-list
                                        if (not (string-match re selfnm))
                                        return (progn (direx:item-hide item)
                                                      nil)
                                        finally return (progn (direx:item-show item)
                                                              t))))))
    (when (and openednode
               (not leaffound)
               ret)
      ;; If any leaf is not match but myself is match, show all leaf.
      (direx:item-show-children item))
    ret))

(defun direx-grep--turn-back-visibility (item opened)
  (if opened
      (direx:item-show item)
    (direx:item-hide item))
  (when (not (direx:item-leaf-p item))
    (loop with opened = (and opened (direx:item-open item))
          for child in (direx:item-children item)
          do (direx-grep--turn-back-visibility child opened))))

(defun direx-grep--ensure-greped-tree (item)
  (when (not (direx:item-leaf-p item))
    (when (not (direx:item-open item))
      (direx:item-ensure-children item)
      (direx:item-hide-children item))
    (dolist (child (direx:item-children item))
      (direx-grep--ensure-greped-tree child))))

(defvar direx-grep:grep-map nil)
(defun direx-grep--make-grep-map ()
  (when (not direx-grep:grep-map)
    (setq direx-grep:grep-map
          (let ((map (copy-keymap minibuffer-local-map)))
            (define-key map (kbd "C-g") 'direx-grep:abort-grep)
            (when (stringp direx-grep:toggle-use-migemo-key)
              (define-key map
                (read-kbd-macro direx-grep:toggle-use-migemo-key)
                'direx-grep:toggle-use-migemo))
            map))))


(defun direx-grep:abort-grep ()
  "Abort `direx-grep:grep-item'."
  (interactive)
  (direx-grep--stop-grep)
  (unwind-protect
      (direx:awhen direx-grep--current-item
        (direx-grep--turn-back-visibility it t))
    (setq direx-grep--current-item nil)
    (abort-recursive-edit)))

(defun direx-grep:toggle-use-migemo ()
  "Toggle the value of `direx-grep:use-migemo'."
  (interactive)
  (setq direx-grep--last-input-value "")
  (message "direx-grep:use-migemo is %s"
           (setq direx-grep--current-use-migemo
                 (setq direx-grep:use-migemo (not direx-grep:use-migemo)))))

(defun direx-grep:grep-item (&optional item)
  "Grep node of direx.el using incremental search like anything.el/helm.el.

If `direx-grep:ensure-greped-tree' is non-nil,
do `direx:item-ensure-children' to ITEM without visibility.
You can find the hidden and not ensured yet item by this action but that may cause slowness."
  (interactive)
  (setq direx-grep--current-use-migemo direx-grep:use-migemo)
  (setq direx-grep--current-item (or item (direx:item-at-point!)))
  (when direx-grep:ensure-greped-tree
    (direx-grep--ensure-greped-tree direx-grep--current-item))
  (let* ((mgmmsg (when (and direx-grep--current-use-migemo
                            (featurep 'migemo))
                   "[MIGEMO] "))
         (prompt (concat mgmmsg "Search: ")))
    (direx-grep--make-grep-map)
    (direx-grep--start-grep)
    (unwind-protect
        (read-from-minibuffer prompt nil direx-grep:grep-map)
      (direx-grep--stop-grep)
      (setq direx-grep--current-item nil))))

(defun direx-grep:grep-item-from-root ()
  "Do `direx-grep:grep-item' using the root of pointed item."
  (interactive)
  (direx-grep:grep-item (direx:item-root (direx:item-at-point!))))

(defun direx-grep:show-all-item-at-point ()
  "Show all item under pointed item."
  (interactive)
  (direx-grep:show-all-item (direx:item-at-point!)))

(defun direx-grep:show-all-item (&optional item)
  "Show all item in current buffer."
  (interactive)
  (setq item (or item direx:root-item))
  (direx-grep--turn-back-visibility item t))


(provide 'direx-grep)
;;; direx-grep.el ends here
