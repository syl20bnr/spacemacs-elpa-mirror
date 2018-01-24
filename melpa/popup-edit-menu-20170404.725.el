;;; popup-edit-menu.el --- a popup context edit menu package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Debugfan Chin

;; Author: Debugfan Chin <debugfanchin@gmail.com>
;; Keywords: lisp, pop-up, context, edit, menu
;; Package-Version: 20170404.725
;; Package-Requires: ((emacs "24"))
;; Version: 0.0.3

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

;; This package used to popup a context edit menu in Emacs.
;; It's convenient for the users those are using mice on hand.
;; The core start codes of this package are from mouse.el,
;; which is a part of Emacs release.

;; # Installation
;;
;; Add the following codes into your init file to enable it.
;;
;;   (require 'popup-edit-menu)
;;   (global-set-key [mouse-3] (popup-edit-menu-stub))
;;
;; You can change the key binding as you want if you don't want
;; to active it by mouse right click
;;
;; # Configuration
;;
;; If you prefer to show the mode menus below, you can add
;; the following codes into your init file:
;;
;;   (setq popup-edit-menu-mode-menus-down-flag t)
;;
;; or set it in Emacs Customization Group.

;;; Code:

(require 'mouse)

;;;###autoload
(defgroup popup-edit-menu nil
    "Popup a convenient context edit menu"
    :group 'convenience
)

;;;###autoload
(defcustom popup-edit-menu-keep-header-flag nil
  "Non-nil means keep header in popup edit menu..."
  :type 'boolean
  :require 'popup-edit-menu
  :group 'popup-edit-menu)

;;;###autoload
(defcustom popup-edit-menu-mode-menus-down-flag nil
  "Non-nil means move mode menus on the bottom..."
  :type 'boolean
  :require 'popup-edit-menu
  :group 'popup-edit-menu)

;;;###autoload
(defcustom popup-edit-menu-never-menu-bar-flag nil
  "Non-nil means never pop-up menu-bar even no menu-bar-lines..."
  :type 'boolean
  :require 'popup-edit-menu
  :group 'popup-edit-menu)

(defun popup-edit-menu-map ()
  "Return a keymap associated with a enhanced context edit menu.
The menu items from global edit menu and various mode menus.
The contents are the items that would be in the menu bar whether or
not it is actually displayed."
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* ((local-menu (and (current-local-map)
			  (lookup-key (current-local-map) [menu-bar])))
	 (global-menu (lookup-key global-map [menu-bar edit]))
	 ;; If a keymap doesn't have a prompt string (a lazy
	 ;; programmer didn't bother to provide one), create it and
	 ;; insert it into the keymap; each keymap gets its own
	 ;; prompt.  This is required for non-toolkit versions to
	 ;; display non-empty menu pane names.
	 (minor-mode-menus
	  (mapcar
           (lambda (menu)
             (let* ((minor-mode (car menu))
                    (menu (cdr menu))
                    (title-or-map (cadr menu)))
               (or (stringp title-or-map)
                   (if popup-edit-menu-keep-header-flag
                       (setq menu
                             (cons 'keymap
                                   (cons (concat
                                          (capitalize (subst-char-in-string
                                                       ?- ?\s (symbol-name
                                                               minor-mode)))
                                          " Menu")
                                         (cdr menu))))))
               menu))
	   (minor-mode-key-binding [menu-bar])))
	 (local-title-or-map (and local-menu (cadr local-menu)))
	 (global-title-or-map (cadr global-menu)))
    (or (null local-menu)
	(stringp local-title-or-map)
	(if popup-edit-menu-keep-header-flag
    (setq local-menu (cons 'keymap
			       (cons (concat (format-mode-line mode-name)
                                             " Mode Menu")
				     (cdr local-menu))))))
    (or (stringp global-title-or-map)
	(setq global-menu (if popup-edit-menu-keep-header-flag
                (cons 'keymap (cons "Edit Menu" (cdr global-menu)))
                (delete "Edit" global-menu))))
    ;; Supplying the list is faster than making a new map.
    ;; FIXME: We have a problem here: we have to use the global/local/minor
    ;; so they're displayed in the expected order, but later on in the command
    ;; loop, they're actually looked up in the opposite order.
    (if popup-edit-menu-mode-menus-down-flag
        (apply 'append
               global-menu
               (and global-menu (or local-menu minor-mode-menus)
                    (list 'keymap (list 'popup-edit-menu-mode-separator "--")))
               local-menu
               minor-mode-menus
               nil)
        (apply 'append
               local-menu
               minor-mode-menus
               (and (or local-menu minor-mode-menus) global-menu
                    (list 'keymap (list 'popup-edit-menu-mode-separator "--")))
               global-menu
               nil))))

(defun popup-edit-menu-make-keymap (&optional _ignore)
  "A binding function for popup edit menu item.
Use the `popup-edit-menu-map' if the menu bar is showing,
otherwise the `mouse-popup-menubar'"
   (if (and (not popup-edit-menu-never-menu-bar-flag)
            (zerop (or (frame-parameter nil 'menu-bar-lines) 0)))
        (mouse-menu-bar-map)
     (popup-edit-menu-map)))

(defun popup-edit-menu-stub ()
  "A entry to define popup edit menu."
   '(menu-item "Popup Edit Menu" ignore
                :filter popup-edit-menu-make-keymap))

(provide 'popup-edit-menu)
;;; popup-edit-menu.el ends here
