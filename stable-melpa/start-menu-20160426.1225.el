;;; start-menu.el --- start-menu for executing external program like in windows -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-01-03
;; Version: 0.1
;; Package-Version: 20160426.1225
;; Keywords: convenience, menu
;; Package-Requires: ((cl-lib "0.5") (config-parser "0.1"))
;; URL: https://github.com/lujun9972/el-start-menu

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; start-menu's code can be found here:
;;   http://github.com/lujun9972/el-start-menu

;;; Commentary:

;; start-menu is a little tool that will add a "Start" menu in the menu bar 
;; You can use the Start menu to executing external program just like the start menu in windows.

;; Quick start:

;; If you are using debian linux or ubuntu linux just M-x `start-menu-enable'
;; Otherwise, you should config `start-menu-menu-conf' first then M-x `start-menu-enable'
;; For Windows user: Sine the path to program often contains space, so may be you use "" to quote the path
;; If you don't need Start menu any more,just M-x start-menu-disable
;; Since many user hidden the menu bar, so there is also a command `start-menu-popup' which will pop up the start menu. 

;;; Code:
(require 'cl-lib)
(require 'easymenu)
(require 'config-parser)


;;; BASIC API

;; (setq item-firefox (start-menu-make-menu-item "C:/Program Files/Mozilla Firefox/firefox.exe"))
;; => ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]
(defun start-menu-make-menu-item (command &optional name hints)
  "return a new menu-item. If NAME is nil the name of new menu-item will be the basename of PROGRAM"
  (let ((name (or name (file-name-base command))))
    (vector name command hints)))

;; (setq menu-start (start-menu-make-menu "Start" (start-menu-make-menu-item "/bin/emacs") (start-menu-make-menu-item "/bin/gvim")))
;; => ("Start" ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu-make-menu (menu-name &rest menu-items)
  "return a new menu which name is MENU-NAME and content is MENU-ITEMS which is a list of menu or menu-item"
  (cons menu-name menu-items))

;; (start-menu-menu-name menu-start)
;; => "Start"
(defun start-menu-menu-name (menu)
  "return name of MENU "
  (car menu))

;; (start-menu-menu-content menu-start)
;; =>(["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu-menu-content (menu)
  "return content of MENU which is list of menu or menu-item"
  (cdr menu))

;; (start-menu-insert-into-menu-first menu-start (start-menu-make-menu "Internet"))
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
;; menu-start
;; => ("Start" ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu-insert-into-menu-first (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the beginning of MENU, return the new menu"
  (let ((menu-name (start-menu-menu-name menu))
        (menu-new-items (append submenu-or-items (start-menu-menu-content menu))))
    (apply #'start-menu-make-menu menu-name menu-new-items)))

;; (start-menu-insert-into-menu-first! menu-start (start-menu-make-menu "Internet"))
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu-insert-into-menu-first! (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the beginning of MENU, return the new menu. MENU will be alerted"
  (let ((menu-name (start-menu-menu-name menu))
        (menu-new-items (append submenu-or-items (start-menu-menu-content menu))))
    (setf (cdr menu) menu-new-items)
    menu))

;; (setq item-chrome (start-menu-make-menu-item "chrome"))
;; (start-menu-insert-into-menu-last menu-start item-chrome)
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu-insert-into-menu-last (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the ending of MENU, return the new menu"
  (append menu submenu-or-items))

;; (start-menu-insert-into-menu-last! menu-start item-chrome)
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
(defun start-menu-insert-into-menu-last! (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the ending of MENU, return the new menu. MENU will be alerted"
  (nconc menu submenu-or-items))

;; (start-menu-menu-p menu-start)
;; => t
;; (start-menu-menu-p item-firefox)
;; => nil
(defun start-menu-menu-p (object)
  (listp object))

;; (start-menu-menu-item-p menu-start)
;; => nil
;; (start-menu-menu-item-p item-firefox)`
;; => t
(defun start-menu-menu-item-p (object)
  (vectorp object))

;; (setq internet-menu (start-menu-find-submenu menu-start '("Internet")))
;; => ("Internet")
;; (start-menu-find-submenu menu-start '("Internet" "Browser"))
;; => nil
;; (setq Browser-menu (start-menu-find-submenu menu-start '("internet" "Browser") t))
;; => ("Browser")
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu-find-submenu (menu submenu-path-list &optional create-p)
  "find submenu which located by SUBMENU-PATH-LIST in MENU.

If CREATE is non-nil, it will create submenu by SUBMENU-PATH-LIST"
  (if (or (null menu)
          (null submenu-path-list))
      menu
    (let* ((submenu-name (car submenu-path-list))
           (submenus (cl-remove-if-not #'start-menu-menu-p (start-menu-menu-content menu)))
           (submenu (cl-find-if (lambda (menu)
                                  (string-equal submenu-name (start-menu-menu-name menu)))
                                submenus)))
      (when (and create-p
                 (null submenu))
        (setq submenu (start-menu-make-menu submenu-name))
        (start-menu-insert-into-menu-last! menu submenu))
      (start-menu-find-submenu submenu (cdr submenu-path-list) create-p))))

;; (start-menu-exist-in-p internet-menu menu-start)
;; => ("Internet")
;; (start-menu-exist-in-p item-firefox menu-start)
;; => nil'
;; (start-menu-exist-in-p item-chrome menu-start)
;; => (["chrome" "chrome" nil])
(defun start-menu-exist-in-p (submenu-or-items menu)
  "Is SUBMENU-OR-ITEMS exist in MENU"
  (cond ((start-menu-menu-p submenu-or-items)
         (let ((submenu-name (start-menu-menu-name submenu-or-items)))
           (start-menu-find-submenu menu (list submenu-name))))
        ((start-menu-menu-item-p submenu-or-items)
         (let* ((menu-items-already-exist (cl-remove-if-not #'start-menu-menu-item-p (start-menu-menu-content menu))))
           (member submenu-or-items menu-items-already-exist)))
        (t (error "Invalid Data Type: %s" submenu-or-items))))

;; (start-menu-add-to-menu-content-first internet-menu item-firefox)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; internet-menu
;; => ("Internet")
(defun start-menu-add-to-menu-content-first (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu-exist-in-p submenu-or-item menu)
    (start-menu-insert-into-menu-first menu submenu-or-item)))

;; (start-menu-add-to-menu-content-first! internet-menu item-firefox)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; (start-menu-add-to-menu-content-first! internet-menu item-firefox)
;; => nil
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu-add-to-menu-content-first! (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU(MENU will be alerted). otherwise do nothing"
  (unless (start-menu-exist-in-p submenu-or-item menu)
    (start-menu-insert-into-menu-first! menu submenu-or-item)))

;; (start-menu-add-to-menu-content-last internet-menu item-chrome)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu-add-to-menu-content-last (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu-exist-in-p submenu-or-item menu)
    (start-menu-insert-into-menu-last menu submenu-or-item)))

;; (start-menu-add-to-menu-content-last! internet-menu item-chrome)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; (start-menu-add-to-menu-content-last! internet-menu item-chrome)
;; => nil
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu-add-to-menu-content-last! (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU(MENU will be alerted). otherwise do nothing. "
  (unless (start-menu-exist-in-p submenu-or-item menu)
    (start-menu-insert-into-menu-last! menu submenu-or-item)))


;;; HANDLE DEBIAN MENU

;; (start-menu-add-menu-item-by-debian-menu-file menu-start "/usr/share/menu/nethack-x11")
;; => ("Adventure" ["X NetHack" "/usr/games/xnethack" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")) ("Games" ("Adventure" ["X NetHack" "/usr/games/xnethack" nil])))
(defun start-menu-add-menu-item-by-debian-menu-file (menu debian-menu-file)
  "add new menu-item according to DEBIAN-MENU-FILE"
  (when (file-exists-p debian-menu-file)
    (let (file-content command section title hints icon)
      (with-temp-buffer
        (insert-file-contents debian-menu-file)
        (setq file-content (buffer-string)))
      (when (string-match "command=\"\\([^\"]+\\)\"" file-content)
        (setq command (match-string 1 file-content)))
      (when (string-match "section=\"\\([^\"]+\\)\"" file-content)
        (setq section (match-string 1 file-content)))
      (when (string-match "title=\"\\([^\"]+\\)\"" file-content)
        (setq title (match-string 1 file-content)))
      (when (string-match "hints=\"\\([^\"]+\\)\"" file-content)
        (setq hints (match-string 1 file-content)))
      (when (string-match "icon=\"\\([^\"]+\\)\"" file-content)
        (setq icon (match-string 1 file-content)))
      (when command 
        (let* ((menu-item (start-menu-make-menu-item command title hints))
               (submenu-path (split-string section "/"))
               (submenu (start-menu-find-submenu menu submenu-path t)))
          (start-menu-add-to-menu-content-last! submenu menu-item)
          menu)))))

(defun start-menu-add-menu-item-by-debian-menu-dir (menu debian-menu-dir)
  "add menu-item to MENU according all debain-menu-file in DEBIAN-MENU-DIR"
  (when (file-exists-p debian-menu-dir)
    (dolist (debian-menu-file (directory-files debian-menu-dir t "[^.].*"))
      (start-menu-add-menu-item-by-debian-menu-file menu debian-menu-file))
    menu))

(defun start-menu-init-debian-menu-conf ()
  (let ((start-menu (start-menu-make-menu "Start")))
    (dolist (debian-menu-dir '("/usr/share/menu/" "/usr/lib/menu/" "/etc/menu/" "~/.menu/"))
      (start-menu-add-menu-item-by-debian-menu-dir start-menu debian-menu-dir))
    start-menu))


;;; HANDLE XDG MENU 

;; (start-menu-add-menu-item-by-xdg-menu-file menu-start "/usr/share/applications/firefox.desktop")
;; => ("Adventure" ["X NetHack" "/usr/games/xnethack" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")) ("Games" ("Adventure" ["X NetHack" "/usr/games/xnethack" nil])))
(defun start-menu-add-menu-item-by-xdg-menu-file (menu xdg-menu-file)
  "add new menu-item according to XDG-MENU-FILE"
  (when (file-exists-p xdg-menu-file)
    (let* ((file-config-data (config-parser-read xdg-menu-file "=")) 
           (type (config-parser-get file-config-data "Desktop Entry" "Type"))
           (exec (string-trim (replace-regexp-in-string "%[a-zA-Z]" "" (config-parser-get file-config-data "Desktop Entry" "Exec") t)))
           (categories (or (config-parser-get file-config-data "Desktop Entry" "Categories")
                           ""))
           (name (config-parser-get file-config-data "Desktop Entry" "Name"))
           (comment (config-parser-get file-config-data "Desktop Entry" "Comment"))
           (icon (config-parser-get file-config-data "Desktop Entry" "Icon")))
      (when (and exec
                 (string-equal "Application" type))
        (let* ((menu-item (start-menu-make-menu-item exec name comment))
               (submenu-path (last (cl-remove "" (split-string categories ";") :test #'string-equal)))
               (submenu (start-menu-find-submenu menu submenu-path t)))
          (start-menu-add-to-menu-content-last! submenu menu-item)
          menu)))))

(defun start-menu-add-menu-item-by-xdg-menu-dir (menu xdg-menu-dir)
  "add menu-item to MENU according all xdg-menu-file in XDG-MENU-DIR"
  (when (file-exists-p xdg-menu-dir)
    (dolist (xdg-menu-file (directory-files-recursively xdg-menu-dir "\\.desktop$"))
      (start-menu-add-menu-item-by-xdg-menu-file menu xdg-menu-file))
    menu))

(defun start-menu-init-xdg-menu-conf ()
  (let ((start-menu (start-menu-make-menu "Start")))
    (dolist (xdg-menu-dir '("/usr/share/applications/"))
      (start-menu-add-menu-item-by-xdg-menu-dir start-menu xdg-menu-dir))
    start-menu))
;; (start-menu-init-menu-conf)

;;; GENERATE THE START MENU
(defgroup start-menu nil
  "Start Menu"
  :group 'extensions
  :group 'convenience
  :prefix "start-menu-")

(defcustom start-menu-init-menu-conf-type 'debian
  "When package loaded, It will automatically generate the init start-menu according to debian-menu or xdg-menu"
  :type '(choice (const debian)
                 (const xdg))
  :group 'start-menu)

(defun start-menu-init-menu-conf ()
  (let ((init-menu-conf-fn (intern (format "start-menu-init-%s-menu-conf" start-menu-init-menu-conf-type))))
    (funcall init-menu-conf-fn)))

(defcustom start-menu-menu-conf (start-menu-init-menu-conf)
  "the format of start-menu-menu-conf is a menu which is actually a list looks like (MENU-NAME MENU... MENU-ITEM...)

MENU-NAME is a string as name of menu.
MENU is another sub-menu
MENU-ITEM is a vector which first element is the name of menu-item and second element is the program to be executed

Here is an example:
(\"Start\"
 [\"Gvim\" \"gvim\"]
 (\"Browser\"
  [\"Firefox\" \"firefox\"]
  [\"Chrome\" \"chromium-browser\"]))"
  :group 'start-menu)

(defun start-menu-start-process (name command)
  "start a program, the buffer will be killed after program exit"
  (let ((program (car (split-string-and-unquote command)))
        (args (cdr (split-string-and-unquote command))))
    (switch-to-buffer (apply #'make-comint name program nil args))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process event)
                            (when (eq 'exit (process-status process))
                              (kill-buffer (process-buffer process)))))))

(defun start-menu-translate-conf-to-menu (menu)
  (let ((menu-name (car menu))
        (items (cdr menu)))
    (cons menu-name
          (mapcar (lambda (item)
                    (cond ((vectorp item)
                           (let ((title (aref item 0))
                                 (command (aref item 1))
                                 (hints (or (ignore-errors (aref item 2))
                                            "")))
                             (vector title
                                     (lambda ()
                                       (interactive)
                                       (funcall 'start-menu-start-process title command ))
                                     :help hints)))
                          ((listp item)
                           (start-menu-translate-conf-to-menu item)))) items))))

;;;###autoload 
(defun start-menu-enable ()
  "enable start menu"
  (interactive)
  (when (start-menu-menu-content start-menu-menu-conf)
    (easy-menu-define start-menu global-map
      "menu for start"
      (start-menu-translate-conf-to-menu start-menu-menu-conf))))

;;;###autoload 
(defun start-menu-disable ()
  "disable start menu"
  (interactive)
  (define-key global-map [menu-bar Start] 'undefined))

;;;###autoload 
(defun start-menu-popup ()
  "pop up the start menu"
  (interactive)
  (popup-menu start-menu))

(provide 'start-menu)

;;; start-menu.el ends here
