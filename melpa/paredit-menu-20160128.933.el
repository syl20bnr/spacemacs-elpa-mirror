;;; paredit-menu.el --- Adds a menu to paredit.el as memory aid

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Keywords: paredit
;; Package-Version: 20160128.933
;; Version: 1.0
;; Package-Requires: ((paredit "25"))

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Paredit mode provides structured editing for Lisp. It achieves this by
;; ensuring that code is always well-formed while editing. While it is very
;; helpful, sometimes it leaves the less experienced user (such as the myself)
;; scratching their head over how to achieve a simple editing task.

;; One solution is to use the cheatsheet
;; (http://emacswiki.org/emacs/PareditCheatsheet). However, this is outside
;; Emacs and does not scale well. This file provides a second solution, which
;; is a menu. While slower than using the equivalent key-presses, it provides
;; an easy mechanism to look up the relevant commands. Tooltips are also
;; provided showing the examples of use.

;; Documentation and examples come directly from paredit, so the menu should
;; automatically stay in sync, regardless of changes to paredit.

;;; Installation:
;;
;; Add (require 'paredit-menu) to your .emacs. This will also force loading of
;; paredit. If you autoload paredit, then
;;
;; (eval-after-load "paredit.el"
;;    '(require 'paredit-menu))
;; 
;; will achieve the same effect.


(require 'paredit)

;;; Code:

(defun paredit-menu-build-menu ()
  "Builds the menu from `paredit-commands'."
  (cons "Paredit"
        (paredit-menu-build-menu-1 paredit-commands nil nil)))

(defun paredit-menu-build-menu-1 (commands menu submenu)
  "Really builds the menu.

COMMANDS is the list of commands remaining to add
MENU is the current menu
SUBMENU is the current submenu"
  (let ((first (car commands))
        (rest (cdr commands)))
    ;; drop last submenu in place and complete
    (if (not first)
        (append menu (list submenu))
      ;; is a submenu title
      (if (stringp first)
          ;; start a new submenu
          (paredit-menu-build-menu-1
           rest
           ;; appending the last submenu if it exists
           (if submenu
               (append menu (list submenu))
             menu)
           (list first))
        ;; we have a command
        (paredit-menu-build-menu-1
         rest menu
         (append submenu
                 (list (vector (paredit-menu-symbol-name
                                (symbol-name (nth 1 first)))
                               (nth 1 first)
                               :help (paredit-menu-help-string first)))))))))


(defun paredit-menu-symbol-name (name)
  "Generate display name from symbol name.

No point putting \"paredit\" on the front of everything, so chop
this off.

NAME is the symbol name."
  (substring name 8))

(defun paredit-menu-help-string (command)
  "Generate help string for command.

COMMAND is the command"
  (let ((string
         (mapconcat
          (lambda (x)
            (format "%s -> \n\t%s" (nth 0 x) (nth 1 x))
            )
          (cddr command) "\n\n")))
    (if (eq "" string)
        "No Example"
      string)))


(easy-menu-define menubar-paredit paredit-mode-map "paredit"
  (paredit-menu-build-menu))

(provide 'paredit-menu)

;;; paredit-menu.el ends here
