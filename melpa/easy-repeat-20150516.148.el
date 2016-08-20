;;; easy-repeat.el --- Repeat easily                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Version: 0.2
;; Package-Version: 20150516.148
;; Package-Requires: ((emacs "24.4"))
;; Keywords: repeat, convenience
;; Created: 2015-05-02
;; URL: https://github.com/xuchunyang/easy-repeat.el

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

;; `easy-repeat' enables you to easily repeat the previous command by using the
;; last short key, for example, 'C-x o' 'o' 'o' 'o'...  will switch windows
;; and 'M-x next-buffer RET' 'RET' 'RET' 'RET'... will switch buffers.

;; ## Setup

;;     (add-to-list 'load-path "/path/to/easy-repeat.el")
;;     (require 'easy-repeat)

;; ## Usage
;; Modify `easy-repeat-command-list' to choose which commands you want to repeat
;; easily.

;; To use: M-x easy-repeat-mode RET

;; ## TODO
;; - [ ] Set up a timer to free repeat key
;; - [x] Allow shorter key, e.g., use single 'a' to repeat 'C-M-a'

;;; Code:

(defcustom easy-repeat-command-list
  '(
    ;; Emacs
    other-window
    next-buffer
    scroll-other-window
    recenter-top-bottom
    kill-buffer
    backward-page
    forward-page
    previous-error
    next-error
    scroll-up-command
    scroll-down-command
    beginning-of-defun
    end-of-defun
    ;; org-mode
    org-previous-visible-heading
    org-next-visible-heading
    org-forward-heading-same-level
    org-backward-heading-same-level
    ;; outline
    outline-up-heading
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-forward-same-level
    outline-backward-same-level
    ;; git-gutter
    git-gutter:previous-hunk
    git-gutter:next-hunk
    ;; Paredit
    paredit-forward
    paredit-backward
    paredit-backward-up)
  "List of commands for easy-repeat.
The term \"command\" here, refers to an interactively callable function."
  :type '(repeat (choice function))
  :group 'convenience)

(defun easy-repeat--repeat (orig-fun &rest args)
  (apply orig-fun args)
  (when (called-interactively-p 'interactive)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector last-command-event) #'repeat)
       (define-key map (vector (event-basic-type last-command-event)) #'repeat)
       map))))

(defun easy-repeat--add ()
  (dolist (command easy-repeat-command-list)
    (advice-add command :around #'easy-repeat--repeat)))

(defun easy-repeat--remove ()
  (dolist (command easy-repeat-command-list)
    (advice-remove command #'easy-repeat--repeat)))

;;;###autoload
(defun easy-repeat-add-last-command ()
  "Add the last command to `easy-repeat-command-list'."
  (interactive)
  (when (yes-or-no-p
         (format "Add '%s' to `easy-repeat-command-list'? " last-command))
    (add-to-list 'easy-repeat-command-list last-command)
    (easy-repeat-mode +1)))

;;;###autoload
(defun easy-repeat-add-key (key)
  "Add the binding of KEY in current keymaps to `easy-repeat-command-list'."
  (interactive "KAdd key: ")
  (let ((binding (key-binding key))
        (desp    (format-kbd-macro key)))
    (cond ((null binding) (error "%s is undefined" desp))
          ((eq binding #'keyboard-quit) (error "Unable to add `keyboard-quit'"))
          (t (when (yes-or-no-p
                    (format
                     "Add '%s' ('%s') to `easy-repeat-command-list'? "
                     desp binding))
               (add-to-list 'easy-repeat-command-list binding)
               (easy-repeat-mode +1))))))

;;;###autoload
(define-minor-mode easy-repeat-mode
  "Repeat easily.
Repeat by last short key, e.g., use 'o' to repeat 'C-x o'."
  :global t :group 'convenience
  (if easy-repeat-mode
      (easy-repeat--add)
    (easy-repeat--remove)))

(provide 'easy-repeat)

;;; easy-repeat.el ends here
