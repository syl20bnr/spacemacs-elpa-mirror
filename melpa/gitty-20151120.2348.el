;;; gitty.el --- vc-mode extension for fast git interaction

;; Copyright (C) 2012  Jorgen Schaefer <forcer@forcix.cx>

;; Version: 1.0
;; Package-Version: 20151120.2348
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/gitty
;; Keywords: vc

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; gitty is a very small extension to `vc-mode' to make common git
;; commands easier to access than in the normal environment. The idea
;; is that you do most interaction with git using the command line or
;; magit, but some often-used commands are accessible easily.

;; C-x v b is the main new command. It prompts for an existing branch
;; with tab completion enabled and checks that out. If the entered
;; branch does not exist, it prompts if it should be created. This
;; makes switching between branches for quick feature branches much
;; easier than with `vc-mode'.

;; gitty also provides quick keys to stash save and stash pop in
;; the current repository, and to see the current `status'.

;; For more information, see the docstring for the command `gitty-mode`.

;; Note, though, that gitty does override some `vc-mode' keybindings
;; that are less useful for git.

;;; Code:

(require 'vc-git)

(defvar gitty-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x v b") 'gitty-checkout)
    (define-key map (kbd "C-x v s") 'gitty-status)
    (define-key map (kbd "C-x v L") 'gitty-update-modelines)
    (define-key map (kbd "C-x v S") 'gitty-stash-save)
    (define-key map (kbd "C-x v P") 'gitty-stash-pop)
    map)
  "Key map for the command `gitty-mode'.")

(define-minor-mode gitty-mode
  "Gitty mode is a small mode for trivial Git interaction.

\\{gitty-mode-map}"
  :global t)

(defun gitty-checkout ()
  "Check out and optionally create a branch named BRANCH.

This always interacts with the user."
  (interactive)
  (gitty-ensure-repository)
  (let* ((branches (gitty-branch-list))
         (branch (completing-read "Branch: " branches)))
    (if (member branch branches)
        (gitty-command "checkout" branch)
      (when (and (not (equal branch ""))
                 (y-or-n-p (format "Branch '%s' does not exist, create? "
                                   branch)))
        (gitty-command "checkout" "-b" branch)))))

(defun gitty-status ()
  "Call git status."
  (interactive)
  (gitty-ensure-repository)
  (gitty-command "status"))

(defun gitty-stash-save ()
  "Call git stash save."
  (interactive)
  (gitty-ensure-repository)
  (gitty-command "stash" "save"))

(defun gitty-stash-pop ()
  "Call git stash pop."
  (interactive)
  (gitty-ensure-repository)
  (gitty-command "stash" "pop"))

(defun gitty-branch-list ()
  "Return a list of branches except for the current one."
  (let ((branches nil)
        (directory (vc-git-root buffer-file-name)))
    (with-temp-buffer
      (let ((default-directory directory))
        (call-process "git" nil t nil "branch"))
      (goto-char (point-min))
      (while (re-search-forward "^  \\(.*\\)$" nil t)
        (setq branches (cons (match-string 1)
                             branches))))
    (reverse branches)))

(defun gitty-update-modelines ()
  "Update VC mode lines with the correct branch in every buffer."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (vc-backend buffer-file-name)
        (vc-file-setprop buffer-file-name 'vc-working-revision nil)
        (vc-mode-line buffer-file-name)))))

(defun gitty-ensure-repository ()
  "Return only if the current file is in a git repository.

Otherwise, throw an error."
  (when (or (not buffer-file-name)
            (not (vc-git-root buffer-file-name)))
    (error "Not a git repository")))

(defun gitty-command (&rest args)
  "Call a git command with ARGS as arguments.

If an error occurred, show the output in a buffer."
  (let ((buffer (get-buffer-create "*Git*"))
        (directory (vc-git-root buffer-file-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((default-directory directory)
               (exitcode (apply 'call-process "git" nil t nil args))
               (lines (count-lines (point-min) (point-max))))
          (help-mode)
          (cond
           ((= lines 1)
            (goto-char (point-min))
            (message "%s" (buffer-substring (point-at-bol)
                                            (point-at-eol))))
           ((> lines 1)
            (display-buffer buffer))))))
    (gitty-update-modelines)))

(provide 'gitty)
;;; gitty.el ends here
