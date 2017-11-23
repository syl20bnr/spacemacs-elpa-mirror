;;; evil-expat.el --- Evil ex commands -*- coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-expat
;; Package-Version: 20171123.606
;; Package-Requires: ((emacs "24.3") (evil "1.0.0"))
;; Version: 0.0.1
;; Keywords: emulations, evil, vim

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; evil-expat extends evil-mode by providing additional ex
;; commands, which a vim user is likely to be familiar with.
;;
;; The provided ex commands typically have the same name as
;; ex commands available in core vim or vim plugins.
;;
;; Usage:
;;
;; (require 'evil-expat)
;;
;; Then the following ex commands will be available:
;;
;; :remove       remove current file and its buffer; like vim-eunuch's :Remove
;; :rename       rename or move current file and its buffer; lime vim-eunuch's :Rename
;; :reverse      reverse visually selected lines
;; :colorscheme  change emacs color theme; like vim's ex command of the same name
;; :diff-orig    get a diff of unsaved changes; like vim's common `:DiffOrig` from the official example vimrc
;; :gblame       git-blame current file, requires `magit`; like vim-fugitive's :Gblame
;; :gremove      git remove current file, requires `magit`; like vim-fugitive's :Gremove
;; :tyank        copy range into tmux paste buffer, requires running under `tmux`; like vim-tbone's :Tyank
;; :tput         paste from tmux paste buffer, requires running under `tmux`; like vim-tbone's :Tput
;;
;;; Code:

(require 'evil)

(evil-define-command evil-expat-reverse (beg end)
  "Reverse the lines between BEG and END."
  :type line
  :repeat nil
  (interactive "<r>")
  (when (eq (line-number-at-pos beg) (line-number-at-pos (1- end)))
    (user-error "More than one lines must be selected"))
  (reverse-region beg end))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "rev[erse]" 'evil-expat-reverse)
     (autoload 'evil-expat-reverse "evil-expat" nil t)))

;; :remove to delete file and buffer
(defun evil-expat-remove ()
  "Remove the current file and its buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (delete-file filename)
    (kill-buffer)
    (message "Removed %s and its buffer" filename)))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "remove" 'evil-expat-remove)
     (autoload 'evil-expat-remove "evil-expat" nil t)))

(evil-define-command evil-expat-rename (bang new-name)
  "Rename the current file and its buffer to NEW-NAME.

If NEW-NAME is a directory, the file is moved there."
  ;; TODO create any missing directory structure
  (interactive "<!><f>")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (new-name (if (file-directory-p new-name)
                       (concat (file-name-as-directory new-name) (file-name-nondirectory filename))
                     new-name)))
    (unless filename
      (user-error "Buffer %s is not visiting a file" name))
    (when (string-equal (expand-file-name filename) (expand-file-name new-name))
      (user-error "%s and %s are the same file" buffer-file-name new-name))
    (when (and (file-exists-p new-name) (not bang))
      (user-error "File %s exists, use :rename! to overwrite it" new-name))

    (condition-case err
        (rename-file filename new-name bang)
      (error
       (if (and (string-match-p "File already exists" (error-message-string err)) (not bang))
           (user-error "File %s exists, use :rename! to overwrite it" new-name)
         (user-error (error-message-string err)))))
    (set-visited-file-name new-name t)
    (set-buffer-modified-p nil)))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "rename" 'evil-expat-rename)
     (autoload 'evil-expat-rename "evil-expat" nil t)))

(declare-function magit-file-tracked-p "ext:magit")
(declare-function magit-file-delete "ext:magit")

(defun evil-expat-gblame ()
  "The ex :gblame command."
  (interactive)
  (unless (require 'magit nil 'noerror)
    (user-error "Package magit isn't installed"))
  (call-interactively 'magit-blame))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "gblame" 'evil-expat-gblame)
     (autoload 'evil-expat-gblame "evil-expat" nil t)))

(evil-define-command evil-expat-gremove (bang)
  "Remove current file and its buffer.

BANG forces removal of files with modifications"
  (interactive "<!>")
  (unless (require 'magit nil 'noerror)
    (user-error "Package magit isn't installed"))
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (unless (magit-file-tracked-p filename)
      (user-error "File %s is not tracked by git" filename))

    ;; call magit to remove the file
    (let ((magit-process-raise-error t))
      (condition-case err
          (magit-call-git "rm" (when bang "--force") "--" filename)
        (magit-git-error
         (if (string-match-p "the following file has local modifications" (error-message-string err))
             (user-error "File %s has modifications, use :gremove! to force" (buffer-name))
           (user-error (error-message-string err))))))
    (when (not (file-exists-p filename))
      (kill-buffer))))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "gremove" 'evil-expat-gremove)
     (autoload 'evil-expat-gremove "evil-expat" nil t)))

;; define :tyank and :tput only when running under tmux
(when (and (getenv "TMUX") (executable-find "tmux"))
  (evil-define-command evil-expat-tyank (begin end _type)
    "Save range in tmux paste buffer"
    (interactive "<R>")
    (shell-command (concat "tmux set-buffer " (shell-quote-argument (buffer-substring begin end)))))

  (defun evil-expat-tput ()
    "Paster from tmux paste buffer."
    (interactive)
    (save-excursion
      (end-of-line)
      (newline)
      (insert (shell-command-to-string "tmux show-buffer")))))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "tyank" 'evil-expat-tyank)
     (autoload 'evil-expat-tyank "evil-expat" nil t)))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "tput" 'evil-expat-tput)
     (autoload 'evil-expat-tput "evil-expat" nil t)))

(defun evil-expat-diff-orig ()
  "Call function `diff-buffer-with-file' non-interactively."
  (interactive)
  (diff-buffer-with-file))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "diff-orig" 'evil-expat-diff-orig)
     (autoload 'evil-expat-diff-orig "evil-expat" nil t)))

(evil-define-interactive-code "<expat-theme>"
  "A color theme ex argument."
  :ex-arg expat-theme
  (list (when (and (evil-ex-p) evil-ex-argument)
          (intern evil-ex-argument))))

(evil-ex-define-argument-type expat-theme
  "Defines an argument type which can take a color theme name."
  :collection
  (lambda (arg predicate flag)
    (let ((completions
           (append '("default") ;; append "default" theme
                   (mapcar 'symbol-name (custom-available-themes)))))
      (when arg
        (cond
         ((eq flag nil)
          (try-completion arg completions predicate))
         ((eq flag t)
          (all-completions arg completions predicate))
         ((eq flag 'lambda)
          (test-completion arg completions predicate))
         ((eq (car-safe flag) 'boundaries)
          (cons 'boundaries
                (completion-boundaries arg
                                       completions
                                       predicate
                                       (cdr flag)))))))))

(evil-define-command evil-expat-colorscheme (theme)
  "The ex :colorscheme command"
  (interactive "<expat-theme>")
  (mapc #'disable-theme custom-enabled-themes)
  (unless (string-equal "default" theme)
    (load-theme theme t)))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (evil-ex-define-cmd "colo[rscheme]" 'evil-expat-colorscheme)
     (autoload 'evil-expat-colorscheme "evil-expat" nil t)))

(provide 'evil-expat)

;;; evil-expat.el ends here
