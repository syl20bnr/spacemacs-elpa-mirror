;;; hacker-typer.el --- Pretend to write code like a pro

;; Copyright (C) 2017 by Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/therockmandolinist/emacs-hacker-typer
;; Package-Version: 0.1.5
;; Git-Repository: git://github.com/therockmandolinist/emacs-hacker-typer.git
;; Created: 2016-01-20
;; Version: 0.1.5
;; Keywords: hacker typer multimedia games
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)

(defcustom hacker-typer-show-hackerman nil
  "If t, show hackerman on calling `hacker-typer'."
  :group 'hacker-typer
  :type 'boolean)

(defcustom hacker-typer-data-dir
  (cl-flet ((var (file dir) (expand-file-name (convert-standard-filename file)
                                              dir)))
    (if (require 'no-littering nil t)
        (progn
          (make-directory (var "hacker-typer/" no-littering-var-directory) t)
          (var "hacker-typer/" no-littering-var-directory))
      (var "hacker-typer/" user-emacs-directory)))
  "Directory in which to store ‘hacker-typer’ files.

If no-littering is installed, defaults to
no-littering-var-directory/hacker-typer. Otherwise, defaults to
~/.emacs.d/hacker-typer."
  :group 'hacker-typer
  :type 'string)

(defcustom hacker-typer-type-rate 'random
  "The number of characters to type per keystroke.

If set to an integer, types that many characters per keystroke.

If set to 'random (default), types between 3-6 characters, with
the exact number chosen at random. See
`hacker-typer-random-range' to customize this range."

  :group 'hacker-typer
  :type '(choice
          (const :tag "Random" random)
          (integer :tag "Fixed" 5)))

(defcustom hacker-typer-random-range '(3 6)
  "Range for `hacker-typer-typer-rate' when set to 'random.

The range from which to select how many characters will be typed
at each keystroke (with the first element being the min value,
the second being the max)."
  :group 'hacker-typer
  :type '(list integer integer))

(defcustom hacker-typer-files
  '("https://raw.githubusercontent.com/emacs-mirror/emacs/master/src/emacs.c"
    "https://raw.githubusercontent.com/emacs-mirror/emacs/master/src/xselect.c"
    "https://raw.githubusercontent.com/emacs-mirror/emacs/master/src/w32font.c"
    "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lib-src/ebrowse.c"
    "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lwlib/xlwmenu.c")
  "A list of files for ‘hacker-typer’ to randomly choose from.

Can be web urls that point directly to files, or local files of
the form: file:///absolute/path/to/file"
  :group 'hacker-typer
  :type '(repeat string))

(defcustom hacker-typer-remove-comments nil
  "If set to t, remove comments from files. This is done using `comment-kill'."
  :group 'hacker-typer
  :type 'boolean)

(defvar hacker-typer-map (make-sparse-keymap)
  "Hacker-typer key map.")

(defvar hacker-typer--start 0)

(defvar hacker-typer--end 0)

(defvar hacker-typer--previous-buffer nil)

;;;###autoload
(defun hacker-typer (arg)
  "Start hacker-typer.

This function randomly selects a file from `hacker-typer-files',
downloading it if necessary, and creates an empty buffer in which
every keystroke types out some characters according to
`hacker-typer-type-rate', a la `http://hackertyper.com'. The
buffer is named after the file, with characters randomly
prepended.

If `hacker-typer-show-hackerman' is set to t, also show an
amusing picture of Rami Malek as \"hackerman\".

With prefix argument ARG, prompt for a file to type."
  (interactive "P")
  (let* ((hack-file (if arg (read-file-name "Choose file: ")
                      (hacker-typer--choose-file)))
         (hacker-typer-buffer-name (file-name-nondirectory hack-file))
         (mycharset "0123456789abcdefghijklmnopqrstuvwxyz")
         (rate-range (apply 'number-sequence hacker-typer-random-range))
         (inc-amount (if (eq hacker-typer-type-rate 'random)
                         (elt rate-range (random (length rate-range)))
                       hacker-typer-type-rate)))
    ;; show hackerman if var set
    (when hacker-typer-show-hackerman
      (hacker-typer-hackerman nil))
    ;; delete previous hacker typer buffer if exists
    (when (and hacker-typer--previous-buffer
               (get-buffer hacker-typer--previous-buffer))
      (kill-buffer hacker-typer--previous-buffer))
    ;; set start/end for fetching from file
    (setq hacker-typer--start 0)
    (setq hacker-typer--end inc-amount)
    ;; add random chars to buffer name and open buffer
    (dotimes (i 20)
      (setq hacker-typer-buffer-name
            (concat (char-to-string (elt mycharset (random (length mycharset))))
                    hacker-typer-buffer-name)))
    (setq hacker-typer--previous-buffer hacker-typer-buffer-name)
    (get-buffer-create hacker-typer-buffer-name)
    (switch-to-buffer hacker-typer-buffer-name)
    (hacker-typer--set-mode hacker-typer-buffer-name)
    ;; unbind annoying modes
    (dolist (mode '(agressive-indent-mode
                    smartparens-mode
                    evil-smartparens-mode))
      (when (fboundp mode)
        (funcall mode 0)))
    ;; rebind commands locally and and use map
    (dolist (cmd '(self-insert-command
                   delete-backward-char
                   newline
                   sp-backward-delete-char))
      (when (fboundp cmd)
        (define-key hacker-typer-map `[remap ,cmd]
          `(lambda () (interactive) (hacker-typer--insert-contents ,hack-file))) ))
    (define-key hacker-typer-map [remap keyboard-quit] 'hacker-typer-quit)
    (use-local-map hacker-typer-map)))

;;;###autoload
(defun hacker-typer-quit ()
  "Kill hacker-typer buffers."
  (interactive)
  (when (get-buffer hacker-typer--previous-buffer)
    (kill-buffer hacker-typer--previous-buffer))
  (when (get-buffer "*hackerman*")
    (kill-buffer "*hackerman*"))
  (delete-other-windows))

;;;###autoload
(defun hacker-typer-hackerman (arg)
  "⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⢀⣀⣀⣀⣀⢀⡀⣀⣀⢀⡀⣀⢀⣀⣀⢀⢀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣻⣻⣯⢷⢺⣻⣟⢻⣟⣻⡟⣯⢻⣾⢟⢿⢺⣟⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢽⣻⣵⠞⠌⣁⣬⣬⣬⣫⣮⣭⡯⡗⢭⢽⢻⣟⢯⢯⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣟⢟⡞⢀⣤⣾⣿⣿⣿⣿⠿⢿⣶⣹⢹⡽⡻⣹⢺⢽⣯⢾⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⢿⢇⣰⣷⣿⣿⡯⠩⠁⢀⠀⠀⠹⣿⣳⢪⣮⡗⢮⣯⢧⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣯⡽⡷⣿⣯⣿⣿⣿⡗⠂⣠⣀⣀⡀⣤⡤⣿⣮⣕⡖⢻⣟⣼⣿⣷⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣯⡿⣯⡷⣟⡟⣟⣿⣿⣿⣿⣿⡏⣿⣿⣿⠇⠈⠿⡛⢹⣿⡞⢮⢞⣺⣮⣾⣯⣾⢿⢿⣿⣿⣻⢯⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣟⣯⣿⣿⣿⣿⣿⢿⣽⣽⣿⣟⣿⣽⣯⣻⢯⡻⣟⢟⢟⣿⣿⣿⣿⣿⡣⠘⠊⠋⢀⣠⡀⠀⠀⣿⣯⣮⣟⣗⡽⡻⣞⢽⢻⣽⣽⣽⡿⣿⡟⣿⡷⣿⣿⣿⣻
⡮⡾⢽⡟⢻⡷⢻⣯⣯⣿⣯⢿⣟⣾⣻⣯⡧⡽⡯⣯⡽⡽⣟⣽⠿⣿⣿⣿⣿⢳⢣⢔⢀⢤⡦⢴⠂⢠⣿⣟⡖⢳⠚⣽⡽⡻⣻⢽⣾⢼⣾⢽⣷⢽⣯⣿⣻⢾⢟⣿
⡯⣟⣿⣟⣿⣿⣻⡷⣽⣼⣾⣽⣧⣯⣽⣼⣟⣯⢿⠗⡏⢯⣧⡞⢿⣿⣿⣿⣿⣿⣮⣱⡕⠵⠙⠋⠀⣼⣟⣟⣮⣯⣻⡳⣗⣟⢾⣽⣾⣽⣾⣽⣾⣿⣧⣯⣾⣽⣿⣷
⣟⣿⣽⣿⣾⣿⣿⣯⣿⣷⣻⣿⣿⣾⣾⣿⣿⢋⠥⣸⡔⡧⢲⡿⣻⣭⣽⣿⣿⣿⣿⣿⣿⣷⣿⣿⣿⡿⣻⡟⣻⣾⣽⢿⣟⡿⣿⣿⣾⣿⣻⣿⣾⣿⣿⣳⣾⣟⣿⣷
⣽⣯⣷⣿⣿⣿⣿⣿⣷⣿⣿⣟⣿⣾⣿⣟⢕⠹⣿⣧⣽⢯⣿⣽⡞⡲⣻⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣟⢿⣿⣿⣿⣿⣾⣿⣿⣿⣾⣿⣿⣿⢿⣿⣷⣿⣟
⣾⡽⣾⣿⣿⣿⣿⣯⣿⣿⣿⣿⣿⣿⣿⡯⢥⡗⣥⣫⡭⣟⢻⣿⣿⣟⣷⣧⢽⢾⣿⡿⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣽⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣾⣿⣿⣾
⣿⢿⣿⣻⣿⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⡯⢕⢝⡞⣲⣹⡎⡯⣟⡿⢾⣯⢿⡽⡻⡽⣿⢸⣿⣿⣿⣿⣞⢳⢝⣻⢿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣷⣿⣿⣿⣿⣿⣿⡿⣻⣿⣿⣿⣿⣿⣿⡣⢬⣱⣎⡧⡕⡮⣕⣬⣽⣿⣽⣿⣟⣿⢻⡝⢺⣿⣟⣿⣯⢯⣮⣿⣧⢽⣼⣿⣿⣿⣿⣻⣿⣿⣿⣿⣿⣿⣿⣷⣽⣿⣿⣿
⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣫⢬⡞⡺⣹⡳⡼⡧⣯⡟⣏⣻⣿⣿⣿⣿⡗⣿⣿⣿⣯⣟⢯⣿⣿⣿⣿⣧⢿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣿
⣿⡿⣟⣯⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣣⢻⢯⣾⣷⣽⣯⣷⡟⡿⡿⣟⣏⣻⣟⣟⢿⣿⣿⣷⣿⣟⣿⣿⣿⣟⣯⣵⣿⣿⣿⣿⣿⣿⣮⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣽⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣯⣞⣿⣻⣟⣞⣾⢷⣾⣿⣿⣿⣿⣿⣿⣿⣷⣷⣯⣼⣻⣿⣿⣿⣿⣿⣿⣿⣟⣽⣿⣿⣿⣿⣿⣿⣿⣮⠻⣿⣟⡻⣿⣿⣟
⣿⣟⣿⣿⣿⣿⣿⣿⣿⣿⡿⣻⣿⣿⣿⣿⣿⣿⣽⣿⣽⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣻⣝⡻⣻⣻⣷⣻
⣿⣿⣿⣿⣿⣿⣿⣿⣿⢏⣾⣿⣿⣿⣿⣿⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⣛⣾⣽⣿⣼⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣝⢻⣿⣿
⣿⣿⣿⣿⣿⣿⣿⢿⣵⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣮⣷⣽⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣬⢿
⣿⣿⣿⣿⣿⡿⣣⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷
⣿⣿⣿⡿⣫⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣫⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⠟⣴⠟⠛⢻⡿⠛⠻⣿⣿⠟⠛⢻⡟⡿⠽⠛⠛⠛⠛⠛⠛⢻⠿⠛⠛⠻⠟⠛⠛⠛⢛⠛⠛⠛⠛⠻⢿⠟⠛⠛⢿⡿⠛⠈⠻⣿⣿⠟⠛⠻⣻⣿⠟⠋⠻⣿⠛⠛⢿
⣿⡏⠀⠀⠾⠀⠀⢸⡿⠁⠀⠀⠐⡟⠁⠀⢠⣤⣤⡔⠀⠀⠀⠀⣠⠄⠀⠀⠤⠀⠠⠄⠀⠀⠤⠀⠀⠈⠀⠀⠀⠟⠁⠀⠀⠠⣿⠁⠀⠀⠀⣵⠅⠀⠀⠀⠊⠀⠠⣾
⣿⠧⢤⣆⡀⠴⢤⠟⠴⢤⠤⠤⠄⠠⠤⠤⡿⡿⡿⠣⠤⠤⡴⠰⣗⠤⠤⠠⠠⠔⠀⠴⠔⡔⠠⠆⢠⠥⠤⠀⠤⠤⠄⠰⠄⠆⠰⠴⠤⠴⠤⢽⠣⠴⣤⠤⠤⢦⣼⣿
⣗⡀⣐⣿⡃⣀⠀⢀⣠⣶⣾⣆⠀⣠⣀⣀⠀⡀⣀⢀⣀⣧⡀⡀⣘⣄⢀⣀⢀⢀⣆⣀⣸⣿⣀⡀⢈⣀⢀⣃⣀⢠⡆⣀⢀⠀⡀⠶⣶⡀⡀⢘⢀⣀⡿⣄⡀⢀⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠄⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿"
  (interactive "P")
  (let ((hacker-file (concat hacker-typer-data-dir "hackerman.png"))
        (inhibit-message t))
    ;; If file doesn't exist, get it.
    (unless (file-exists-p hacker-file)
      (url-copy-file "http://i.imgur.com/hpV2qGim.png" hacker-file t))
    ;; Print file url to buffer and turn on iimage-mode
    (with-output-to-temp-buffer "*hackerman*"
      (princ (concat "file://" hacker-file)))
    (with-current-buffer "*hackerman*"
      (iimage-mode))
    ;; (other-window 1)
    ;; ;; Readjust window if it's easy to
    ;; (when (fboundp 'evil-window-set-width)
    ;;   (evil-window-set-width 117))
    ;; (other-window 1)
    ;; If prefix arg, delete other windows.
    (when arg
      (other-window 1)
      (delete-other-windows))))

;; utils
(defun hacker-typer--insert-contents (filename)
  "Insert contents from FILENAME into hacker-typer buffer."
  (let* ((rate-range (apply 'number-sequence hacker-typer-random-range))
         (inc-amount (if (eq hacker-typer-type-rate 'random)
                         (elt rate-range (random (length rate-range)))
                       hacker-typer-type-rate)))
    (insert-file-contents filename nil hacker-typer--start hacker-typer--end)
    (forward-char (- hacker-typer--end hacker-typer--start))
    (setq hacker-typer--start (+ hacker-typer--end))
    (setq hacker-typer--end (+ hacker-typer--start inc-amount))))

(defun hacker-typer--set-mode (buffer-name)
  "Set major mode for hacker-typer buffer named BUFFER-NAME based on buffer extension."
  (let ((name buffer-name)
        (mode nil))
    ;; Remove backup-suffixes from file name.
    (setq name (file-name-sans-versions name))
    ;; Remove remote file name identification.
    (while name
      ;; Find first matching alist entry.
      (setq mode
            ;; Filesystem is case-sensitive.
            (or
             ;; First match case-sensitively.
             (let ((case-fold-search nil))
               (assoc-default name auto-mode-alist
                              'string-match))
             ;; Fallback to case-insensitive match.
             (and auto-mode-case-fold
                  (let ((case-fold-search t))
                    (assoc-default name auto-mode-alist
                                   'string-match)))))
      (if (and mode
               (consp mode)
               (cadr mode))
          (setq mode (car mode)
                name (substring name 0 (match-beginning 0)))
        (setq name nil))
      (when mode
        (set-auto-mode-0 mode 'keep-mode-if-same)))))

(defun hacker-typer--choose-file ()
  "Randomly choose file from `hacker-typer-files', downloading if necessary."
  (let* ((file-url (elt hacker-typer-files
                        (random (length hacker-typer-files))))
         (base-name (file-name-nondirectory file-url))
         (base-name-nc (concat "no-comment-" base-name))
         (hacker-file (concat hacker-typer-data-dir base-name))
         (hacker-file-nc (concat hacker-typer-data-dir base-name-nc))
         (inhibit-redisplay t))
    ;; If file doesn't exist, get it
    (unless (file-exists-p hacker-file)
      (url-copy-file file-url hacker-file t))
    ;; If remove-comments is on and uncommented file doesn't exist, get it.
    (unless (or (not hacker-typer-remove-comments)
                (file-exists-p hacker-file-nc))
      (url-copy-file file-url hacker-file-nc t)
      (find-file hacker-file-nc)
      ;; remove comments
      (goto-char (point-min))
      (let (kill-ring)
        (comment-kill (count-lines (point-min) (point-max))))
      ;; remove awkward newlines
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t) (replace-match "\n\n"))
      ;; save an dkill
      (save-buffer)
      (kill-buffer base-name-nc))
    ;; return appropriate file name.
    (if hacker-typer-remove-comments
        hacker-file-nc
      hacker-file)))

(provide 'hacker-typer)

;;; hacker-typer.el ends here
