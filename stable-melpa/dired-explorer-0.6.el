;;; dired-explorer.el --- minor-mode provides Explorer like select file at dired.
;; Original: http://homepage1.nifty.com/blankspace/emacs/dired.html
;; Original2: http://www.bookshelf.jp/soft/meadow_25.html#SEC286
;; Introduce and Supervise: rubikitch
;; Maintainer: jidaikobo-shibata
;; Contributions: syohex, Steve Purcell
;; Keywords: dired explorer
;; Package-Version: 0.6
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.6
;; for Emacs 24.5.1 - 26.1

;; This program is free software; you can redistribute it and/or modify
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
;; [en]
;; This mode provides "Windows / Macintosh (Mac OS X) like file selection" for dired's buffer.
;; Move cursor by just pressing alphabet or number key.
;; And also it prohibits dired from opening many buffers.
;; of course, at this mode, cannot use dired's default keybind like "c".
;; You may use keybind that made of one alphabet, use with Meta (e.g. M-d).
;; toggle mode by ":".
;; rubikitch told me about this elisp's url at his school.
;; but I couldn't know who made this originally.
;;
;; [ja]
;; WindowsやMac OS Xのデフォルトのファイラのようなファイル選択をdiredで行います。
;; 英数字のキーを打鍵するだけで、diredでファイル／ディレクトリを選択します。
;; また、diredeがたくさんのバッファを開きすぎることを抑止しています。
;; 当然ながら、このモードを有効にするとデフォルトのdiredのキーバインドが使えません。
;; diredのアルファベット一文字のキーバインドは基本的に"M-"にあて直しています。
;; モードの切り替えは":"で行ってください。
;; このelispは、るびきちさんが彼のEmacs塾で、僕にURLを教えてくれましたが、
;; 僕にはオリジナルの作者が誰かわからなかったので、URLだけ明示しています。

;;; Usage:
;; just write below in your .init.
;; (require 'dired-explorer)
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map ":" (lambda () (interactive) (dired-explorer-mode t)))
;;             (dired-explorer-mode t)))
;;
;; toggle mode by ":".

;;; Change Log:
;; If You troubled with my change. Please contact me on Email.
;;
;; 0.6
;; add make-directory
;;
;; 0.5
;; dired-explorer-dired-open is deleted. it seems meanless.
;; I was too foolish that I killed important Emacs keybind M-x at this mode.
;;
;; 0.4
;; first release

;;; Code:

(require 'dired)
(require 'cl-lib)

(defvar dired-explorer-mode              nil)
(defvar dired-explorer-isearch-next      "\C-r")
(defvar dired-explorer-isearch-prev      "\C-e")
(defvar dired-explorer-isearch-backspace "\C-h")
(defvar dired-explorer-isearch-return    "\C-g")
(defvar dired-explorer-isearch-returnkey "\C-m")
(defvar dired-explorer-isearch-word      "")
(defvar dired-explorer-mode-hook         nil)
(defvar dired-mode-old-local-map)

(defvar dired-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Lower keys for normal dired-mode are replaced M-* at thid mode.
    ;; except for "x".
    (define-key map "\M-a" 'dired-find-alternate-file)
    (define-key map "\M-d" 'dired-flag-file-deletion)
    (define-key map "\M-e" 'dired-find-file)
    (define-key map "\M-f" 'dired-find-file)
    (define-key map "\M-g" 'revert-buffer)
    (define-key map "\M-i" 'dired-maybe-insert-subdir)
    (define-key map "\M-j" 'dired-goto-file)
    (define-key map "\M-k" 'dired-do-kill-lines)
    (define-key map "\M-l" 'dired-do-redisplay)
    (define-key map "\M-m" 'dired-mark)
    (define-key map "\M-n" 'dired-next-line)
    (define-key map "\M-o" 'dired-find-file-other-window)
    (define-key map "\M-p" 'dired-previous-line)
    (define-key map "\M-s" 'dired-sort-toggle-or-edit)
    (define-key map "\M-t" 'dired-toggle-marks)
    (define-key map "\M-u" 'dired-unmark)
    (define-key map "\M-v" 'dired-view-file)
    (define-key map "\M-w" 'dired-copy-filename-as-kill)
    (define-key map "\M-X" 'dired-do-flagged-delete) ; this must be capital
    (define-key map "\M-y" 'dired-show-file-type)
    (define-key map ":"    'dired-explorer-mode)
    (define-key map "+"    'make-directory)
    ;; (define-key map "\C-m" 'dired-find-file)
    ;; (define-key map (kbd "<return>") 'dired-find-file)
    ;; (define-key map "^" 'dired-find-file)
    ;; (define-key map "I" 'dired-kill-subdir)
    map))

(define-minor-mode dired-explorer-mode
  "Minor-mode dired-explorer-mode."
  :lighter " Expl")

(defun dired-explorer-do-isearch (REGEX1 REGEX2 FUNC1 FUNC2 RPT)
  "Dired explorer isearch.  REGEX1 REGEX2 FUNC1 FUNC2 RPT."
  (interactive)
  (let ((input last-command-event)
        (inhibit-quit t)
        (oldpoint (point))
        regx
        str
        (n 1))
    (save-match-data
      (catch 'END
        (while t
          (funcall FUNC1)
          (cond
           ;;end
           ((and (integerp input) (= input ?:))
            (setq unread-command-events (append (list input) unread-command-events))
            (throw 'END nil))

           ;; character
           ;;_.-+~#
           ((and (integerp input)
                 (or (and (>= input ?a) (<= input ?z))
                     ;; (and (>= input ?A) (<= input ?Z)) ; ignore to use command like "C"
                     (and (>= input ?0) (<= input ?9))))
            (setq str (char-to-string input))
            (if (string= dired-explorer-isearch-word str)
                (setq n 2)
              (setq n 1))
            ;; .meadow.el meadow.el は同一視
            (setq regx (concat REGEX1 "[\.~#+_]*" str REGEX2))
            (if (not (re-search-forward regx nil t n))
                (progn
                  (goto-char (point-min))
                  (re-search-forward regx nil t nil)))
            (setq dired-explorer-isearch-word str))

           ;; backspace
           ((and (integerp input)
                 (or (eq 'backspace input)
                     (= input (string-to-char dired-explorer-isearch-backspace))))
            (setq str (if (eq 0 (length str)) str (substring str 0 -1)))
            (setq regx (concat REGEX1 str REGEX2))
            (goto-char oldpoint)
            (re-search-forward regx nil t nil))

           ;; next
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-next)))
            (re-search-forward regx nil t RPT))

           ;; previous
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-prev)))
            (re-search-backward regx nil t nil))

           ;; return
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-return)))
            (goto-char oldpoint)
            (message "return")
            (throw 'END nil))

           ;; other command
           (t
            (setq unread-command-events (append (list input) unread-command-events))
            (throw 'END nil)))

          (funcall FUNC2)
          ;; (highline-highlight-current-line)
          ;; (message str)
          (setq input (read-event)))))))

(defun dired-explorer-isearch()
  "Incremental search for dired."
  (interactive)
  (dired-explorer-do-isearch
   "[0-9] "                                                        ; REGEX1
   "[^ \n]+$"                                                      ; REGEX2
   (lambda() (if (not (= (point-min) (point))) (backward-char 3))) ; FUNC1
   'dired-move-to-filename                                         ; FUNC2
   2                                                               ; RPT
   ))

(defun dired-explorer-isearch-define-key (str)
  "Dired explorer isearch define key.  STR."
  (cl-loop for ch across str do
           (define-key dired-explorer-mode-map (char-to-string ch) 'dired-explorer-isearch)))
(dired-explorer-isearch-define-key "abcdefghijklmnopqrstuvwxyz0123456789")

;; for older environment
(defsubst dired-explorer-string-trim (string)
  "Remove leading and trailing whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string)
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun dired-mac-alias-path (path)
  "Mac alias path.  PATH is POSIX path."
  (let (mac-orig-path)
    (setq mac-orig-path
          (when (eq system-type 'darwin)
            (dired-explorer-string-trim
             (shell-command-to-string
              (concat "osascript -e 'tell application \"Finder\" to return POSIX path of (original item of item (POSIX file \"" path "\") as alias)'")))))
    (when (and mac-orig-path ;; thx syohex
               (not (string-match "execution error" mac-orig-path))
               (file-exists-p mac-orig-path))
      mac-orig-path)))

;;; ------------------------------------------------------------
;;; Provide

(provide 'dired-explorer)

;;; dired-explorer.el ends here
