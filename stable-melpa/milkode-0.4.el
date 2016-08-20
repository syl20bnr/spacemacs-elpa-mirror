;;; milkode.el --- Command line search and direct jump with Milkode

;; Copyright (C) 2012 ongaeshi

;; Author: ongaeshi
;; Keywords: milkode, search, grep, jump, keyword
;; Package-Version: 0.4
;; Version: 0.4
;; Package-Requires:

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Command line search and direct jump with Milkode.
;; Milkode(http://milkode.ongaeshi.me) of the installation is required.

;; Feature
;;   1. Search (milkode:search). Jump to row C-c C-c.
;;   2. When you search for direct pass ('/path/to/dir:15') jump directly to the specified row.
;;   3. Move the cursor to direct pass on a text file, (milkode:search) can jump
;;
;; Displayed direct pass to click the line number in the 'milk web' (ex. http://kodeworld.ongaeshi.me/)

;; URL
;;   https://github.com/ongaeshi/emacs-milkode

;;; Install:
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/milkode.el")

;;; Initlial Setting:
;; (require 'milkode)
;;
;; ;; Shortcut setting (Your favorite things)
;; (global-set-key (kbd "M-g") 'milkode:search)
;;
;; ;; popwin setting (Optional)
;; (push '("*grep*" :noselect t) popwin:special-display-config)

;;; Code:

(declare-function jtl-push-stack "jump-to-line")

;;; Variables:
(defvar milkode:windows-p
  (let ((cygwin-p (eq system-type 'cygwin))
        (nt-p (eq system-type 'windows-nt))
        (meadow-p (featurep 'meadow)))
    (or cygwin-p nt-p meadow-p))
  "Flag that system is Window")

(defvar milkode:history nil
  "History of gmilk commands.")

(defface milkode:highlight-line-face '((t (:background "#66ccff" :underline t)))
  "Face for jump highlight." :group 'jump-to-line)

(defvar gmilk-command
  (if milkode:windows-p "gmilk.bat" "gmilk")
  "gmilk command.")

(defvar jump-from-browser-command
  (if milkode:windows-p "gmilk.bat" "gmilk")
  "M-x jump-from-browser-command.")

(defvar milk-command
  (if milkode:windows-p "milk.bat" "milk")
  "milk command.")

;;; Public:

;;;###autoload
(defun milkode:search ()
  "Milkode search current package using `M-x grep`"
  (interactive)
  (let ((at-point (thing-at-point 'filename)))
    (if (milkode:is-directpath at-point)
        (progn
          (setq milkode:history (cons at-point milkode:history))
          (milkode:jump-directpath at-point))
      (let ((input (read-string "gmilk: " (thing-at-point 'symbol) 'milkode:history)))
        (if (milkode:is-directpath input)
            (milkode:jump-directpath input)
          (milkode:grep input))))))

;;;###autoload
(defun milkode:search-at-point (n)
  "Milkode search current package at point text. If the prefix was C-u, search all registered packages"
  (interactive "P")
  (let ((at-point (thing-at-point 'filename))
        (is-all-search (consp n)))
    (if (milkode:is-directpath at-point)
        (progn
          (setq milkode:history (cons at-point milkode:history))
          (milkode:jump-directpath at-point))
      (let ((input (thing-at-point 'symbol)))
        (if (milkode:is-directpath input)
            (milkode:jump-directpath input)
          (if is-all-search
              (let ((gmilk-command-option "-a"))
                (milkode:grep input))
            (milkode:grep input)))))))

;;;###autoload
(defun milkode:search-from-all-packages ()
  "Milkode search all registered packages using `M-x grep`"
  (interactive)
  (let ((gmilk-command-option "-a"))
    (milkode:search)))

;;;###autoload
(defun milkode:display-history ()
  "Dispaly search history"
  (interactive)
  (with-current-buffer (get-buffer-create "*milkode*")
    (erase-buffer)
    (insert (mapconcat #'identity milkode:history "\n"))
    (pop-to-buffer "*milkode*")))

;;;###autoload
(defun milkode:add (directory)
  "Execute `milk add`"
  (interactive "Dmilk add: ")
  (with-current-buffer (get-buffer-create "*milkode*")
    (erase-buffer)
    (insert (shell-command-to-string (format "%s add %s" milk-command directory)))
    (pop-to-buffer "*milkode*")))

;;;###autoload
(defun milkode:update (directory)
  "Execute `milk update`"
  (interactive "Dmilk update: ")
  (with-current-buffer (get-buffer-create "*milkode*")
    (setq default-directory directory)
    (erase-buffer)
    (insert (shell-command-to-string (format "%s update" milk-command)))
    (pop-to-buffer "*milkode*")))

(when (featurep 'moz)
;;;###autoload
(defun milkode:jump-from-browser ()
  (interactive)
  (let* ((url (milkode:moz-get-url))
         (directpath (milkode:url-to-directpath url)))
    (if directpath
        (milkode:jump-directpath directpath)
      (message (format "Invalid milkode(milk web) url: %s" url)))))

;; Private
(defun milkode:url-to-directpath-lineno (src)
  (if (string-match "#n\\([0-9]+\\)" src)
      (substring src (match-beginning 1) (match-end 1))
    1))

(defun milkode:url-to-directpath-path (src)
  (if (string-match "\\(.*\\)\\?.*" src)
      (substring src (match-beginning 1) (match-end 1))
    (if (string-match "\\(.*\\)#n[0-9]+" src)
        (substring src (match-beginning 1) (match-end 1))
      src)))

(defun milkode:url-to-directpath (url)
  (if (string-match "/home/\\(.*\\)" url)
      (let* ((match1 (substring url (match-beginning 1) (match-end 1)))
             (path   (milkode:url-to-directpath-path match1))
             (lineno (milkode:url-to-directpath-lineno match1)))
        (format "/%s:%s" path lineno))))

(defun milkode:moz-get-url ()
  (let ((moz-proc (inferior-moz-process)))
    ;; Send message to moz.el
    (comint-send-string moz-proc "repl._workContext.content.location.href")
    (sleep-for 0 100)
    (with-current-buffer (process-buffer moz-proc)
      ;; Extract URL from *MozRepl* buffer
      (goto-char (point-max))
      (line-move -1)
      (let ((url (buffer-substring-no-properties
                 (+ (point-at-bol) (length moz-repl-name) 3)
                 (- (point-at-eol) 1))))
        ;; Return result
        (message "%s" url)
        url))))
)

;;; Private:
(defun milkode:jump-directpath (path)
  (if (featurep 'jump-to-line)
      (jtl-push-stack (point-marker)))
  (with-temp-buffer
    (message (format "Jump to %s ..." path))
    (call-process jump-from-browser-command nil t nil path)
    (goto-char (point-min))
    (milkode:goto-line (thing-at-point 'filename))
    (milkode:highlight-line 0.6)))

(defun milkode:command (path)
  (if (boundp 'gmilk-command-option)
      (concat gmilk-command " " gmilk-command-option " " path)
    (concat gmilk-command " " path)))

(defun milkode:grep (path)
  (grep (milkode:command path)))

(defun milkode:is-directpath (str)
  (unless (null str)
    (string-match "^/.*:[0-9]+" str)))

(defun milkode:is-windows-abs (str)
  (string-match "^[a-zA-Z]:" str))

(defun milkode:goto-line (str)
  (let ((list (split-string str ":"))
        file line)
    (if (milkode:is-windows-abs str)
        (setq file (concat (nth 0 list) ":" (nth 1 list))
              line (string-to-number (nth 2 list)))
      (setq file (nth 0 list) line (string-to-number (nth 1 list))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun milkode:highlight-line (seconds)
  (milkode:highlight-line-start)
  (sit-for seconds)
  (milkode:highlight-line-end))

(defvar milkode:match-line-overlay nil)

(defun milkode:highlight-line-start ()
  (let ((args (list (line-beginning-position) (1+ (line-end-position)) nil)))
    (if (not milkode:match-line-overlay)
        (setq milkode:match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay milkode:match-line-overlay args))
    (overlay-put milkode:match-line-overlay 'face 'milkode:highlight-line-face)))

(defun milkode:highlight-line-end ()
  (when milkode:match-line-overlay
    (delete-overlay milkode:match-line-overlay)
    (setq milkode:match-line-overlay nil)))

;;

(provide 'milkode)
;;; milkode.el ends here
