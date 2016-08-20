;;; eopengrok.el --- opengrok interface for emacs

;; Copyright (C) 2015 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.3.0
;; Package-Version: 0.3.0
;; Keywords: tools
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (magit "2.1.0") (cl-lib "0.5"))

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

;; opengrok interface for emacs
;;
;; See documentation on https://github.com/youngker/eopengrok.el

;;; Code:

(require 's)
(require 'dash)
(require 'etags)
(require 'magit)
(require 'cl-lib)

(defvar eopengrok-pending-output nil)
(defvar eopengrok-last-filename nil)
(defvar eopengrok-search-text nil)
(defvar eopengrok-first-match-point nil)
(defvar eopengrok-start-time nil)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defconst eopengrok-source-regexp
  "\\([^ ]*?\\):\\([0-9]+\\)[ \t]+\\[\\(.*\\)\\]")

(defconst eopengrok-file-regexp
  "\\([^ ]*?\\):\\(\\)[ \t]+\\[\\(\\.\\.\\.\\)\\]")

(defconst eopengrok-history-regexp
  "\\([^ ]*?\\):[ \t]+\\[\\(\\w+\\)[ \t]\\(.*\\)\\]")

(defconst eopengrok-unknown-regexp
  "\\([^ ]*?\\):\\(\\)[ \t]+\\[\\(.*\\)\\]")

(defcustom eopengrok-jar
  nil
  "The location of file `opengrok.jar'."
  :group 'eopengrok)

(defcustom eopengrok-ctags
  nil
  "The location of file `ctags'."
  :group 'eopengrok)

(defcustom eopengrok-configuration
  ".opengrok/configuration.xml"
  "Configuration file."
  :group 'eopengrok)

(defcustom eopengrok-ignored-dir
  '(".opengrok" "out" "*.so" "*.a" "*.o" "*.gz" "*.bz2" "*.jar" "*.zip" "*.class")
  "Ignored file or directory."
  :group 'eopengrok)

(defcustom eopengrok-abbreviate-filename 80
  "Abbreviate filename length."
  :group 'eopengrok)

(defcustom eopengrok-line-length 500
  "Truncate line length."
  :group 'eopengrok)

(defface eopengrok-file-face
  '((t :inherit font-lock-function-name-face))
  "Face for files."
  :group 'eopengrok)

(defface eopengrok-info-face
  '((t :inherit font-lock-constant-face))
  "Face for info."
  :group 'eopengrok)

(defface eopengrok-source-face
  '((t :inherit font-lock-doc-face))
  "Face for source."
  :group 'eopengrok)

(defface eopengrok-highlight-face
  '((t :inherit highlight))
  "Face for highlight item."
  :group 'eopengrok)

(defun eopengrok-resume ()
  "Open *eopengrok* buffer."
  (interactive)
  (when (get-buffer eopengrok-buffer)
    (pop-to-buffer eopengrok-buffer)))

(defun eopengrok-kill-process ()
  "Kill process."
  (interactive)
  (-when-let* ((proc (get-process "eopengrok"))
               (status (process-status proc))
               (run (eq status 'run)))
    (kill-process proc)))

(defun eopengrok-index-option-list (dir)
  "Opengrok index option list, target is DIR."
  (-flatten (list "-Xmx2048m"
                  "-jar" eopengrok-jar
                  "-r" "on"
                  "-c" eopengrok-ctags
                  "-a" "on"
                  "-W" (concat dir eopengrok-configuration)
                  "-S" "-P"
                  "-s" dir
                  "-d" (concat dir ".opengrok")
                  "-H" "-q"
                  (--mapcat (list "-i" it) eopengrok-ignored-dir))))

(defun eopengrok-get-configuration ()
  "Search for Project configuration.xml."
  (let ((dir (expand-file-name default-directory)))
    (catch 'done
      (while dir
        (when (file-exists-p (concat dir eopengrok-configuration))
          (throw 'done (concat dir eopengrok-configuration)))
        (setq dir (file-name-as-directory
                   (file-name-directory
                    (directory-file-name dir))))
        (when (string-match "^\\(/\\|[A-Za-z]:[\\/]\\)$" dir)
          (error "Can't find a configuration.xml"))))))

(defun eopengrok-search-option-list (configuration find-option text)
  "Opengrok search option list with CONFIGURATION FIND-OPTION TEXT."
  (list "-Xmx2048m"
        "-cp" eopengrok-jar "org.opensolaris.opengrok.search.Search"
        "-R" configuration find-option text))

(defmacro eopengrok-properties-region (props &rest body)
  "Add PROPS and Execute BODY to all the text it insert."
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun eopengrok-get-properties (pos)
  "Get properties at POS."
  (list (get-text-property pos :name)
        (get-text-property pos :info)))

(defun eopengrok-show-source ()
  "Display original source."
  (with-current-buffer eopengrok-buffer
    (-when-let* (((file number) (eopengrok-get-properties (point))))
      (let* ((buffer (find-file-noselect file))
             (window (display-buffer buffer)))
        (set-buffer buffer)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- number)))
        (set-window-point window (point))
        window))))

(defun eopengrok-show-commit ()
  "Display magit-show-commit."
  (-when-let* (((file commit-id) (eopengrok-get-properties (point))))
    (setq default-directory (file-name-directory file))
    (magit-git-string "rev-parse" "--show-toplevel")
    (magit-show-commit commit-id)))

(defun eopengrok-jump-to-source ()
  "Jump point to the other window."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (-when-let (info (get-text-property (point) :info))
      (if (numberp info)
          (-when-let (window (eopengrok-show-source))
            (select-window window)
            (ring-insert find-tag-marker-ring (point-marker)))
        (eopengrok-show-commit)))))

(defun eopengrok-number-p (str)
  "Check commitid from STR."
  (< (length str) 8))

(defun eopengrok-next-line ()
  "Move point to the next search result, if one exists."
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (next-single-property-change
                     (save-excursion (end-of-line) (point)) :info))
      (goto-char pos)
      (if (numberp (get-text-property pos :info))
          (eopengrok-show-source)
        (let ((magit-display-buffer-noselect t))
          (eopengrok-show-commit))))))

(defun eopengrok-previous-line ()
  "Move point to the previous search result, if one exists."
  (interactive)
  (with-current-buffer eopengrok-buffer
    (-when-let (pos (previous-single-property-change
                     (save-excursion (beginning-of-line) (point)) :info))
      (goto-char pos)
      (beginning-of-line)
      (if (numberp (get-text-property (point) :info))
          (eopengrok-show-source)
        (let ((magit-display-buffer-noselect t))
          (eopengrok-show-commit))))))

(defun eopengrok-abbreviate-file (file)
  "Abbreviate FILE name."
  (let* ((start (- (point) (length file)))
         (end (point))
         (amount (if (numberp eopengrok-abbreviate-filename)
                     (- (- end start) eopengrok-abbreviate-filename)
                   999))
         (advance-word (lambda ()
                         "Return the length of the text made invisible."
                         (let ((wend (min end (progn (forward-word 1) (point))))
                               (wbeg (max start (progn (backward-word 1) (point)))))
                           (goto-char wend)
                           (if (<= (- wend wbeg) 1)
                               0
                             (put-text-property (1+ wbeg) wend 'invisible t)
                             (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (cl-decf amount (funcall advance-word)))
    (goto-char end)))

(defun eopengrok-truncate-line (line)
  "Truncate long text from LINE."
  (let ((len eopengrok-line-length))
    (if (> (length line) len)
        (format "%s...]" (substring line 0 (- len 4)))
      line)))

(defun eopengrok-remove-html-tags (line)
  "Remove html tag from LINE."
  (->> line
       (replace-regexp-in-string "<[^>]*>" "")
       (s-replace-all '(("&lt;" . "<") ("&gt;" . ">")
                        ("&amp;" . "&") ("\r" . "")))))

(defun eopengrok-text-highlight (line)
  "Highlighting Text from LINE."
  (let ((pos 0))
    (while (string-match (concat "\\b" eopengrok-search-text "\\b") line pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'face 'eopengrok-highlight-face line))))

(defun eopengrok-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (eopengrok-jump-to-source))

(defvar eopengrok-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'eopengrok-handle-mouse)
    map))

(defun eopengrok-print-line (arg-list)
  "Print line from ARG-LIST with their propertize."
  (with-current-buffer eopengrok-buffer
    (-if-let* (((file info src) arg-list)
               (file (propertize file 'face 'eopengrok-file-face))
               (info (propertize info
                                 'face 'eopengrok-info-face
                                 'mouse-face 'highlight
                                 'keymap eopengrok-mouse-map))
               (src (propertize src 'face 'eopengrok-source-face)))
        (progn
          (unless (string= file eopengrok-last-filename)
            (insert (format "\n%s\n" file))
            (eopengrok-abbreviate-file file)
            (unless eopengrok-first-match-point
              (setq eopengrok-first-match-point (point))))
          (eopengrok-properties-region
           (list :name (expand-file-name file)
                 :info (cond
                        ((equal info "") 1)
                        ((eopengrok-number-p info)
                         (string-to-number info))
                        (t info)))
           (eopengrok-text-highlight src)
           (insert (concat (format "%08s" info) " " src)))
          (insert "\n")
          (setq eopengrok-last-filename file))
      (insert (car arg-list) "\n"))))

(defun eopengrok-read-line (line)
  "Read the LINE and return the list for print."
  (cond
   ((string-match eopengrok-source-regexp line))
   ((string-match eopengrok-history-regexp line))
   ((string-match eopengrok-file-regexp line))
   ((string-match eopengrok-unknown-regexp line))
   (t (string-match "\\(.*\\)" line)))
  (mapcar (lambda (arg) (match-string arg line)) '(1 2 3)))

(defun eopengrok-process-filter (process output)
  "Process eopengrok output from PROCESS containted in OUTPUT."
  (with-current-buffer eopengrok-buffer
    (let ((buffer-read-only nil)
          (pos 0)
          (output (concat eopengrok-pending-output output)))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            (-> line
                eopengrok-truncate-line
                eopengrok-remove-html-tags
                eopengrok-read-line
                eopengrok-print-line))))
      (setq eopengrok-pending-output (substring output pos)))))

(defun eopengrok-process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (with-current-buffer eopengrok-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (setq eopengrok-pending-output nil)
      (insert (format "\nSearch complete.  Search time = %.2f seconds.\n"
                      (float-time (time-subtract (current-time)
                                                 eopengrok-start-time)))))
    (when eopengrok-first-match-point
      (goto-char eopengrok-first-match-point))))

(defun eopengrok-init (text dir kind)
  "Initialize function from TEXT & DIR & KIND."
  (setq eopengrok-start-time (current-time))
  (setq eopengrok-last-filename nil)
  (setq eopengrok-first-match-point nil)
  (with-current-buffer eopengrok-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (s-repeat 80 "="))
      (insert (format "\n%s%s" kind text))
      (insert (format "\nDirectory: %s\n"
                      (s-chop-suffix eopengrok-configuration dir))))))

(defmacro eopengrok-define-find (sym option)
  "Make function with SYM and OPTION."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(progn
       (defun ,fun (text) ,doc
              (interactive (list (read-string ,str (current-word))))
              (let* ((conf (eopengrok-get-configuration))
                     (proc (apply 'start-process
                                  "eopengrok"
                                  eopengrok-buffer
                                  "java"
                                  (eopengrok-search-option-list
                                   conf ,option text))))
                (eopengrok-init text conf ,str)
                (setq eopengrok-search-text text)
                (set-process-filter proc 'eopengrok-process-filter)
                (set-process-sentinel proc 'eopengrok-process-sentinel)
                (with-current-buffer eopengrok-buffer
                  (eopengrok-mode t)
                  (setq truncate-lines t)
                  (setq buffer-read-only t)
                  (set-buffer-modified-p nil)
                  (pop-to-buffer eopengrok-buffer)
                  (goto-char (point-max))))))))

(eopengrok-define-find definition "-d")
(eopengrok-define-find file "-p")
(eopengrok-define-find reference "-r")
(eopengrok-define-find text "-f")
(eopengrok-define-find history "-h")

(defun eopengrok-index-process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (with-current-buffer eopengrok-indexing-buffer
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (format "\nIndexing complete.  Indexing time = %.2f seconds.\n"
                      (float-time (time-subtract (current-time)
                                                 eopengrok-start-time)))))))

(defun eopengrok-make-index (dir)
  "Make an Index file in a DIR."
  (interactive "DIndex files in directory: ")
  (let ((proc (apply 'start-process
                     "eopengrok-indexer"
                     eopengrok-indexing-buffer
                     "java"
                     (eopengrok-index-option-list (expand-file-name dir)))))
    (set-process-sentinel proc 'eopengrok-index-process-sentinel)
    (with-current-buffer eopengrok-indexing-buffer
      (setq buffer-read-only t)
      (setq eopengrok-start-time (current-time))
      (goto-char (point-max))
      (let ((buffer-read-only nil))
        (insert (s-repeat 80 "=")))
      (pop-to-buffer eopengrok-indexing-buffer))))

(defun eopengrok-startup-asserts ()
  "Check the requirements."
  (when (or (not eopengrok-jar)
            (not eopengrok-ctags))
    (display-warning 'eopengrok (format "\
eopengrok requires opengrok.jar and ctags path.

You can add these lines to your init file.
(setq eopengrok-jar   \"/path/to/opengrok-0.12.1.5/lib/opengrok.jar\")
(setq eopengrok-ctags \"/path/to/ctags\")

See the README for more info:
https://github.com/youngker/eopengrok.el\n") :error)))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("\C-c\C-csI" . eopengrok-make-index)
          ("\C-c\C-csd" . eopengrok-find-definition)
          ("\C-c\C-csf" . eopengrok-find-file)
          ("\C-c\C-css" . eopengrok-find-reference)
          ("\C-c\C-cst" . eopengrok-find-text)
          ("\C-c\C-csh" . eopengrok-find-history)
          ("\C-c\C-csb" . eopengrok-resume)
          ("n"          . eopengrok-next-line)
          ("p"          . eopengrok-previous-line)
          ("c"          . eopengrok-kill-process)
          ("<return>"   . eopengrok-jump-to-source))
  (define-key eopengrok-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode eopengrok-mode
  "Minor mode for opengrok."
  nil " eopengrok" eopengrok-mode-map)

(provide 'eopengrok)

(if after-init-time
    (eopengrok-startup-asserts)
  (add-hook 'after-init-hook #'eopengrok-startup-asserts t))

;;; eopengrok.el ends here
