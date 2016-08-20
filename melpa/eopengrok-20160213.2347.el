;;; eopengrok.el --- opengrok interface for emacs

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.4.0
;; Package-Version: 20160213.2347
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

;; opengrok interface for Emacs
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
(defvar eopengrok-start-time nil)
(defvar eopengrok-search-text nil)
(defvar eopengrok-current-search nil)
(defvar eopengrok-current-configuration nil)
(defvar eopengrok-skip-output-p nil)

(defconst eopengrok-buffer "*eopengrok*")
(defconst eopengrok-indexing-buffer "*eopengrok-indexing-buffer*")

(defconst eopengrok-source-regexp
  "^\\([^ ]*?\\):\\([0-9]+\\):\\(.*\\)")

(defconst eopengrok-history-regexp
  "^\\([^ ]*?\\)::[ \t]+\\(\\w+\\)\\(.*\\)")

(defgroup eopengrok nil
  "Opengrok interface for emacs."
  :prefix "eopengrok-"
  :group 'applications)

(defcustom eopengrok-jar
  nil
  "The location of file `clj-opengrok.jar'."
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
  '("d:.opengrok" "d:out" "*.so" "*.a" "*.o" "*.gz" "*.bz2" "*.jar" "*.zip" "*.class")
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
  (-if-let* ((proc (get-process "eopengrok"))
             (status (process-status proc))
             (run (eq status 'run)))
      (kill-process proc)
    (kill-buffer eopengrok-buffer)))

(defun eopengrok-index-option-list (dir enable-projects-p)
  "Index option list, target is DIR with ENABLE-PROJECTS-P flag."
  (-flatten (list "-Xmx2048m"
                  "-cp" eopengrok-jar "org.opensolaris.opengrok.index.Indexer"
                  "-r" "on"
                  "-c" eopengrok-ctags
                  "-a" "on"
                  "-W" (concat dir eopengrok-configuration)
                  "-S"
                  "-s" dir
                  "-d" (concat dir ".opengrok")
                  "-H" "-q"
                  (--mapcat (list "-i" it) eopengrok-ignored-dir)
                  (when enable-projects-p "-P"))))

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
  (list "-R" configuration find-option text))

(defun eopengrok-quick-search-option (configuration find-option text)
  "Opengrok search option list with CONFIGURATION FIND-OPTION TEXT."
  (format "search %S %S %S %S\n" "-R" configuration find-option text))

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

(defun eopengrok-send-process (str)
  "Send STR to process."
  (eopengrok-display-info)
  (setq eopengrok-skip-output-p t)
  (process-send-string (get-process "eopengrok") str))

(defun eopengrok-next-page ()
  "Next page."
  (interactive)
  (eopengrok-send-process "n\n"))

(defun eopengrok-previous-page ()
  "Previous page."
  (interactive)
  (eopengrok-send-process "p\n"))

(defun eopengrok-goto-page (number)
  "Go to the NUMBER of page."
  (interactive "nPage number: ")
  (eopengrok-send-process (concat "g " (number-to-string number) "\n")))

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
  (let (beg end)
    (with-temp-buffer
      (insert (propertize line 'read-only nil))
      (goto-char (point-min))
      (while (and (re-search-forward
                   (s-replace "\"" "" eopengrok-search-text) nil t)
                  (> (- (setq end (match-end 0))
                        (setq beg (match-beginning 0))) 0))
        (add-text-properties beg end '(face eopengrok-highlight-face)))
      (buffer-string))))

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
            (eopengrok-abbreviate-file file))
          (eopengrok-properties-region
           (list :name (expand-file-name file)
                 :info (cond
                        ((equal info "") 1)
                        ((eopengrok-number-p info)
                         (string-to-number info))
                        (t info)))
           (insert (concat (format "%08s" info) " "
                           (eopengrok-text-highlight src))))
          (insert "\n")
          (setq eopengrok-last-filename file))
      (insert (car arg-list) "\n"))))

(defun eopengrok-read-line (line)
  "Read the LINE and return the list for print."
  (cond
   ((string-match eopengrok-source-regexp line))
   ((string-match eopengrok-history-regexp line))
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
            (if eopengrok-skip-output-p
                (when (string-match "^Results:" line)
                  (setq eopengrok-skip-output-p nil)
                  (insert line "\n"))
              (-> line
                  eopengrok-remove-html-tags
                  eopengrok-read-line
                  eopengrok-print-line)))))
      (setq eopengrok-pending-output (substring output pos)))))

(defun eopengrok-process-sentinel (process event)
  "Handle eopengrok PROCESS EVENT."
  (with-current-buffer eopengrok-buffer
    (when (string-match "killed" event)
      (kill-buffer eopengrok-buffer))))

(defun eopengrok-display-info ()
  "Print infomation."
  (with-current-buffer eopengrok-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert (format "%s%s" eopengrok-current-search
                      eopengrok-search-text))
      (insert (format "\nDirectory: %s\n"
                      (s-chop-suffix eopengrok-configuration
                                     eopengrok-current-configuration))))))

(defun eopengrok-init (text configuration kind)
  "Initialize function from TEXT & CONFIGURATION & KIND."
  (setq eopengrok-search-text text)
  (setq eopengrok-last-filename nil)
  (setq eopengrok-current-search kind)
  (setq eopengrok-current-configuration configuration)
  (eopengrok-display-info))

(defun eopengrok-quick-search (options)
  "Search from exist process with OPTIONS."
  (process-send-string (get-process "eopengrok") options))

(defmacro eopengrok-define-find (sym option)
  "Make function with SYM and OPTION."
  (let ((fun (intern (format "eopengrok-find-%s" sym)))
        (doc (format "Find option %s" option))
        (str (format "Find %s: " sym)))
    `(progn
       (defun ,fun (text) ,doc
              (interactive (list (read-string ,str (current-word))))
              (let ((conf (eopengrok-get-configuration))
                    (proc (get-process "eopengrok")))
                (when (and proc
                           (not (equal eopengrok-current-configuration conf)))
                  (kill-process proc)
                  (sleep-for 0.1))
                (if (get-process "eopengrok")
                    (progn
                      (eopengrok-init text conf ,str)
                      (eopengrok-quick-search (eopengrok-quick-search-option
                                               conf ,option text)))
                  (let ((proc (apply 'start-process
                                     "eopengrok"
                                     eopengrok-buffer
                                     "java"
                                     "-jar" eopengrok-jar
                                     (eopengrok-search-option-list
                                      conf ,option text))))
                    (eopengrok-init text conf ,str)
                    (set-process-filter proc 'eopengrok-process-filter)
                    (set-process-sentinel proc 'eopengrok-process-sentinel))))
              (with-current-buffer eopengrok-buffer
                (eopengrok-mode t)
                (setq truncate-lines t)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (pop-to-buffer eopengrok-buffer)
                (goto-char (point-max)))))))

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

(defun eopengrok-make-index (dir &optional enable-projects-p)
  "Make an Index file in DIR, ENABLE-PROJECTS-P is flag for enable projects.
If not nil every directory in DIR is considered a separate project."
  (interactive "DRoot directory: ")
  (let ((proc (apply 'start-process
                     "eopengrok-indexer"
                     eopengrok-indexing-buffer
                     "java"
                     (eopengrok-index-option-list (expand-file-name dir)
                                                  enable-projects-p))))
    (set-process-sentinel proc 'eopengrok-index-process-sentinel)
    (with-current-buffer eopengrok-indexing-buffer
      (setq buffer-read-only t)
      (setq eopengrok-start-time (current-time))
      (goto-char (point-max))
      (pop-to-buffer eopengrok-indexing-buffer))))

(defun eopengrok-make-index-with-enable-projects (dir)
  "Make an Index file, every directory in DIR is considered a separate project."
  (interactive "DRoot directory (enable projects): ")
  (eopengrok-make-index dir t))

(defun eopengrok-startup-asserts ()
  "Check the requirements."
  (when (or (not eopengrok-jar)
            (not eopengrok-ctags))
    (display-warning 'eopengrok (format "\
eopengrok requires clj-opengrok.jar and ctags path.

You can add these lines to your init file.
(setq eopengrok-jar   \"/path/to/clj-opengrok-0.3.0-standalone.jar\")
(setq eopengrok-ctags \"/path/to/ctags\")

See the README for more info:
https://github.com/youngker/eopengrok.el\n") :error)))

(defvar eopengrok-mode-map nil
  "Keymap for eopengrok minor mode.")

(unless eopengrok-mode-map
  (setq eopengrok-mode-map (make-sparse-keymap)))

(--each '(("\C-c\C-csi" . eopengrok-make-index)
          ("\C-c\C-csI" . eopengrok-make-index-with-enable-projects)
          ("\C-c\C-csd" . eopengrok-find-definition)
          ("\C-c\C-csf" . eopengrok-find-file)
          ("\C-c\C-css" . eopengrok-find-reference)
          ("\C-c\C-cst" . eopengrok-find-text)
          ("\C-c\C-csh" . eopengrok-find-history)
          ("\C-c\C-csb" . eopengrok-resume)
          ("n"          . eopengrok-next-line)
          ("p"          . eopengrok-previous-line)
          ("f"          . eopengrok-next-page)
          ("b"          . eopengrok-previous-page)
          ("g"          . eopengrok-goto-page)
          ("q"          . eopengrok-kill-process)
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
