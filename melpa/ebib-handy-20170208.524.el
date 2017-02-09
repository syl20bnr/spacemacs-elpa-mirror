;;; ebib-handy.el --- ebib window as a cite chooser when write org file

;; * Header
;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ebib-handy
;; Package-Version: 20170208.524
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (ebib "1.0") (chinese-pyim "0.1"))

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * README                                                             :README:
;; ** Introduce

;; This package cannot work again, please don't install it .....

;; ebib-handy is a ebib tool, which can let ebib become a cite chooser.
;; [[./snapshots/ebib-handy.gif]]

;; ** Install
;; 1. Config melpa: http://melpa.org/#/getting-started
;; 2. M-x package-install RET ebib-handy RET

;; ** Configure
;; #+BEGIN_EXAMPLE
;; (require 'ebib-handy)
;; (ebib-handy-enable)

;; (setq ebib-extra-fields
;;       '((BibTeX "keywords" "abstract" "timestamp"
;;                 "file"  "url" "crossref" "annote" "doi")
;;         (biblatex "keywords" "abstract" "timestamp"
;;                   "file"  "url" "crossref" "annote" "doi")))
;; #+END_EXAMPLE

;; ** Usage
;; #+BEGIN_EXAMPLE
;; (global-set-key "\C-c b" 'ebib-handy)
;; #+END_EXAMPLE

;; You can open "example/thesis.org" then type 'C-c b'.

;;; Code:
;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'ebib)
(require 'bibtex)
(require 'reftex)
(require 'org)

(defgroup ebib-handy nil
  "Ebib window as a cite chooser when write org file"
  :group 'ebib)

(defcustom ebib-handy-width 80
  "Set ebib-handy window's width."
  :group 'ebib-handy
  :type 'numeric)

(defcustom ebib-handy-index-window-size 14
  "Set ebib-handy index window's size."
  :group 'ebib-handy
  :type 'numeric)

(defcustom ebib-handy-bibtex-fill-column 80
  "Default column when wash bib file by ebib-handy."
  :group 'ebib-handy
  :type 'numeric)

(defcustom ebib-handy-org-cite-link-style " [[cite:%key][(%author %year)]]"
  "Org cite link format used by ebib-handy. %key, %author and %year will
be replaced with according value."
  :group 'ebib-handy
  :type 'string)

(defface ebib-handy-display-default-face
  '((t (:inherit default :family "Liberation Serif")))
  "Face to display ebib key string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-key1-face
  '((t (:inherit default
                 :height 100
                 :box t
                 :bold t)))
  "Face to display ebib key string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-key2-face
  '((t (:inherit default :height 30)))
  "Face to display ebib key string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-key3-face
  '((t (:inherit ,ebib-handy-display-key1-face :box nil)))
  "Face to display ebib key string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-separator-face
  '((t (:inherit ,ebib-handy-display-default-face)))
  "Face to display separator string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-author-face
  '((t (:inherit ,ebib-handy-display-default-face
                 :foreground "green")))
  "Face to display author string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-title-face
  '((t (:inherit ,ebib-handy-display-default-face)))
  "Face to display title string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-publisher-face
  '((t (:inherit ,ebib-handy-display-default-face
                 :foreground "blue")))
  "Face to display publisher string, when run ebib-handy."
  :group 'ebib-handy)

(defface ebib-handy-display-year-face
  '((t (:inherit ,ebib-handy-display-default-face
                 :foreground "orange"
                 :italic t)))
  "Face to display year string, when run ebib-handy."
  :group 'ebib-handy)

(defvar ebib-handy-recently-opened-bibfile nil
  "Record the last bibfile name opened by ebib-handy.")

(defvar ebib-handy-the-last-entry-key ""
  "Record the last citation string.")

(defun ebib-handy-view-and-edit-abstract ()
  (interactive)
  (ebib-handy-view-and-edit-field "abstract"))

(defun ebib-handy-view-and-edit-note ()
  (interactive)
  (ebib-handy-view-and-edit-field "annote"))

(defun ebib-handy-view-and-edit-field (field)
  "View and edit abstract quickly."
  (interactive)
  (ebib--execute-when
    ((entries)
     (ebib--edit-entry-internal)
     (let ((text (ebib-db-get-field-value
                  field
                  (ebib--cur-entry-key)
                  ebib--cur-db 'noerror)))
       (if (ebib-db-unbraced-p text) ; unbraced fields cannot be multiline
           (beep)
         (ebib--multiline-edit
          (list 'field (ebib-db-get-filename ebib--cur-db)
                (ebib--cur-entry-key) field)
          (ebib-handy--wash-text
           (or (ebib-db-unbrace text) "") fill-column t))
         (setq header-line-format
               (format "* View %s buffer. Edit `e', Save `S', Quit `q'. " field))
         (view-mode 1)
         (ebib-handy--view-mode-map-config))))
    ((default)
     (beep))))

(defun ebib-handy--wash-text (text &optional fill-width indent)
  "Insert text into a temp buffer and wash it,
if `fill-width' is a number, the temp buffer will be filled to the number,
if `indent' is a number ,the temp buffer will be indent the number,
then the formated buffer will be exported with `buffer-string',
this function  derived from `article-strip-multiple-blank-lines' in
`gnus-art.el'."
  (interactive)
  (with-temp-buffer
    (goto-char (point-min))
    (insert text)
    ;; Make all blank lines empty.
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]	\t]+$" nil t)
      (replace-match "" nil t))

    ;; Replace multiple empty lines with a single empty line.
    (goto-char (point-min))
    (while (re-search-forward "^\n\\(\n+\\)" nil t)
      (delete-region (match-beginning 1) (match-end 1)))

    ;; Remove a leading blank line.
    (goto-char (point-min))
    (if (looking-at "\n")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; Remove a trailing blank line.
    (goto-char (point-max))
    (if (looking-at "\n")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; remove "{"
    (goto-char (point-min))
    (if (looking-at "^[[:space:]	\t]*{")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; remove "}"
    (goto-char (point-max))
    (if (looking-at "}^[[:space:]	\t]*")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; fill buffer
    (when fill-width
      ;; unindent the buffer
      (indent-region (point-min) (point-max) 0)
      ;; unfill the buffer
      (let ((fill-column 100000))
        (fill-region (point-min) (point-max)))
      ;; fill the buffer to fill-width
      (let ((fill-column fill-width))
        (fill-region (point-min) (point-max))))

    ;;indent buffer
    (when indent
      (indent-region (point-min) (point-max) indent))
    (buffer-string)))

(defun ebib-handy--view-mode-map-config ()
  (use-local-map
   (let ((map view-mode-map))
     (define-key map "C" 'ebib-handy-cancel-multiline-buffer)
     (define-key map "c" 'ebib-handy-cancel-multiline-buffer)
     (define-key map "Q" 'ebib-handy-cancel-multiline-buffer)
     (define-key map "E" 'ebib-handy-edit-multiline-buffer)
     (define-key map "e" 'ebib-handy-edit-multiline-buffer)
     (define-key map "q" 'ebib-handy-cancel-multiline-buffer)
     (define-key map "S" 'ebib-save-from-multiline-buffer)
     map)))

(defun ebib-handy--multiline-mode-map-config ()
  (use-local-map
   (let ((map ebib-multiline-mode-map))
     (define-key map "\C-c\C-c" 'ebib-handy-quit-multiline-buffer-and-save)
     (define-key map "\C-c\C-k" 'ebib-handy-cancel-multiline-buffer)
     map)))

(defun ebib-handy-edit-multiline-buffer ()
  (interactive)
  (ebib-multiline-mode 1)
  (setq header-line-format
        "* Edit buffer. Save+Quit `C-c C-c', Abort `C-c C-k' ")
  (ebib-handy--multiline-mode-map-config))

(defun ebib-handy-quit-multiline-buffer-and-save ()
  (interactive)
  (ebib-quit-multiline-buffer-and-save)
  (ebib-quit-entry-buffer))

(defun ebib-handy-cancel-multiline-buffer ()
  (interactive)
  (ebib-cancel-multiline-buffer)
  (ebib-quit-entry-buffer))

(defun ebib-handy--split-key (key)
  (split-string
   (replace-regexp-in-string
    "\\(.*[0-9]\\)\\([a-z].*\\)" "\\1@\\2" key)
   "@"))

(defun ebib-handy--remove-newlines (string)
  (replace-regexp-in-string "\n+" " " string))

;; ebib index buffer format setting
(defun ebib-handy--display-entry-key (entry-key)
  "Display entry-key, title, journal, publisher and school in the index buffer at POINT. "
  (with-current-ebib-buffer
    'index
    (with-ebib-buffer-writable
     (setq cursor-type t)
     (insert (concat
              ;; entry key
              (let* ((list (ebib-handy--split-key entry-key))
                     (str1 (car list))
                     (str2 (car (cdr list))))
                (concat
                 (propertize str1 'face 'ebib-handy-display-key1-face)
                 (propertize (format "%-20s" (or str2 ""))
                             'face 'ebib-handy-display-key2-face)
                 (propertize (make-string (max (- 20 (length str1)) 0) ? )
                             'face 'ebib-handy-display-key3-face)))
              ;; author
              (propertize
               (car (split-string
                     (or (ebib-db-get-field-value 'author entry-key ebib--cur-db 'noerror 'unbraced)
                         "  ") "[ \t\n]+and[ \t\n]+" ))
               'face 'ebib-handy-display-author-face)
              ;; separator
              (propertize ". " 'face 'ebib-handy-display-separator-face)
              ;; year
              (propertize
               (or (ebib-db-get-field-value 'year entry-key ebib--cur-db 'noerror 'unbraced 'xref) "20??")
               'face 'ebib-handy-display-year-face)
              ;; separator
              (propertize ". " 'face 'ebib-handy-display-separator-face)
              ;; title
              (propertize
               (ebib-handy--remove-newlines
                (or (ebib-db-get-field-value 'title entry-key ebib--cur-db 'noerror 'unbraced) ""))
               'face 'ebib-handy-display-title-face)
              ;; separator
              (propertize ". " 'face 'ebib-handy-display-separator-face)
              ;; journal publisher or school
              (propertize
               (ebib-handy--remove-newlines
                (or (ebib-db-get-field-value 'journal entry-key ebib--cur-db 'noerror 'unbraced)
                    (ebib-db-get-field-value 'publisher entry-key ebib--cur-db 'noerror 'unbraced)
                    (ebib-db-get-field-value 'school entry-key ebib--cur-db 'noerror 'unbraced)
                    ""))
               'face 'ebib-handy-display-publisher-face)
              "\n")))))

(defun ebib-handy-get-matched-files (files-list match-str)
  (let ((match-string (replace-regexp-in-string "[ +-=_]+" "" match-str)))
    (if (string= match-string "")
        ""
      (cl-delete-if
       #'(lambda (str)
           (let ((case-fold-search t)
                 (string (replace-regexp-in-string "[ +-=_]+" "" str)))
             (not (or (string-match-p match-string string)
                      (when (featurep 'chinese-pyim)
                        (string-match-p match-string (pyim-hanzi2pinyin string)))))))
       files-list))))

(defun ebib-handy-directory-files-recursively (&optional directory regexp)
  "recursively list all the files in a directory"
  (let* ((directory (or directory default-directory))
         (regexp (or regexp ".*"))
         (files (cl-delete-if
                 #'(lambda (s)
                     (string-match (rx bol (repeat 1 2 ".") eol)
                                   (file-name-nondirectory s)))
                 (directory-files directory t nil t))))
    (cl-loop for file in files
             when (string-match regexp (file-name-nondirectory file))
             collect file into ret
             when (file-directory-p file)
             nconc (ebib-handy-directory-files-recursively file regexp) into ret
             finally return ret)))

(defun ebib-handy-view-file ()
  (interactive)
  (ebib--execute-when
    ((entries)
     (let* ((key (ebib--cur-entry-key))
            (db ebib--cur-db)
            (name-string
             (car (split-string
                   (or (ebib-db-get-field-value 'author key db 'noerror 'unbraced 'xref)
                       "  ") "[ \t\n]+and[ \t\n]+\\|," )))
            (all-files (ebib-handy-directory-files-recursively
                        (file-name-directory (ebib-db-get-filename db))))
            (files-matched (ebib-handy-get-matched-files all-files name-string)))
       (cond ((> (length files-matched) 1)
              (let ((file (completing-read "Open file:" files-matched)))
                ;; (ebib-db-set-field-value 'file (file-relative-name file (expand-file-name "."))
                ;;                          key db 'overwrite)
                ;; (ebib-save-database db)
                (start-process "" nil "xdg-open" file)))
             ((= (length files-matched) 1)
              (let ((file (car files-matched)))
                (message "Opening file: %s" file)
                ;; (ebib-db-set-field-value 'file (file-relative-name file (expand-file-name "."))
                ;;                          key db 'overwrite)
                ;; (ebib-save-database db)
                (start-process "" nil "xdg-open" file)))
             ((< (length files-matched) 1)
              (message "Can't find the corresponding file")))))
    ((default)
     (beep))))

(defun ebib-handy-insert-bibfile-info ()
  (interactive)
  (let* ((bibfiles-list (ebib-handy-directory-files-recursively "." ".bib$"))
         (file (or (when (and ebib-handy-recently-opened-bibfile
                              (y-or-n-p (format "Insert recently opened bibfile (%s)?  "
                                                ebib-handy-recently-opened-bibfile)))
                     ebib-handy-recently-opened-bibfile)
                   (when bibfiles-list
                     (completing-read "Insert bibfile:" bibfiles-list))
                   (read-file-name "Insert bibfile:" (car ebib-file-search-dirs)))))
    (insert (format "# \\bibliography{%s}\n"
                    (file-relative-name file (expand-file-name "."))))))

(defun ebib-handy (&optional key)
  "Open ebib then search the marked string"
  (interactive)
  (let* ((ebib-layout 'full)
         (ebib-window-vertical-split nil)
         (ebib-width ebib-handy-width)
         (ebib-index-window-size ebib-handy-index-window-size)
         ;; allow the same keys
         (ebib-uniquify-keys nil)
         ;; If this varible is `t', index buffer will
         ;; highlight lines instead of autokey word
         ;; setting this varible to *a list of field*
         ;; is *useless* in my configure
         (ebib-index-display-fields t)
         (bibfiles-list (ebib-handy-directory-files-recursively "." ".bib$"))
         (file (or (when (buffer-file-name)
                     (car (ignore-errors (reftex-get-bibfile-list))))
                   (when (and ebib-handy-recently-opened-bibfile
                              (y-or-n-p (format "Load recently opened bibfile (%s)?  "
                                                ebib-handy-recently-opened-bibfile)))
                     ebib-handy-recently-opened-bibfile)
                   (when bibfiles-list
                     (completing-read "Open bibfile:" bibfiles-list))
                   (read-file-name "Open bibfile:" (car ebib-file-search-dirs))))
         (line-content (buffer-substring
                        (max (line-beginning-position)
                             (- (point) 40))
                        (point))))
    (setq ebib-handy-push-buffer (current-buffer))
    (ebib file (or key ebib-handy-the-last-entry-key))
    (setq ebib-handy-recently-opened-bibfile file)
    (setq header-line-format (format "Referenced Words: %s -A-" line-content))))

(defun ebib-handy-push-bibtex-key (&optional leave-ebib-window)
  (interactive)
  (let ((buffer-mode (buffer-local-value 'major-mode (get-buffer ebib-handy-push-buffer))))
    (cond ((string= buffer-mode "org-mode")
           (ebib-handy-push-org-cite-link leave-ebib-window))
          (t (ebib-push-bibtex-key)
             (when leave-ebib-window
               (ebib-db-unmark-entry 'all ebib--cur-db)
               (ebib--fill-index-buffer)
               (setq ebib-handy-push-buffer nil)
               (ebib-leave-ebib-windows))))))

(defun ebib-handy--format-org-cite-link (key)
  (let ((author (car (split-string
                      (or (ebib-db-get-field-value 'author key ebib--cur-db 'noerror 'unbraced 'xref)
                          "  ") "[ \t\n]+and[ \t\n]+\\|," )))
        (year (or (ebib-db-get-field-value 'year key ebib--cur-db 'noerror 'unbraced 'xref) "20??")))
    (replace-regexp-in-string
     "%year" year
     (replace-regexp-in-string
      "%author" author
      (replace-regexp-in-string
       "%key" key
       ebib-handy-org-cite-link-style)))))

(defun ebib-handy-push-org-cite-link (&optional leave-ebib-window)
  "Pushes the cite link of current entry to a org-mode buffer."
  (interactive)
  (ebib--execute-when
    ((entries)
     (let* ((key (ebib--cur-entry-key))
            (citation-string
             (if (ebib-db-marked-entries-p ebib--cur-db)
                 (mapconcat #'ebib-handy--format-org-cite-link (ebib-db-list-marked-entries ebib--cur-db) " ")
               (ebib-handy--format-org-cite-link key))))
       ;; 将 citation-string 插入到 ebib-handy-push-buffer 变量所
       ;; 对应的 buffer, (调用ebib-handy命令时,会设置ebib-handy-push-buffer变量)
       (when citation-string
         (with-current-buffer ebib-handy-push-buffer
           ;; (let* ((point1 (or (save-excursion (search-forward "[[" nil t)) (+ 1 (point-max))))
           ;;        (point2 (save-excursion (search-forward "]]" nil t)))
           ;;        (point3 (save-excursion (search-backward "[[" nil t)))
           ;;        (point4 (or (save-excursion (search-backward "]]" nil t)) -1)))
           ;;   (when (and point2 point3 (> point1 point2) (> point3 point4))
           ;;     (search-forward "]]" nil t)))
           (insert citation-string)
           (message "Pushed \"%s\" to buffer: \"%s\"" citation-string ebib-handy-push-buffer))
         (setq ebib-handy-the-last-entry-key (ebib--cur-entry-key))
         ;; 隐藏ebib窗口
         (when leave-ebib-window
           (ebib-db-unmark-entry 'all ebib--cur-db)
           (ebib--fill-index-buffer)
           (setq ebib-handy-push-buffer nil)
           (ebib-leave-ebib-windows)))))
    ((default)
     (beep))))

(defun ebib-handy-reformat-all-entries ()
  "1. Add language field to all entries.
   2. Add alias field to all entries.
   3. reformat all the entries"
  (interactive)
  (let* ((current-bib-file (ebib-db-get-filename ebib--cur-db)))
    (ebib--execute-when
      ((entries)
       (when (yes-or-no-p (concat (format "Reformat bibfile: %s  " current-bib-file)))
         (ebib-save-current-database t)
         (message "Reformat ... ")
         (with-current-buffer (find-file-noselect current-bib-file)
           (goto-char (point-min))
           (ebib-handy-bibtex-reformat)
           (save-buffer)
           (kill-buffer))
         ;; reload the current database
         (ebib--reload-database ebib--cur-db)
         (ebib--set-modified nil)
         (ebib--redisplay)
         (message "Reformat complete")))
      ((default)
       (beep)))))

(defun ebib-handy--bibtex-wash-field (field)
  "Wash the content of field"
  (goto-char begin)
  (let ((field-content (bibtex-autokey-get-field field))
        (field-position (bibtex-search-forward-field field t)))
    (when field-position
      (goto-char (car (cdr field-position)))
      (bibtex-kill-field))
    (bibtex-make-field
     (list field nil
           (ebib-handy--wash-text
            field-content
            ebib-handy-bibtex-fill-column
            (+ bibtex-text-indentation 1 )) nil) t)))

(defun ebib-handy-bibtex-reformat ()
  (interactive)
  (goto-char (point-min))
  ;; Clean elide blank lines of entries,
  ;; which make abstrack field look beautiful
  (save-restriction
    (bibtex-map-entries
     (lambda (key begin end)
       (let ((case-fold-search t)
             (entry-type (bibtex-type-in-head)))
         (save-excursion
           ;; Add language field
           (goto-char begin)
           (let ((language-field (bibtex-search-forward-field "language" t)))
             (when language-field
               (goto-char (car (cdr language-field)))
               (bibtex-kill-field)))
           (when (string-match-p "\\cc+" (bibtex-autokey-get-field "title"))
             (bibtex-make-field '("language" nil "Chinese" nil) t))

           ;; Add alias field
           (when (featurep 'chinese-pyim)
             (goto-char begin)
             (let ((alias-field (bibtex-search-forward-field "alias" t))
                   (title (bibtex-autokey-get-field "title"))
                   (author (bibtex-autokey-get-field "author")))
               (when alias-field
                 (goto-char (car (cdr alias-field)))
                 (bibtex-kill-field))
               (bibtex-make-field
                (list "alias" nil
                      (replace-regexp-in-string
                       "\n" ""
                       (pyim-hanzi2pinyin-simple
                        (concat author ", " title) t)) nil) t)))

           ;; Wash abstract field
           (ebib-handy--bibtex-wash-field "abstract")

           ;; Add autokey
           (goto-char begin)
           (let (auto-key)
             (re-search-forward (if (bibtex-string= entry-type "string")
                                    bbibtex-string-maybe-empty-head
                                  bibtex-entry-maybe-empty-head))
             (if (match-beginning bibtex-key-in-head)
                 (delete-region (match-beginning bibtex-key-in-head)
                                (match-end bibtex-key-in-head)))
             (setq auto-key (bibtex-generate-autokey))

             ;; Sometimes `bibtex-generate-autokey' returns an empty string
             (if (string= "" auto-key)
                 (setq auto-key "!NEED_EDIT"))
             (insert auto-key)
             (let ((bibtex-entry-format
                    ;; Don't add `realign' to this list
                    '(opts-or-alts  delimiters
                                    last-comma page-dashes unify-case inherit-booktitle
                                    braces strings sort-fields whitespace
                                    ;; numerical-fields
                                    )))
               (bibtex-clean-entry nil t))

             ;; Add keyhistory field
             (let ((keyhistory-field (bibtex-search-forward-field "keyhistory" t))
                   (keyhistory (bibtex-autokey-get-field "keyhistory")))
               (when keyhistory-field
                 (goto-char (car (cdr keyhistory-field)))
                 (bibtex-kill-field))
               (bibtex-make-field
                (list "keyhistory" nil
                      (mapconcat #'identity
                                 (delq "" (delete-dups
                                           (cons auto-key (split-string (or keyhistory "") "; "))))
                                 "; ") nil) t)))))))))

(defun ebib-handy--bibtex-chinese-autokey-setup ()
  "Bibtex 中文文献 autokey 生成规则：

    <第一作者拼音><年份><标题前两个汉字字符拼音>

比如：[3] 徐琳玲. P公司标准成本制度研究[D]. 华东理工大学, 2013.
将生成：xulinling2013pgong

注：bibtex开启了词法绑定。"
  (setq bibtex-autokey-names 1
        bibtex-autokey-name-separator ""
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titlewords 2
        bibtex-autokey-titleword-length 2
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titleword-ignore nil
        bibtex-autokey-before-presentation-function
        #'(lambda (x)
            (downcase (pyim-hanzi2pinyin-simple x)))))

(defun ebib-handy--bibtex-english-autokey-setup ()
  "Bibtex 英文文献 autokey 生成规则。"
  (setq bibtex-autokey-names 1
        bibtex-autokey-name-separator ""
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titlewords 3
        bibtex-autokey-titleword-length 5
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
          "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")))

(defun ebib-handy--bibtex-autokey-get-title (orig-fun &rest args)
  (let ((case-fold-search t)
        (titlestring (bibtex-autokey-get-field "title")))
    (if (string-match-p "\\cc" titlestring)
        (ebib-handy--bibtex-chinese-autokey-setup)
      (ebib-handy--bibtex-english-autokey-setup))
    (apply orig-fun args)))

(defun ebib-handy-enable ()
  (interactive)
  (advice-add 'ebib--display-entry-key :override #'ebib-handy--display-entry-key)
  (advice-add 'bibtex-autokey-get-title :around #'ebib-handy--bibtex-autokey-get-title)
  ;; org cite link setting
  (org-add-link-type "cite" 'ebib-handy)
  (ebib-key index "\C-xb"
            (lambda ()
              (interactive)
              (ebib-leave-ebib-windows)
              (ibuffer)))
  (ebib-key index "\C-c\C-c"
            (lambda ()
              (interactive)
              (ebib-handy-push-bibtex-key t)))
  (ebib-key index "\C-xb" ebib-handy)
  (ebib-key index "\C-xk" ebib-leave-ebib-windows)
  (ebib-key index "\C-xq" ebib-force-quit)
  (ebib-key index "v" ebib-handy-view-and-edit-abstract)
  (ebib-key index "N" ebib-handy-view-and-edit-note)
  (ebib-key index "p" ebib-handy-push-bibtex-key)
  (ebib-key index "q" ebib-force-quit)
  (ebib-key index "f" ebib-handy-view-file)
  (ebib-key index "R" ebib-handy-reformat-all-entries)
  (ebib-key index [(return)] ebib-select-and-popup-entry))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'ebib-handy)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ebib-handy.el ends here
;; #+END_SRC
