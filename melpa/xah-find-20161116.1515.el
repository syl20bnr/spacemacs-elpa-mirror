;;; xah-find.el --- find replace in pure emacs lisp. Purpose similar to unix grep/sed.

;; Copyright © 2012-2015 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 3.0.1
;; Package-Version: 20161116.1515
;; Created: 02 April 2012
;; Keywords: convenience, extensions, files, tools, unix
;; Homepage: http://ergoemacs.org/emacs/elisp-xah-find-text.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Provides emacs commands for find/replace text of files in a directory, written entirely in emacs lisp.

;; This package provides these commands:

;; xah-find-text
;; xah-find-text-regex
;; xah-find-count
;; xah-find-replace-text
;; xah-find-replace-text-regex

;; • Pure emacs lisp. No dependencies on unix/linux grep/sed/find. Especially useful on Windows.

;; • Output is highlighted and clickable for jumping to occurrence.

;; • Using emacs regex, not bash/perl etc regex.

;; These commands treats find/replace string as sequence of chars, not as lines as in grep/sed, so it's easier to find or replace a block of text, especially programming language source code.

;; • Reliably Find/Replace string that contains newline chars.

;; • Reliably Find/Replace string that contains lots Unicode chars. See http://xahlee.info/comp/unix_uniq_unicode_bug.html and http://ergoemacs.org/emacs/emacs_grep_problem.html

;; • Reliably Find/Replace string that contains lots escape slashes or backslashes. For example, regex in source code, Microsoft Windows' path.

;; The result output is also not based on lines. Instead, visual separators are used for easy reading.

;; For each occurrence or replacement, n chars will be printed before and after. The number of chars to show is defined by `xah-find-context-char-count-before' and `xah-find-context-char-count-after'

;; Each “block of text” in output is one occurrence.
;; For example, if a line in a file has 2 occurrences, then the same line will be reported twice, as 2 “blocks”.
;; so, the number of blocks corresponds exactly to the number of occurrences.

;; Keys
;; -----------------------
;; TAB             xah-find-next-match
;; <backtab>       xah-find-previous-match

;; RET             xah-find--jump-to-place
;; <mouse-1>       xah-find--mouse-jump-to-place

;; <left>          xah-find-previous-match
;; <right>         xah-find-next-match

;; <down>          xah-find-next-file
;; <up>            xah-find-previous-file

;; M-n             xah-find-next-file
;; M-p             xah-find-previous-file

;; IGNORE DIRECTORIES

;; By default, .git dir is ignored. You can add to it by adding the following in your init:

;; (setq
;;  xah-find-dir-ignore-regex-list
;;  [
;;   "\\.git/"
;;    ; more regex here. regex is matched against file full path
;;   ])

;; BACKGROUND COLORS

;; To change background color of result match string, put this in your emacs init:

;; (setq xah-find-file-background-color "blue")
;; (setq xah-find-match-background-color "green")

;; Call list-colors-display to see available colors names or hex syntax.

;; USE CASE

;; To give a idea what file size, number of files, are practical, here's my typical use pattern:
;; • 5 thousand HTML files match file name regex.
;; • Each HTML file size are usually less than 200k bytes.
;; • search string length have been up to 13 lines of text.

;; Homepage: http://ergoemacs.org/emacs/elisp-xah-find-text.html

;; Like it?
;; Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html
;; Thank you.

;;; INSTALL

;; To install manually, place this file in the directory 〔~/.emacs.d/lisp/〕

;; Then, place the following code in your emacs init file

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'xah-find-text "xah-find" "find replace" t)
;; (autoload 'xah-find-text-regex "xah-find" "find replace" t)
;; (autoload 'xah-find-replace-text "xah-find" "find replace" t)
;; (autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
;; (autoload 'xah-find-count "xah-find" "find replace" t)

;;; HISTORY

;; version 2.1.0, 2015-05-30 Complete rewrite.
;; version 1.0, 2012-04-02 First version.

;;; CONTRIBUTOR
;; 2015-12-09 Peter Buckley (dx-pbuckley). defcustom for result highlight color.

;;; TODO:
;; 2015-05-20 the feeble find-lisp-find-files is becoming a efficiency pain. It uses one regex to list all files, then you have to filter dir. There doesn't seem to be alternative except roll one's own or use third-party package


;;; Code:

(require 'find-lisp) ; in emacs
(require 'ido)       ; in emacs
(ido-common-initialization) ; 2015-07-26 else, when ido-read-directory-name is called, Return key insert line return instead of submit. For some reason i dunno.

(defcustom
  xah-find-context-char-count-before
  100
  "Number of characters to print before search string."
  :group 'xah-find
  )

(defcustom xah-find-context-char-count-after
  50
  "Number of characters to print after search string."
  :group 'xah-find
  )

(defcustom xah-find-dir-ignore-regex-list
  [
   "\\.git/"
   ]
  "A list or vector of regex patterns, if match, that directory will be ignored. Case is dependent on current value of `case-fold-search'"
  :group 'xah-find
  )

(defcustom xah-find-file-separator
  "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'xah-find )

(defcustom
  xah-find-occur-separator
  "────────────────────────────────────────────────────────────\n"
  "A string as visual separator."
  :group 'xah-find )

(defcustom
  xah-find-file-background-color
  "pink"
  "Background color for files where a text match is found."
  :group 'xah-find )

(defcustom
  xah-find-match-background-color
  "yellow"
  "Background color for text when a match is found."
  :group 'xah-find )

(defcustom xah-find-occur-prefix
"「"
  "A left-bracket string that marks matched text and navigate previous/next."
  :group 'xah-find
  )

(defcustom xah-find-occur-postfix
  "」"
  "A right-bracket string that marks matched text and navigate previous/next."
  :group 'xah-find
  )

;; brackets 「 」 〈 〉 《 》 【 】 〔 〕 ⦗ ⦘ 『 』 〖 〗 〘 〙
;; more at
;; http://xahlee.info/comp/unicode_matching_brackets.html

(defcustom xah-find-filepath-prefix
"«"
  "A left-bracket string used to mark file path and navigate previous/next."
  :group 'xah-find
  )

(defcustom xah-find-filepath-postfix
  "»"
  "A right-bracket string used to mark file path and navigate previous/next."
  :group 'xah-find
  )



(defvar xah-find-file-path-regex-history '() "File path regex history list, used by `xah-find-text' and others.")

(defun xah-find--filter-list (*predicate *sequence)
  "Return a new list such that *PREDICATE is true on all members of *SEQUENCE.

URL `http://ergoemacs.org/emacs/elisp_filter_list.html'
Version 2015-05-23"
  (delete
   "e3824ad41f2ec1ed"
   (mapcar
    (lambda (-x)
      (if (funcall *predicate -x)
          -x
        "e3824ad41f2ec1ed" ))
    *sequence)))

(defun xah-find--ignore-dir-p (*path )
  "Return true if one of `xah-find-dir-ignore-regex-list' matches *PATH. Else, nil.
2016-11-16"
  (catch 'exit25001
    (mapc
     (lambda (-regex)
       (when (string-match -regex *path) (throw 'exit25001 -regex)))
     xah-find-dir-ignore-regex-list)
    nil
    ))


(defvar xah-find-output-mode-map nil "Keybinding for `xah-find.el output'")
(progn
  (setq xah-find-output-mode-map (make-sparse-keymap))

  (define-key xah-find-output-mode-map (kbd "<left>") 'xah-find-previous-match)
  (define-key xah-find-output-mode-map (kbd "<right>") 'xah-find-next-match)
  (define-key xah-find-output-mode-map (kbd "<down>") 'xah-find-next-file)
  (define-key xah-find-output-mode-map (kbd "<up>") 'xah-find-previous-file)

  (define-key xah-find-output-mode-map (kbd "TAB") 'xah-find-next-match)
  (define-key xah-find-output-mode-map (kbd "<backtab>") 'xah-find-previous-match)
  (define-key xah-find-output-mode-map (kbd "<mouse-1>") 'xah-find--mouse-jump-to-place)
  (define-key xah-find-output-mode-map (kbd "M-n") 'xah-find-next-file)
  (define-key xah-find-output-mode-map (kbd "M-p") 'xah-find-previous-file)
  (define-key xah-find-output-mode-map (kbd "RET") 'xah-find--jump-to-place)
  )

(define-derived-mode xah-find-output-mode fundamental-mode "ξxah-find"
  "Major mode for reading output for xah-find commands.
home page:
URL `http://ergoemacs.org/emacs/elisp-xah-find-text.html'

\\{xah-find-output-mode-map}"

  (progn
    (when (null buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face)))))

  :group 'xah-find
  )

(defun xah-find-next-match ()
  "Put cursor to next occurrence."
  (interactive)
  (search-forward xah-find-occur-prefix nil "NOERROR" ))

(defun xah-find-previous-match ()
  "Put cursor to previous occurrence."
  (interactive)
  (search-backward xah-find-occur-postfix nil "NOERROR" )
  (left-char) ; todo. this is a hack. move point to inside of text with highlight property, so it's clickable. Look into modify xah-find--jump-to-place instead
  )

(defun xah-find-next-file ()
  "Put cursor to next file."
  (interactive)
  (search-forward xah-find-filepath-prefix nil "NOERROR" ))

(defun xah-find-previous-file ()
  "Put cursor to previous file."
  (interactive)
  (search-backward xah-find-filepath-postfix nil "NOERROR" )
  (left-char) ; todo. this is a hack. move point to inside of text with highlight property, so it's clickable. Look into modify xah-find--jump-to-place instead
  )

(defun xah-find--mouse-jump-to-place (*event)
  "Open file and put cursor at location of the occurrence."
  (interactive "e")
  (let* ((-window (posn-window (event-end *event)))
         (-pos (posn-point (event-end *event)))
         (-fpath (get-text-property -pos 'xah-find-fpath))
         (-pos-jump-to (get-text-property -pos 'xah-find-pos)))
    (when (not (null -fpath))
      (progn
        (find-file-other-window -fpath)
        (when -pos-jump-to (goto-char -pos-jump-to))))))

(defun xah-find--jump-to-place ()
  "Open file and put cursor at location of the occurrence."
  (interactive)
  (let ((-fpath (get-text-property (point) 'xah-find-fpath))
        (-pos-jump-to (get-text-property (point) 'xah-find-pos)))
    (when (not (null -fpath))
      (progn
        (find-file-other-window -fpath)
        (when -pos-jump-to (goto-char -pos-jump-to))))))


(defun xah-find--backup-suffix (*s)
  "Return a string of the form 「~‹*s›~‹date time stamp›~」"
  (concat "~" *s (format-time-string "%Y%m%dT%H%M%S") "~"))

(defun xah-find--current-date-time-string ()
  "Return current date-time string in this format 「2012-04-05T21:08:24-07:00」"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z"))))

(defun xah-find--print-header (*bufferObj *cmd *input-dir *path-regex *search-str &optional *replace-str )
  "Print things"
  (princ
   (concat
    "-*- coding: utf-8 -*-" "\n"
    "Datetime: " (xah-find--current-date-time-string) "\n"
    "Result of: " *cmd "\n"
    (format "Directory 「%s」\n" *input-dir )
    (format "Path regex 「%s」\n" *path-regex )
    (format "Search string 「%s」\n" *search-str )
    (when *replace-str (format "Replace string ❬%s❭\n" *replace-str))
    xah-find-file-separator
    )
   *bufferObj))

;; (defun xah-find--print-occur-block (*p1 *p2 *buff)
;;   "print "
;;   (princ
;;    (concat
;;     (buffer-substring-no-properties (max 1 (- *p1 xah-find-context-char-count-before )) *p1 )
;;     xah-find-occur-prefix
;;     (buffer-substring-no-properties *p1 *p2 )
;;     xah-find-occur-postfix
;;     (buffer-substring-no-properties *p2 (min (point-max) (+ *p2 xah-find-context-char-count-after )))
;;     "\n"
;;     xah-find-occur-separator)
;;    *buff))

(defun xah-find--occur-output (*p1 *p2 *fpath *buff &optional *no-context-string-p *alt-color)
  "print to output, with text properties.
*p1 *p2 are region boundary. Text of current buffer are grabbed.
*fpath is file path to be used as property value for clickable link.
*buff is the buffer to insert *p1 *p2 text.
*no-context-string-p if true, don't add text before and after the region of interest.
*alt-color if true, use a different highlight color."
  (let (
        (-p3 (max 1 (- *p1 xah-find-context-char-count-before )))
        (-p4 (min (point-max) (+ *p2 xah-find-context-char-count-after )))
        -textBefore
        -textMiddle
        -textAfter
        (-color (if *alt-color
                    (list :background "green")
                  (list :background xah-find-match-background-color))))
    (put-text-property *p1 *p2 'face -color)
    (put-text-property *p1 *p2 'xah-find-fpath *fpath)
    (put-text-property *p1 *p2 'xah-find-pos *p1)
    (add-text-properties *p1 *p2 '(mouse-face highlight))

    (setq -textBefore (buffer-substring -p3 *p1 ))
    (setq -textMiddle (buffer-substring *p1 *p2 ))
    (setq -textAfter (buffer-substring *p2 -p4))
    (with-current-buffer *buff
      (if *no-context-string-p
          (insert xah-find-occur-prefix -textMiddle xah-find-occur-postfix "\n" xah-find-occur-separator )
        (insert -textBefore xah-find-occur-prefix -textMiddle xah-find-occur-postfix -textAfter "\n" xah-find-occur-separator )))))

;; (defun xah-find--print-replace-block (*p1 *p2 *buff)
;;   "print "
;;   (princ (concat "❬" (buffer-substring-no-properties *p1 *p2 ) "❭" "\n" xah-find-occur-separator) *buff))

(defun xah-find--print-file-count (*filepath4287 *count8086 *buffObj32)
  "Print file path and count"
  (princ (format "%d %s%s%s\n%s"
                 *count8086
                 xah-find-filepath-prefix
                 *filepath4287
                 xah-find-filepath-postfix
                 xah-find-file-separator)
         *buffObj32))

;; (defun xah-find--highlight-output (*buffer &optional *search-str *replace-str)
;;   "switch to *buffer and highlight stuff"
;;   (let ((-search (concat xah-find-occur-prefix *search-str xah-find-occur-postfix))
;;         (-rep (concat "❬" *replace-str "❭")))
;;     (switch-to-buffer *buffer)
;;     (fundamental-mode)
;;     (progn
;;       (goto-char 1)
;;       (while (search-forward-regexp "❨\\([^❩]+?\\)❩" nil "NOERROR")
;;         (put-text-property
;;          (match-beginning 0)
;;          (match-end 0)
;;          'face (list :background "yellow"))))
;;     (progn
;;       (goto-char 1)
;;       (while (search-forward-regexp "❬\\([^❭]+?\\)❭" nil "NOERROR")
;;         (put-text-property
;;          (match-beginning 0)
;;          (match-end 0)
;;          'face (list :background "green"))))
;;     (progn
;;       (goto-char 1)
;;       (while (search-forward xah-find-filepath-prefix nil "NOERROR")
;;         (put-text-property
;;          (line-beginning-position)
;;          (line-end-position)
;;          'face (list :background "pink"))))
;;     (goto-char 1)
;;     (search-forward-regexp "━+" nil "NOERROR")
;;     (use-local-map xah-find-output-mode-map)))

(defun xah-find--switch-to-output (*buffer)
  "switch to *buffer and highlight stuff"
  (let (-p3 -p4)
    (switch-to-buffer *buffer)
    (progn
      (goto-char 1)
      (while (search-forward xah-find-filepath-prefix nil "NOERROR")
        (setq -p3 (point))
        (search-forward xah-find-filepath-postfix nil "NOERROR")
        (setq -p4 (- (point) (length xah-find-filepath-postfix)))
        (put-text-property -p3 -p4 'xah-find-fpath (buffer-substring-no-properties -p3 -p4))
        (add-text-properties -p3 -p4 '(mouse-face highlight))
        (put-text-property (line-beginning-position) (line-end-position) 'face (list :background xah-find-file-background-color))))

    (goto-char 1)
    (search-forward "━" nil "NOERROR") ; todo, need fix
    (search-forward xah-find-occur-prefix nil "NOERROR")
    (xah-find-output-mode)
    ))



;;;###autoload
(defun xah-find-count (*search-str *count-expr *count-number *input-dir *path-regex)
  "Report how many occurrences of a string, of a given dir.
Similar to `rgrep', but written in pure elisp.
Result is shown in buffer *xah-find output*.
Case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change.
\\{xah-find-output-mode-map}"
  (interactive
   (let ( -operator)
     (list
      (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
      (setq -operator (ido-completing-read "Report on: " '("greater than" "greater or equal to" "equal" "not equal" "less than" "less or equal to" )))
      (read-string (format "Count %s: "  -operator) "0")
      (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "File path regex: " (xah-find--get-default-file-extension-regex "el") nil nil 'dired-regexp-history))))
  (let* ((-outBufName "*xah-find output*")
         -outBufObj
         (-countOperator
          (cond
           ((string-equal "less than" *count-expr ) '<)
           ((string-equal "less or equal to" *count-expr ) '<=)
           ((string-equal "greater than" *count-expr ) '>)
           ((string-equal "greater or equal to" *count-expr ) '>=)
           ((string-equal "equal" *count-expr ) '=)
           ((string-equal "not equal" *count-expr ) '/=)
           (t (error "count expression 「%s」 is wrong!" *count-expr ))))
         (-countNumber (string-to-number *count-number)))
    (when (get-buffer -outBufName) (kill-buffer -outBufName))
    (setq -outBufObj (generate-new-buffer -outBufName))
    (xah-find--print-header -outBufObj "xah-find-count" *input-dir *path-regex *search-str )
    (mapc
     (lambda (-f)
       (let ((-count 0))
         (with-temp-buffer
           (insert-file-contents -f)
           (goto-char 1)
           (while (search-forward *search-str nil "NOERROR") (setq -count (1+ -count)))
           (when (funcall -countOperator -count -countNumber)
             (xah-find--print-file-count -f -count -outBufObj)))))
     (xah-find--filter-list (lambda (x) (not (xah-find--ignore-dir-p x))) (find-lisp-find-files *input-dir *path-regex)))
    (xah-find--switch-to-output -outBufObj)))

(defun xah-find--get-default-file-extension-regex (&optional *default-ext)
  "Returns a string, that is a regex to match a file extension.
The result is based on current buffer's file extension.
If current file doesn't have extension or current buffer isn't a file, then extension *default-ext is used.
*default-ext should be a string, without dot, such as 「\"el\"」.
If *default-ext is nil, 「\"el\"」 is used.
Example return value: 「ββ.htmlββ'」, where β is a backslash.
"
  (let (
        (-buff-is-file-p (buffer-file-name))
        -fname-ext
        -default-ext
        )
    (setq -default-ext (if (null *default-ext)
                           (progn "el")
                         (progn *default-ext)))
    (if -buff-is-file-p
        (progn
          (setq -fname-ext (file-name-extension (buffer-file-name)))
          (if (or (null -fname-ext) (equal -fname-ext ""))
              (progn (concat "\\." -default-ext "\\'"))
            (progn (concat "\\." -fname-ext "\\'"))))
      (progn (concat "\\." -default-ext "\\'")))))

;;;###autoload
(defun xah-find-text (*search-str1 *input-dir *path-regex *fixed-case-search-p *printContext-p)
  "Report files that contain string.
By default, not case sensitive, and print surrounding text.
If `universal-argument' is called first, prompt to ask.
Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}"
  (interactive
   (let ((-default-input (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) (current-word))))
     (list
      (read-string (format "Search string (default %s): " -default-input) nil 'query-replace-history -default-input)
      (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "File path regex: " (xah-find--get-default-file-extension-regex "el") nil nil 'dired-regexp-history)
      (if current-prefix-arg (y-or-n-p "Fixed case in search?") nil )
      (if current-prefix-arg (y-or-n-p "Print surrounding Text?") t ))))
  (let* ((case-fold-search (not *fixed-case-search-p))
         (-count 0)
         (-outBufName "*xah-find output*")
         -outBufObj
         )
    (setq *input-dir (file-name-as-directory *input-dir)) ; normalize dir path
    (when (get-buffer -outBufName) (kill-buffer -outBufName))
    (setq -outBufObj (generate-new-buffer -outBufName))
    (xah-find--print-header -outBufObj "xah-find-text" *input-dir *path-regex *search-str1  )
    (mapc
     (lambda (-path)
       (setq -count 0)
       (with-temp-buffer
         (insert-file-contents -path)
         (while (search-forward *search-str1 nil "NOERROR")
           (setq -count (1+ -count))
           (when *printContext-p (xah-find--occur-output (match-beginning 0) (match-end 0) -path -outBufObj)))
         (when (> -count 0) (xah-find--print-file-count -path -count -outBufObj))))
     (xah-find--filter-list (lambda (x) (not (xah-find--ignore-dir-p x))) (find-lisp-find-files *input-dir *path-regex)))
    (xah-find--switch-to-output -outBufObj)))

;;;###autoload
(defun xah-find-replace-text (*search-str *replace-str *input-dir *path-regex *write-to-file-p *fixed-case-search-p *fixed-case-replace-p &optional *backup-p)
  "Find/Replace string in all files of a directory.
Search string can span multiple lines.
No regex.

Backup, if requested, backup filenames has suffix with timestamp, like this: ~xf20150531T233826~

Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}"
  (interactive
   (let ( x-search-str x-replace-str x-input-dir x-path-regex x-write-to-file-p x-fixed-case-search-p x-fixed-case-replace-p x-backup-p )
     (setq x-search-str (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word)))
     (setq x-replace-str (read-string (format "Replace string: ") nil 'query-replace-history))
     (setq x-input-dir (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH"))
     (setq x-path-regex (read-from-minibuffer "File path regex: " (xah-find--get-default-file-extension-regex "el") nil nil 'dired-regexp-history))
     (setq x-write-to-file-p (y-or-n-p "Write changes to file?"))
     (setq x-fixed-case-search-p (y-or-n-p "Fixed case in search?"))
     (setq x-fixed-case-replace-p (y-or-n-p "Fixed case in replacement?"))
     (if x-write-to-file-p
         (setq x-backup-p (y-or-n-p "Make backup?"))
       (setq x-backup-p nil))
     (list x-search-str x-replace-str x-input-dir x-path-regex x-write-to-file-p x-fixed-case-search-p x-fixed-case-replace-p x-backup-p )))
  (let ((-outBufName "*xah-find output*")
        -outBufObj
        (-backupSuffix (xah-find--backup-suffix "xf")))
    (when (get-buffer -outBufName) (kill-buffer -outBufName))
    (setq -outBufObj (generate-new-buffer -outBufName))
    (xah-find--print-header -outBufObj "xah-find-replace-text" *input-dir *path-regex *search-str *replace-str )
    (mapc
     (lambda (-f)
       (let ((case-fold-search (not *fixed-case-search-p))
             (-count 0))
         (with-temp-buffer
           (insert-file-contents -f)
           (while (search-forward *search-str nil t)
             (setq -count (1+ -count))
             (replace-match *replace-str *fixed-case-replace-p "literalreplace")
             (xah-find--occur-output (match-beginning 0) (point) -f -outBufObj))
           (when (> -count 0)
             (when *write-to-file-p
               (when *backup-p (copy-file -f (concat -f -backupSuffix) t))
               (write-region 1 (point-max) -f))
             (xah-find--print-file-count -f -count -outBufObj )))))
     (xah-find--filter-list (lambda (x) (not (xah-find--ignore-dir-p x))) (find-lisp-find-files *input-dir *path-regex)))
    (xah-find--switch-to-output -outBufObj)))

;;;###autoload
(defun xah-find-text-regex (*search-regex *input-dir *path-regex *fixed-case-search-p *print-context-level )
  "Report files that contain a string pattern, similar to `rgrep'.
Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}"
  (interactive
   (list
    (read-string (format "Search regex (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "File path regex: " (xah-find--get-default-file-extension-regex "el") nil nil 'dired-regexp-history)
    (y-or-n-p "Fixed case search?")
    (ido-completing-read "Print context level: " '("with context string" "just matched pattern" "none" ))))
  (let ((-count 0)
        (-outBufName "*xah-find output*")
        -outBufObj
        (-pos1 1) ; beginning of line
        (-pos2 1))
    (setq *input-dir (file-name-as-directory *input-dir)) ; add ending slash
    (when (get-buffer -outBufName) (kill-buffer -outBufName))
    (setq -outBufObj (generate-new-buffer -outBufName))
    (xah-find--print-header -outBufObj "xah-find-text-regex" *input-dir *path-regex *search-regex  )
    (mapc
     (lambda (-fp)
       (setq -count 0)
       (with-temp-buffer
         (insert-file-contents -fp)
         (setq case-fold-search (not *fixed-case-search-p))
         (while (search-forward-regexp *search-regex nil t)
           (setq -count (1+ -count))
           (cond
            ((equal *print-context-level "none") nil)
            ((equal *print-context-level "just matched pattern")
             (xah-find--occur-output (match-beginning 0) (match-end 0) -fp -outBufObj))
            ((equal *print-context-level "with context string")
             (xah-find--occur-output (match-beginning 0) (match-end 0) -fp -outBufObj t))))
         (when (> -count 0) (xah-find--print-file-count -fp -count -outBufObj))))
     (xah-find--filter-list (lambda (x) (not (xah-find--ignore-dir-p x))) (find-lisp-find-files *input-dir *path-regex)))
    (xah-find--switch-to-output -outBufObj)))

;;;###autoload
(defun xah-find-replace-text-regex (*regex *replace-str *input-dir *path-regex *write-to-file-p *fixed-case-search-p *fixed-case-replace-p)
  "Find/Replace by regex in all files of a directory.

Backup, if requested, backup filenames has suffix with timestamp, like this: ~xf20150531T233826~

When called in lisp code:
*REGEX is a regex pattern.
*REPLACE-STR is replacement string.
*INPUT-DIR is input directory to search (includes all nested subdirectories).
*PATH-REGEX is a regex to filter file paths.
*WRITE-TO-FILE-P, when true, write to file, else, print a report of changes only.
*FIXED-CASE-SEARCH-P sets `case-fold-search' for this operation.
*FIXED-CASE-REPLACE-P if true, then the letter-case in replacement is literal. (this is relevant only if *FIXED-CASE-SEARCH-P is true.)
Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}"
  (interactive
   (list
    (read-regexp "Find regex: " )
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "File path regex: " (xah-find--get-default-file-extension-regex "el") nil nil 'dired-regexp-history)
    (y-or-n-p "Write changes to file?")
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Fixed case in replacement?")))
  (let ((-outBufName "*xah-find output*")
        -outBufObj
        (-backupSuffix (xah-find--backup-suffix "xfr")))
    (when (get-buffer -outBufName) (kill-buffer -outBufName))
    (setq -outBufObj (generate-new-buffer -outBufName))
    (xah-find--print-header -outBufObj "xah-find-replace-text-regex" *input-dir *path-regex *regex *replace-str )
    (mapc
     (lambda (-fp)
       (let ((-count 0))
         (with-temp-buffer
           (insert-file-contents -fp)
           (setq case-fold-search (not *fixed-case-search-p))
           (while (re-search-forward *regex nil t)
             (setq -count (1+ -count))
             ;; (xah-find--print-occur-block (match-beginning 0) (match-end 0) -outBufObj)
             (xah-find--occur-output (match-beginning 0) (match-end 0) -fp -outBufObj t)
             (replace-match *replace-str *fixed-case-replace-p)
             (xah-find--occur-output (match-beginning 0) (point) -fp -outBufObj nil t))
           (when (> -count 0)
             (xah-find--print-file-count -fp -count -outBufObj)
             (when *write-to-file-p (copy-file -fp (concat -fp -backupSuffix) t) (write-region 1 (point-max) -fp))))))
     (xah-find--filter-list (lambda (x) (not (xah-find--ignore-dir-p x))) (find-lisp-find-files *input-dir *path-regex)))
    (xah-find--switch-to-output -outBufObj)))

(provide 'xah-find)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-find.el ends here
