;;; fm-bookmarks.el --- Use file manager bookmarks (eg Dolphin, Nautilus, PCManFM) in Dired  -*- lexical-binding: t; -*-

;; Author: Ono Hiroko <azazabc123@gmail.com>
;; Keywords: files, convenience
;; Package-Version: 20170104.1716
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; X-URL: http://github.com/kuanyui/fm-bookmarks.el
;; Version: 0.1
;; Keywords: tools

;; The MIT License (MIT)
;; Copyright (C) 2015  hiroko
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; For more detailed configuration & usage, visit:
;; https://github.com/kuanyui/fm-bookmarks.el

;; Use existing bookmarks of file managers (e.g. Dolphin, Nautilus,
;; PCManFM) in Dired.

;;   (add-to-list 'load-path "/path/to/fm-bookmarks.el")
;;   (require 'fm-bookmarks)
;;   (setq fm-bookmarks-enabled-file-managers '(kde4 gnome3 pcmanfm custom media))
;;
;;   ;; Add customized bookmarks
;;   (setq fm-bookmarks-custom-bookmarks
;;         '(("Root" . "/")
;;           ("Tmp" . "/tmp/")
;;           ))
;;   ;; Shortcut to open FM bookmark.
;;   (global-set-key (kbd "C-x `") #'fm-bookmarks)
;;   ;; Use ` to open FM bookmark in Dired-mode
;;   (define-key dired-mode-map (kbd "`") #'fm-bookmarks)


;;; Code:

(require 'xml)
(require 'cl-lib)
(require 'dired)

;; ======================================================
;; Major Mode
;; ======================================================

(defgroup fm-bookmarks nil
  "Use existing FM bookmark in Dired"
  :prefix "fm-bookmarks-"
  :link '(url-link "http://github.com/kuanyui/fm-bookmarks.el"))

(defgroup fm-bookmarks-faces nil
  "Faces used in fm-bookmarks"
  :group 'fm-bookmarks
  :group 'faces)

(defcustom fm-bookmarks-mode-hook nil
  "Normal hook run when entering fm-bookmarks-mode."
  :type 'hook
  :group 'fm-bookmarks)

(defvar fm-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") '(lambda ()
				 (interactive)
				 (fm-bookmarks-update-last-line-position)
				 (kill-buffer-and-window)
				 ))
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "g") 'fm-bookmarks-refresh)
    (define-key map (kbd "RET") 'fm-bookmarks-open-this)
    (define-key map (kbd "TAB") 'fm-bookmarks-next-category)
    (define-key map (kbd "<backtab>") 'fm-bookmarks-previous-category)
    (define-key map (kbd "<up>") 'fm-bookmarks-previous-line)
    (define-key map (kbd "<down>") 'fm-bookmarks-next-line)
    map)
  "")   ;document

(define-derived-mode fm-bookmarks-mode nil "FM Bookmarks"
  ""
  (set (make-local-variable 'buffer-read-only) t)
  (hl-line-mode t)
  )

;; ======================================================
;; Faces
;; ======================================================
(defgroup fm-bookmarks-faces nil
  ""
  :group 'fm-bookmarks
  :group 'faces)

(defface fm-bookmarks-title
  '((((class color) (background light)) (:foreground "#808080"))
    (((class color) (background dark)) (:foreground "#a0a0a0")))
  "" :group 'fm-bookmarks-faces)

(defface fm-bookmarks-file-manager
  '((((class color) (background light)) (:bold t :foreground "#6faaff"))
    (((class color) (background dark)) (:bold t :foreground "#6faaff")))
  "" :group 'fm-bookmarks-faces)

(defface fm-bookmarks-custom
  '((((class color) (background light)) (:bold t :foreground "#5fd700"))
    (((class color) (background dark)) (:bold t :foreground "#a1db00")))
  "" :group 'fm-bookmarks-faces)

(defface fm-bookmarks-media
  '((((class color) (background light)) (:bold t :foreground "#ff4ea3"))
    (((class color) (background dark)) (:bold t :foreground "#ff6fa5")))
  "" :group 'fm-bookmarks-faces)

(defun fm-bookmarks-get-face (symbol)
  (cdr (cl-assoc symbol '(((kde4 kde5 gnome3 pcmanfm) . fm-bookmarks-file-manager)
                          ((media)		. fm-bookmarks-media)
                          ((custom)		. fm-bookmarks-custom))
                 :test (lambda (sym pair) (memq sym pair))
                 )))

;; ======================================================
;; Variables
;; ======================================================

(defvar fm-bookmarks-buffer-name "*FM Bookmarks*"
  "Name of the buffer.")

(defvar fm-bookmarks-buffer-width 25
  "Width of buffer"
  )

(defvar fm-bookmarks-enabled-file-managers '(kde4 custom media)
  "Enabled file managers/items. Ordering is sensitive.
Add custom bookmarks manually via `fm-bookmarks-custom-bookmarks'.
Available options: '(kde4 gnome3 pcmanfm custom media)

Notice that 'media is only available on Unix-like OS (exclude Mac
OS X)
")


(defvar fm-bookmarks-enable-cache t
  "Use cache to avoid re-generating list every time.")
(defvar fm-bookmarks--cache nil
  "Used to store generated propertized & formatted list, which to
  prevent unnecessarily re-generate. DON'T CHANGE THIS." )

(defvar fm-bookmarks-hide-duplicated t
  "Hide duplicated path.")

(defvar fm-bookmarks-hide-by-name-pattern '("Bluetooth")
  "Patterns to hide (by name).")
(defvar fm-bookmarks-hide-by-path-pattern '()
  "Patterns to hide (by path).")

(defconst fm-bookmarks-supported-file-managers
  '((kde4	.	"~/.kde4/share/apps/kfileplaces/bookmarks.xml")
    (kde5	.	"~/.local/share/user-places.xbel")
    (gnome3	.	"~/.config/gtk-3.0/bookmarks")
    (pcmanfm	.	"~/.gtk-bookmarks")))

(defvar fm-bookmarks-file-managers-display-name
  '((kde4	.	"Dolphin (KDE4)")
    (kde5	.	"Dolphin (KDE5)")
    (gnome3	.	"Nautilus")
    (pcmanfm	.	"PCManFM")
    (custom     .       "Custom Bookmarks")
    (media	.	"External Media")
    )
  "Display names of each file manager"
  )

(defvar fm-bookmarks-custom-bookmarks nil
  "Besides the bookmarks grabbed from file managers, you can also
  add other new bookmarks manually. Example:
  '((\"Root\" . \"/\")
    (\"Dir Name\" . \"/path/to/dir\" ))

Finally, please remember to add 'custom into
`fm-bookmarks-enabled-file-managers'" )

(defvar fm-bookmarks--last-line-position 0
  "Internal use. Don't change.")

;; ======================================================
;; External Media (Experimental, Linux Only)
;; ======================================================

(defun fm-bookmarks-get-and-parse-media-list ()
  "Get raw list from `mount` command and parse.
Output is like:
((\"/dev/sdb1\" . \"/var/run/media/kuanyui/kuanyui\")
 (\"/dev/sdb2\" . \"/var/run/media/kuanyui/windows\")
 (\"/dev/sdc1\" . \"/var/run/media/kuanyui/kuanyui 1G\"))"
(cl-remove-duplicates
 (mapcar (lambda (line)
	   (save-match-data
	     (string-match "^\\([^ ]+\\) on \\(.+\\) type [^ ]+ [^ ]+$" line)
	     (cons (match-string 1 line)
		   (match-string 2 line)
		   )
	     ))
         (let ((cmd-output (shell-command-to-string "mount | grep 'media'")))
           (if (> (length cmd-output) 2)
               (split-string (substring cmd-output 0 -1) "\n") ;fuck you elisp
             ())))
 :test (lambda (a b) (equal (car a) (car b)))))

(defun fm-bookmarks-generate-media-pair-list ()
  (if (member system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
      (mapcar (lambda (x)
		(cons (file-name-base (cdr x)) (cdr x))
		)
              (remove-if
               (lambda (y) (null (cdr y)))
               (fm-bookmarks-get-and-parse-media-list))
	      )
    ))

;; ======================================================
;; Main
;; ======================================================

(defun fm-bookmarks--set-width (window n)
  "Make window N columns width."
  (let ((w (max n window-min-width)))
    (unless (null window)
      (if (> (window-width) w)
	  (shrink-window-horizontally (- (window-width) w))
	(if (< (window-width) w)
	    (enlarge-window-horizontally (- w (window-width))))))))
(defalias 'fm-bookmarks #'fm-bookmarks-open-buffer)

(defun fm-bookmarks-open-buffer ()
  (interactive)
  (when (window-live-p (get-buffer-window fm-bookmarks-buffer-name))
    (switch-to-buffer (get-buffer fm-bookmarks-buffer-name))
    (kill-buffer-and-window))
  (select-window (window-at 0 0))
  (split-window-horizontally)
  (switch-to-buffer fm-bookmarks-buffer-name)
  (kill-all-local-variables)
  (fm-bookmarks--set-width (selected-window) fm-bookmarks-buffer-width)
  (set-window-dedicated-p (selected-window) t)
  (let (buffer-read-only)
    (erase-buffer)
    (if fm-bookmarks-enable-cache
        (insert (or fm-bookmarks--cache
                    (setq fm-bookmarks--cache (fm-bookmarks-generate-propertized-list))))
      (insert (fm-bookmarks-generate-propertized-list))))
  (fm-bookmarks-mode)
  (goto-line fm-bookmarks--last-line-position)
  ;; Disable linum
  (when (and (boundp 'linum-mode)
	     (not (null linum-mode)))
    (linum-mode -1))
  )

(defun fm-bookmarks-symbol-to-title (symbol)
  (let ((display-name
	 (or (cdr (assq symbol fm-bookmarks-file-managers-display-name))
	     (symbol-name symbol))))
    (concat
     display-name " "
     (make-string (- fm-bookmarks-buffer-width (+ 2 (length display-name))) ?=)
     "\n"
     )))

(defun fm-bookmarks-refresh ()
  (interactive)
  (if (equal (buffer-name) fm-bookmarks-buffer-name)
      (let ((buffer-read-only nil)
            (curr-point (point)))
        (delete-region (point-min) (point-max))
        (message "Refreshing...")
        (insert
         (setq fm-bookmarks--cache (fm-bookmarks-generate-propertized-list)))
        (goto-char curr-point)
        (message "Done!")
        )
    (message "Sorry, this function should be only used in FM bookmark buffer.")))

(defun fm-bookmarks-generate-propertized-list ()
  "Generate a formatted dir list with text propertized.
kde4 =======
  dir1
  dir2
gnome3 =====
  dir1
  dir2
 "
  (let (seen-paths)
    (mapconcat
     (lambda (fm-symbol)		;kde4, gnome3...etc
       (concat (propertize (fm-bookmarks-symbol-to-title fm-symbol)
                           'face 'fm-bookmarks-title)
               (mapconcat
                (lambda (item)
                  (let ((path (replace-regexp-in-string "^file://" "" (cdr item))))
                    (cond ((and fm-bookmarks-hide-duplicated
                                (member path seen-paths))
                           "")
                          ;; Patterns to hide items
                          ((cl-some (lambda (patt) (string-match patt path)) fm-bookmarks-hide-by-path-pattern)
                           "")
                          ((cl-some (lambda (patt) (string-match patt (car item))) fm-bookmarks-hide-by-name-pattern)
                           "")
                          (t
                           (progn (push path seen-paths)
                                  (propertize (concat "  " (car item) "\n")
                                              'face (fm-bookmarks-get-face fm-symbol)
                                              'href path))))))
                (cond ((eq fm-symbol 'kde4)
                       (fm-bookmarks-kde4-parser))
                      ((eq fm-symbol 'kde5)
                       (fm-bookmarks-kde5-parser))
                      ((eq fm-symbol 'gnome3)
                       (fm-bookmarks-gtk-parser fm-symbol))
                      ((eq fm-symbol 'pcmanfm)
                       (fm-bookmarks-gtk-parser fm-symbol))
                      ((eq fm-symbol 'custom)
                       fm-bookmarks-custom-bookmarks)
                      ((eq fm-symbol 'media)
                       (fm-bookmarks-generate-media-pair-list)
                       )
                      )
                "")))
     fm-bookmarks-enabled-file-managers
     "")))

(defun fm-bookmarks-open-this ()
  (interactive)
  (if (eolp) (left-char))
  (let ((link (get-text-property (point) 'href)))
    (if link
	(progn (fm-bookmarks-update-last-line-position)
	       (delete-window (selected-window))
	       (kill-buffer fm-bookmarks-buffer-name)
	       (find-file-other-window link)
	       )
      (message "There's no link"))
    ))


;; ======================================================
;; Helm support
;; ======================================================

;;(when (require 'helm nil :noerror)
;;  (defun helm-fm-bookmarks ()
;;    (interactive)
;;    (helm :sources (helm-build-sync-source "Open bookmark: "
;;                     :candidates #'fm-bookmarks--get-helm-candidates
;;                     :action #'identity)
;;          :buffer "File Manager Bookmarks"
;;          :prompt "Bookmarks list:"))
;;
;;  (defun fm-bookmarks--get-helm-candidates ()
;;    (mapcar (lambda (fm-symbol)
;;              (cond ((eq fm-symbol 'kde4)
;;                     (fm-bookmarks-kde4-parser))
;;                    ((eq fm-symbol 'kde5)
;;                     (fm-bookmarks-kde5-parser))
;;                    ((eq fm-symbol 'gnome3)
;;                     (fm-bookmarks-gtk-parser fm-symbol))
;;                    ((eq fm-symbol 'pcmanfm)
;;                     (fm-bookmarks-gtk-parser fm-symbol))
;;                    ((eq fm-symbol 'custom)
;;                     fm-bookmarks-custom-bookmarks)
;;                    ((eq fm-symbol 'media)
;;                     (fm-bookmarks-generate-media-pair-list))))
;;            fm-bookmarks-enabled-file-managers)
;;    )
;;
;;  )

;; ======================================================
;; Tools for UX
;; ======================================================

(defun fm-bookmarks-update-last-line-position ()
  (setf fm-bookmarks--last-line-position (line-number-at-pos)))

(defun fm-bookmarks-next-category ()
  "Move cursor to next category"
  (interactive)
  (next-line)
  (when (get-text-property (point) 'face)
    (goto-char (next-single-property-change (point) 'face nil (point-max))))
  (if (eobp) (goto-char (point-min)))
  (next-line)
  )

(defun fm-bookmarks-previous-category ()
  "Move cursor to previous category"
  (interactive)
  (previous-line)
  (when (get-text-property (point) 'face)
    (goto-char (previous-single-property-change (point) 'face nil (point-min))))
  (when (bobp)
    (goto-char (point-max))
    (fm-bookmarks-previous-category))
  )

(defun fm-bookmarks-next-line ()
  (interactive)
  (next-line)
  (if (eq (face-at-point) 'fm-bookmarks-title)
      (next-line))
  (when (eobp) (goto-line 2))
  )

(defun fm-bookmarks-previous-line ()
  (interactive)
  (previous-line)
  (when (eq (line-number-at-pos) 1)
    (goto-char (point-max))
    (previous-line))
  (if (eq (face-at-point) 'fm-bookmarks-title)
      (previous-line)))

;; ======================================================
;; Parser
;; ======================================================

(defun fm-bookmarks-kde4-parser ()
  (let* ((root (xml-parse-file (cdr (assoc 'kde4 fm-bookmarks-supported-file-managers))))
	 (bookmarks (xml-get-children (car root) 'bookmark)))
    (cl-remove-if
     #'null
     (mapcar (lambda (bookmark)
	       (unless (let ((metadata (apply #'append (xml-get-children (assoc 'info bookmark) 'metadata))))
			 (or (assoc 'isSystemItem metadata) ;No add if exist
			     (assoc 'OnlyInApp metadata)))  ;No add if exist
		 (cons
		  (nth 2 (car (xml-get-children bookmark 'title))) ;title
		  (decode-coding-string ;link
		   (url-unhex-string
		    (cdr (assoc 'href (nth 1 bookmark))))
		   'utf-8)
		  )
		 ))
	     bookmarks
	     )
     )))


(defun fm-bookmarks-kde5-parser ()
  (let* ((root (xml-parse-file (cdr (assoc 'kde5 fm-bookmarks-supported-file-managers))))
	 (bookmarks (xml-get-children (car root) 'bookmark)))
    (cl-remove-if
     #'null
     (mapcar (lambda (bookmark)
	       (unless (let ((metadata (apply #'append (xml-get-children (assoc 'info bookmark) 'metadata))))
			 (or (assoc 'isSystemItem metadata) ;No add if exist
			     (assoc 'OnlyInApp metadata)))  ;No add if exist
		 (cons
		  (nth 2 (car (xml-get-children bookmark 'title))) ;title
		  (decode-coding-string ;link
		   (url-unhex-string
		    (cdr (assoc 'href (nth 1 bookmark))))
		   'utf-8)
		  )
		 ))
	     bookmarks
	     )
     )))

(defun fm-bookmarks-gtk-parser (symbol)
  "Available arg: 'gnome3 'pcmanfm"
  (with-temp-buffer
    (insert-file-contents (cdr (assoc symbol fm-bookmarks-supported-file-managers)))
    (mapcar
     (lambda (str)
       (let* ((line (split-string str " " t))
	      (link (decode-coding-string (url-unhex-string (car line)) 'utf-8))
	      (title (if (> (length line) 1)
			 (mapconcat #'identity (cdr line) " ")
		       (file-name-base link))
		     ))

	 (cons title link))
       )
     (split-string (buffer-string) "\n" t))
    ))



(provide 'fm-bookmarks)

;;; fm-bookmarks.el ends here
