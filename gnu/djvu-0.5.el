;;; djvu.el --- Edit and view Djvu files via djvused

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Roland Winkler <winkler@gnu.org>
;; Keywords: files, wp
;; Version: 0.5

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; djvu.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with djvu.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a front end for the command-line program djvused
;; from DjVuLibre, see http://djvu.sourceforge.net/.  It assumes you
;; have the programs djvused, djview, and ddjvu installed.
;;
;; A normal work flow is as follows:
;;
;; To visit a djvu file type M-x fjvu-find-file.  This command is the
;; only entry point to this package.  You may want to bind this
;; command to a key you like.  I use
;;
;;   (global-set-key "\C-cd" 'djvu-find-file)
;;
;; If you use this command to visit file foo.djvu, it puts you into
;; the (read-only) buffer foo@djvu.  Normally, this buffer is all you
;; need.
;;
;; The menu bar of this buffer lists most of the commands with their
;; repsective key bindings.  For example, you can:
;;
;; - Use `g' to go to the page you want. (Yes, this package operates on
;;   one page at a time. I guess that anything else would be too slow
;;   for large documents.)
;;
;; - Use `v' to (re)start djview using the position in foo.djvu
;;   matching where point is in foo@djvu.  (I find djview fast enough
;;   for this, even for larger documents.)
;;
;; - To highlight a region in foo.djvu mark the corresponding region
;;   in foo@djvu (as usual, `transient-mark-mode' comes handy for
;;   this).  Then type `h' and add a comment in the minibuffer if you
;;   like.  Type C-x C-s to save this editing.  Then type `v' to
;;   (re)start djview to show what you have done.
;;
;; - Type i to enable `djvu-image-mode', a minor mode displaying the
;;   current page as an image.  Then
;;     drag-mouse-1 defines a region where to put a comment,
;;     C-drag-mouse-1 defines a region where to put a pushpin comment,
;;     S-drag-mouse-1 defines a region to highlight
;;
;; - The editing of the text, annotation and outline (bookmark) layers
;;   really happens in the buffers foo@djvu-t.el, foo@djvu-a.el, and
;;   foo@djvu-o.el.  (The djvused syntax used in these buffers is so
;;   close to elisp that it was natural to give these buffers a
;;   djvu-edit-mode that is derived from emacs-lisp-mode.)
;;
;;   You can check what is happening by switching to these buffers.
;;   The respective switching commands put point in these buffers such
;;   that it matches where you were in foo@djvu.
;;
;;   In these buffers, the menu bar lists a few low-level commands
;;   available for editing these buffers directly.  If you know the
;;   djvused syntax, sometimes it can also be helpful to do such
;;   editing "by hand".
;;
;; But wait: the syntax in the annotations buffer foo@djvu-a.el is a
;; slightly modified djvused syntax.  djvused can only highlight
;; rectangles.  So the highlighting of larger regions of text must use
;; multiple rectangles (i.e., multiple djvused "mapareas").  To make
;; editing easier, these are combined in the buffer foo@djvu-a.el.
;; (Before saving these things, they are converted using the proper
;; djvused syntax.)
;;
;; When you visit a djvu file, djvu-mode recognizes mapareas belonging
;; together by checking that "everything else in these mapareas except
;; for the rects" is the same.  So if you entered a (unique) comment,
;; this allows djvu-mode to combine all the mapareas when you visit
;; such a file the second time.  Without a comment, this fails!
;;
;; A second difference between what is displayed in the djvu buffers
;; and the input/output of djvused refers to nonascii characters.  I
;; am using djvused from DjVuLibre-3.5.22 which handles utf-8 by
;; backslash sequences.  So djvu mode converts these backslash
;; sequences into the corresponding utf-8 characters.  (More recent
;; versions of djvused can do this conversion, too.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Djvu internals:
;; (see /usr/share/doc/libdjvulibre-dev/djvu3spec.djvu)
;;
;; Supported area attributes             rect  oval  poly  line  text
;; (none)/(xor)/(border c)                X     X     X     X     X
;; (shadow_* t)                           X
;; (border_avis)                          X     X     X
;; (hilite color) / (opacity o)           X
;; (arrow) / (width w) / (lineclr c)                        X
;; (backclr c) / (textclr c) / (pushpin)                          X
;;
;; c = #RRGGBB   t = thickness (1..32)
;; o = opacity = 0..100

;;; Code:

(defvar djvu-color-highlight "yellow"
  "Default color for highlighting.")

(defvar djvu-color-himark "red"
  "Default color for highmarking.")

(defvar djvu-color-url "blue"
  "Default color for URLs.")

(defvar djvu-color-background "white"
  "Default background.")

(defvar djvu-color-alist
  ;; If the keys are strings, they are directly compatible with what
  ;; we get back from something like `completing-read'.
  '(("red"    . "#FF0070")
    ("green"  . "#00FF00")
    ("blue"   . "#6666FF")
    ("yellow" . "#EEFF00")
    ("white"  . "#FFFFFF"))
  "Alist of colors for highlighting.")

(defvar djvu-opacity 50
  "Default opacity for Highlighting.")

(defvar djvu-coords-justify 0.02
  "Upper threshold for justifying rect coordinates.")

(defvar djvu-fill-column 50
  "Fill column for Djvu annotations.")

(defvar djvu-all-buffer "*djvu*"
  "Buffer for `all' operations.")

(defvar djvu-buffer-name-extensions
  '("@djvu" "@djvu-t.el" "@djvu-a.el" "@djvu-o.el")
  "Extensions for Djvu buffer names.
This is a list with four elements (READ TEXT ANNOT OUTLINE).")

(defvar djvu-resolve-url nil
  "Flag for resolving internal URLs.
If 'long replace short page numbers by long FileIDs.
If 'short replace long FileIDs by short page numbers.
If nil do nothing.")

(defvar djvu-image-size 1024
  "Size of internally displayed image.")

;; Internal variables

(defvar djvu-test nil
  "If non-nil do not process / delete djvused scripts.")
;; (setq djvu-test t) (setq djvu-test nil)

(defvar djvu-doc nil
  "Internal look-up table (a vector) for each Djvu document.
For the different buffers of one Djvu document the buffer-local
value of this variable is the same vector holding all the
relevant information about this document. This way, we obtain a
\"document-local\" variable, where changes are seen in all buffers
refering to this Djvu document.")
(make-variable-buffer-local 'djvu-doc)

;;; Helper functions

;; "read" refers to the text-only display of djvu files inside emacs
;; "view" refers to external graphical viewers (default djview)

(eval-and-compile
  (let ((count 0))
    (dolist (elt '(file basename text-buf read-buf annot-buf outline-buf
                        page pagemax page-id pagesize pos view-proc image))
      (eval (list 'defsubst (intern (concat "djvu-doc-" (symbol-name elt)))
                  '(&optional doc) `(aref (or doc djvu-doc) ,count)))
      (eval (list 'defsubst (intern (concat "djvu-doc-set-" (symbol-name elt)))
                  '(val &optional doc) `(aset (or doc djvu-doc) ,count val)))
      (setq count (1+ count)))
    (eval `(defconst djvu-doc-length ,count))))

(defun djvu-switch-text ()
  "Switch to Djvu Text buffer."
  (interactive)
  (let ((pos (djvu-read-pos)))
    (switch-to-buffer (djvu-doc-text-buf))
    (djvu-locate-pos 'word pos)))

(defun djvu-switch-annot ()
  "Switch to Djvu Annotations buffer."
  (interactive)
  (let ((pos (djvu-read-pos)))
    (switch-to-buffer (djvu-doc-annot-buf))
    (djvu-locate-pos 'rect pos)))

(defun djvu-switch-outline ()
  "Switch to Djvu Outline buffer."
  (interactive)
  ;; Try to locate the current page in the outline buffer.
  ;; If this page is not defined, try to locate the nearest preceding page.
  (let ((page (djvu-doc-page)) pnt)
    (with-current-buffer (djvu-doc-outline-buf)
      (goto-char (point-min))
      (if (looking-at "(bookmarks")
          (while (and (< 0 page)
                      (not (setq pnt (re-search-forward
                                      (format "\"#%d\"" page) nil t))))
            (setq page (1- page)))))
    (switch-to-buffer (djvu-doc-outline-buf))
    (if pnt (goto-char pnt))))

(defun djvu-switch-read ()
  "Switch to Djvu Read buffer."
  (interactive)
  (switch-to-buffer (djvu-doc-read-buf)))

(defun djvu-goto-page (page)
  "Goto PAGE of Djvu document."
  (interactive
   (let ((str (read-string (format "Page (f, 1-%d, l): " (djvu-doc-pagemax)))))
     (list (cond ((string-match "\\`f" str) 1)
                 ((string-match "\\`l" str) (djvu-doc-pagemax))
                 ((string-match "\\`[[:digit:]]+\\'" str)
                  (string-to-number str))
                 (t (error "Page `%s' invalid" str))))))
  (if (or (not (integerp page))
          (<= page 0) (< (djvu-doc-pagemax) page))
      (error "Page `%s' out of range" page))
  (djvu-init-page djvu-doc page))

(defun djvu-next-page (n)
  (interactive "p")
  (djvu-goto-page (+ (djvu-doc-page) n)))

(defun djvu-prev-page (n)
  (interactive "p")
  (djvu-goto-page (- (djvu-doc-page) n)))

(defun djvu-set-color-highlight (color)
  "Set color for highlighting based on `djvu-color-alist'."
  (interactive (list (completing-read "Color: " djvu-color-alist nil t)))
  (setq djvu-color-highlight color))

(defun djvu-kill-view (&optional doc)
  (when (djvu-doc-view-proc doc)
    (unless (memq (process-status (djvu-doc-view-proc doc))
                  '(exit signal))
      (kill-process (djvu-doc-view-proc doc)))
    (djvu-doc-set-view-proc nil doc)))

(defun djvu-kill-doc (&optional doc)
  (interactive)
  (djvu-save doc t)
  (djvu-kill-view doc)
  (mapc 'kill-buffer (list (djvu-doc-text-buf doc) (djvu-doc-read-buf doc)
                           (djvu-doc-annot-buf doc) (djvu-doc-outline-buf doc))))

(defsubst djvu-delete-file (script)
  (unless djvu-test (delete-file script)))

(defun djvu-save (&optional doc query)
  "Save Djvu DOC."
  (interactive)
  (let ((pos (djvu-read-pos))
        (text-modified  (buffer-modified-p (djvu-doc-text-buf doc)))
        (annot-modified (buffer-modified-p (djvu-doc-annot-buf doc)))
        (outline-modified (buffer-modified-p (djvu-doc-outline-buf doc)))
        script)
    (when (and (or text-modified annot-modified outline-modified)
               (or (not query)
                   (yes-or-no-p (format "Save %s? " (djvu-doc-basename doc)))))
      (unwind-protect
          (progn
            (setq script (make-temp-file "djvu-el-"))
            (if text-modified (djvu-process-text script doc))
            (if annot-modified (djvu-process-annot script doc))
            (if outline-modified (djvu-process-outline script doc))
            (djvu-djvused doc nil "-f" script "-s")
            (dolist (buf (list (djvu-doc-text-buf doc) (djvu-doc-annot-buf doc)
                               (djvu-doc-outline-buf doc) (djvu-doc-read-buf doc)))
              (with-current-buffer buf (set-buffer-modified-p nil)))
            (if text-modified (djvu-locate-read-pos pos)))
        (djvu-delete-file script)))))

(defun djvu-modified ()
  "Mark Djvu Read buffer as modified if necessary.
Used in `post-command-hook' of the Djvu Outline, Text and Read buffers."
  (with-current-buffer (djvu-doc-read-buf)
    (set-buffer-modified-p (or (buffer-modified-p (djvu-doc-outline-buf))
                               (buffer-modified-p (djvu-doc-text-buf))
                               (buffer-modified-p (djvu-doc-annot-buf))))))

(defun djvu-process (&optional doc view)
  "Process Djvu DOC."
  (interactive (list djvu-doc t))
  (djvu-save doc)
  (if view (djvu-view doc)))

(defun djvu-djvused (doc buffer &rest args)
  ;; BUFFER is nil if we update the Djvu file.
  (unless (or buffer (file-writable-p (djvu-doc-file doc)))
    (error "File `%s' not writable"
           (abbreviate-file-name (djvu-doc-file doc))))
  (unless (and (not buffer) djvu-test)
    (let ((status (apply 'call-process "djvused" nil buffer nil
                         (djvu-doc-file doc) args)))
      (unless (zerop status)
        (error "Djvused error %s (args: %s)" status args)))))

(defun djvu-hide-hash (&optional recover)
  (let* ((old (concat " " (if recover "@!@" "#") "\\([[:xdigit:]]\\)"))
         (new (concat " " (if recover "#" "@!@") "\\1")))
    (goto-char (point-min))
    (while (re-search-forward old nil t) (replace-match new))))

(defun djvu-interactive-region ()
  "Return active region for use in interactive calls."
  (let (beg end)
    (if (use-region-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point) end (1+ (point))))
    (cons (if (get-text-property beg 'word)
              (djvu-property-beg beg 'word)
            (next-single-property-change beg 'word nil end))
          (if (get-text-property (1- end) 'word)
              (djvu-property-end end 'word)
            (previous-single-property-change end 'word nil beg)))))

(defun djvu-interactive-color (color)
  "Return color specification for use in interactive calls."
  (let ((colnum (or (and (consp current-prefix-arg)
                         (1- (/ (car current-prefix-arg) 4)))
                    (and (integerp current-prefix-arg)
                         current-prefix-arg))))
    (if (and colnum (>= colnum (length djvu-color-alist)))
        (error "Color undefined"))
    (if colnum (car (nth colnum djvu-color-alist)) color)))

(defun djvu-interactive-url (&optional color interrupt)
  "Return URL specification for use in interactive calls."
  (unless color (setq color djvu-color-url))
  (let ((fmt (format "(%s) Page URL: " (or color djvu-color-url)))
        (page "") num ignore)
    (while (and (not ignore)
                (or (not (integerp (setq num (string-to-number page))))
                    (< num 1) (< (djvu-doc-pagemax) num)))
      (setq page (read-string fmt))
      (if (and interrupt (string= "" page)) (setq ignore t)))
    (unless ignore (concat "#" page))))

(defsubst djvu-color-string-to-hex (color i)
  "Convert rgb COLOR string (part I) to hex number."
  (string-to-number (substring-no-properties
                     (cdr (assoc color djvu-color-alist))
                     (1+ (* i 2)) (+ 3 (* i 2))) 16))

(defun djvu-color-background (color background opacity)
  "For rgb COLOR and BACKGROUND apply OPACITY.
Return the new rgb color string."
  (let* ((str "#")
         ;; Why opacity squared??
         (a (/ (float (* opacity opacity)) 10000))
         (b (- 1 a)))
    (dotimes (i 3 str)
      (setq str (concat str (format "%X"
            (round (+ (* a (djvu-color-string-to-hex color i))
                      (* b (djvu-color-string-to-hex background i))))))))))

;;; Djvu modes

(defvar djvu-read-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "i"           'djvu-image-mode)
    (define-key km "v"           'djvu-view)
    (define-key km "\C-c\C-v"    'djvu-view)
    (define-key km "n"           'djvu-next-page)
    (define-key km "p"           'djvu-prev-page)
    (define-key km "g"           'djvu-goto-page)
    (define-key km "k"           'djvu-kill-doc)
    (define-key km "\C-c\C-c"    'djvu-process)
    (define-key km "\C-x\C-s"    'djvu-save)

    (define-key km "h"           'djvu-highlight)
    (define-key km "u"           'djvu-url)
    (define-key km "a"           'djvu-switch-annot)
    (define-key km "A"           'djvu-display-annot-all)

    (define-key km "c"           'djvu-comment)
    (define-key km "C"           'djvu-comment-pushpin)
    (define-key km "b"           'djvu-bookmark)
    (define-key km "m"           'djvu-himark)
    (define-key km "o"           'djvu-switch-outline)

    (define-key km "s"           'djvu-split-word)
    (define-key km "w"           'djvu-merge-words)
    (define-key km "l"           'djvu-merge-lines)

    (define-key km "t"           'djvu-switch-text)
    (define-key km "T"           'djvu-display-text-all)
    km)
  "Keymap for Djvu Read Mode.
This is a child of `special-mode-map'.")

(easy-menu-define
  djvu-read-menu djvu-read-mode-map "Djvu Menu"
  '("Djvu"
    ["View File" djvu-view t]
    ["Image File" djvu-image-mode t]
    ["Go to Page" djvu-goto-page t]
    ["Process Doc" djvu-process t]
    ["Save Doc" djvu-save t]
    "---"
    ["Split Word" djvu-split-word t]
    ["Merge Words" djvu-merge-words t]
    ["Merge Lines" djvu-merge-lines t]
    ["Switch to Text" djvu-switch-text t]
    "---"
    ["Highlight Region" djvu-highlight t]
    ["URL over Region" djvu-url t]
    ["Himark Region" djvu-himark t]
    ["Add Comment" djvu-comment t]
    ["Add Comment w/pushpin" djvu-comment-pushpin t]
    ["Switch to Annotations" djvu-switch-annot t]
    "---"
    ["Show all Text" djvu-display-text-all t]
    ["Show all Annotations" djvu-display-annot-all t]
    ["Resolve all URLs" djvu-resolve-all-urls t]
    ["Process all Annotations" djvu-process-all t]
    ["Remove Annot / Outline" djvu-make-clean t]
    "---"
    ["Add Bookmark" djvu-bookmark t]
    ["Switch to Outline" djvu-switch-outline t]
    "---"
    ["Quit Djvu" quit-window t]
    ["Kill Djvu buffers" djvu-kill-doc t]))

(define-derived-mode djvu-read-mode special-mode "Djview"
  "Mode for reading Djvu files."
  (setq mode-line-buffer-identification
        (list 24 (buffer-name) "  "
              '(:eval (format "p%d" (djvu-doc-page))))))

(defvar djvu-edit-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-c\C-r"    'djvu-switch-read)
    (define-key km "\C-c\C-g"    'djvu-goto-page)
    (define-key km "\C-c\C-s"    'djvu-split-word-internal)
    (define-key km "\C-c\C-m"    'djvu-merge-words-internal)
    (define-key km "\C-c\M-m"    'djvu-merge-lines-internal)
    (define-key km "\C-c\C-c"    'djvu-process)
    (define-key km "\C-x\C-s"    'djvu-save)
    (define-key km "\C-c\C-v"    'djvu-view)
    (define-key km "\C-c\C-k"    'djvu-kill-doc)
    km)
  "Keymap for Djvu Annot Mode.
This is a child of `text-mode-map'.")

(easy-menu-define
  djvu-annot-menu djvu-edit-mode-map "Djvu Menu"
  '("Djvu"
    ["Go to Page" djvu-goto-page t]
    ["Switch to Read" djvu-switch-read t]
    ["Process Doc" djvu-process t]
    ["Save Doc" djvu-save t]
    "---"
    ["Switch to Text" djvu-switch-text t]
    ["Split Word" djvu-split-word-internal t]
    ["Merge Words" djvu-merge-words-internal t]
    ["Merge Lines" djvu-merge-lines-internal t]
    "---"
    ["Switch to Annot" djvu-switch-annot t]
    "---"
    ["Quit Djvu" quit-window t]
    ["Kill Djvu buffers" djvu-kill-doc t]))

(define-derived-mode djvu-edit-mode emacs-lisp-mode "Djvu Edit"
  "Mode for editing (parts of) Djvu files."
  (setq mode-line-buffer-identification
        (list 24 (buffer-name) "  "
              '(:eval (format "p%d" (djvu-doc-page))))))

;;; General Setup

;;;###autoload
(defun djvu-find-file (file &optional page view)
  "Read and edit Djvu FILE on PAGE.
If VIEW is non-nil start external viewer."
  (interactive
   (list (read-file-name "Find Djvu file: " nil nil nil nil
                         (lambda (f)
                           (or (equal "djvu" (file-name-extension f))
                               (file-directory-p f))))
         (prefix-numeric-value current-prefix-arg)))
  (unless page (setq page 1))
  (setq file (expand-file-name file))
  (unless (file-regular-p file)
    (error "Cannot open Djvu file `%s'." file))
  ;; Initialize `djvu-doc' for FILE.
  (let* ((basename (file-name-sans-extension
                    (file-name-nondirectory file)))
         (read-buf  (concat basename (nth 0 djvu-buffer-name-extensions)))
         (text-buf  (concat basename (nth 1 djvu-buffer-name-extensions)))
         (annot-buf (concat basename (nth 2 djvu-buffer-name-extensions)))
         (outline-buf (concat basename (nth 3 djvu-buffer-name-extensions)))
         (buffers (list text-buf read-buf annot-buf outline-buf))
         doc)
    ;; Do nothing if we are already visiting FILE such that all required
    ;; buffers are properly defined.  If some buffers were killed
    ;; do not attempt to recycle the remaining buffers.
    (if (eval (cons 'and (mapcar 'get-buffer buffers)))
        (with-current-buffer read-buf
          (setq doc djvu-doc))
      (setq doc (make-vector djvu-doc-length nil))
      (dolist (buf buffers)
        (if (get-buffer buf)  (kill-buffer buf)))
      (djvu-doc-set-file file doc)
      (djvu-doc-set-basename basename doc)
      (djvu-doc-set-text-buf (get-buffer-create text-buf) doc)
      (djvu-doc-set-read-buf (get-buffer-create read-buf) doc)
      (djvu-doc-set-annot-buf (get-buffer-create annot-buf) doc)
      (djvu-doc-set-outline-buf (get-buffer-create outline-buf) doc)
      ;; Initialize all buffers.
      (dolist (buf (list (djvu-doc-text-buf doc) (djvu-doc-annot-buf doc)
                         (djvu-doc-outline-buf doc)))
        (with-current-buffer buf
          (djvu-edit-mode)
          (setq djvu-doc doc)
          (cd (file-name-directory (djvu-doc-file)))
          (add-hook 'post-command-hook 'djvu-modified nil t)))
      (with-current-buffer (djvu-doc-read-buf doc)
        (djvu-read-mode)
        (setq djvu-doc doc)
        (cd (file-name-directory (djvu-doc-file)))
        (add-hook 'post-command-hook 'djvu-modified nil t))
      (djvu-init-page doc page))
    (if view (djvu-view doc))
    (switch-to-buffer read-buf)))

(defun djvu-init-page (&optional doc page)
  "For Djvu DOC initialize PAGE."
  (if (djvu-doc-pagemax doc) (djvu-save doc t))
  (djvu-doc-set-pos nil doc)
  (if page (djvu-doc-set-page page doc))
  (let* ((doc (or doc djvu-doc))
         (new (not (djvu-doc-pagemax doc)))
         object alist)
    (with-temp-buffer
      (djvu-djvused doc t "-e"
                    (format "%sselect %d; size; print-txt; print-ant;"
                            (if new "n; ls; print-outline; " "")
                            (djvu-doc-page doc)))
      (goto-char (point-min))
      (when new
        ;; page max
        (djvu-doc-set-pagemax (read (current-buffer)) doc)
        ;; page id
        (let (page-id)
          (while (progn
                   (skip-chars-forward " \t\n")
                   (looking-at "\\(?:\\([0-9]+\\)[ \t]+\\)?\\([PIAT]\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\n]+\\)$"))
            (if (match-string 1)
                ;; page-id is an alist with elements (PAGE-NUM . FILE-ID)
                (push (cons (match-string 1) (match-string 4)) page-id))
            (goto-char (match-end 0)))
          (unless (eq (djvu-doc-pagemax doc) (length page-id))
            (error "Page id list broken"))
          (djvu-doc-set-page-id (nreverse page-id) doc))
        ;; bookmarks
        (skip-chars-forward " \t\n")
        (when (looking-at "(bookmarks")
          (setq object (read (current-buffer)))
          (djvu-decode-outline (cdr object))
          (with-current-buffer (djvu-doc-outline-buf doc)
            (insert "(bookmarks")
            (let (print-escape-newlines)
              (djvu-insert-outline (cdr object) " "))
            (insert ")\n")
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil))))

      ;; page size
      (skip-chars-forward " \t\n")
      (if (looking-at "width=\\([[:digit:]]+\\)[ \t]+height=\\([[:digit:]]+\\)$")
          (djvu-doc-set-pagesize (cons (string-to-number (match-string 1))
                                       (string-to-number (match-string 2))) doc)
        (error "No pagesize"))

      ;; raw text
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (setq object (if (looking-at "(\\(page\\|column\\|region\\|para\\|line\\|word\\|char\\)")
                       (read (current-buffer))))
      (djvu-decode-text object)
      (with-current-buffer (djvu-doc-text-buf doc)
        (erase-buffer)
        (djvu-insert-text object "")
        (insert "\n")
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil))

      ;; Set up read buffer
      (djvu-init-read doc object)

      ;; Set up annotations buffer:
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max))
          (djvu-hide-hash)))
      (setq object nil)
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
        (if (looking-at "(\\(background\\|zoom\\|mode\\|align\\|maparea\\|metadata\\)\\>")
            (push (read (current-buffer)) object)
          (error "Unknown annotation `%s'" (buffer-substring-no-properties
                                           (point) (line-end-position)))))
      ;; To simplify the editing of annotations, identify mapareas (rect)
      ;; sharing the same text string.
      (dolist (elt object)
        (if (not (eq 'maparea (car elt)))
            (push elt alist)
          (setcar (cdr elt) (decode-coding-string (nth 1 elt) 'utf-8))
          (setcar (nthcdr 2 elt) (decode-coding-string (nth 2 elt) 'utf-8))
          (cond ((eq 'rect (car (nth 3 elt))) ; rect
                 (let ((rect (djvu-rect (nth 3 elt)))
                       e)
                   (setcdr (nthcdr 2 elt) (nthcdr 4 elt)) ; remove rect destructively
                   ;; The new elements of alist are cons cells, where the car is the
                   ;; maparea without rect, and the cdr is the list of rect areas.
                   ;; Even if we have just an empty string, we still want to massage
                   ;; the rect box.
                   (if (or (string= "" (nth 2 elt))
                           (not (setq e (assoc elt alist))))
                       (push (cons elt (list rect)) alist)
                     (setcdr e (cons rect (cdr e))))))
                ((eq 'text (car (nth 3 elt))) ; text
                 (setcar (nthcdr 3 elt) (djvu-rect (nth 3 elt)))
                 (push elt alist))
                (t (push elt alist)))))
      ;; Pretty print annotations.
      (with-current-buffer (djvu-doc-annot-buf doc)
        (let ((standard-output (current-buffer))
              print-escape-newlines)
          (erase-buffer)
          (dolist (elt alist)
            (cond ((consp (car elt)) ;; maparea with list of rects
                   (let ((c (car elt)))
                     (insert (format "(maparea %S\n %S\n ("
                                     (djvu-resolve-url (nth 1 c) doc) (nth 2 c))
                             (mapconcat 'prin1-to-string (cdr elt) "\n  ") ")\n " ; rect
                             (mapconcat 'prin1-to-string (nthcdr 3 c) " ") ; rest
                             ")")))
                  ((not (eq 'maparea (car elt)))
                   (prin1 elt))
                  ((eq 'text (car (nth 3 elt))) ; text
                   (insert (format "(maparea %S\n %S\n " (nth 1 elt) (nth 2 elt))
                           (mapconcat 'prin1-to-string (nthcdr 3 elt) " ") ; rest
                           ")"))
                  (t (error "Djvu maparea %s undefined" (car (nth 3 elt)))))
            (insert "\n\n")))
        (djvu-hide-hash t)
        (goto-char (point-max))
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)))))

(defun djvu-resolve-url (url &optional doc)
  "Resolve internal URLs.  See variable `djvu-resolve-url'."
  (cond ((eq 'long djvu-resolve-url)
         ;; Replace page number by file id
         (cond ((string-match "\\`#[0-9]+\\'" url)
                (let ((page-id (assoc (substring-no-properties url 1)
                                      (djvu-doc-page-id doc))))
                  (if page-id
                      (concat "#" (cdr page-id))
                    (error "Page id broken: %s" url))))
               ((string-match "\\`#" url)
                (if (rassoc (substring-no-properties url 1)
                            (djvu-doc-page-id doc))
                    url
                  (error "Page id broken: %s" url)))
               (t url))) ; some other URL
        ((eq 'short djvu-resolve-url)
         ;; Replace file id by page number
         (cond ((string-match "\\`#[0-9]+\\'" url)
                url)
               ((string-match "\\`#" url)
                (let ((page-id (rassoc (substring-no-properties url 1)
                                       (djvu-doc-page-id doc))))
                  (if page-id
                      (concat "#" (car page-id))
                    (error "Page id broken: %s" url))))
               (t url))) ; some other URL
        (t url))) ; do nothing

(defun djvu-resolve-all-urls (dir)
  "Resolve all internal URLs in a Djvu file."
  (interactive
   (list (intern (completing-read "Direction: " '((long) (short)) nil t))))
  (if (djvu-modified) (error "Djvu file should be saved"))
  (let ((page-id (djvu-doc-page-id djvu-doc))
        (djvu-all-buffer (generate-new-buffer " *djvu*"))
        (djvu-resolve-url dir))
    (djvu-display-annot-all)
    (with-current-buffer djvu-all-buffer
      (goto-char (point-min))
      (cond ((eq dir 'long)
             (while (re-search-forward "^(maparea[ \t]+\"#\\([0-9]+\\)\"" nil t)
               (replace-match (cdr (assoc (match-string 1) page-id))
                              nil nil nil 1)))
            ((eq dir 'short)
             (while (re-search-forward "^(maparea[ \t]+\"#\\([^\"]+\\)\"" nil t)
               (replace-match (car (rassoc (match-string 1) page-id))
                              nil nil nil 1)))))
    (djvu-process-all)
    (kill-buffer djvu-all-buffer)
    (with-current-buffer (djvu-doc-outline-buf)
      (set-buffer-modified-p t))
    (djvu-save)))

(defun djvu-rect (rect &optional back)
  "Convert (rect xmin ymin width height) to (rect xmin ymin xmax ymax).
If BACK is non-nil do inverse transformation."
  (let* ((f (if back '- '+))
         (lst (list (nth 0 rect) (nth 1 rect) (nth 2 rect)
                    (funcall f (nth 3 rect) (nth 1 rect))
                    (funcall f (nth 4 rect) (nth 2 rect)))))
    ;; Only for back transforms we might get an error...
    (if (or (> 0 (nth 3 lst)) (> 0 (nth 4 lst)))
        (error "Annotation rect dimensions %s, %s" (nth 3 lst) (nth 4 lst)))
    lst))

(defun djvu-view (&optional doc)
  "Start Djview for DOC."
  (interactive (list djvu-doc))
  (if (not (window-system))
      (message "No window system available")
    (djvu-kill-view doc)
    (let* ((djvu-doc doc)
           (pos (or (djvu-doc-pos) (djvu-read-pos)))
           (px (/ (float (car pos))
                  (float (car (djvu-doc-pagesize)))))
           (py (- 1 (/ (float (cdr pos))
                       (float (cdr (djvu-doc-pagesize))))))
           process-connection-type)  ; Use a pipe.
      (if (or (< px 0) (< 1 px) (< py 0) (< 1 py))
          (error "px=%s, py=%s out of range" px py))
      (djvu-doc-set-pos nil)
      (djvu-doc-set-view-proc
       (start-process "djview" nil "djview"
                      (format "-page=%d" (djvu-doc-page))
                      (format "-showposition=%06f,%06f" px py)
                      (djvu-doc-file))))))

;;; Djvu Text mode

(defun djvu-split-word (pos)
  "Split word at position POS.
This command operates on the read buffer."
  (interactive "d")
  (let ((beg (djvu-property-beg pos 'word))
        (rpos (djvu-read-pos pos)))
    (with-current-buffer (djvu-doc-text-buf)
      (djvu-split-word-internal (djvu-locate-pos 'word rpos)
                                (- pos beg))))
  ;; Things get rather confusing without updating the read buffer.
  ;; So we better save everything.
  (djvu-save))

(defun djvu-split-word-internal (wpos split)
  "Split word at position WPOS at character position SPLIT.
This command operates on the text buffer."
  (interactive
   (let* ((pnt (point))
          (pps (parse-partial-sexp (line-beginning-position) pnt)))
     (unless (nth 3 pps) (error "Not inside string"))
     (list pnt (1- (- pnt (nth 8 pps))))))
  (goto-char wpos)
  (beginning-of-line)
  (skip-chars-forward " \t")
  (setq wpos (point))
  (let ((indent (buffer-substring-no-properties
                 (line-beginning-position) wpos))
        word)
    (condition-case nil
        (progn
          (setq word (read (current-buffer)))
          (unless (eq 'word (car word)) (error "invalid")))
      (error (error "Syntax error in raw text")))
    (if (or (< split 1) (<= (length (nth 5 word)) split))
        (error "nothing to split"))
    (delete-region wpos (point))
    ;; To split the bounding box horizontally, we take the fraction
    ;; of the number of characters in each fragment.  This scheme
    ;; is only approximate, but it is better than nothing.
    (let ((frac (round (* (/ (float split) (length (nth 5 word)))
                          (- (nth 3 word) (nth 1 word))))))
      (djvu-insert-text (list 'word (nth 1 word) (nth 2 word)
                              (+ (nth 1 word) frac) (nth 4 word)
                              (substring (nth 5 word) 0 split)) "")
      (insert "\n" indent)
      (djvu-insert-text (list 'word (+ (nth 1 word) frac 1) (nth 2 word)
                              (nth 3 word) (nth 4 word)
                              (substring (nth 5 word) split)) ""))))

(defun djvu-merge-words (beg end)
  "Merge words between positions BEG and END.
This command operates on the read buffer."
  (interactive "r")
  (let ((bpos (djvu-read-pos beg))
        (epos (djvu-read-pos (1- end))))
    (with-current-buffer (djvu-doc-text-buf)
      (djvu-merge-words-internal (djvu-locate-pos 'word bpos)
                                 (djvu-locate-pos 'word epos))))
  ;; Things get rather confusing without updating the read buffer.
  ;; So we better save everything.
  (djvu-save))

(defun djvu-merge-words-internal (beg end)
  "Merge words between positions BEG and END.
This command operates on the text buffer."
  (interactive "r")
  (let (words)
    (goto-char end)
    (if (bolp) (setq end (1- end)))
    (goto-char beg)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (setq beg (point))
    (condition-case nil
        (while (< (point) end)
          (push (read (current-buffer)) words)
          (unless (eq 'word (caar words)) (error "invalid")))
      (error (error "Syntax error in raw text")))
    (delete-region beg (point))
    (let ((object (apply 'list 'word 0 0 0 0 (nreverse words))))
      (djvu-process-text-bbox object 0 (make-vector 3 nil))
      (setcdr (nthcdr 4 object) (list (mapconcat (lambda (w) (nth 5 w))
                                                 (nthcdr 5 object) "")))
      (djvu-insert-text object "")))
  (undo-boundary))

(defun djvu-merge-lines (beg end)
  "Merge lines between positions BEG and END.
This command operates on the read buffer."
  (interactive "r")
  (let ((bpos (djvu-read-pos beg))
        (epos (djvu-read-pos (1- end))))
    (with-current-buffer (djvu-doc-text-buf)
      (djvu-merge-lines-internal (djvu-locate-pos 'word bpos)
                                 (djvu-locate-pos 'word epos))))
  ;; Things get rather confusing without updating the read buffer.
  ;; So we better save everything.
  (djvu-save))

(defun djvu-merge-lines-internal (beg end)
  "Merge lines between positions BEG and END.
This command operates on the text buffer."
  (interactive "r")
  ;; Calculate proper value of END
  (goto-char end)
  (beginning-of-line)
  (unless (looking-at "[ \t]*(line ")
    (re-search-backward "^[ \t]*(line ")
    (forward-sexp)
    (setq end (point)))
  ;; Calculate proper value of BEG
  (goto-char beg)
  (beginning-of-line)
  (unless (looking-at "[ \t]*(line ")
    (re-search-backward "^[ \t]*(line "))
  (skip-chars-forward " \t")
  (setq beg (point))
  (unless (< beg end) (error "Nothing to merge"))
  ;; Parsing fails if the words belong to different paragraphs,
  ;; regions or columns. We would have to determine the lowest common
  ;; object level of these words. Then we could possibly merge
  ;; everything (!) within this level
  (if (re-search-forward "^[ \t]*\\(?:para\\|region\\|column\\)" end t)
      (error "Cannot merge paragraphs, regions or columns"))
  (let (words)
    ;; Collect all words
    (condition-case nil
        (while (<= (point) end)
          (cond ((looking-at "[ \t]*(word ")
                 (push (read (current-buffer)) words))
                ((not (looking-at "[ \t]*(line "))
                 (error "invalid")))
          (forward-line))
      (error (error "Syntax error in raw text")))
    ;; Remove old words
    (goto-char beg)
    (while (let ((start (point)))
             (forward-sexp)
             (or (<= (point) end)
                 (progn (goto-char start) nil))))
    (delete-region beg (point))
    ;; Re-insert words
    (let ((indent (buffer-substring-no-properties
                   (line-beginning-position) (point)))
          (object (apply 'list 'line 0 0 0 0 (nreverse words))))
      (djvu-process-text-bbox object 0 (make-vector 3 nil))
      (delete-region (line-beginning-position) (point))
      (djvu-insert-text object indent)))
  (undo-boundary))

(defun djvu-decode-text (object &optional encode)
  (if (stringp (nth 5 object))
      (setcar (nthcdr 5 object)
              (if encode
                  (encode-coding-string (nth 5 object) 'utf-8)
                (decode-coding-string (nth 5 object) 'utf-8)))
    (dolist (elt (nthcdr 5 object))
      (djvu-decode-text elt encode))))

(defun djvu-insert-text (object indent)
  ;; This function is called recursively.
  (insert indent "("
          (mapconcat 'prin1-to-string
                     (list (nth 0 object) (nth 1 object) (nth 2 object)
                           (nth 3 object) (nth 4 object)) " "))
  (let ((tail (nthcdr 5 object))
        (indent (concat indent " ")))
    (if (stringp (car tail))
        ;; use `prin1-to-string' as we use this function both for
        ;; utf-8 and encoded stuff.
        (insert " " (prin1-to-string (car tail)) ")")
      (dolist (elt tail)
        (insert "\n")
        (djvu-insert-text elt indent))
      (insert ")"))))

(defun djvu-process-text (script &optional doc)
  (let ((doc (or doc djvu-doc))
        object)
    (with-current-buffer (djvu-doc-text-buf doc)
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (setq object (read (current-buffer)))
          (error (error "Syntax error in raw text")))
        (skip-chars-forward " \t\n")
        ;; We should have swallowed all raw text.
        (unless (eobp)
          (error "Syntax error in raw text (end of buffer)"))))
    (djvu-process-text-bbox object 0 (make-vector 7 nil))
    ;; Update read buffer
    (djvu-init-read doc object)
    ;; FIXME: Should we also update the text buffer?
    ;; A transparent solution would update only the part of the buffer
    ;; that we actually changed so that `undo' works as expected.
    (djvu-decode-text object t)
    (with-temp-buffer
      (insert (format "select %d\nremove-txt\nset-txt\n" (djvu-doc-page doc)))
      (djvu-insert-text object "")
      (insert "\n.\n")
      (write-region nil nil script t 0))))

(defun djvu-process-text-bbox (object depth coords)
  "Evaluate bounding box for text OBJECT recursively."
  (if (stringp (nth 5 object))
      (aset coords depth (vector (nth 1 object) (nth 2 object)
                                 (nth 3 object) (nth 4 object)))
    (let ((depth1 (1+ depth))
          coord)
      (aset coords depth nil)
      (dolist (elt (nthcdr 5 object))
        (djvu-process-text-bbox elt depth1 coords)
        (if (setq coord (aref coords depth))
            (let ((coord1 (aref coords depth1)))
              (aset coord 0 (min (aref coord 0) (aref coord1 0)))
              (aset coord 1 (min (aref coord 1) (aref coord1 1)))
              (aset coord 2 (max (aref coord 2) (aref coord1 2)))
              (aset coord 3 (max (aref coord 3) (aref coord1 3))))
          (aset coords depth (copy-sequence (aref coords depth1)))))
      (if (setq coord (aref coords depth))
          (setcdr object (apply 'list (aref coord 0) (aref coord 1)
                                (aref coord 2) (aref coord 3)
                                (nthcdr 5 object)))
        (error "No coords??")))))

(defun djvu-display-text-all ()
  "Display text for all pages."
  (interactive)
  (let ((doc djvu-doc)
        (buf (get-buffer-create djvu-all-buffer)))
    ;; Put this in a separate buffer!
    (with-current-buffer buf
      (let (buffer-read-only)
        (emacs-lisp-mode)
        (erase-buffer)
        (djvu-djvused doc t "-e" "output-txt")
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    (switch-to-buffer buf)))

(defun djvu-process-all ()
  "Process all pages.  Use at your own risk.  You get what you want."
  (interactive)
  (let ((buf (get-buffer djvu-all-buffer))
        script)
    (unless buf (error "No buffer `%s'" buf))
    (unless djvu-doc (error "No Djvu doc"))
    (unwind-protect
        (progn
          (setq script (make-temp-file "djvu-el-"))
          (with-temp-file script (insert-buffer-substring buf))
          (djvu-djvused djvu-doc nil "-f" script "-s"))
      (djvu-delete-file script))))

;;; Djvu Read mode

(defun djvu-init-read (doc object)
  (with-current-buffer (djvu-doc-read-buf doc)
    (let (buffer-read-only)
      (erase-buffer)
      (djvu-insert-read object))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (djvu-image)))

(defun djvu-insert-read (object)
  "Display text OBJECT."
  ;; This function is called recursively.
  (let ((opoint (point))
        (tail (nthcdr 5 object)))
    (if (stringp (car tail))
        (insert (decode-coding-string (car tail) 'utf-8))
      (let* ((obj (caar tail))
             (sep (cond ((eq 'line obj) "\n")
                        ((eq 'word obj) "\s")
                        ((eq 'char obj) nil)
                        (t "\n\n")))
             elt)
        (while (setq elt (pop tail))
          (djvu-insert-read elt)
          (if (and sep tail (not (looking-back sep)))
              (insert sep)))))
    (put-text-property opoint (point) (car object)
                       (vector (nth 1 object) (nth 2 object)
                               (nth 3 object) (nth 4 object)))))

(defun djvu-read-pos (&optional point)
  "Return Djvu position (x . y) of POINT in Djvu Read buffer."
  (with-current-buffer (djvu-doc-read-buf)
    ;; An empty djvu page gives us something like (page 0 0 0 0 "")
    (if (= (point-min) (point-max))
        ;; Take the center of an empty page
        (cons (/ (car (djvu-doc-pagesize)) 2)
              (/ (cdr (djvu-doc-pagesize)) 2))
      (unless point (setq point (point)))
      (djvu-mean-pos ; Return mean coordinates
       (or (get-text-property point 'word)
           (get-text-property (1- point) 'word)
           (get-text-property
            ;; Search backward because more often point is at the end
            ;; of region we operated on
            (1- (previous-single-property-change point 'word)) 'word))))))

(defun djvu-mean-pos (reg)
  "For region REG return mean coordinates (x . y)."
  ;; This works both for REG being vectors and lists.
  (cons (/ (+ (elt reg 0) (elt reg 2)) 2)
        (/ (+ (elt reg 1) (elt reg 3)) 2)))

(defun djvu-locate-pos (object pos)
  "Locate OBJECT at position POS in the text or annotation buffer.
If found, return corresponding position. Otherwise, return nil."
  (goto-char (point-min))
  (when pos
    (let ((re (concat "\\<" (symbol-name object) "\\> +"
                      (mapconcat 'identity
                                 (make-list 4 "\\([[:digit:]]+\\)") " +")
                      "\\( +\"\\)?"))
          done)
      (while (and (not done)
                  (re-search-forward re nil t))
        (let ((x1 (string-to-number (match-string 1)))
              (x2 (string-to-number (match-string 3)))
              (y1 (string-to-number (match-string 2)))
              (y2 (string-to-number (match-string 4))))
          (setq done (and (<= x1 (car pos))
                          (<= (car pos) x2)
                          (<= y1 (cdr pos))
                          (<= (cdr pos) y2)))))
      (if done (point)
        (goto-char (point-min)) nil))))

(defsubst djvu-dist (width height)
  (+ (* width width) (* height height)))

(defun djvu-locate-read-pos (pos)
  "Locate POS in Djvu Read buffer. Return corresponding position."
  (with-current-buffer (djvu-doc-read-buf)
    (if (not pos)
        (goto-char (point-min))
      (let ((hpos (car pos)) (vpos (cdr pos))
            (good-dist (djvu-dist (car (djvu-doc-pagesize))
                                  (cdr (djvu-doc-pagesize))))
            (pnt (point-min)) (good-pnt (point-min))
            word dist)
        (while (progn
                 (when (setq word (get-text-property pnt 'word))
                   (setq dist (djvu-dist (- (/ (+ (aref word 0) (aref word 2)) 2) hpos)
                                         (- (/ (+ (aref word 1) (aref word 3)) 2) vpos)))
                   (if (< dist good-dist)
                       (setq good-pnt pnt good-dist dist)))
                 (setq pnt (next-single-property-change pnt 'word))))
        (goto-char good-pnt)))))

;;; Djvu Annotation mode

(defun djvu-comment-interactive (&optional border backclr textclr pushpin)
  "Interactive spec for `djvu-comment' and friends."
  (let ((pos (djvu-read-pos))
        (pagesize (djvu-doc-pagesize))
        (color (djvu-interactive-color djvu-color-highlight)))
    (list "" (read-string (format "(%s) Text: " color))
          (list (car pos) (cdr pos)
                (+ (car pos) (/ (car pagesize) 2))
                (+ (cdr pos) (/ (cdr pagesize) 30)))
          border
          (or backclr
              (djvu-color-background color djvu-color-background
                                     djvu-opacity))
          textclr pushpin)))

(defsubst djvu-insert-color (key color)
  (if color
      (format " (%s %s)" key
              (cond ((string-match "\\`#" color) color)
                    ((cdr (assoc color djvu-color-alist)))
                    (t (error "Color `%s' undefined" color))))
    ""))

(defun djvu-comment (url text rect &optional border backclr textclr pushpin)
  "Using URL and TEXT, highlight RECT.
This defines a rect area for djvused."
  (interactive (djvu-comment-interactive))
  (with-current-buffer (djvu-doc-annot-buf)
    (goto-char (point-max))
    (let (print-escape-newlines)
      (insert (format "(maparea %S\n %S\n " url (djvu-fill text))
              (apply 'format "(text %d %d %d %d)" rect)
              (if border (format " (%s)" border) "")
              (djvu-insert-color "backclr" backclr)
              (djvu-insert-color "textclr" textclr)
              (if pushpin " (pushpin)" "")
              ")\n\n"))
    (undo-boundary)))

(defun djvu-comment-pushpin (url text rect
                                 &optional border backclr textclr pushpin)
  (interactive (djvu-comment-interactive nil nil nil t))
  (djvu-comment url text rect border backclr textclr pushpin))

(defun djvu-himark (beg end url text &optional color opacity border)
  "Himark region between BEG and END.
This highlights the region between BEG and END and creates a bookmark entry."
  (interactive
   (let ((region (djvu-interactive-region)))
     (list (car region) (cdr region) "" ""
           djvu-color-himark djvu-opacity 'none)))
  (djvu-highlight beg end url text color opacity border)
  (djvu-bookmark (buffer-substring-no-properties beg end) (djvu-doc-page)))

(defun djvu-url (beg end url text &optional color opacity border)
  (interactive
   (let* ((region (djvu-interactive-region))
          (color (djvu-interactive-color djvu-color-url))
          (url (djvu-interactive-url color)))
     (list (car region) (cdr region) url "" color djvu-opacity 'xor)))
  (djvu-highlight beg end url text color opacity border))

(defun djvu-highlight (beg end url text &optional color opacity border)
  "Highlight region between BEG and END, add annotation TEXT."
  (interactive
   (let ((region (djvu-interactive-region))
         (color (djvu-interactive-color djvu-color-highlight)))
     (list (car region) (cdr region) ""
           (read-string (format "(%s) Annotation: " color))
           color djvu-opacity 'none)))

  (unless (get-text-property beg 'word)
    (error "Start position `%s' not a word" beg))
  (unless (get-text-property (1- end) 'word)
    (error "End position `%s' not a word" end))
  (let ((words (djvu-region-count beg end 'word))
        (lines (djvu-region-count beg end 'line))
        (paras (djvu-region-count beg end 'para))
        (regions (djvu-region-count beg end 'region))
        (columns (djvu-region-count beg end 'column))
        coords)
    (unless (and (>= 1 paras) (>= 1 regions) (>= 1 columns))
      (error "Region spans multiple paragraphs"))

    (if (eq 1 lines)
        (setq coords (list (djvu-scan-coords beg end 'word)))

      (if (eq 2 lines)
          (let ((c1 (djvu-scan-coords beg (djvu-property-end (1+ beg) 'line) 'word))
                (c2 (djvu-scan-coords (djvu-property-beg (1- end) 'line) end 'word)))
            ;; If BEG is beginning of first line, both lines share same left margin.
            (if (and (= beg (djvu-property-beg beg 'line))
                     (djvu-coords-justify t c1 c2))
                (djvu-justify-coords 'min 0 c1 c2))
            ;; If END is end of second line, both lines share same right margin.
            (if (and (= end (djvu-property-end end 'line))
                     (djvu-coords-justify nil c2 c1))
                (djvu-justify-coords 'max 2 c1 c2))
            (if (<= (aref c1 0) (aref c2 2))
                ;; Lower bound of upper box and upper bound of lower box coincide.
                (let ((tmp (/ (+ (aref c1 1) (aref c2 3)) 2)))
                  (aset c1 1 tmp) (aset c2 3 tmp)))
            (setq coords (list c1 c2)))
        ;; 3 lines
        (let* ((l1e (djvu-property-end (1+ beg) 'line))
               (l2b (djvu-property-beg (1- end) 'line))
               (c1  (djvu-scan-coords beg l1e 'word))
               (ci  (djvu-scan-coords (1+ l1e) (1- l2b) 'line))
               (c2  (djvu-scan-coords l2b end 'word)))
          ;; If BEG is beginning of first line, all lines share same left margin.
          (cond ((and (= beg (djvu-property-beg beg 'line))
                      (djvu-coords-justify t c1 ci c2))
                 (djvu-justify-coords 'min 0 c1 ci c2))
                ((djvu-coords-justify t ci c2)
                 (djvu-justify-coords 'min 0 ci c2)))
          ;; If END is end of last line, all lines share same right margin.
          (cond ((and (= end (djvu-property-end end 'line))
                      (djvu-coords-justify nil c2 ci c1))
                 (djvu-justify-coords 'max 2 c1 ci c2))
                ((djvu-coords-justify nil c1 ci)
                 (djvu-justify-coords 'max 2 c1 ci)))
          (let ((tmp1 (/ (+ (aref c1 1) (aref ci 3)) 2))
                (tmp2 (/ (+ (aref ci 1) (aref c2 3)) 2)))
            ;; Lower bound of upper boxes and upper bound of lower boxes coincide.
            (aset c1 1 tmp1) (aset ci 3 tmp1)
            (aset ci 1 tmp2) (aset c2 3 tmp2))
          (setq coords (list c1 ci c2)))))

    (djvu-highlight-region url text coords color opacity border)))

(defun djvu-highlight-region (url text coords &optional color opacity border)
  "Using URL and TEXT, highlight COORDS.
This defines a hilite area for djvused."
    ;; Record position where annotation was made.
  (let ((posl (mapcar 'djvu-mean-pos coords))
        (n (length coords)))
    (djvu-doc-set-pos (cons (/ (apply '+ (mapcar 'car posl)) n)
                            (/ (apply '+ (mapcar 'cdr posl)) n))))
  ;; Insert in Annotations buffer.
  (with-current-buffer (djvu-doc-annot-buf)
    (goto-char (point-max))
    (let (print-escape-newlines)
      (insert (format "(maparea %S\n %S\n (" url (djvu-fill text))
              (mapconcat
               (lambda (rect) (apply 'format "(rect %d %d %d %d)" (append rect nil)))
               coords "\n  ") ")\n"
               (djvu-insert-color "hilite" color)
               (if opacity (format " (opacity %s)" opacity) "")
               (if border (format " (%s)" border) "")
               ")\n\n"))
    (undo-boundary)))

(defun djvu-fill (text)
  "Fill string TEXT using `djvu-fill-column'."
  (if djvu-fill-column
      (with-temp-buffer
        (insert text)
        (let ((fill-column djvu-fill-column))
          (fill-region (point-min) (point-max)))
        (buffer-substring-no-properties
         (point-min) (point-max)))
    text))

(defun djvu-property-beg (pnt prop)
  ;; Assume that PNT has PROP.  Otherwise we would not know whether
  ;; to search for it before or after PNT.
  (let ((p1 (get-text-property pnt prop)))
    (unless p1 (error "Position %s does not have property %s" pnt prop))
    (if (> pnt (point-min))
        (let ((p0 (get-text-property (1- pnt) prop)))
          (if (eq p0 p1)
              (setq pnt (previous-single-property-change
                         pnt prop nil (point-min))))))
    pnt))

(defun djvu-property-end (pnt prop)
  ;; Assume that (1- PNT) has PROP.  Otherwise we would not know whether
  ;; to search for it before or after PNT.
  (let ((p1 (get-text-property (1- pnt) prop)))
    (unless p1 (error "Position %s does not have property %s" pnt prop))
    (if (< pnt (point-max))
        (let ((p0 (get-text-property pnt prop)))
          (if (eq p0 p1)
              (setq pnt (next-single-property-change
                         (1- pnt) prop nil (point-max))))))
    pnt))

(defun djvu-coords-justify (left &rest ci)
  "Return non-nil if rect coordinates CI shall be justified horizontally.
If LEFT is nil analyze left boundaries of CI, otherwise the right boundaries."
  (let ((xl (apply 'min (mapcar (lambda (c) (aref c 0)) ci)))
        (xr (apply 'max (mapcar (lambda (c) (aref c 2)) ci))))
    (> djvu-coords-justify
       (/ (apply 'max (mapcar (lambda (cj)
                                (abs (float (if left (- (aref cj 0) xl)
                                              (- xr (aref cj 2))))))
                              ci))
          (float (- xr xl))))))

(defun djvu-justify-coords (fun n &rest ci)
 "Pass Nth elements of arrays CI to function FUN.
Set these elements to return value of FUN.
If FUN is `min' or `max' these elements are set to the respective minimum
or maximum among the Nth elements of all arrays CI."
  (let ((tmp (apply fun (mapcar (lambda (c) (aref c n)) ci))))
    (dolist (c ci)
      (aset c n tmp))))

(defun djvu-scan-coords (beg end prop)
  "Between BEG and END calculate total bounding box for PROP."
  ;; Assume that BEG has PROP.
  (let ((coords (copy-sequence (get-text-property beg prop)))
        (pnt beg) val)
    (while (and (/= pnt end)
                (setq pnt (next-single-property-change pnt prop nil end)))
      (when (setq val (get-text-property pnt prop))
        (aset coords 0 (min (aref coords 0) (aref val 0)))
        (aset coords 1 (min (aref coords 1) (aref val 1)))
        (aset coords 2 (max (aref coords 2) (aref val 2)))
        (aset coords 3 (max (aref coords 3) (aref val 3)))))
    coords))

(defun djvu-region-count (beg end prop)
  "Count regions between BEG and END with distinct non-nil values of PROP."
  (let ((count 0)
        (pnt beg))
    (while (and (/= pnt end)
                (setq pnt (next-single-property-change pnt prop nil end)))
      (if (get-text-property (1- pnt) prop)
          (setq count (1+ count))))
    count))

(defun djvu-process-annot (script &optional doc)
  (let ((doc djvu-doc) object)
    (with-temp-buffer
      (insert-buffer-substring (djvu-doc-annot-buf doc))
      (djvu-hide-hash)
      (goto-char (point-min))
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
        (if (looking-at "(\\(background\\|zoom\\|mode\\|align\\|maparea\\|metadata\\)\\>")
            (condition-case nil
                (push (read (current-buffer)) object)
              (error (error "Syntax error in annotations")))
          (error "Unknown annotation `%s'" (buffer-substring-no-properties
                                            (point) (line-end-position))))))
    (setq object (nreverse object))
    (dolist (elt object)
      (when (eq 'maparea (car elt))
        ;; URL
        (setcar (cdr elt) (encode-coding-string (djvu-resolve-url (nth 1 elt)) 'utf-8))
        ;; Comment
        (setcar (nthcdr 2 elt) (encode-coding-string (nth 2 elt) 'utf-8))))

    (with-temp-buffer
      (let ((standard-output (current-buffer))
            (print-escape-newlines t)
            str)
        (insert (format "select %d\nremove-ant\nset-ant\n"
                        (djvu-doc-page doc)))
        (dolist (elt object)
          (cond ((not (eq 'maparea (car elt)))
                 (prin1 elt)
                 (insert "\n"))
                ((consp (car (nth 3 elt))) ; rect
                 (dolist (e (nth 3 elt))
                   (insert (prin1-to-string
                            (apply 'list (car elt) (nth 1 elt) (nth 2 elt)
                                   (djvu-rect e t) (nthcdr 4 elt))) "\n")))
                ((eq 'text (car (nth 3 elt))) ; text
                 (insert (prin1-to-string
                          (apply 'list (car elt) (nth 1 elt) (nth 2 elt)
                                 (djvu-rect (nth 3 elt) t)
                                 (nthcdr 4 elt))) "\n"))
                (t (error "Djvu maparea %s undefined" (car (nth 3 elt))))))
        (insert ".\n")
        (djvu-hide-hash t))
      (write-region nil nil script t 0))))

(defun djvu-display-annot-all (&optional display)
  "Print annotations for all pages."
  (interactive (list t))
  (let ((doc djvu-doc)
        (buf (get-buffer-create djvu-all-buffer)))
    ;; Put this in a separate buffer!
    (with-current-buffer buf
      (let (buffer-read-only)
        (emacs-lisp-mode)
        (erase-buffer)
        (djvu-djvused doc t "-e" "output-ant")
        (goto-char (point-min))
        (while (re-search-forward "^(maparea" nil t)
          (forward-sexp) ; jump over URL
          ;; replace newlines within text
          (let ((limit (save-excursion (forward-sexp) (point))))
            (while (search-forward "\\n" limit t)
              (replace-match "\n"))))
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil))
    (if display (switch-to-buffer buf))))

;;; Djvu Outline mode

(defun djvu-bookmark (text page)
  "Create bookmark"
  (interactive
   (let ((region (djvu-interactive-region)))
     (list (read-string "Bookmark: " (buffer-substring-no-properties
                                      (car region) (cdr region)))
           (djvu-doc-page))))
  ;; Remove newlines that are ignored anyway
  (setq text (replace-regexp-in-string "\n" " " text))
  (let (object)
    (with-current-buffer (djvu-doc-outline-buf)
      (goto-char (point-min))
      (if (equal (point) (point-max))
          (setq object (list 'bookmarks))
        (condition-case nil
            (setq object (read (current-buffer)))
          (error (error "Syntax error in outline"))))
      (unless (eq 'bookmarks (car object))
        (error "No bookmarks"))
      ;; No decoding/encoding necessary if we add another bookmark.
      (setcdr object (sort (append (cdr object)
                                   (list (list text (format "#%d" page))))
                           (lambda (x y)
                             (< (string-to-number (substring (nth 1 x) 1))
                                (string-to-number (substring (nth 1 y) 1))))))
      (erase-buffer)
      (insert "(bookmarks")
      (let (print-escape-newlines)
        (djvu-insert-outline (cdr object) " "))
      (insert ")\n")
      (goto-char (point-min))
      (undo-boundary))))

(defun djvu-decode-outline (object &optional encode)
  "Decode Djvu Outline OBJECT. Encode if ENCODE is non-nil."
  (dolist (elt object)
    ;; Title
    (setcar elt
            (if encode
                (encode-coding-string (car elt) 'utf-8)
              (decode-coding-string (car elt) 'utf-8)))
    ;; URL
    (setcar (cdr elt)
            (djvu-resolve-url
             (if encode
                 (encode-coding-string (cadr elt) 'utf-8)
               (decode-coding-string (cadr elt) 'utf-8))))
    ;; Continue with subtree.
    (djvu-decode-outline (nthcdr 2 elt) encode)))

(defun djvu-insert-outline (object indent)
  "Insert Outline OBJECT."
  ;; This function is called recursively.
  (let ((indent1 (concat indent " ")))
    (dolist (elt object)
      (insert (format "\n%s(%S\n%s %S" indent (car elt) indent (nth 1 elt)))
      (djvu-insert-outline (nthcdr 2 elt) indent1)
      (insert ")"))))

(defun djvu-process-outline (script &optional doc)
  (let (object)
    (with-current-buffer (djvu-doc-outline-buf doc)
      (save-excursion
        (goto-char (point-min))
        (unless (= (point-min) (point-max))
          (condition-case nil
              (setq object (read (current-buffer)))
            (error (error "Syntax error in outline"))))
        (skip-chars-forward " \t\n")
        ;; We should have swallowed all bookmarks.
        (unless (eobp)
          (error "Syntax error in outline (end of buffer)"))))
    (unless (eq 'bookmarks (car object))
      (error "No bookmarks"))
    (djvu-decode-outline (cdr object) t)
    (with-temp-buffer
      (insert "set-outline\n")
      (when object
        (insert "(bookmarks")
        (let ((print-escape-newlines t))
          (djvu-insert-outline (cdr object) " "))
        (insert ")\n"))
      (insert ".\n")
      (write-region nil nil script t 0))))

;;; Image minor mode

(define-minor-mode djvu-image-mode
  "Toggle image display of current page."
  :lighter "Image"
  :keymap '(([drag-mouse-1]   . djvu-mouse-comment)
            ([C-drag-mouse-1] . djvu-mouse-comment-pushpin)
            ([S-drag-mouse-1] . djvu-mouse-highlight)
            ;; (Global) bindings of down-mouse events would take precedence over
            ;; drag-mouse events. So we bind the down-mouse events to `ignore'.
            ([down-mouse-1]   . ignore)
            ([C-down-mouse-1] . ignore)
            ([S-down-mouse-1] . ignore)
            ("+" . djvu-image-zoom-in)
            ("-" . djvu-image-zoom-out))
  (djvu-image))

(defun djvu-image-zoom-in ()
  (interactive)
  (djvu-image (round (* (nth 1 (djvu-doc-image)) 1.2))))

(defun djvu-image-zoom-out ()
  (interactive)
  (djvu-image (round (/ (nth 1 (djvu-doc-image)) 1.2))))

(defun djvu-image (&optional isize)
  "If `djvu-image-mode' is enabled, display image of current Djvu page.
Otherwise remove the image."
  (if (not djvu-image-mode)
      (let (buffer-read-only)
        (remove-text-properties (point-min) (point-max) '(display nil)))
    ;; Update image if necessary.
    (if (or (not (eq (djvu-doc-page) (car (djvu-doc-image))))
            (and isize
                 (not (eq isize (nth 1 (djvu-doc-image))))))
        (let ((file (make-temp-file "djvu-"))
              (isize (or isize
                         (nth 1 (djvu-doc-image))
                         djvu-image-size)))
          (unwind-protect
              ;; ddjvu does not send tiff files to stdout
              (let ((doc djvu-doc)
                    (status (call-process "ddjvu" nil t nil
                                          (format "-size=%dx%d" isize isize)
                                          "-format=tiff"
                                          (format "-page=%d" (djvu-doc-page))
                                          (djvu-doc-file)
                                          file)))
                (unless (zerop status)
                  (error "Ddjvu error %s" status))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (djvu-doc-set-image
                   (list (djvu-doc-page doc)
                         isize
                         (create-image (buffer-substring-no-properties
                                        (point-min) (point-max))
                                       'tiff t)) doc)))
            (djvu-delete-file file))))
    ;; Display image.
    (let (buffer-read-only)
      (put-text-property (point-min) (point-max)
                         'display (nth 2 (djvu-doc-image))))))

(defun djvu-event-to-rect (event)
  "Convert mouse EVENT to Djvu rect coordinates."
  (let* ((start (posn-object-x-y (event-start event)))
         (end (posn-object-x-y (event-end event)))
         (x1 (car start)) (y1 (cdr start)) (x2 (car end)) (y2 (cdr end))
         (size (posn-object-width-height (event-start event)))
         (width  (/ (float (car (djvu-doc-pagesize))) (car size)))
         (height (/ (float (cdr (djvu-doc-pagesize))) (cdr size))))
    (list (round (* (min x1 x2) width))
          (round (* (- (cdr size) (max y1 y2)) height))
          (round (* (max x1 x2) width))
          (round (* (- (cdr size) (min y1 y2)) height)))))

(defun djvu-mouse-highlight (event)
  (interactive "e")
  ;; Mouse events ignore prefix args?
  (let ((color (djvu-interactive-color djvu-color-highlight)))
    (djvu-highlight-region "" (read-string (format "(%s) H-Text: " color))
                           (list (djvu-event-to-rect event))
                           color djvu-opacity)))

(defun djvu-mouse-comment (event &optional pushpin)
  (interactive "e")
  ;; Mouse events ignore prefix args?
  (let ((color (djvu-interactive-color djvu-color-highlight)))
    (djvu-comment "" (read-string (format "(%s) C-Text: " color))
                  (djvu-event-to-rect event) nil
                  (djvu-color-background color djvu-color-background
                                         djvu-opacity)
                  nil pushpin)))

(defun djvu-mouse-comment-pushpin (event)
  (interactive "e")
  (djvu-mouse-comment event t))

;;; clean up

(defun djvu-make-clean ()
  "Remove Outline and Annotations."
  (interactive)
  (when (yes-or-no-p "Remove Outline and Annotations ")
    (djvu-djvused djvu-doc nil "-e"
                  "select; remove-ant; set-outline;\n." "-s")
    (djvu-init-page)))



;;;; ChangeLog:

;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary fix for quarter-plane.el.
;; 
;; 2011-11-01  Roland Winkler  <winkler@gnu.org>
;; 
;; 	Small bugfixes
;; 
;; 2011-10-29  Roland Winkler  <winkler@gnu.org>
;; 
;; 	new package djvu.el
;; 

(provide 'djvu)
;;; djvu.el ends here
