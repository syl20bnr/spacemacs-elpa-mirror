;;; helm-kythe.el --- Google Kythe helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Google Inc.

;; Author: Fangrui Song <i@maskray.me>
;; Package-Version: 20170709.726
;; Package-X-Original-Version: 20170708.1
;; Version: 0.0.1
;; URL: https://github.com/MaskRay/emacs-helm-kythe
;; Package-Requires: ((emacs "25") (dash "2.12.0") (helm "2.0"))

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

;; `helm-kythe.el' is a `helm' interface of Google Kythe.

;; This package is enlightened by `helm-gtags.el'.

;; Usage
;; For C++:
;; (add-hook 'c++-mode-hook 'helm-kythe-mode)
;;
;; For Haskell:
;; (add-hook 'haskell-mode-hook 'helm-kythe-mode)
;;
;; If eldoc-mode is enabled, when the point is at a reference, the `snippet' of its definition will be displayed in the echo area..

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'evil-jumps nil t)
(require 'easymenu)
(require 'helm)
(require 'json)
(require 'subr-x)

(defcustom helm-kythe-display-one-anchor-per-line t
  "If non-nil, for cross refernces, display at most one anchor per line."
  :group 'helm-kythe
  :type 'boolean)

(defcustom helm-kythe-highlight-candidate t
  "Highlight candidate or not."
  :group 'helm-kythe
  :type 'boolean)

;; TODO 0.0.26 /opt/kythe/tools/http_server --listen localhost:8080 does not listen on ::1
(defcustom helm-kythe-http-server-url "http://127.0.0.1:8080"
  "HTTP URL served by `kythe/tools/http_server --listen' through which ‘helm-kythe-apply-decorations’ fetch cross references information."
  :group 'helm-kythe
  :type 'string)

(defcustom helm-kythe-prefix-key (kbd "C-c k")
  "If non-empty, it is used for the prefix key of helm-kythe-xxx command."
  :group 'helm-kythe
  :type 'string)

(defcustom helm-kythe-preselect t
  "If non-nil, preselect the Helm candidate corresponding to current ticket."
  :group 'helm-kythe
  :type 'string)

(defcustom helm-kythe-filesystem-path-to-corpus-root nil
  "A list of `(filesystem-path-prefix corpus root)' for converting between filenames `(filesystem-path-prefix/path)' and Kythe `(corpus, root, path)' tuples, which is used by `helm-kythe--filename-to-triple-search-path' and `helm-kythe-triple-to-filename-search-path'."
  :group 'helm-kythe
  :type '(list (list string)))

(defcustom helm-kythe-filename-to-triple-functions '(helm-kythe--filename-to-triple-hackage helm-kythe--filename-to-triple-search-path)
  "A list of functions for finding corresponding Kythe triple given filename.
The first function (applied to `(buffer-file-name)') returns a non-nil value
will be used. This is used to apply decorations for current file.

Example:
  (defun foo (filename)
    (list \"corpus\" \"root\" \"a/b/c.hs\"))"
  :group 'helm-kythe
  :type '(repeat function))

(defcustom helm-kythe-triple-to-filename-functions '(helm-kythe--triple-to-filename-hackage helm-kythe--triple-to-filename-search-path)
  "A list of functions for finding filenames given Kythe triple and ticket.
The first function returns a non-nil value will be used. This is used to map a
cross reference to a local file.

Example:
 (defun foo (triple &optional ticket)
   \"/tmp/a/b/c.hs\")"
  :group 'helm-kythe
  :type '(repeat function))

(defcustom helm-kythe-recenter 'non-local
  "Whether to (recenter) after a jump.
`always' always
`never' never
`non-local' if jumps to another buffer. In this case, (recenter) may still be
  triggered if the number of scrolling lines is greater than `scroll-conservatively'."
  :group 'helm-kythe
  :type '(choice (const always) (const never) (const non-local)))

(defface helm-kythe-column
  '((t :inherit font-lock-constant-face))
  "Face for column numbers.")

(defface helm-kythe-file
  '((t :inherit font-lock-string-face))
  "Face for filenames in the error list.")

(defface helm-kythe-inactive
  '((t (:strike-through t)))
  "Face for mode line when Kythe is inactive.")

(defface helm-kythe-line
  '((t :inherit font-lock-constant-face))
  "Face for line numbers.")

(defface helm-kythe-match
  '((t :inherit helm-match))
  "Face for word matched against tagname")

(defvar helm-kythe--eldoc-cache (make-hash-table :test 'equal))

(defvar helm-kythe--find-file-action
  (helm-make-actions
   "Open file" #'helm-kythe--action-openfile
   "Open file other window" #'helm-kythe--action-openfile-other-window))

(defvar helm-kythe--jumps (make-hash-table)
  "Hashtable which stores all helm-kythe jumps on a per window basis.")

(defvar helm-kythe--use-otherwin nil)

(defvar helm-kythe-source-definitions
  (helm-build-in-buffer-source "Find definitions"
    :init #'helm-kythe--source-definitions
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-kythe-source-definitions-prompt
  (helm-build-in-buffer-source "Find definitions with prompt"
    :init #'helm-kythe--source-definitions-prompt
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-kythe-source-imenu
  (helm-build-in-buffer-source "imenu"
    :init #'helm-kythe--source-imenu
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-kythe-source-references
  (helm-build-in-buffer-source "Find references"
    :init #'helm-kythe--source-references
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-kythe-source-references-prompt
  (helm-build-in-buffer-source "Find references with prompt"
    :init #'helm-kythe--source-references-prompt
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar-local helm-kythe-decorated nil
  "Non-nil if decorations have been applied.")

(defconst helm-kythe--buffer "*helm helm-kythe*")
(defconst helm-kythe--ticket-anchor-regex "\\`\\([^\0]+\\)\0\\([^\0]+\\)\0\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\(.*\\)")

(define-error 'helm-kythe-error "helm-kythe error")

(defun helm-kythe--action-openfile (candidate)
  (when (string-match helm-kythe--ticket-anchor-regex candidate)
    (let* ((ticket (match-string-no-properties 1 candidate))
           (path (match-string-no-properties 3 candidate))
           (line (string-to-number (match-string-no-properties 4 candidate)))
           (column (string-to-number (match-string-no-properties 5 candidate)))
           (old-buffer (current-buffer))
           (old-pos (point))
           (do-found (lambda ()
                       ;; Push old position to the jump list.
                       (with-current-buffer old-buffer
                         (save-excursion
                           (goto-char old-pos)
                           (if (featurep 'evil-jumps)
                               (helm-kythe--with-evil-jumps (evil-set-jump)))))
                       (pcase helm-kythe-recenter
                         ('always (recenter))
                         ('never)
                         ('non-local
                          (unless (eq (current-buffer) old-buffer)
                            (recenter)))))))
      (when (helm-kythe--find-file ticket)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char column)
        ;; If the target has been modified, enumerate all decorations and find the ticket.
        (if (helm-kythe--ticket-equal? ticket (or (helm-kythe--definition-at-point) (helm-kythe--reference-at-point)))
            (funcall do-found)
          (let ((p (point)))
            (unless
                (cl-loop for (prop . get-prop) in
                         '((helm-kythe-definition . helm-kythe--definition-at-point)
                           (helm-kythe-reference . helm-kythe--reference-at-point)) do
                           (goto-char (point-min))
                           (while (not (eobp))
                             (let ((ticket1 (funcall get-prop))
                                   (next-change
                                    (or (next-single-property-change (point) prop)
                                        (point-max))))
                               (when (helm-kythe--ticket-equal? ticket ticket1)
                                 (funcall do-found)
                                 (cl-return t))
                               (goto-char next-change))))
              (goto-char p))))))))

(defun helm-kythe--action-openfile-other-window (candidate)
  (let ((helm-kythe--use-otherwin t))
    (helm-kythe--action-openfile candidate)))

(defun helm-kythe--anchor-keep-one-per-line (anchors)
  "If there are more than one Kythe anchors in one line, keep the first and discard the rest."
  (let (ret triple1 line1)
    (dolist (anchor anchors)
      (let ((triple (helm-kythe--ticket-to-triple (alist-get 'parent anchor)))
            (line (alist-get 'line_number (alist-get 'start anchor))))
        (unless (and line1 (= line line1) (equal triple triple1))
          (!cons anchor ret))
        (setq triple1 triple)
        (setq line1 line)))
    (nreverse ret)))

(defun helm-kythe--anchor-to-candidate (anchor)
  "Convert a Kythe anchor to Helm candidates. The first field is an invisible field containing its ticket."
  (format "%s\0%s\0%s:%d:%d:%s"
          (alist-get 'ticket anchor)
          (alist-get 'text anchor)
          (caddr (helm-kythe--ticket-to-triple (alist-get 'parent anchor)))
          (alist-get 'line_number (alist-get 'start anchor))
          (or (alist-get 'column_offset (alist-get 'start anchor)) 0)  ;; TODO Cabal cpu: 'start' of definition_locations of getSystemArch = X86_64 does not have column_offset
          (alist-get 'snippet anchor)))

(defun helm-kythe--anchors-to-candidates (anchors)
  (mapcar #'helm-kythe--anchor-to-candidate
          (if helm-kythe-display-one-anchor-per-line
              (helm-kythe--anchor-keep-one-per-line anchors)
            anchors)))

(defun helm-kythe--definition-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-definition) (alist-get 'ticket)))

(defun helm-kythe--filename-to-triple-hackage (filename)
  "Convert a filename in a Haskell package to `(corpus root path)'.
/tmp/mtl-2.2.1/Control/Monad/Cont/Class.hs => (\"\" \"\" mtl-2.2.1/Control/Monad/Cont/Class.hs)"
  (when (eq major-mode 'haskell-mode)
    (require 'inf-haskell)  ;; for inferior-haskell-find-project-root
    (when-let (root (helm-kythe--haskell-find-project-root filename))
      (list "" "" (concat (file-name-nondirectory root) "/" (file-relative-name filename root))))))

(defun helm-kythe--filename-to-triple-search-path (filename)
  (cl-loop for (prefix corpus root) in helm-kythe-filesystem-path-to-corpus-root do
           (when-let (path (file-relative-name filename prefix))
             (cl-return (list corpus root path)))))

(defun helm-kythe--haskell-find-project-root (filename)
  ;; TODO /tmp/lens-4.15.1/src => /tmp/lens-4.15.1
  (when (string-match "^\\(.+-[0-9][^/]+\\)/" filename)
    (match-string 1 filename)))

(defun helm-kythe--candidate-transformer (candidate)
  (let* ((e-ticket (string-match "\0" candidate))
         (e-text (string-match "\0" candidate (1+ e-ticket)))
         (e-path (string-match ":" candidate (1+ e-text)))
         (e-line (string-match ":" candidate (1+ e-path)))
         (e-column (string-match ":" candidate (1+ e-line))))
    (put-text-property 0 (1+ e-text) 'invisible t candidate)
    (when helm-kythe-highlight-candidate
      (put-text-property (1+ e-text) e-path 'face 'helm-kythe-file candidate)
      (put-text-property (1+ e-path) e-line 'face 'helm-kythe-line candidate)
      (put-text-property (1+ e-line) e-column 'face 'helm-kythe-column candidate)
      (let ((i (1+ e-column))
            (len (- e-text e-ticket 1))
            ;; imprecise but useful
            (needle (concat "\\b" (regexp-quote (substring-no-properties candidate (1+ e-ticket) e-text)) "\\b")))
        (while (setq i (string-match needle candidate i))
          (put-text-property i (+ i len) 'face 'helm-match candidate)
         (cl-incf i len))))
    candidate))

(defun helm-kythe--char-offsets (object start-key end-key)
  "Convert Kythe byte offsets to Emacs positions."
  (if-let ((start (alist-get 'byte_offset (alist-get start-key object)))
           (end (alist-get 'byte_offset (alist-get end-key object))))
    (cons (byte-to-position (1+ start))
          (byte-to-position (1+ end)))
    '(nil . nil)))

(defun helm-kythe--common (srcs &optional caller-ticket)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t))
    (if (and helm-kythe-preselect caller-ticket)
        (helm :sources srcs :buffer helm-kythe--buffer :preselect (concat "^" (regexp-quote caller-ticket)))
      (helm :sources srcs :buffer helm-kythe--buffer))))

(defun helm-kythe--find-file (ticket)
  "Switch to a buffer visiting the file which is mapped to the Kythe ticket."
  (let ((triple (helm-kythe--ticket-to-triple ticket))
        (open-func (if helm-kythe--use-otherwin #'find-file-other-window #'find-file)))
    (cl-loop for func in helm-kythe-triple-to-filename-functions do
             (when-let (filename (funcall func triple ticket))
               (funcall open-func filename)
               (cl-return t)))))

(defmacro helm-kythe--fontify (mode line)
  "Fontify a Kythe snippet which will be displayed in echo area."
  (let ((mode-hook (intern (format "%S-hook" mode))))
    `(with-temp-buffer
      (let ((flycheck-checkers nil)
            (,mode-hook nil))
        (,mode))
      (insert ,line)
      (font-lock-ensure)
      (buffer-string))))

(defun helm-kythe--mode-line ()
  "Text for the mode line."
  (if helm-kythe-decorated
      " Kythe"
    (propertize " Kythe" 'face 'helm-kythe-inactive)))

(defun helm-kythe--ticket-to-triple (ticket)
  "Convert a Kythe ticket to a `(corpus root path)' triple.
kythe://corpus?lang=java?root=a?path=b.c => (\"corpus\" \"a\" \"b.c\")"
  (when-let (i (string-match "?path=\\([^#?]+\\)" ticket))
    (let ((corpus "") (root "") (path (match-string 1 ticket)))
      (when (string-match "//\\([^#?]+\\)" ticket)
        (setq corpus (match-string 1 ticket)))
      (when (string-match "?root=\\([^?]+\\)" ticket)
        (setq root (match-string 1 ticket)))
      (list corpus root path))))

(defun helm-kythe--triple-to-filename-hackage (triple ticket)
  "Find filename given Kythe triple and ticket.
(\"\" \"\" mtl-2.2.1/Control/Monad/Cont/Class.hs) => $hackage-root/../mtl-2.2.1/Control/Monad/Cont/Class.hs"
  (-if-let* ((_ (eq major-mode 'haskell-mode))
             (root (helm-kythe--haskell-find-project-root (buffer-file-name)))
             (f (concat (file-name-directory root) (caddr triple)))
             (_ (file-exists-p f)))
      f))

(defun helm-kythe--triple-to-filename-search-path (path ticket)
  "Find filename given Kythe triple and ticket.
(\"\" \"\" /absolute/file) => /absolute/file
(\"\" \"\" a/b.cc) => $search_path/a/b.cc"
  (if (file-name-absolute-p path)
      (when (file-exists-p path) path)
    (cl-loop for (prefix corpus root) in helm-kythe-filesystem-path-to-corpus-root do
             (-if-let* ((_ (and (equal corpus (car ticket)) (equal root (cadr ticket))))
                        (f (concat (file-name-as-directory prefix) path))
                        (_ (file-exists-p f)))
                 (cl-return f)))))

(defun helm-kythe--reference-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-reference) (alist-get 'source_ticket)))

(defun helm-kythe--reference-target-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-reference) (alist-get 'target_ticket)))

(defun helm-kythe--ticket-equal? (ticket0 ticket1)
  "Return t if two Kythe tickets are identical."
  (and ticket1 (string= (replace-regexp-in-string "%3A" "/" ticket0 t) (replace-regexp-in-string "%3A" "/" ticket1 t))))

(defun helm-kythe-post (path data)
  (let ((url-request-method "post")
        (url-request-extra-headers '(("content-type" . "application/json")))
        (url-request-data (json-encode data)))
    (with-current-buffer (url-retrieve-synchronously (concat helm-kythe-http-server-url path) t)
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (json-read-from-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
        (json-error
         (signal 'helm-kythe-error (concat "Kythe http_server error: " (string-trim (buffer-substring-no-properties (point) (point-max))))))
        (error
         (signal 'helm-kythe-error (concat "Kythe http_server error")))
        ))))

(defun helm-kythe--propertize-mode-line (face)
  (propertize " Kythe" 'face face))

(defun helm-kythe--source-definitions ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-kythe--reference-target-at-point))
               (defs (helm-kythe-get-definitions ticket)))
      (helm-init-candidates-in-buffer 'global (helm-kythe--anchors-to-candidates defs))
    (message "No definitions")))

;; TODO No results. Why?
(defun helm-kythe--source-definitions-prompt ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-comp-read "Ticket: " '()))
               (defs (helm-kythe-get-definitions ticket)))
      (helm-init-candidates-in-buffer 'global (helm-kythe--anchors-to-candidates defs))
    (message "No definitions")))

(defun helm-kythe--source-imenu ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (let ((anchors))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((def (get-text-property (point) 'helm-kythe-definition))
              (next-change
               (or (next-single-property-change (point) 'helm-kythe-definition)
                   (point-max))))
          (when def (push def anchors))
          (goto-char next-change))))
    (helm-init-candidates-in-buffer 'global (helm-kythe--anchors-to-candidates (nreverse anchors)))))

(defun helm-kythe--source-references ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-kythe--reference-target-at-point))
               (refs (helm-kythe-get-references ticket)))
      (helm-init-candidates-in-buffer 'global (helm-kythe--anchors-to-candidates refs))
    (message "No references")))

;; TODO No results. Why?
(defun helm-kythe--source-references-prompt ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-comp-read "Ticket: " '()))
             (refs (helm-kythe-get-references ticket)))
      (helm-init-candidates-in-buffer 'global (helm-kythe--anchors-to-candidates refs))
    (message "No references")))

(defmacro helm-kythe--with-evil-jumps (&rest body)
  "Make `evil-jumps.el' commands work on `helm-kythe--jumps'."
  (declare (indent 1))
  `(let ((evil--jumps-window-jumps ,helm-kythe--jumps))
     ,@body
     ))

(defun helm-kythe-post-decorations (ticket)
  (helm-kythe-post "/decorations"
              `((location . ((ticket . ,ticket)))
                (references . t)
                (target_definitions . t))))

(defun helm-kythe-post-dir (path)
  (condition-case ex
      (helm-kythe-post "/dir" `((path . ,path)))
    (helm-kythe-error (error "%s: %s" (nth 1 (backtrace-frame 4)) (cdr ex)))))

(defun helm-kythe-post-xrefs (ticket data)
  (condition-case ex
      (helm-kythe-post "/xrefs"
                    `((anchor_text . t)
                      (ticket . [,ticket])
                      ,@data
                      ))
    (helm-kythe-error (error "%s: %s" (nth 1 (backtrace-frame 4)) (cdr ex)))))

(defun helm-kythe-apply-decorations ()
  "Fetch cross references information and decorate definitions/references with text properties."
  (interactive)
  (helm-kythe-remove-decorations)
  (with-silent-modifications
    (cl-loop for func in helm-kythe-filename-to-triple-functions do
             (when-let (triple (funcall func (buffer-file-name)))
               (condition-case ex
                   (helm-kythe-decorations triple)
                 (helm-kythe-error
                  (when (called-interactively-p 'any)
                    (error "helm-kythe-apply-decorations: %s" (cdr ex)))))
               (cl-return)))))

;; .cross_references | values[].definition[].anchor
(defun helm-kythe-get-definitions (ticket)
  (when-let (xrefs (-some->> (helm-kythe-post-xrefs ticket '((definition_kind . "BINDING_DEFINITIONS")))
                             (assoc 'cross_references) (cdr)))
    (-mapcat (lambda (xref)
               (when-let (defs (alist-get 'definition (cdr xref)))
                 (mapcar (lambda (x) (alist-get 'anchor x)) defs)
                 )) xrefs)))

;; .cross_references | values[].reference[].anchor
(defun helm-kythe-get-references (ticket)
  (when-let (xrefs (-some->> (helm-kythe-post-xrefs ticket '((reference_kind . "ALL_REFERENCES")))
                             (assoc 'cross_references) (cdr)))
    (-mapcat (lambda (xref)
                   (when-let (refs (alist-get 'reference (cdr xref)))
                     ;; 'start' of /kythe/edge/ref/doc do not have column_offset
                     ;; (-filter (lambda (x) (not (equal (alist-get 'kind x) "/kythe/edge/ref/doc"))) (mapcar (lambda (x) (alist-get 'anchor x)) refs))
                     (mapcar (lambda (x) (alist-get 'anchor x)) refs)
                     )) xrefs)))

(defun helm-kythe-get-xrefs (ticket)
  (-some->> (helm-kythe-post-xrefs ticket '((declaration_kind . "ALL_DECLARATIONS")
                              (definition_kind . "BINDING_DEFINITIONS")
                              (reference_kind . "ALL_REFERENCES")
                              ))
           (assoc 'cross_references) (cdr)))

(defvar helm-kythe--mouse-map-definition (make-sparse-keymap))
(defvar helm-kythe--mouse-map-reference (make-sparse-keymap))
(define-key helm-kythe--mouse-map-definition [mouse-1] (lambda () (interactive) (set-mark nil) (helm-kythe-find-definitions)))
(define-key helm-kythe--mouse-map-reference [mouse-1] (lambda () (interactive) (set-mark nil) (helm-kythe-find-references)))

(defun helm-kythe-decorations (triple)
  (-let (((corpus root path) triple))
    (when-let (refs (helm-kythe-post-decorations (format "kythe://%s%s?path=%s" corpus (if (string-empty-p root) "" (concat "?root=" root)) path)))
      (mapc (lambda (ref)
              (-let [(start . end) (helm-kythe--char-offsets ref 'anchor_start 'anchor_end)]
                (when end
                  (add-text-properties start end `(helm-kythe-reference ,ref keymap ,helm-kythe--mouse-map-definition mouse-face highlight))
                  )))
            (alist-get 'reference refs))
      (mapc (lambda (ticket-def)
              (-let [def (cdr ticket-def)]
                ;; cxx_extractor or cxx_indexer, "definition_locations" of a.cc may include tickets of "stdio.h"
                (when (equal triple (helm-kythe--ticket-to-triple (alist-get 'ticket def)))
                  (-let [(start . end) (helm-kythe--char-offsets def 'start 'end)]
                    (when end
                      (add-text-properties start end `(helm-kythe-definition ,def keymap ,helm-kythe--mouse-map-reference mouse-face highlight)))))))
            (alist-get 'definition_locations refs))
      (setq-local helm-kythe-decorated t)
      nil)))

(defun helm-kythe-dwim ()
  (interactive)
  (if (helm-kythe--definition-at-point)
      (helm-kythe-find-references)
    (helm-kythe-find-definitions)))

(defun helm-kythe-eldoc-clear-cache ()
  (interactive)
  (clrhash helm-kythe--eldoc-cache))

(defun helm-kythe-eldoc-function ()
  (when-let (ticket (helm-kythe--reference-target-at-point))
    (-let [doc (or
                (gethash ticket helm-kythe--eldoc-cache)
                (when-let (defs (helm-kythe-get-definitions ticket))
                  (puthash ticket (alist-get 'snippet (car defs)) helm-kythe--eldoc-cache)))]
      (when doc
        (pcase major-mode
          ((or 'c-mode 'c++-mode) (helm-kythe--fontify c++-mode doc))
          ('go-mode (helm-kythe--fontify go-mode doc))
          ('haskell-mode (helm-kythe--fontify haskell-mode doc))
          ('java-mode (helm-kythe--fontify java-mode doc))
          ('python-mode (helm-kythe--fontify python-mode doc))
          (_ doc))))))

(defun helm-kythe-find-definitions ()
  (interactive)
  (helm-kythe--common '(helm-kythe-source-definitions) (helm-kythe--definition-at-point)))

(defun helm-kythe-find-definitions-other-window ()
  (interactive)
  (-let [helm-kythe--use-otherwin t]
    (helm-kythe-find-definitions)))

(defun helm-kythe-find-definitions-prompt ()
  (interactive)
  (helm-kythe--common '(helm-kythe-source-definitions-prompt)))

(defun helm-kythe-find-references ()
  (interactive)
  (helm-kythe--common '(helm-kythe-source-references) (helm-kythe--reference-at-point)))

(defun helm-kythe-find-references-other-window ()
  (interactive)
  (-let [helm-kythe--use-otherwin t]
    (helm-kythe-find-references)))

(defun helm-kythe-find-references-prompt ()
  (interactive)
  (helm-kythe--common '(helm-kythe-source-references-prompt)))

(if (featurep 'evil-jumps)
    (progn
      (evil-define-motion helm-kythe-jump-backward (count)
        (helm-kythe--with-evil-jumps
            (evil--jump-backward count)))

      (evil-define-motion helm-kythe-jump-forward (count)
        (helm-kythe--with-evil-jumps
            (evil--jump-forward count))))

  (defun helm-kythe-jump-backward (count)
    (error "Install `evil-jumps' to enable this feature"))

  (defun helm-kythe-jump-forward (count)
    (error "Install `evil-jumps' to enable this feature")))

(defun helm-kythe-imenu ()
  (interactive)
  (helm-kythe--common '(helm-kythe-source-imenu) (helm-kythe--definition-at-point)))

(defun helm-kythe-remove-decorations ()
  (interactive)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(helm-kythe-definition nil helm-kythe-reference nil keymap nil mouse-face nil))
    (setq-local helm-kythe-decorated nil)))

(defun helm-kythe-resume ()
  "Resume a previous `helm-kythe` session."
  (interactive)
  (unless (get-buffer helm-kythe--buffer)
    (error "Error: helm-kythe buffer does not exist"))
  (helm-resume helm-kythe--buffer))

(defvar helm-kythe-map (make-sparse-keymap))
(defvar helm-kythe-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-kythe-mode ()
  "Enable helm-kythe-mode"
  :init-value nil
  :lighter (:eval (helm-kythe--mode-line))
  :global nil
  :keymap helm-kythe-mode-map
  (cond
   (helm-kythe-mode
    (add-function :before-until (local 'eldoc-documentation-function) #'helm-kythe-eldoc-function)
    (helm-kythe-apply-decorations))
   (t
    (remove-function (local 'eldoc-documentation-function)
                     #'helm-kythe-eldoc-function)
    (helm-kythe-remove-decorations))))

(cl-loop
 for (key . command) in
 '(("a" . helm-kythe-apply-decorations)
   ("d" . helm-kythe-find-definitions)
   ;; ("D" . helm-kythe-find-definitions-prompt)
   ("i" . helm-kythe-imenu)
   ("r" . helm-kythe-find-references)
   ;; ("R" . helm-kythe-find-references-prompt)
   ("l" . helm-kythe-resume)
   ("C-d" . helm-kythe-find-definitions-other-window)
   ("C-i" . helm-kythe-jump-forward)
   ("C-o" . helm-kythe-jump-backward)
   ("C-r" . helm-kythe-find-references-other-window))
 do
 (define-key helm-kythe-map (kbd key) command))

(when helm-kythe-prefix-key
  (define-key helm-kythe-mode-map helm-kythe-prefix-key helm-kythe-map))

(easy-menu-define helm-kythe-menu helm-kythe-mode-map "helm-kythe menu"
  '("Kythe"
    ["Apply decorations" helm-kythe-apply-decorations t]
    ["Clear eldoc cache" helm-kythe-eldoc-clear-cache t]
    ["Find definitions" helm-kythe-find-definitions t]
    ["Find references" helm-kythe-find-references t]
    ["imenu" helm-kythe-imenu t]
    ["Resume" helm-kythe-resume t]
    "---"
    ["Jump backward" helm-kythe-jump-backward t]
    ["Jump forward" helm-kythe-jump-forward t]
    "---"
    ["Customize" (customize-group 'helm-kythe)]
    ))

(provide 'helm-kythe)

;;; helm-kythe.el ends here
