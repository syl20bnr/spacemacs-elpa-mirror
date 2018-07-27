;;; bbcode-mode.el --- Major mode for phpBB posts (BBCode markup) -*- lexical-binding: t -*-
;;
;; Copyright 2012, 2013, 2014 Eric James Michael Ritz
;; Copyright 2018 Lassi Kortela
;;
;; Author: Eric James Michael Ritz <lobbyjones@gmail.com>
;; Maintainer: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/bbcode-mode
;; Package-Version: 20180505.927
;; Version: 2.1.0
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: bbcode languages
;; License: GPL
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;; Commentary:
;;
;; This major mode lets you write phpBB forum posts in Emacs. It
;; implements syntax highlighting and keyboard commands for BBCode
;; (Bulletin Board Code), the markup language used by phpBB.
;;
;; Do M-x bbcode-scratch to instantly get a temp buffer to write
;; posts. Use M-x bbcode-mode to switch an existing buffer to BBCode
;; mode. The file name extension .bbcode is also recognized.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))

;; Keys that insert most tags are prefixed with 'C-c C-t'.
;; Keys related to modifying font properties begin with 'C-c C-f'.
;; Keys for creating lists begin with 'C-c C-l'.
;; Keys for tables begin with 'C-c C-b'
;; Keys for special, uncommon tags begin with 'C-c C-s'.
(eval-and-compile
  (defconst bbcode-tags
    '(("*"           nil                           "C-c C-l *" nil)
      ("attachment"  font-lock-variable-name-face  "C-c C-s a" 1)
      ("b"           bold                          "C-c C-t b" 1)
      ("center"      nil                           "C-c C-t n" 1)
      ("code"        font-lock-function-name-face  "C-c C-t c" t)
      ("color"       font-lock-variable-name-face  "C-c C-f c" 1 color)
      ("del"         nil                           "C-c C-t d" 1)
      ("email"       link                          "C-c C-t e" 1)
      ("font"        font-lock-variable-name-face  "C-c C-f f" 1)
      ("gvideo"      font-lock-variable-name-face  "C-c C-s g" 1)
      ("i"           italic                        "C-c C-t i" 1)
      ("img"         link                          "C-c C-t m" 1 width height)
      ("li"          font-lock-keyword-face        "C-c C-l i" 1)
      ("list"        nil                           "C-c C-l l" t)
      ("manual"      font-lock-variable-name-face  "C-c C-s m" 1)
      ("ol"          nil                           "C-c C-l o" t)
      ("quote"       nil                           "C-c C-t q" t name)
      ("s"           nil                           "C-c C-t s" 1)
      ("size"        font-lock-variable-name-face  "C-c C-f s" 1 size)
      ("style"       nil                           "C-C C-f y" 1 color size)
      ("table"       nil                           "C-c C-b t" t)
      ("td"          font-lock-variable-name-face  "C-c C-b d" 1)
      ("th"          bold                          "C-c C-b h" 1)
      ("tr"          nil                           "C-c C-b r" 1)
      ("u"           underline                     "C-c C-t u" 1)
      ("ul"          nil                           "C-c C-l u" t)
      ("url"         link                          "C-c C-t l" 1 url)
      ("wiki"        link                          "C-c C-s w" 1)
      ("youtube"     link                          "C-c C-s y" 1))))

(eval-and-compile
  (defconst bbcode-font-lock-keywords
    `(;; Opening tag.
      (,(concat (regexp-quote "[")
                (regexp-opt (mapcar #'car bbcode-tags) t)
                "]")
       (0 'font-lock-keyword-face))
      ;; Opening tag with attribute.
      (,(concat (regexp-quote "[")
                (regexp-opt (mapcar #'car bbcode-tags) t)
                "[ =]\\(.*?\\)"
                "]")
       (0 'font-lock-keyword-face)
       (2 'font-lock-preprocessor-face t))
      ;; Closing tag.
      (,(concat (regexp-quote "[/")
                (regexp-opt (mapcar #'car bbcode-tags) t)
                "]")
       (0 'font-lock-keyword-face))
      ;; Highlight the body of some tags with a tag-specific face
      ,@(let (patterns (face->tags (make-hash-table)))
          (dolist (tag-spec bbcode-tags)
            (let* ((tag (nth 0 tag-spec)) (face (nth 1 tag-spec)))
              (puthash face (cons tag (gethash face face->tags)) face->tags)))
          (maphash (lambda (face tags)
                     (when face
                       (push `(,(concat (regexp-quote "[")
                                        (regexp-opt tags t)
                                        "]"
                                        "\\([^][]+\\)"
                                        (regexp-quote "["))
                               (2 ',face t))
                             patterns)))
                   face->tags)
          patterns))
    "Regular expressions to highlight BBCode markup."))

(defun bbcode-quote-attribute-value (value)
  "Put quotes around BBCode tag attribute VALUE if needed."
  (save-match-data
    (if (string-match "[^A-Za-z0-9]" value)
        (concat "\"" value "\"")
      value)))

(defun bbcode-quote-attributes (attributes)
  "Quote one or more BBCode tag ATTRIBUTES to put inside [tag].

If ATTRIBUTES is a string, it denotes a single attribute VALUE.
Otherwise ATTRIBUTES must be a list of (NAME . VALUE) pairs.
Attributes with blank values are pruned."
  (if (stringp attributes)
      (if (equal "" attributes) ""
        (format "=%s" (bbcode-quote-attribute-value attributes)))
    (mapconcat (lambda (attr)
                 (cl-destructuring-bind (name . value) attr
                   (if (equal "" value) ""
                     (format " %s=%s" name
                             (bbcode-quote-attribute-value value)))))
               attributes "")))

(defun bbcode-insert-tag (tag body attributes)
  "Insert the BBCode tag named TAG at point.

BODY is 1 for a one-line tag, t for a multi-line tag, or nil to
omit the closing tag entirely.

ATTRIBUTES is either VALUE or a list of (NAME . VALUE) pairs.

If the region is active then the tag is inserted around the
region.  Point is placed between the tags so you can start typing
text there."
  (let ((opening-tag (format "[%s%s]" tag (bbcode-quote-attributes attributes)))
        (closing-tag (if body (format "[/%s]" tag) ""))
        (between-tags (if (equal t body) "\n\n" ""))
        (body-offset (if (equal t body) 1 0))
        start)
    (when (use-region-p)
      (let (end)
        (setq start (region-beginning) end (region-end)
              between-tags (buffer-substring start end)
              body-offset (length between-tags))
        (goto-char start)
        (delete-region start end)))
    (setq start (point))
    (insert (concat opening-tag between-tags closing-tag))
    (deactivate-mark)
    (set-mark (+ start (length opening-tag)))
    (goto-char (+ (mark) body-offset))))

;;;###autoload
(define-derived-mode bbcode-mode text-mode "BBCode"
  "Major mode for writing BBCode markup.

\\{bbcode-mode-map}"
  ;; Setup font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(bbcode-font-lock-keywords nil t))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; The most commonly predicted use-case for this mode is writing
  ;; text that will be posted on a website forum.  Those forum
  ;; programs automatically turn newlines into <br/> tags, which is
  ;; not what we want.  But we still want automatic newlines for
  ;; paragraphs as we write.  So we disable auto-fill-mode in order to
  ;; avoid actual newlines, but enable visual-line-mode so that text
  ;; is automatically wrapped for readability.
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defmacro bbcode-define-insert-tag-commands ()
  "Define insert-tag-* commands and key bindings for `bbcode-mode'."
  `(progn
     ,@(cl-mapcan
        (lambda (tag-spec)
          (cl-destructuring-bind (tag _face key body . attrs) tag-spec
            (let ((function-name (intern (format "bbcode-insert-tag-%s" tag)))
                  (insert-tag (format "MInsert BBCode tag: [%s" tag)))
              `((defun ,function-name ,attrs
                  ,(format "Insert the [%s] tag at point or %s the region."
                           tag (if body "around" "before"))
                  (interactive
                   ,(if (= 1 (length attrs))
                        (concat insert-tag "=")
                      (mapconcat (lambda (attr)
                                   (format "%s %s=" insert-tag attr))
                                 attrs "\n")))
                  (bbcode-insert-tag
                   ,tag ,body
                   ,(if (= 1 (length attrs))
                        (car attrs)
                      `(list ,@(mapcar (lambda (attr)
                                         `(cons ,(format "%s" attr) ,attr))
                                       attrs)))))
                (define-key bbcode-mode-map (kbd ',key) ',function-name)))))
        bbcode-tags)))

(bbcode-define-insert-tag-commands)

;;;###autoload
(defun bbcode-scratch ()
  "Open *bbcode-scratch* buffer to quickly edit BBCode posts."
  (interactive)
  (switch-to-buffer (get-buffer-create "*bbcode-scratch*"))
  (unless (equal 'bbcode-mode major-mode)
    (bbcode-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bbcode$" . bbcode-mode))

(provide 'bbcode-mode)

;;; bbcode-mode.el ends here
