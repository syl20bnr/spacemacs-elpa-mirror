;;; bbcode-mode.el --- Major mode for writing BBCode markup
;;
;; Copyright 2012, 2013, 2014 Eric James Michael Ritz
;; Copyright 2018 Lassi Kortela
;;
;; Author: Eric James Michael Ritz <lobbyjones@gmail.com>
;; Maintainer: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/bbcode-mode
;; Package-Version: 20180316.519
;; Version: 2.1.0
;; Package-Requires: ()
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
;; Put this file in your Emacs Lisp path (i.e. site-lisp) and add
;; this to your `.emacs' file:
;;
;;     (require 'bbcode-mode)
;;
;; Files with the '.bbcode' extension automatically enable
;; bbcode-mode.  No other extensions are associated with the mode.

;;; Code:

;; Keys that insert most tags are prefixed with 'C-c C-t'.
;; Keys related to modifying font properties begin with 'C-c C-f'.
;; Keys for creating lists begin with 'C-c C-l'.
;; Keys for tables begin with 'C-c C-b'
;; Keys for special, uncommon tags begin with 'C-c C-s'.
(defconst bbcode-tags
  '(("*"           font-lock-keyword-face        "C-c C-l *")
    ("attachment"  font-lock-variable-face       "C-c C-s a")
    ("b"           bold                          "C-c C-t b")
    ("center"      font-lock-keyword-face        "C-c C-t n")
    ("code"        font-lock-function-name-face  "C-c C-t c")
    ("color"       font-lock-variable-name-face  "C-c C-f c")
    ("del"         default                       "C-c C-t d")
    ("email"       link                          "C-c C-t e")
    ("font"        font-lock-variable-name-face  "C-c C-f f")
    ("gvideo"      font-lock-variable-name-face  "C-c C-s g")
    ("i"           italic                        "C-c C-t i")
    ("img"         link                          "C-c C-t m")
    ("li"          font-lock-keyword-face        "C-c C-l i")
    ("list"        font-lock-keyword-face        "C-c C-l l")
    ("manual"      font-lock-variable-name-face  "C-c C-s m")
    ("ol"          font-lock-keyword-face        "C-c C-l o")
    ("quote"       font-lock-doc-face            "C-c C-t q")
    ("s"           default                       "C-c C-t s")
    ("size"        font-lock-variable-name-face  "C-c C-f s")
    ("table"       font-lock-keyword-face        "C-c C-b t")
    ("td"          font-lock-variable-name-face  "C-c C-b d")
    ("th"          bold                          "C-c C-b h")
    ("tr"          font-lock-keyword-face        "C-c C-b r")
    ("u"           underline                     "C-c C-t u")
    ("ul"          font-lock-keyword-face        "C-c C-l u")
    ("url"         link                          "C-c C-t l")
    ("wiki"        font-lock-variable-name-face  "C-c C-s w")
    ("youtube"     font-lock-variable-name-face  "C-c C-s y")))

(defconst bbcode-font-lock-keywords
  `(;; Opening tag.
    (,(concat (regexp-quote "[")
              (regexp-opt (mapcar #'car bbcode-tags) t)
              (regexp-quote "]"))
     (0 font-lock-keyword-face))
    ;; Opening tag with attribute.
    (,(concat (regexp-quote "[")
              (regexp-opt (mapcar #'car bbcode-tags) t)
              (concat "=" "\"?" "\\(.*?\\)" "\"?")
              (regexp-quote "]"))
     (0 font-lock-keyword-face)
     (2 font-lock-preprocessor-face t))
    ;; Closing tag.
    (,(concat (regexp-quote "[/")
              (regexp-opt (mapcar #'car bbcode-tags) t)
              (regexp-quote "]"))
     (0 font-lock-keyword-face)))
  "Regular expressions to highlight BBCode markup.")

(defun bbcode-insert-tag (prefix tag)
  "Insert BBCode tag pair at point.

If the region is active then the tag is inserted around the
region.  Point is placed between the tags so you can start typing
text there.  With the PREFIX argument, point is placed inside the
opening tag so you can enter attributes for the tag.

TAG is the name of the tag to insert."
  (interactive "PMTag: ")
  (let ((opening-tag (format "[%s%s]" tag (if prefix "=" "")))
        (closing-tag (format "[/%s]" tag))
        (between-tags "")
        start end)
    (when (use-region-p)
      (setq start (region-beginning) end (region-end))
      (setq between-tags (buffer-substring start end))
      (goto-char start)
      (delete-region start end))
    (setq start (point))
    (insert (concat opening-tag between-tags closing-tag))
    (deactivate-mark)
    (cond (prefix
           (set-mark (goto-char (+ start (1- (length opening-tag))))))
          (t
           (set-mark (+ start (length opening-tag)))
           (goto-char (+ start (length opening-tag) (length between-tags)))))))

;;;###autoload
(define-derived-mode bbcode-mode text-mode "BBCode"
  "Major mode for writing BBCode markup.

\\{bbcode-mode-map}"
  ;; Setup font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(bbcode-font-lock-keywords nil t))
  (set (make-local-variable 'font-lock-multiline) t)
  (font-lock-mode 1)
  ;; The most commonly predicted use-case for this mode is writing
  ;; text that will be posted on a website forum.  Those forum
  ;; programs automatically turn newlines into <br/> tags, which is
  ;; not what we want.  But we still want automatic newlines for
  ;; paragraphs as we write.  So we disable auto-fill-mode in order to
  ;; avoid actual newlines, but enable visual-line-mode so that text
  ;; is automatically wrapped for readability.
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defmacro bbcode-make-key-binding (key tag)
  "Bind the sequence KEY to insert TAG into the buffer.

KEY must be a valid argument for the macro `kbd'."
  (let ((function-name (intern (format "bbcode-insert-tag-%s" tag))))
    `(progn
       (defun ,function-name (prefix)
         ,(format "Insert the [%s] tag at point or around the current region" tag)
         (interactive "P")
         (bbcode-insert-tag prefix ,tag))
       (define-key bbcode-mode-map (kbd ,key) ',function-name))))

(dolist (spec bbcode-tags)
  (let ((tag (nth 0 spec)) (key (nth 2 spec)))
    (eval `(bbcode-make-key-binding ,key ,tag))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bbcode$" . bbcode-mode))

(provide 'bbcode-mode)

;;; bbcode-mode.el ends here
