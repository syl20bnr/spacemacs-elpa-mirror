;;; bbcode-mode.el --- Major mode for writing BBCode markup
;;
;; Copyright 2012, 2013 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz <lobbyjones@gmail.com>
;; URL: https://github.com/ejmr/bbcode-mode
;; Package-Version: 2.0.0
;; Version: 2.0.0
;;
;;
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
;;
;;
;;; Commentary:
;;
;; Put this file in your Emacs lisp path (i.e. site-lisp) and add
;; this to your `.emacs' file:
;;
;;     (require 'bbcode-mode)
;;
;; Files with the '.bbcode' extension automatically enable
;; bbcode-mode.  No other extensions are associated with the mode.

;;; Code:

(defconst bbcode-mode-version-number "2.0.0"
  "BBCode Mode version number.")

(defun bbcode/make-tag-regex (tag)
  "Makes a regular expression that matches the given `tag' name.
The expression contains no capture groups."
  (unless (stringp tag)
    (error "Requires a string but called with %s" tag))
  (format "\\[%s.*?\\].*?\\[/%s\\]" tag tag))

(defconst bbcode/font-lock-keywords
  (list
   `(,(bbcode/make-tag-regex "attachment") . 'font-lock-variable-face)
   `(,(bbcode/make-tag-regex "b") . 'bold)
   `(,(bbcode/make-tag-regex "center") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "code") . 'font-lock-function-name-face)
   `(,(bbcode/make-tag-regex "color") . 'font-lock-variable-name-face)
   `(,(bbcode/make-tag-regex "email") . 'link)
   `(,(bbcode/make-tag-regex "gvideo") . 'font-lock-variable-name-face)
   `(,(bbcode/make-tag-regex "i") . 'italic)
   `(,(bbcode/make-tag-regex "img") . 'link)
   `(,(bbcode/make-tag-regex "li") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "list") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "ol") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "quote") . 'font-lock-doc-face)
   `(,(bbcode/make-tag-regex "s") . 'default)
   `(,(bbcode/make-tag-regex "size") . 'font-lock-variable-name-face)
   `(,(bbcode/make-tag-regex "table") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "td") . 'font-lock-variable-name-face)
   `(,(bbcode/make-tag-regex "th") . 'bold)
   `(,(bbcode/make-tag-regex "tr") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "u") . 'underline)
   `(,(bbcode/make-tag-regex "ul") . 'font-lock-keyword-face)
   `(,(bbcode/make-tag-regex "url") . 'link)
   `(,(bbcode/make-tag-regex "youtube") . 'font-lock-variable-name-face))
  "Regular expressions to highlight BBCode markup.")

(defun bbcode/insert-tag (prefix start end tag)
  "Inserts a pair of `tag' in the buffer at the current point and
then places the point in the middle of the tags.  The tag will be
wrapped around the points `start' and `end' if the user has
selected a region.  If the function is called with the universal
prefix argument then the point will be placed in the opening tag
so the user can enter any attributes."
  (interactive "PrMTag: ")
  (if (use-region-p)
      (progn
        (kill-region start end)
        (insert (format "[%s]" tag))
        (yank)
        (insert (format "[/%s]" tag)))
    (insert (format "[%s][/%s]" tag tag)))
  (if prefix
      ;; If the user called the command with the 'C-u' prefix then we
      ;; need to find the first tag in the pair instead of the second
      ;; like usual.  And we want the end of the tag.  But if we just
      ;; search backwards for ']' we will get the second tag.  Since
      ;; the point is on that character now we can (backward-char)
      ;; once and *then* search backwards to get the proper position.
      (progn
        (backward-char)
        (search-backward "]")
        ;; Since we are entering an attribute go ahead and add the
        ;; (typically) mandatory '=' character.
        (insert "="))
    (search-backward "[")))

;;;###autoload
(define-derived-mode bbcode-mode text-mode "BBCode"
  "Major mode for writing BBCode markup.

\\{bbcode-mode-map}"
  ;; Setup font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(bbcode/font-lock-keywords nil t))
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

(defmacro bbcode/make-key-binding (key tag)
  "Binds the sequence `key', which must be a valid argument for
the macro `kbd', to a function that will insert `tag' into the
buffer."
  (let ((function-name (intern (format "bbcode/insert-tag-%s" tag))))
  `(progn
     (defun ,function-name (prefix)
       ,(format "Insert the [%s] tag at point or around the current region" tag)
       (interactive "P")
       (if (use-region-p)
           (bbcode/insert-tag prefix (region-beginning) (region-end) ,tag)
         (bbcode/insert-tag prefix nil nil ,tag)))
     (define-key bbcode-mode-map (kbd ,key) ',function-name))))

;; Keys that insert most tags are prefixed with 'C-c C-t'.
(bbcode/make-key-binding "C-c C-t b" "b")
(bbcode/make-key-binding "C-c C-t c" "code")
(bbcode/make-key-binding "C-c C-t d" "del")
(bbcode/make-key-binding "C-c C-t e" "email")
(bbcode/make-key-binding "C-c C-t i" "i")
(bbcode/make-key-binding "C-c C-t l" "url")
(bbcode/make-key-binding "C-c C-t m" "img")
(bbcode/make-key-binding "C-c C-t n" "center")
(bbcode/make-key-binding "C-c C-t q" "quote")
(bbcode/make-key-binding "C-c C-t s" "s")
(bbcode/make-key-binding "C-c C-t u" "u")

;; Keys related to modifying font properties begin with 'C-c C-f'.
(bbcode/make-key-binding "C-c C-f c" "color")
(bbcode/make-key-binding "C-c C-f s" "size")

;; Keys for creating lists begin with 'C-c C-l'.
(bbcode/make-key-binding "C-c C-l i" "li")
(bbcode/make-key-binding "C-c C-l l" "list")
(bbcode/make-key-binding "C-c C-l o" "ol")
(bbcode/make-key-binding "C-c C-l u" "ul")

;; Keys for tables begin with 'C-c C-b'
(bbcode/make-key-binding "C-c C-b d" "td")
(bbcode/make-key-binding "C-c C-b h" "th")
(bbcode/make-key-binding "C-c C-b r" "tr")
(bbcode/make-key-binding "C-c C-b t" "table")

;; Keys for special, uncommon tags begin with 'C-c C-s'.
(bbcode/make-key-binding "C-c C-s a" "attachment")
(bbcode/make-key-binding "C-c C-s g" "gvideo")
(bbcode/make-key-binding "C-c C-s m" "manual")
(bbcode/make-key-binding "C-c C-s w" "wiki")
(bbcode/make-key-binding "C-c C-s y" "youtube")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bbcode$" . bbcode-mode))

(provide 'bbcode-mode)

;;; bbcode-mode.el ends here
