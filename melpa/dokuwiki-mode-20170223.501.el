;;; dokuwiki-mode.el --- Major mode for DokuWiki document

;; Copyright (C)  2013-2017 Tsunenobu Kai

;; Author: Tsunenobu Kai <kai2nenobu@gmail.com>
;; URL: https://github.com/kai2nenobu/emacs-dokuwiki-mode
;; Package-Version: 20170223.501
;; Version: 0.1.1
;; Keywords: hypermedia text DokuWiki

;; This file is not part of GNU Emacs.

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

;;; Code:

(defgroup dokuwiki-mode nil
  "Major mode for DokuWiki document."
  :group 'text
  :group 'dokuwiki
  :tag "DokuWiki"
  :link '(url-link "https://www.dokuwiki.org/dokuwiki"))

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
    (define-key map (kbd "C-c C-u") 'outline-up-heading)
    (define-key map (kbd "C-c C-@") 'outline-mark-subtree)
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-smiley-list
  '("8-)" "8-O" ":-(" ":-)" "=) " ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME")
  "Smiley list in DokuWiki.")

(defvar dokuwiki-outline-regexp " ?\\(=\\{2,6\\}\\)"
  "Regexp which indicates headline in DokuWiki.
See also `outline-regexp'.")

;;;; Faces
(defface dokuwiki-box '((t (:box t)))
  "Face enabled box property")

(defface dokuwiki-code '((t (:inherit shadow)))
  "DokuWiki face for code."
  :group 'dokuwiki)

(defface dokuwiki-list '((t (:inherit font-lock-type-face)))
  "DokuWiki face for list."
  :group 'dokuwiki)

(defface dokuwiki-verbatim '((t (:inherit shadow)))
  "DokuWiki face for text as is."
  :group 'dokuwiki)

(defface dokuwiki-footnote '((t (:inherit font-lock-builtin-face)))
  "DokuWiki face for footnote."
  :group 'dokuwiki)

(defface dokuwiki-headline-1 '((t (:inherit outline-1)))
  "DokuWiki face for level 1 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-2 '((t (:inherit outline-2)))
  "DokuWiki face for level 2 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-3 '((t (:inherit outline-3)))
  "DokuWiki face for level 3 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-4 '((t (:inherit outline-4)))
  "DokuWiki face for level 4 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-5 '((t (:inherit outline-5)))
  "DokuWiki face for level 5 headline."
  :group 'dokuwiki)

(defface dokuwiki-link '((t (:inherit link)))
  "DokuWiki face for link."
  :group 'dokuwiki)

(defface dokuwiki-image '((t (:inherit font-lock-variable-name-face)))
  "DokuWiki face for image."
  :group 'dokuwiki)

(defface dokuwiki-table '((t (:inherit font-lock-function-name-face)))
  "DokuWiki face for table."
  :group 'dokuwiki)

(defface dokuwiki-smiley '((t (:inherit font-lock-constant-face)))
  "DokuWiki face for smiley."
  :group 'dokuwiki)

(defvar dokuwiki-font-lock-keywords
  `(
   ;; bold
   ("\\*\\*.+?\\*\\*" (0 'bold append))
   ;; italic
   ("//.+?//" . (0 'italic append))
   ;; underline
   ("__.+?__" . (0 'underline append))
   ;; monospace
   ("''\\(.+?\\)''" (0 'dokuwiki-code append) (1 'dokuwiki-box append))
   ;; verbatim
   ("%%.+?%%" (0 'dokuwiki-code t))
   ;; footnote
   ("((.+?))" (0 'dokuwiki-footnote))
   ;; headline
   (" ?======.+======[ \t]*$" (0 'dokuwiki-headline-1))
   (" ?=====.+=====[ \t]*$" (0 'dokuwiki-headline-2))
   (" ?====.+====[ \t]*$" (0 'dokuwiki-headline-3))
   (" ?===.+===[ \t]*$" (0 'dokuwiki-headline-4))
   (" ?==.+==[ \t]*$" (0 'dokuwiki-headline-5))
   ;; link
   ("\\[\\[[^|]+?\\(?:\\(|\\)\\(.*?\\)\\)?\\]\\]"
    (0 'dokuwiki-link) (1 'dokuwiki-code t t)
    (2 'font-lock-string-face t t) (2 'underline append t))
   ("https?://\\(\\([-_.!~*'()a-zA-Z0-9;?:@&=+$,%#]+\\)/?\\)+" (0 'dokuwiki-link))
   ;; image
   ("{{[^|]+?\\(|\\(.*?\\)\\)?}}"
    (0 'dokuwiki-image t)
    (1 'dokuwiki-code t t) (2 'font-lock-string-face t t))
   ;; table
   ("^[ \t]*[|^].*$" (0 'dokuwiki-table))
   ;; linebreak
   ("\\\\\\\\\\s-+" (0 'dokuwiki-code t))
   ;; list
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" "\\([-*]\\).*$" nil nil (1 'dokuwiki-list))
   ;; code block
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" dokuwiki-code-block-search
     nil nil (0 'dokuwiki-code t))
   ;; smiley
   ,@(mapcar #'(lambda (smiley)
                 (list (concat "\\W\\(" (regexp-quote smiley) "\\)\\W")
                       1 'dokuwiki-smiley))
             dokuwiki-smiley-list)
   ))

(defun dokuwiki-code-block-search (limit)
  (if (not (looking-at "[-*]"))
      (re-search-forward ".*$" limit t)))

(defun dokuwiki-outline-level ()
  "Compute a header's nesting level in `dokuwiki-mode'.
See also `outline-level'."
  (when (looking-at outline-regexp)
    (let ((const 7)
          (headline (match-string 1)))
      (- const (length headline)))))

;;;; Work with `outline-magic'
(eval-after-load "outline-magic"
  '(progn
     (define-key dokuwiki-mode-map (kbd "TAB") 'outline-cycle)
     (define-key dokuwiki-mode-map (kbd "<S-tab>")
       '(lambda () (interactive) (outline-cycle '(4))))
     (define-key dokuwiki-mode-map (kbd "<M-S-right>") 'outline-demote)
     (define-key dokuwiki-mode-map (kbd "<M-S-left>") 'outline-promote)
     (define-key dokuwiki-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
     (define-key dokuwiki-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
     (add-hook 'dokuwiki-mode-hook 'dokuwiki-outline-magic-hook)
     ;; Enable outline-magic features in `dokuwiki-mode' buffers
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (eq major-mode 'dokuwiki-mode) (dokuwiki-outline-magic-hook))))
     ))

(defun dokuwiki-outline-magic-hook ()
  "Hook to configure `outline-magic'."
  (set (make-local-variable 'outline-promotion-headings)
       '(("======" . 1) ("=====" . 2) ("====" . 3) ("===" . 4) ("==" . 5)))
  (set (make-local-variable 'outline-cycle-emulate-tab) t))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki document."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  (set (make-local-variable 'outline-regexp) dokuwiki-outline-regexp)
  (set (make-local-variable 'outline-level) 'dokuwiki-outline-level)
  (outline-minor-mode 1)
  )

(provide 'dokuwiki-mode)

;;; dokuwiki-mode.el ends here
