;;; cakecrumbs.el --- Show parents on header for HTML/Jade/Sass/Stylus -*- lexical-binding: t; -*-

;; Copyright (C) 2017 ono hiroko

;; Author: ono hiroko <kuanyui.github.io>
;; Keywords: languages, html, jade, pug, sass, scss, stylus
;; Package-Version: 20180127.456
;; Package-Requires: ((emacs "24.4"))
;; X-URL: https://github.com/kuanyui/cakecrumbs.el
;; Version: 0.1

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
;; Display parents’ chain on header for:
;;
;;   HTML
;;   Jade / Pug
;;   SCSS / LESS
;;   Stylus / Sass

;; Life is painful, see screenshot on Github to know what on earth is this doing::
;;   https://github.com/kuanyui/cakecrumbs.el
;;; Code:


;; ======================================================
;; Buffer Local Variables
;; ======================================================

(defvar-local cakecrumbs--formatted-header nil)
(defvar-local cakecrumbs--idle-timer nil
  "Buffer-local timer.")
(defvar-local cakecrumbs--original-head-line-format nil
  "Sotre the value of `header-line-format' before calling `cakecrumbs-install-header'")
(defvar-local cakecrumbs--header-installed nil
  "Internal use.")

;; ======================================================
;; Variables For Customization
;; ======================================================
(defgroup cakecrumbs nil
  "Cakecrumbs can show parents on header for HTML/Jade/Sass/Stylus"
  :link '(url-link "https://github.com/kuanyui/cakecrumbs.el"))

(defgroup cakecrumbs-faces nil
  "Faces for Cakecrumbs")

(defcustom cakecrumbs-refresh-delay-seconds 0.1
  "Set to number to refresh after idling N seconds.
Set to nil or 0, refresh without any delay."
  :group 'cakecrumbs :type 'number)

(defcustom cakecrumbs-separator " | "
  "The separator between each hierarchy."
  :group 'cakecrumbs :type 'string)
(defcustom cakecrumbs-ellipsis "[...] "
  "When parent list too long, this will be displayed."
  :group 'cakecrumbs :type 'string)

(defcustom cakecrumbs-html-major-modes   '(html-mode web-mode nxml-mode sgml-mode)
  "The major-mode which will be seemed as HTML."
  :group 'cakecrumbs :type '(repeat symbol))
(defcustom cakecrumbs-jade-major-modes   '(yajade-mode jade-mode pug-mode)
  "The major-mode which will be seemed as Jade / Pug Template"
  :group 'cakecrumbs :type '(repeat symbol))
(defcustom cakecrumbs-scss-major-modes   '(scss-mode less-css-mode css-mode)
  "The major-mode which will be seemed as CSS / SCSS / LESS"
  :group 'cakecrumbs :type '(repeat symbol))
(defcustom cakecrumbs-stylus-major-modes '(stylus-mode sass-mode)
  "The major-mode which will be seemed as Stylus / SASS"
  :group 'cakecrumbs :type '(repeat symbol))

(defcustom cakecrumbs-ignored-patterns '(
                                         "[.]col-[a-z][a-z]-[0-9]+"  ; Bootstrap's .col-*
                                         )
  "RegExp patterns to ignore specific elements."
  :group 'cakecrumbs :type '(repeat string))

(defface cakecrumbs-ellipsis
  '((t :inherit font-lock-comment-face))
  "Ellipsis" :group 'cakecrumbs-faces)

(defface cakecrumbs-separator
  '((t :inherit font-lock-comment-face))
  "Seperator between each level" :group 'cakecrumbs-faces)

(defface cakecrumbs-tag
  '((t :inherit font-lock-function-name-face))
  "HTML/CSS tag" :group 'cakecrumbs-faces)

(defface cakecrumbs-id
  '((t :inherit font-lock-keyword-face))
  "HTML/CSS #id" :group 'cakecrumbs-faces)

(defface cakecrumbs-class
  '((t :inherit font-lock-type-face))
  "HTML/CSS .class" :group 'cakecrumbs-faces)

(defface cakecrumbs-pseudo
  '((t :inherit font-lock-constant-face))
  "CSS :pseudo selector" :group 'cakecrumbs-faces)

(defface cakecrumbs-attr
  '((t :inherit font-lock-variable-name-face))
  "CSS [attribute=] selector" :group 'cakecrumbs-faces)

(defface cakecrumbs-preprocessor
  '((t :inherit font-lock-preprocessor-face))
  "SCSS/LESS/Stylus @.+ or CSS @media" :group 'cakecrumbs-faces)

;; ======================================================
;; Variables
;; ======================================================

(defvar cakecrumbs-re-tag "^[^ .#:&@()]+")
(defvar cakecrumbs-re-class "[.][-A-z0-9_]+")
(defvar cakecrumbs-re-id "#[-A-z0-9_]+")
(defvar cakecrumbs-re-attr "\\[[-A-z0-9_]+.*\\]")
(defvar cakecrumbs-re-pseudo "::?[-A-z0-9_]+")
(defvar cakecrumbs-re-preprocessor "@[-A-z0-9_]+")

;; ======================================================
;; Utils Function
;; ======================================================

(defun cakecrumbs-matched-positions-all (regexp string &optional subexp-depth)
  "Return a list of matched positions for REGEXP in STRING.
SUBEXP-DEPTH is 0 by default."
  (if (null subexp-depth)
      (setq subexp-depth 0))
  (save-match-data
    (let ((pos 0) result)
      (while (and (string-match regexp string pos)
                  (< pos (length string)))
        (progn
          (push (cons (match-beginning subexp-depth) (match-end subexp-depth)) result)
          (setq pos (match-end 0))))
      (nreverse result))))

;; (setq ex "span.col-md-3.col-xs-6#test-hello")
;; (setq cs "span .col-md-3.col-xs-6 > #test-hello[disabled=true] :not(:nth-child(42))")
;; (cakecrumbs-matched-positions-all cakecrumbs-re-tag cs 0)
;; (cakecrumbs-matched-positions-all cakecrumbs-re-id cs 0)
;; (cakecrumbs-matched-positions-all cakecrumbs-re-class cs 0)
;; (cakecrumbs-matched-positions-all cakecrumbs-re-attr cs 0)
;; (cakecrumbs-matched-positions-all cakecrumbs-re-pseudo cs 0)

;; Syntax
(defun cakecrumbs-in-paren-p (&optional pos)
  "Return nearest paren's beginning POS.
This is useless in `web-mode'."
  (car (nth 9 (syntax-ppss pos))))

(defun cakecrumbs-in-comment-p (&optional pos)
  (nth 4 (syntax-ppss pos)))

(defun cakecrumbs-in-string-p (&optional pos)
  (nth 3 (syntax-ppss pos)))

(defun cakecrumbs-invisible-line-p ()
  (string-match-p "^[ \t]*$" (cakecrumbs-current-line-string)))

(defun cakecrumbs-get-indentation-at-pos (pos)
  (save-excursion (goto-char pos) (current-indentation)))

(defun cakecrumbs-current-line-string ()
  (substring (thing-at-point 'line t) 0 -1))

(defun cakecrumbs-string-match (regexp num string)
  (save-match-data
    (if (string-match regexp string)
        (match-string num string)
      nil)))

;; ======================================================
;; HTML
;; ======================================================

(defun cakecrumbs-html-search-backward-< (&optional pos)
  "return the position of backwardly nearest syntactic < (not in
string && not in comment) from POS. If not found, return nil

`syntax-ppss' cannot detect comment in web-mode, so use such way."
  ;; Don't just check wether in paren via `syntax-ppss' because
  ;; `web-mode' redefined `syntax-table', which makes `syntax-ppss'
  ;; unable to check if in <...> paren || in attr string.
  (if (memq major-mode cakecrumbs-html-major-modes)
      (save-excursion
        (if pos (goto-char pos))
        (let ((fin nil))
          (while (progn
                   (setq fin (re-search-backward "<"  nil :no-error))
                   (cond ((null fin) nil)
                         ((eq (point-min) (point)) nil) ; break
                         ((not (memq major-mode cakecrumbs-html-major-modes)) nil)  ; break. Is this condition possible in mmm-mode?
                         ((cakecrumbs-in-string-p) t) ; continue
                         ((equal (buffer-substring-no-properties (point) (+ 4 (point))) "<!--") t) ; continue
                         (t nil))) ; found
            (setq fin nil))
          fin))))

(defun cakecrumbs-html-search-forward-> (&optional pos)
  "return the position of forwardly nearest syntactic > (not in
string && not in comment) from POS. If not found, return nil"
  (if (memq major-mode cakecrumbs-html-major-modes)
      (save-excursion
        (if pos (goto-char pos))
        (let ((fin nil))
          (while (progn
                   (setq fin (re-search-forward ">"  nil :no-error))
                   (cond ((null fin) nil)
                         ((eq (point-max) (point)) nil) ; break
                         ((not (memq major-mode cakecrumbs-html-major-modes)) nil)  ; break. Is this condition possible in mmm-mode?
                         ((cakecrumbs-in-string-p) t) ; continue
                         ((equal (buffer-substring-no-properties (- (point) 3) (point)) "-->") t) ; continue
                         (t nil))) ; found
            (setq fin nil))
          fin))))

(defun cakecrumbs-html-search-nearest-tag (&optional pos)
  "Get position of the nearest tag from POS (or `point' when POS is nil).
Always search backwardly, and comment tag never involved.

If not found (no valid HTML tag existed before POS), returns nil;
else, returns a list with following elements:

0. int,    the position of the found tag begins from.
1. int,    the position of the found tag ends at.
2: bool,   if current point is within this HTML tag, t.
3. symbol, type: `self-closing-tag', `start-tag', `end-tag'
4. string, tag name.
5. string, id name.
6. list,   class names.

"
  (let* ((begin (cakecrumbs-html-search-backward-< pos))
         (end (if begin (cakecrumbs-html-search-forward-> begin)))
         (in-tag (if end (eq end (cakecrumbs-html-search-forward-> pos)))))
    (if (or (null begin) (null end))
        nil
      (let* ((raw (replace-regexp-in-string "\\(?:\n\\| \\|\t\\)+" " " (buffer-substring-no-properties (1+ begin) (1- end))))
             (tag-role (cond ((string-match-p "^ */" raw) 'end-tag)
                             ((string-match-p "/ *$" raw) 'self-closing-tag)
                             (t 'start-tag)))
             (tag-name (if (eq tag-role 'end-tag)
                           (cakecrumbs-string-match "\\([^ /]+\\)$" 1 raw)
                         (cakecrumbs-string-match "^\\([^ ]+\\)" 1 raw)))
             (tag-id (if (memq tag-role '(self-closing-tag start-tag))
                         (cakecrumbs-string-match "[ \"']id ?= ?['\"]\\([A-Za-Z0-9 _-]+\\)['\"]" 1 raw)))
             (tag-classes (if (memq tag-role '(self-closing-tag start-tag))
                              (cakecrumbs-string-match "[ \"']class ?= ?['\"]\\([A-Za-z0-9 _-]+\\)['\"]" 1 raw))))
        (if (or (string-prefix-p "?" tag-name)  ;; <?xml ...>
                (string-prefix-p "!" tag-name) ;; <!DOCTYPE ...>
                (and (eq tag-role 'start-tag)
                     (equal tag-name '("img" "link"))))
            (setq tag-role 'self-closing-tag))
        (if tag-id (setq tag-id (string-trim tag-id)))
        (if tag-classes (setq tag-classes (split-string (string-trim tag-classes) " +")))
        (list begin end in-tag tag-role tag-name tag-id tag-classes)))))


(defun cakecrumbs-html-get-parent (&optional from-pos)
  "return list. (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF).
string PARENT-TAG has been formatted as CSS/Jade/Pug-liked.
bool IN-TAG-ITSELF "
  (save-excursion
    (let* ((m (cakecrumbs-html-search-nearest-tag from-pos))
           (m-pos (nth 0 m))
           (init-in-paren (nth 2 m))
           (m-tag-role (nth 3 m))
           (m-tag-name (nth 4 m))
           (stack '())
           )
      (while (cond ((null m) nil) ; break
                   (init-in-paren nil) ; break (the tag currently within is just parent)
                   ((and (null stack) (eq m-tag-role 'start-tag)) nil) ; break (found parent)
                   (t t)) ; continue
        ;; WHILE BODY
        ;; stack manipulate
        (cond ((eq m-tag-role 'start-tag)
               (if (equal (car stack) m-tag-name)
                   (pop stack)))
              ((eq m-tag-role 'end-tag)
               (push m-tag-name stack))
              (t nil)) ; ignore
        (setq m (cakecrumbs-html-search-nearest-tag m-pos))
        (setq m-pos (nth 0 m))
        (setq m-tag-role (nth 3 m))
        (setq m-tag-name (nth 4 m)))
      (if m
          (let* ((-id (nth 5 m))
                 (id (if -id (concat "#" -id)))
                 (-kls (nth 6 m))
                 (kls (if -kls (mapconcat (lambda (s) (concat "." s)) -kls "")))
                 (-name (nth 4 m))
                 (name (if (and kls (equal -name "div"))
                           ""
                         -name)))
            (list
             (concat name id kls)
             (nth 0 m)
             (nth 2 m)))))))


(defun cakecrumbs-html-get-parents (&optional point)
  (let ((fin '())
        (last-parent-pos (or point (point))))
    (while (let ((parent-obj (cakecrumbs-html-get-parent last-parent-pos)))
             (if (or (null (car parent-obj))
                     (null (nth 1 parent-obj)))
                 nil ; break
               (prog1 t ; continue
                 (push (car parent-obj) fin)
                 (setq last-parent-pos (nth 1 parent-obj))
                 ))))
    fin))

;; (defun h () (interactive) (message "%s, %s" (point) (cakecrumbs-html-search-nearest-tag)))
;; (defun hh () (interactive) (message "%s" (cakecrumbs-html-get-parent)))
;; (defun hhh () (interactive) (message "%s" (cakecrumbs-html-get-parents)))

;; ======================================================
;; Jade / Pug
;; ======================================================

(setq cakecrumbs-jade-invalid-tag-pattern
      ;; In fact, I tested tag `if-xxx', `each-xxx' with pug/jade compiler, but they will cause error..
      (concat "^[ \t]*"
              (regexp-opt
               '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends" "var"
                 "append" "prepend" "include" "yield" "mixin"))))

(defun cakecrumbs-jade-comment-line-p ()
  (string-match-p "^[ \t]*//" (cakecrumbs-current-line-string)))

(defun cakecrumbs-jade-string-line-p ()
  (string-match-p "^[ \t]*[|]" (cakecrumbs-current-line-string)))

(defun cakecrumbs-jade-line-starts-with-tag ()
  (string-match-p "^[ \t]*[A-z]" (cakecrumbs-current-line-string)))

(defun cakecrumbs-jade-search-nearest-plain-tag (&optional pos)
  "Search for the nearest plain-tag from POS (or `point' when POS is nil).
return a list: (ELEM-SELECTOR TAG-POS INIT-IN-PAREN)

Always search backwardly.

All comment-tags and nested-tags are ignored."
  (save-excursion
    (if pos (goto-char pos))
    (let* ((init-in-parenthesis (cakecrumbs-in-paren-p))
           (PATTERN "^ *\\([.#A-z0-9_-]+\\)")
           (m (progn (if (> (current-column) (current-indentation))
                         (end-of-line))
                     (re-search-backward PATTERN nil :no-error))))
      (while (cond ((null m) nil) ; break (not found)
                   ((cakecrumbs-in-paren-p) t) ; continue
                   (t nil)  ; break (found)
                   )
        (setq m (re-search-backward PATTERN nil :no-error)))
      (if m
          (list (match-string 1)
                (progn (back-to-indentation) (point))
                (if init-in-parenthesis t)
                )))))

(defun cakecrumbs-jade-get-parent (&optional point)
  "return value (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF).
Find backward lines up to parent"
  (save-excursion
    (if point (goto-char point))
    (let* ((init-in-parenthesis (nth 9 (syntax-ppss)))
           (init-indentation (if (cakecrumbs-invisible-line-p)  ; parent's indentation must less than this
                                 (current-column)
                               (current-indentation)))
           (possible-parent nil)
           (found-parent nil)
           (continue t))
      (while continue
        (setq possible-parent (cakecrumbs-jade-search-nearest-plain-tag))
        (when possible-parent
          (if init-in-parenthesis
              (setq found-parent possible-parent)
            (let* ((tag-pos (nth 1 possible-parent))
                   (tag-indentation (cakecrumbs-get-indentation-at-pos tag-pos)))
              (if (< tag-indentation init-indentation)
                  (setq found-parent possible-parent)
                (goto-char tag-pos)))))
        (if (or found-parent
                (null possible-parent))
            (setq continue nil)))
      found-parent
      )))

(defun cakecrumbs-jade-get-parents (&optional point)
  (save-excursion
    (if point (goto-char point))
    (let ((fin '())
          (pos point))
      (while (let ((parent (cakecrumbs-jade-get-parent pos)))
               (if parent
                   (prog1 t ; continue WHILE
                     (setq pos (nth 1 parent))
                     (push (car parent) fin))
                 nil))) ; break WHILE
      fin)))

;; (defun ppss () (interactive) (message "%s" (syntax-ppss)))
;; (defun j () (interactive) (message (format "%s\n%s" (point) (cakecrumbs-jade-search-nearest-plain-tag))))
;; (defun jj () (interactive) (message (format "%s\n%s" (point) (cakecrumbs-jade-get-parent))))
;; (defun jjj () (interactive) (message"%s" (cakecrumbs-jade-get-parents)))

;; ======================================================
;; SCSS / LESS
;; ======================================================
(defun cakecrumbs-scss-extract-selector-from-pos (&optional pos)
  "Search backward. Use with `cakecrumbs-scss-get-parent'"
  (save-excursion
    (if pos (goto-char pos))
    (let* ((to pos)
           (from (progn (re-search-backward "[,;{}^\n]" nil t)
                        (1+ (point)))))
      (string-trim (buffer-substring from to)))))

(defun cakecrumbs-scss-get-parents (&optional point)
  "Return a string list. Each string is the selectors at its level."
  (save-excursion
    (let* ((parent-pos-list (nth 9 (syntax-ppss point))))
      (mapcar #'cakecrumbs-scss-extract-selector-from-pos parent-pos-list))))

(defun cakecrumbs-scss-get-parent (&optional from-pos)
  "return a list if found parent: (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF);
otherwise, nil.
IN-TAG-ITSELF is always nil."
  (let ((left-paren-pos (car (last (nth 9 (syntax-ppss from-pos))))))
    (if left-paren-pos
        (list
         (cakecrumbs-scss-extract-selector-from-pos left-paren-pos)
         left-paren-pos
         nil))))

;; (defun c () (interactive) (message "%s" (cakecrumbs-scss-get-parent)))
;; (defun cc () (interactive) (message "%s" (cakecrumbs-scss-get-parents)))

;; ======================================================
;; Stylus / Sass
;; ======================================================
;; Sass and Stylus has some small differences.
;; Following is valid for Stylus:
;;
;;  .tags > .tag
;;    &.foo  {background-color: #afd7ff; color: #005f87;}
;;    &.bar  {background-color: #afd7ff; color: #005f87;}
;;
;; Sass not support brackets like this.
;;
;; However, I don't want to deal with such condition because I've
;; started to lose my patience on this project.

(defun cakecrumbs-stylus-get-parent (&optional point)
  "return value (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF).
Find backward lines up to parent
Currently IN-TAG-ITSELF is always nil."
  (save-excursion
    (if point (goto-char point))
    (let* ((PATTERN "^ *\\([^/][^,]+\\)")
           (init-indentation (if (cakecrumbs-invisible-line-p)  ; parent's indentation must less than this
                                 (current-column)
                               (current-indentation)))
           (m nil)
           (found-parent nil)
           (continue t))
      (while continue
        (setq m (re-search-backward PATTERN nil :no-error))
        (if (and m
                 (< (cakecrumbs-get-indentation-at-pos m) init-indentation))
            (setq found-parent (list (string-trim (match-string-no-properties 1))
                                     m
                                     nil)))
        (if (or (null m) found-parent)
            (setq continue nil)))
      found-parent)))

(defun cakecrumbs-stylus-get-parents (&optional point)
  "return string list or nil."
  (save-excursion
    (let ((fin '())
          (parent-obj (cakecrumbs-stylus-get-parent point))
          (continue t))
      (if parent-obj
          (push (car parent-obj) fin)
        (setq continue nil))
      (while continue
        (setq parent-obj (cakecrumbs-stylus-get-parent (nth 1 parent-obj)))
        (if parent-obj
            (push (car parent-obj) fin)
          (setq continue nil)))
      fin)))

;; (defun ss () (interactive) (message (format "%s\n%s" (point) (cakecrumbs-stylus-get-parent))))
;; (defun sss () (interactive) (message (format "%s\n%s" (point) (cakecrumbs-stylus-get-parents))))

;; ======================================================
;; Main
;; ======================================================

(defun cakecrumbs-propertize-string (level-str)
  "Input is single-level string"
  (mapc (lambda (patt)
          (setq level-str (replace-regexp-in-string patt "" level-str)))
        cakecrumbs-ignored-patterns)
  (if (equal "" level-str) (setq level-str "div"))
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-tag level-str)))) ; tag
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-tag) level-str)))
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-id level-str)))) ; id
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-id) level-str)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-class level-str))) ; class
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-class) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-attr level-str))) ; attr
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-attr) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-pseudo level-str))) ; pseudo
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-pseudo) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-preprocessor level-str))) ; preprocessor
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-preprocessor) level-str))
                m)))
  level-str)

(defun cakecrumbs-format-parents (parents)
  "PARENTS is a (no-propertized) string list"
  (mapconcat #'cakecrumbs-propertize-string
             parents
             (propertize cakecrumbs-separator 'face 'cakecrumbs-separator)))

(defun cakecrumbs-generate-header-string ()
  ""
  (let ((parents (cakecrumbs-get-parents)))
    (if (null parents)
        (propertize "(cakecrumbs idle)" 'face 'cakecrumbs-ellipsis)
      (let* ((fin (cakecrumbs-format-parents parents))
             (ellipsis-str (or cakecrumbs-ellipsis "[...]"))
             (ellipsis-len (length ellipsis-str))
             (need-ellipsis nil))
        (while (> (+ (length fin) (if need-ellipsis ellipsis-len 0))
                  (window-body-width))
          (setq need-ellipsis t)
          (pop parents)
          (setq fin (cakecrumbs-format-parents parents)))
        (if need-ellipsis
            (concat (propertize ellipsis-str 'face 'cakecrumbs-ellipsis) fin)
          fin)))))

(defun cakecrumbs-get-parents (&optional point)
  "return string list, containing parents."
  (cond ((memq major-mode cakecrumbs-html-major-modes)
         (cakecrumbs-html-get-parents point))
        ((memq major-mode cakecrumbs-jade-major-modes)
         (cakecrumbs-jade-get-parents point))
        ((memq major-mode cakecrumbs-scss-major-modes)
         (cakecrumbs-scss-get-parents point))
        ((memq major-mode cakecrumbs-stylus-major-modes)
         (cakecrumbs-stylus-get-parents point))))

(defun cakecrumbs-get-parent (&optional point)
  "return a list: (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF)"
  (cond ((memq major-mode cakecrumbs-html-major-modes)
         (cakecrumbs-html-get-parent point))
        ((memq major-mode cakecrumbs-jade-major-modes)
         (cakecrumbs-jade-get-parent point))
        ((memq major-mode cakecrumbs-scss-major-modes)
         (cakecrumbs-scss-get-parent point))
        ((memq major-mode cakecrumbs-stylus-major-modes)
         (cakecrumbs-stylus-get-parent point))))

(defun cakecrumbs-show-in-minibuffer ()
  "Display full parents list in minibuffer"
  (interactive)
  (let ((parents (cakecrumbs-get-parents)))
    (if (null parents)
        (message (propertize "[Cakecrumbs] Not in a supported area!" 'face 'cakecrumbs-ellipsis))
      (message (cakecrumbs-format-parents parents)))))

(defun cakecrumbs-goto-parent ()
  "Jump to the position of parent"
  (interactive)
  (let ((parent-obj (cakecrumbs-get-parent)))
    (if parent-obj
        (goto-char (nth 1 parent-obj))
      (message "[Cakecrumbs] Not found parent!"))))

;; ======================================================
;; Idle Timer
;; ======================================================
;;
(defun cakecrumbs-timer-handler (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq cakecrumbs--formatted-header (cakecrumbs-generate-header-string))
      (force-mode-line-update))))

(defun cakecrumbs-install-header ()
  (when (not cakecrumbs--header-installed)
    (if (timerp cakecrumbs--idle-timer)
        (cancel-timer cakecrumbs--idle-timer))
    (setq cakecrumbs--original-head-line-format header-line-format)
    (cond ((and (numberp cakecrumbs-refresh-delay-seconds)
                (> cakecrumbs-refresh-delay-seconds 0))
           (progn (setq cakecrumbs--idle-timer
                        (run-with-idle-timer cakecrumbs-refresh-delay-seconds t #'cakecrumbs-timer-handler (current-buffer)))
                  (setq header-line-format '((:eval cakecrumbs--formatted-header)))))
          ((> (buffer-size) (* 1024 1024 100))  ;; if file size > 100 MB, always use idle timer.
           (progn (setq cakecrumbs--idle-timer
                        (run-with-idle-timer 0.3 t #'cakecrumbs-timer-handler (current-buffer)))
                  (setq header-line-format '((:eval cakecrumbs--formatted-header)))))
          (t
           (progn (setq header-line-format '((:eval (cakecrumbs-generate-header-string)))))))
    (add-hook 'kill-buffer-hook 'cakecrumbs-uninstall-header nil t)
    (setq cakecrumbs--header-installed t)))

(defun cakecrumbs-uninstall-header ()
  (when cakecrumbs--header-installed
    (if (timerp cakecrumbs--idle-timer)
        (cancel-timer cakecrumbs--idle-timer))
    (setq header-line-format cakecrumbs--original-head-line-format)
    (setq cakecrumbs--original-head-line-format nil)
    (remove-hook 'kill-buffer-hook 'cakecrumbs-uninstall-header t)))

;; ======================================================
;; Minor Mode
;; ======================================================

;;;###autoload
(define-minor-mode cakecrumbs-mode
  "doc"
  :init-value nil
  :lighter " cakecrumbs"
  ;; :keymap cakecrumbs-mode-map
  :global nil
  (if cakecrumbs-mode
      (cakecrumbs-install-header)
    (cakecrumbs-uninstall-header)))

(defalias 'cakecrumbs 'cakecrumbs-mode)

;;;###autoload
(defun cakecrumbs-enable-if-disabled ()
  (interactive)
  (if cakecrumbs-mode
      (cakecrumbs-mode)))

;; ======================================================
;; Setup
;; ======================================================

;;;###autoload
(defun cakecrumbs-auto-setup ()
  "Use in your Emacs config file. Auto add-hook to all modes
defined in:
- `cakecrumbs-html-major-modes'
- `cakecrumbs-jade-major-modes'
- `cakecrumbs-scss-major-modes'
- `cakecrumbs-stylus-major-modes'
"
  (mapc (lambda (mode-symbol)
          (let* ((mode-name (symbol-name mode-symbol))
                 (hook-symbol (intern (concat mode-name "-hook"))))
            (eval-after-load mode-symbol
              `(add-hook (quote ,hook-symbol) 'cakecrumbs-enable-if-disabled)
              )))
        (append cakecrumbs-html-major-modes
                cakecrumbs-jade-major-modes
                cakecrumbs-scss-major-modes
                cakecrumbs-stylus-major-modes)))

(provide 'cakecrumbs)
;;; cakecrumbs.el ends here
