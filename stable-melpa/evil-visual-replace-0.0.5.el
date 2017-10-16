;;; evil-visual-replace.el --- search/replace commands for evil visual state, inc. blocks

;; Copyright (C) 2016-2017 Troy Pracy

;; Author: Troy Pracy
;; URL: https://github.com/troyp/evil-visual-replace
;; Package-Version: 0.0.5
;; Version: 0.0.5
;; Keywords: evil search replace regexp block rectangular region visual
;; Package-Requires: ((evil "1.0.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides versions of the `query-replace' (M-%) and `replace-regexp' (C-M-%)
;; commands which work for evil-mode visual-state, including visual blocks
;; (rectangular regions).  The native Emacs versions don't understand evil's
;; visual blocks, and treat them as normal regions.
;;
;; Note that these commands are specifically intended for visual state and have
;; barely been tested in non-visual states.  Rather than globally replacing
;; the native commands, it is recommended to rebind them in
;; `evil-visual-state-map'.
;;
;; Install:
;;
;; (evil-visual-replace-visual-bindings)

;;; Code:

(require 'evil)

;;;###autoload
(defun evil-visual-replace-visual-bindings (&optional use-pcre)
  "Set up evil-visual-replace default key bindings.

Bind `evil-visual-replace-query-replace' to M-% and
`evil-visual-replace-replace-regexp' to C-M-% in `evil-visual-state-map'.

If the optional parameter USE-PCRE is non-nil, C-M-% is instead bound to
`evil-visual-replace-pcre-query-replace'."
  (interactive)
  (define-key evil-visual-state-map (kbd "M-%")   'evil-visual-replace-query-replace)
  (if use-pcre
      (progn
        (global-set-key (kbd "C-M-%") 'pcre-query-replace-regexp)
        (define-key evil-visual-state-map (kbd "C-M-%") 'evil-visual-replace-pcre-replace-regexp))
    (define-key evil-visual-state-map (kbd "C-M-%") 'evil-visual-replace-replace-regexp)))

(evil-define-command evil-visual-replace-query-replace
  (start end type fromstr tostr  &optional delimited backward)
  "Replace FROMSTR with TOSTR from START to END with CHAR.

If DELIMITED is non-nil (or a prefix argument is given interactively), only
matches surrounded by word boundaries are replaced.

If BACKWARD is non-nil (or a negative prefix argument is given interactively),
the replacement proceeds backward.

This operator respects visual-block selections. For non-block visual state
operations, it is identical to `query-replace'.

For non-visual-state replacements, use `query-replace'."
  (interactive
   (let ((selection (evil-visual-range))
         (args (query-replace-read-args
                (concat
                 "Query replace"
                 (if current-prefix-arg
                     (let (arg (prefix-numeric-value current-prefix-arg))
                       (cond
                        ((< arg 0) "backward")
                        (t         "word")))
                   "")
                 (if (and transient-mark-mode mark-active) " in region" ""))
                nil)))
     (list (nth 0 selection)
           (nth 1 selection)
           (nth 2 selection)
           (nth 0 args)
           (nth 1 args)
           (nth 2 args)
           (nth 3 args))))
  (when fromstr
    (if (eq type 'block)
        (save-excursion
          (cl-flet ((do-replace
                     (begcol endcol regexp tostr)
                     (let* ((maxcol (evil-column (line-end-position)))
                            (endcol (min endcol maxcol)))
                       (unless (> begcol maxcol)
                         (let ((begpos (evil-move-to-column begcol))
                               (endpos (evil-move-to-column endcol)))
                           (perform-replace fromstr tostr
                                            t nil delimited nil nil
                                            begpos endpos backward))))))
            (evil-apply-on-rectangle
             #'do-replace start end fromstr tostr)))
      :else
      (perform-replace fromstr tostr
                       t nil delimited nil nil
                       (if (evil-visual-state-p) start (point))
                       (if (evil-visual-state-p) end   (point-max))
                       backward))))

(evil-define-command evil-visual-replace-replace-regexp
    (start end type regexp tostr  &optional delimited backward)
    "Replace REGEXP with TOSTR from START to END with CHAR.

If DELIMITED is non-nil (or a prefix argument is given interactively), only
matches surrounded by word boundaries are replaced.

If BACKWARD is non-nil (or a negative prefix argument is given interactively),
the replacement proceeds backward.

This operator respects visual-block selections. For non-block visual state
operations, it is identical to `replace-regexp'.

For non-visual-state replacements, use `replace-regexp'."
    (interactive
     (let ((selection (evil-visual-range))
           (args (query-replace-read-args
                  (concat
                   "Query replace"
                   (if current-prefix-arg
                       (let (arg (prefix-numeric-value current-prefix-arg))
                         (cond
                          ((< arg 0) "backward")
                          (t         "word")))
                     "")
                   (if (and transient-mark-mode mark-active) " in region" ""))
                  nil)))
       (list (nth 0 selection)
             (nth 1 selection)
             (nth 2 selection)
             (nth 0 args)
             (nth 1 args)
             (nth 2 args)
             (nth 3 args))))
    (when regexp
      (if (eq type 'block)
          (save-excursion
            (cl-flet ((do-replace
                       (begcol endcol regexp tostr)
                       (let* ((maxcol (evil-column (line-end-position)))
                              (endcol (min endcol maxcol)))
                         (unless (> begcol maxcol)
                           (let ((begpos (evil-move-to-column begcol))
                                 (endpos (evil-move-to-column endcol)))
                             (perform-replace regexp tostr
                                              t t delimited nil nil
                                              begpos endpos backward))))))
              (evil-apply-on-rectangle
               #'do-replace start end regexp tostr)))
        :else
        (perform-replace regexp tostr
                         t t delimited nil nil
                         (if (evil-visual-state-p) start (point))
                         (if (evil-visual-state-p) end   (point-max))
                         backward))))

(defun evil-visual-replace-pcre-replace-regexp
    (start end type regexp tostr  &optional delimited backward)
    "Replace pcre REGEXP with TOSTR from START to END with CHAR.

Similar to `evil-visual-replace-replace-regexp', but uses PCRE regexps rather
than native elisp regexps. Requires the package pcre2el, to provide the
function `pcre-to-elisp'.

If DELIMITED is non-nil (or a prefix argument is given interactively), only
matches surrounded by word boundaries are replaced.

If BACKWARD is non-nil (or a negative prefix argument is given interactively),
the replacement proceeds backward.

This operator respects visual-block selections. For non-block visual state
operations, it is identical to `replace-regexp'.

For non-visual-state replacements, use `replace-regexp'."
  (interactive
   (let ((selection (evil-visual-range))
         (args (query-replace-read-args
                (concat
                 "Query replace"
                 (if current-prefix-arg
                     (let (arg (prefix-numeric-value current-prefix-arg))
                       (cond
                        ((< arg 0) "backward")
                        (t         "word")))
                   "")
                 (if (and transient-mark-mode mark-active) " in region" ""))
                nil)))
     (list (nth 0 selection)
           (nth 1 selection)
           (nth 2 selection)
           (nth 0 args)
           (nth 1 args)
           (nth 2 args)
           (nth 3 args))))
    (when regexp
      (let ((regexp (pcre-to-elisp regexp)))
      (if (eq type 'block)
          (save-excursion
            (cl-flet ((do-replace
                       (begcol endcol regexp tostr)
                       (let* ((maxcol (evil-column (line-end-position)))
                              (endcol (min endcol maxcol)))
                         (unless (> begcol maxcol)
                           (let ((begpos (evil-move-to-column begcol))
                                 (endpos (evil-move-to-column endcol)))
                             (perform-replace regexp tostr
                                              t t delimited nil nil
                                              begpos endpos backward))))))
              (evil-apply-on-rectangle
               #'do-replace start end regexp tostr)))
        :else
        (perform-replace regexp tostr
                         t t delimited nil nil
                         (if (evil-visual-state-p) start (point))
                         (if (evil-visual-state-p) end   (point-max))
                         backward)))))

(provide 'evil-visual-replace)

;;; evil-visual-replace.el ends here
