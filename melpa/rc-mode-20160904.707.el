;;; rc-mode.el --- Major mode for the Plan9 rc shell

;; Copyright © Jordan Brown

;; Author: Jordan Brown
;; URL: https://github.com/mrhmouse/rc-mode.el
;; Package-Version: 20160904.707
;; Version: 1.0.1
;; Keywords: rc, plan9, shell

;;; License:

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom
;; the Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(defun rc-intersperse (items sep)
  (if (null (cdr items))
      items
    (cons (car items)
          (cons sep (rc-intersperse (cdr items) sep)))))

(defun rc-join-string (strings sep)
  (apply #'concat (rc-intersperse strings sep)))

(defvar rc-highlights
  `(("'[^']*'"
     . font-lock-string-face)
        
    ("#.*$"
     . font-lock-comment-face)
        
    (,(rc-join-string '("fn" "break"
                        "builtin" "cd"
                        "echo" "eval"
                        "exec" "exit"
                        "limit" "newpgrp"
                        "return" "shift"
                        "umask" "wait"
                        "whatis" "\\$#?\\*"
                        "\\$0" "\\$apids?"
                        "\\$bqstatus" "\\$cdpath"
                        "\\$history" "\\$home"
                        "\\$ifs" "\\$path" "\\$pid"
                        "\\$prompt" "\\$status"
                        "\\$version")
                      "\\|")
     . font-lock-builtin-face)
        
    (,(rc-join-string '("if" "while" "for" "else" "if not"
                        "switch"
                        "@" "=" "&" "&&" "\\^"
                        "|" ";"
                        "<<?" ">>?"
                        "\\(>>?\\|<<?\\||\\)\\[\\d+\\(=\\d+\\)\\]"
                        "||" "~")
                      "\\|")
     . font-lock-keyword-face)
        
    ("\\(?1:\\$#?\\$*\\w+\\)\\|\\(?1:\\w+\\)[[:space:]]*="
     1 font-lock-variable-name-face)

    ("!"
     . font-lock-negation-char-face)))

(defun rc-indent-line ()
  "Indent current line as Plan9 RC shell script"
  (interactive)
  (indent-line-to 
   (save-excursion
     (beginning-of-line)
     (cond
      ((bobp) 0)
      ((or (rc-looking-at-continuation)
           (rc-under-block-header))
       (+ (rc-previous-line-indentation) 2))
      ((rc-looking-at-block-end)
       (- (rc-previous-line-indentation) 2))
      ((rc-inside-list-continuation)
       (rc-start-of-list-on-previous-line))
      (t (rc-previous-line-indentation))))))

(defun rc-start-of-list-on-previous-line ()
  (save-excursion
    (rc-previous-line)
    (end-of-line)
    (while (not (looking-at "("))
      (backward-char))
    (+ 1 (current-column))))

(defun rc-inside-list-continuation ()
  (save-excursion
    (rc-previous-line)
    (looking-at ".*([^)]*$")))

(defun rc-looking-at-block-end ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*}")))

(defun rc-looking-at-continuation ()
  (save-excursion
    (rc-previous-line)
    (or (looking-at ".*\\\\$")
        (looking-at ".*([^)]*$"))))

(defun rc-previous-line ()
  (forward-line -1)
  (beginning-of-line)
  (while (and (looking-at "^[ \t]*$")
              (not (bobp)))
    (forward-line -1)
    (beginning-of-line)))

(defun rc-under-block-header ()
  (save-excursion
    (rc-previous-line)
    (looking-at ".*{[ \t]*\\(#[^']*\\)?$")))

(defun rc-previous-line-indentation ()
  (save-excursion
    (rc-previous-line)
    (while (rc-looking-at-continuation)
      (forward-line -1))
    (current-indentation)))

;;;###autoload
(define-derived-mode rc-mode fundamental-mode "plan9-rc"
  (setq font-lock-defaults '(rc-highlights))
  (setq indent-line-function 'rc-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.rc\\'" . rc-mode))

(provide 'rc-mode)
;;; rc-mode.el ends here
