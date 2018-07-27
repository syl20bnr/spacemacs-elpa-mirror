;;; xah-reformat-code.el --- commands to reformat source code.

;; Copyright © 2013-2017, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 1.1.20170820
;; Package-Version: 20170821.1111
;; Created: 13 Dec 2016
;; Keywords: convenience
;; License: GPL v3
;; Homepage: http://ergoemacs.org/emacs/emacs_reformat_lines.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-reformat-code contains commands to reformat current paragraph into 1 long line or multiple short lines.

;; It is like emacs `fill-region', but designed for programing language source code.

;; it contain commands to strictly exchange whitespaces by newline, or vice versa. No adding other char or removing other char.

;; this is suitable for languages that strictly consider whitespaces equivalent except in string or comment. For example, XML, HTML, CSS, lisp, Wolfram Language.

;; 2016-12-16 todo:
;; • auto skip strings and comments.
;; • cut lines at the proper logical locations, not just around 70 char.
;; • do proper indentation when changing to multi-line.
;; • cater to different languages. e.g. lisp, JavaScript, ruby, php, Java, etc.
;; • add commands to work on buffer, file, or all files in a directory.

;; --------------------------------------------------
;; MANUAL INSTALL

;; put the file xah-reformat-code.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-reformat-code)

;; --------------------------------------------------
;; HOW TO USE

;; M-x xah-reformat-lines

;; don't use this in Python code!

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;; • Whitespace here is defined to be space, tab, and newline character.
;; • Repeated whitespace is considered equivalent to just 1 space or just 1 newline char.
;; • Whitespace is never created if it didn't exist before.
;; • Whitespace is never removed.
;; • No character other than whitespace are removed or inserted.

;; in the future, this package will expand to complete reformat. That is, given a singe very loooooooong line of code, it will reformat it into multiple lines in a pretty way, with proper place to insert newline, proper indentation, and skipping comment line or string.


;;; Code:

;;;###autoload
(defun xah-reformat-lines ( &optional *length)
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

When the command is called for the first time, it checks the current line's length to decide to go into 1 line or multiple lines. If current line is short, it'll reformat to 1 long lines. And vice versa.

Repeated call toggles between formatting to 1 long line and multiple lines.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-07-06"
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (*length (if *length
                      *length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 70 )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            (> (- (line-end-position) (line-beginning-position)) *length)))
         (deactivate-mark nil)
         (-blanks-regex "\n[ \t]*\n")
         -p1 -p2
         )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward -blanks-regex nil "move")
            (progn (re-search-forward -blanks-regex)
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (if (re-search-forward -blanks-regex nil "move")
            (progn (re-search-backward -blanks-regex)
                   (setq -p2 (point)))
          (setq -p2 (point)))))
    (save-excursion
      (if current-prefix-arg
          (xah-reformat-to-multi-lines -p1 -p2 *length)
        (if is-longline-p
            (xah-reformat-to-multi-lines -p1 -p2 *length)
          (xah-reformat-whitespaces-to-one-space -p1 -p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))

(defun xah-reformat-whitespaces-to-one-space (*begin *end)
  "Replace whitespaces by one space.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil "move")
        (replace-match " ")))))

(defun xah-reformat-to-multi-lines ( &optional *begin *end *min-length)
  "Replace spaces by a newline at places so lines are not long.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-07-06"
  (interactive )
  (let (
        -p1 -p2
        (-blanks-regex "\n[ \t]*\n")
        (-minlen (if *min-length
                     *min-length
                   (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 70 ))))
    (if (and  *begin *end)
        (setq -p1 *begin -p2 *end)
      (if (region-active-p)
          (progn (setq -p1 (region-beginning) -p2 (region-end)))
        (save-excursion
          (if (re-search-backward -blanks-regex nil "move")
              (progn (re-search-forward -blanks-regex)
                     (setq -p1 (point)))
            (setq -p1 (point)))
          (if (re-search-forward -blanks-regex nil "move")
              (progn (re-search-backward -blanks-regex)
                     (setq -p2 (point)))
            (setq -p2 (point))))))
    (save-restriction
      (narrow-to-region -p1 -p2)
      (goto-char (point-min))
      (while
          (re-search-forward " +" nil "move")
        (when (> (- (point) (line-beginning-position)) -minlen)
          (replace-match "\n" ))))))

(provide 'xah-reformat-code)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-reformat-code.el ends here
