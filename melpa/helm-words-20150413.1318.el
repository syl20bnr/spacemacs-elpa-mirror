;;; helm-words.el -- Helm extension for looking up words in dictionaries and thesauri.
;; Package-Version: 20150413.1318

;; Copyright (C) 2014 Andrzej Pronobis <a.pronobis@gmail.com>

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
;; Please see https://github.com/pronobis/helm-words for a documentation.

;;; Code:

;; For lexical-let
(eval-when-compile
  (require 'cl))

;; Requires
(require 'helm)
(require 'ispell)
(require 'xml)
(require 'browse-url)

;; Soft requires since only used for default settings which can
;; be changed by the user.
(require 'dictionary nil t)
(require 'shr nil t)
(require 'eww nil t)


(defgroup helm-words nil
  "Helm extension for looking up words in dictionaries and thesauri."
  :group 'helm)


(defcustom helm-words-dictionaries
  '(("en". ("English"
            ("Emacs Dictionary" dictionary-search)
            ("English Dictionary" "http://dictionary.reference.com/browse/%s?s=t")
            ("Thesaurus" "http://www.thesaurus.com/browse/%s?s=t")
            ("English-Polish Dictionary" "http://portalwiedzy.onet.pl/tlumacz.html?qs=%s&tr=ang-pol&x=38&y=9")))
    ("pl" . ("Polish"
             ("Emacs Dict.pl" helm-words-dict-pl-search)
             ("Polish Dictionary" "http://sjp.pwn.pl/szukaj/%s.html")
             ("Thesaurus" "http://www.synonimy.pl/synonim/%s/")
             ("Polish-English Dictionary" "http://portalwiedzy.onet.pl/tlumacz.html?qs=%s&tr=pol-ang&x=39&y=8"))))
  "ASpell dictionaries to search the words in and their configuration."
  :group 'helm-words)


;;;###autoload
(defun helm-words-dict-pl-search (word)
  "Show results from the dict.pl dictionary for the given WORD."
  (interactive "sWord: ")
  (let ((dom
         (with-current-buffer
             (url-retrieve-synchronously (format "http://dict.pl/dict?word=%s&words=&lang=PL" word))
           ;; Delete header and footer
           (let ((point0 (search-forward "<html>"))
                 (point1 (search-forward "<body>"))
                 (point2 (search-forward "<table "))
                 (point3 (search-forward "</table>"))
                 (point4 (search-forward "</body>")))
             (delete-region point3 (- point4 8))
             (delete-region point1 (- point2 7))
             (delete-region 1 (- point0 6)))
           ;; Remove all unnecessary images
           (goto-char 1)
           (while (search-forward-regexp "<img[^<>].*>" nil t)
             (replace-match ""))
           ;; Parse
           (libxml-parse-html-region (point-min) (point-max))
           ))
        (buf (get-buffer-create "*Dict.pl*")))
    (with-current-buffer buf
      (erase-buffer)
      (eww-mode)
      (shr-insert-document dom)
      (goto-char (point-min))
      )
    (display-buffer buf)
    (switch-to-buffer-other-window buf)
    ))


(defun helm-words--get-candidates ()
  "Return candidates for a source stored in helm-source-name."
  (let* ((output-all (with-temp-buffer
                       (insert helm-pattern)
                       (call-process-region (point-min) (point-max) "aspell" t t nil "-a" "--guess" "-d" helm-source-name)
                       (buffer-string)))
         (output-raw (split-string output-all "\n"))
         (output-parsed)
         (candidates ()))
    ;; Parse the output
    (pop output-raw)
    (setq output-parsed (ispell-parse-output (pop output-raw)))
    (cond ((stringp output-parsed)
           (setq candidates (list output-parsed)))
          ((listp output-parsed)
           (setq candidates (nth 2 output-parsed)))
          (t
           (setq candidates (list helm-pattern))))

    candidates
    )
  )


(defun helm-words--get-header-name (dict)
  "Return header names for dictionary name DICT."
  (format "Dictionary: %s" (car (cdr (assoc dict helm-words-dictionaries)))))


(defun helm-words--get-action-function (descr)
  "Create an action function with lexical binding using DESCR as action description."
  (lexical-let ((tmp (car descr)))
    #'(lambda (candidate)
        (cond
         ((functionp tmp)
          (funcall tmp candidate)
          )
         ((stringp tmp)
          (browse-url (format tmp candidate))
          ))
        )
    )
  )


(defun helm-words--get-actions (dict)
  "Return a list of actions for the given dictionary DICT."
  (let ((list (cdr (assoc dict helm-words-dictionaries)))
        (actions ()))
    (pop list)                          ; The first element is name
    (while list
      (let ((cur (pop list)))
        (add-to-list 'actions
                     (cons (car cur)
                           (helm-words--get-action-function (cdr cur)))
                   t)
      ))
    actions
    )
  )


(defun helm-words--get-sources ()
  "Return the sources used."
  (let ((list helm-words-dictionaries)
        (sources ()))
    (while list
      (let ((cur-dict (car (pop list))))
          (add-to-list 'sources
                       (helm-build-sync-source cur-dict
                         :header-name 'helm-words--get-header-name
                         :candidates 'helm-words--get-candidates
                         :action (helm-words--get-actions cur-dict)
                         :candidate-number-limit 9999
                         :volatile t
                         :fuzzy-match t
                         :requires-pattern t
                         )
                       t)
          ))
    sources))


;;;###autoload
(defun helm-words ()
  "Run helm-words."
  (interactive)
  (helm :sources (helm-words--get-sources)
        :buffer "*Helm Words*"
        :prompt "Word: "))


;;;###autoload
(defun helm-words-at-point ()
  "Run helm-words with thing at point."
  (interactive)
  (helm :sources (helm-words--get-sources)
        :buffer "*Helm Words*"
        :prompt "Word: "
        :input (thing-at-point 'word)))


(provide 'helm-words)
;;; helm-words.el ends here
