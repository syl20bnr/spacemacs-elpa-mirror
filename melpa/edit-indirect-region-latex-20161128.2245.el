;;; edit-indirect-region-latex.el --- Edit LaTeX regions in separate buffers, e.g. for English grammar checks

;; Author: Hirotaka Niitsuma <hirotaka.niitsuma@gmail.com>
;; URL: https://github.com/niitsuma/edit-indirect-region-latex
;; Package-Version: 20161128.2245
;; Package-X-Original-Version: 20161125.1740
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.3") (ht "2.2") (edit-indirect "0.1.4") )

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2016, Hirotaka Niitsuma
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Edit regions in separate buffers based on `edit-indirect' especially for latex.
;; In the indirect buffer, latex special expressions like \ref{...} $...$ are translated to [number].
;; When commiting the edit, the translated [number] are backed to original expressions \ref{...} $...$.
;; Such translation is useful when english grammar cheker, for example ginger,
;; detect errors for latex special expressions like \ref{...} $...$.
;;
;; Translation example
;;
;;
;; Equation (\ref{eq:aa}) and (\ref{eq:bb}) shows an results.
;; X shows an results\cite{AAa,Bb}.
;; $x=234$
;; Prolog is logical languages.
;; \verb|<Opinions>|
;; hello
;; \scheme|(setq q 2)|
;; hello
;; \lstinline$category$
;; xxxxxxxxxxxxx
;; \footnote{[1234]}
;; \footnote{\url{http://example.com/}}
;;
;;
;; is translated in the edit buffer
;;
;;
;; Equation ([26530]) and ([98699]) shows an results.
;; X shows an results[96852].
;; [11593]
;; Prolog is logical languages.
;; [26630]
;; hello
;; [39322]
;; hello
;; [99858]
;; xxxxxxxxxxxx
;; [21335]
;; [16739]
;;
;;
;; Then, we can deal the translated text with english grammar checker ( ginger grammarly etc ).
;; After the edit, we can commit the translated text to the original buffer(C-c C-c). 
;; When commit, the translated expressions [number] back to the original expressions automatically.
;;
;; Usually, many technical terms, for example 'Prolog' in the abobe Latex code,
;; are dealed as miss-spell in many english grammar checker.
;; Such words also can be translated by resisting the translation dictonary
;;
;; (edit-indirect-region-latex-ht-resister "Prolog")
;;
;; Then 
;;
;; "Prolog is logical languages."
;;
;; is tranlslated to 
;;
;; "[78137] is logical languages."
;;

;;; Code:

(require 'ht)
(require 'edit-indirect)

(defgroup edit-indirect-region-latex nil
  "Editing regions in separate buffers for latex."
  :group 'editing)

(defcustom edit-indirect-region-latex-after-creation-functions nil
  "Functions called after an edit-indirect-latex buffer has been committed."
  :type 'hook
  :group 'edit-indirect-region-latex )

(defcustom edit-indirect-region-latex-ht (ht-create)
  "Translation table [number] to latex special expressions."
  :type 'hash-table
  :group 'edit-indirect-region-latex )


(defun edit-indirect-region-latex-ht-valueds (table valued)
  "Retuen keys of TABLE which has VALUED."
  (let (results)
    (maphash
     (lambda (key value)
       (when (equal value valued)
	 (push key results)))
       table)
    results))

(defun edit-indirect-region-latex-ht-valued (table valued)
  "Retuen one key of TABLE which has VALUED."
  (car (edit-indirect-region-latex-ht-valueds table valued)))


(defun edit-indirect-region-latex-ht-gen-key (table)  
  "Generate new identical key which does not contain TABLE ."
  (let* ((keynum (+ 100 (random 899)))
         (key (concat "[" (number-to-string keynum) "]")))
    (if (ht-get table key)
	(edit-indirect-region-latex-ht-gen-key table)
      key)))

(defun edit-indirect-region-latex-ht-store-value! (table value)
  "TABLE store VALUE with new identical and return the new key."
  (let ((key (edit-indirect-region-latex-ht-valued table value)))
    (if key key
      (let ((key-new (edit-indirect-region-latex-ht-gen-key table)))
	(ht-set! table key-new value)
	key-new))))

(defun edit-indirect-region-latex-ht-resister (word &optional key)
  "Store WORD to the translate dictionary with KEY. 
When KEY is nil, new identical key is assgined.
Here the translate dictionary is `edit-indirect-region-latex-ht'."
  (when (or (not (boundp 'edit-indirect-region-latex-ht))
            (not (ht? edit-indirect-region-latex-ht)) )
    (setq edit-indirect-region-latex-ht (ht-create)))
  (if key
      (if (ht-get edit-indirect-region-latex-ht key)
          key
        (progn (ht-set! edit-indirect-region-latex-ht key word)
               key))
      (edit-indirect-region-latex-ht-store-value! edit-indirect-region-latex-ht word)))


(defun edit-indirect-region-latex-prepossess ()
  "Check current buffer whether the translated pattern [number].
When current buffer contain the pattarn, the pattarn is scored in the translate dictionary."
  (when (or (not (boundp 'edit-indirect-region-latex-ht))
            (not (ht? edit-indirect-region-latex-ht)) )
    (setq edit-indirect-region-latex-ht (ht-create)))
  (goto-char (point-min))
  (while (re-search-forward "[\[0-9\]+]" nil t)
    (let ((key (buffer-substring-no-properties (match-beginning 0) (match-end 0) )))
      (edit-indirect-region-latex-ht-resister key key))))




(defun edit-indirect-region-latex-after-creation-hook1 ()
  "Latex special expressions like \ref{...} $...$ are translated to [number].
The translated relations are stored in the translate dictionary:`edit-indirect-region-latex-ht'."
  (when (or (not (boundp 'edit-indirect-region-latex-ht))
            (not (ht? edit-indirect-region-latex-ht)) )
    (setq edit-indirect-region-latex-ht (ht-create)))
  (goto-char (point-min))
  
  (ht-each
   (lambda (key value)
     (goto-char (point-min))
     (while (search-forward value nil t) (replace-match key)) )
   edit-indirect-region-latex-ht)
     
  (mapcar
   (lambda (pat)
     (goto-char (point-min))
     (while (re-search-forward pat nil t)
       (let ((key (edit-indirect-region-latex-ht-resister
		   (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
	 (replace-match key))))
   (list
       "\\(\\\\\\(verb\\|scheme\\|lstinline\\)|[^|]*|\\|\\\\\\(cite\\|label\\|ref\\|url\\|lstinline\\){[^}]*}\\|\\\\lstinline\\$[^\\$]*\\$\\)"  ;;;1st step
       "\\$[^\\$]*\\$" ;;2nd
       "\\\\footnote{[^}]*}"  ;; 3rd
       ))  )


(defun edit-indirect-region-latex-after-commit (beg end)
  "Back the tranlated the Latex special expressions in region BEG to END."
  (dotimes (number 3)
    (dolist (key (ht-keys edit-indirect-region-latex-ht))
      (goto-char (point-min))
      (while (search-forward key nil t)
	 (replace-match (ht-get edit-indirect-region-latex-ht key) 'fixed-case 'literal) ))  ))

(defun edit-indirect-region-wrap-latex (s e o)
  "Wrap original `edit-indirect-region' ( S E O ) with initialization for the translate dictionary:`edit-indirect-region-latex-ht'."
  (save-excursion (edit-indirect-region-latex-prepossess))
  (setq edit-indirect-after-creation-hook (append (list #'edit-indirect-region-latex-after-creation-hook1 ) edit-indirect-region-latex-after-creation-functions ))
  (setq edit-indirect-after-commit-functions (list  #'edit-indirect-region-latex-after-commit))
  (edit-indirect-region s e o)   )

;;;###autoload
(defun edit-indirect-latex (s e)
  "Edit the region S to E in a separate buffer.
When no region selected, automaticaly select region around current point.
Then the region pass to `edit-indirect-region-wrap-latex' ."
  (interactive "r")
  (let ((pt (point))
	(region-start)
	(region-end)
	)
    (cond ((region-active-p)
           (edit-indirect-region-wrap-latex s e t)
           )
          ((progn
             (save-excursion
               (setq s (re-search-backward "\\(\\\\begin{[^{}]+}\\|\\\\end{[^{}]+}\\)" nil t)
                     region-start (match-end 0))
               (goto-char pt)
               (setq e (re-search-forward "\\(\\\\begin{[^{}]+}\\|\\\\end{[^{}]+}\\)" nil t)
                     region-end (match-beginning 0))
               (print (list s pt (point) e ))
               (and s e (< s pt e))))
           (edit-indirect-region-wrap-latex region-start region-end t)
           )
          (t (user-error "No region")))))

;;;###autoload
(defun edit-indirect-region-latex (beg end &optional display-buffer)
  "Edit the region BEG to END in a separate buffer.
The arguments pass to `edit-indirect-region' (BEG END DISPLAY-BUFFER).
Before switch the edit mode, latex special expressions 
are translated to [number].
After commit the edit, the translated words are backed to original."
  (interactive
   (if (or (use-region-p) (not transient-mark-mode))
       (prog1 (list (region-beginning) (region-end) t)
         (deactivate-mark))
     (user-error "No region")))

  (save-excursion (edit-indirect-region-latex-prepossess))
  (setq edit-indirect-after-creation-hook (append (list #'edit-indirect-region-latex-after-creation-hook1 ) edit-indirect-region-latex-after-creation-functions ))
  (setq edit-indirect-after-commit-functions (list  #'edit-indirect-region-latex-after-commit))
  
  (let ((buffer (edit-indirect--get-edit-indirect-buffer beg end)))
    (when display-buffer
      (with-current-buffer buffer
        (setq-local edit-indirect--should-quit-window t))
      (select-window (display-buffer buffer)))
    buffer))

(provide 'edit-indirect-region-latex)
;;; edit-indirect-region-latex.el ends here
