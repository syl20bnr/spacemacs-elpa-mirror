;;; ciel.el --- A command that is clone of "ci" in vim.

;; Copyright (C) 2016 Takuma Matsushita

;; Author: Takuma Matsushita <cs14095@gmail.com>
;; Created: 2 Jul 2016
;; Version: 0.0.1
;; Package-Version: 20161230.2212
;; Keywords: convinience
;; Homepage: https://github.com/cs14095/ciel.el
;; Package-Requires: ((emacs "24"))


;; This file is not part of GNU Emacs

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; You can use ci", ci(, ciw and so on with Ctrl-c, i.
;; Also you can copy them with Ctrl-c, o instead of Ctrl-c, i.
;; This is standalone package and you can probably use any mode.

;; I decided to remove cit on master branch, because it's too huge.
;; I'm not going to add cit againg for now.
;; Other command is still available and I think it's almost complete.

;; ## Installation

;; Download ci.el somewhere.
;; For example:

;; cd ~/.emacs.d/elisp/
;; git clone https://github.com/cs14095/ciel.el

;; Then add the following in your .emacs file:

;; (setq load-path (cons "~/.emacs.d/elisp/ciel.el" load-path))
;; (require 'ciel)
;; (global-set-key "\C-ci" 'ciel-ci)
;; (global-set-key "\C-co" 'ciel-co)

;; or you installed by melpa, then just add

;; (global-set-key "\C-ci" 'ciel-ci)
;; (global-set-key "\C-co" 'ciel-co)


;; ## Usage

;; Press `Ctrl-c, i` or `Ctrl-c, o` and enter available character.
;; Watch example or vim usage.

;; ## Example

;; 	Ctrl-c, i, w => kill a word  
;; 	Ctrl-c, i, ' => kill inside ''
;; 	Ctrl-c, i, " => kill inside ""  
;; 	Ctrl-c, i, ` => kill inside ``  
;; 	Ctrl-c, i, [()] => kill inside ()  
;; 	Ctrl-c, i, [{}] => kill inside {}  
;; 	Ctrl-c, i, [<>] => kill inside <>  
;; 	Ctrl-c, i, [[]] => kill inside []  

;; 	Ctrl-c, o, w => copy a word  
;; 	Ctrl-c, o, ' => copy inside ''
;; 	Ctrl-c, o, " => copy inside ""  
;; 	Ctrl-c, o, ` => copy inside ``  
;; 	Ctrl-c, o, [()] => copy inside ()  
;; 	Ctrl-c, o, [{}] => copy inside {}  
;; 	Ctrl-c, o, [<>] => copy inside <>  
;; 	Ctrl-c, o, [[]] => copy inside []  

;; You can also kill the nested parentheses as you can see.
;; https://raw.githubusercontent.com/cs14095/cs14095.github.io/master/ci-el.gif


;;; Code:

(require 'cl)

;;; Customization

(defcustom ciel-c-mode nil
  "Auto indent when killing region encolosed with braces"
  :type 'boolean
  :group 'ciel)

;;; Errors

(put 'no-match-paren-error 'error-message "Couldn't find matching parentheses")
(put 'no-match-quote-error 'error-message "Couldn't find matching quotes")

;; Commands

;;;###autoload
(defun ciel-ci (arg)
  "Clear Inside."
  (interactive "cci: ")
  (when (integerp arg) (setq arg (char-to-string arg)))
  (let ((region))
    (cond ((or (string= arg "(") (string= arg ")")) (setq region (ciel--region-paren "(")))
  	  ((or (string= arg "[") (string= arg "]")) (setq region (ciel--region-paren "[")))
  	  ((or (string= arg "{") (string= arg "}")) (setq region (ciel--region-paren "{")))
  	  ((or (string= arg "\"")
  	       (string= arg "\'")
  	       (string= arg "\`"))
  	   (setq region (ciel--region-quote arg)))
  	  ((string= arg "w") (setq region (ciel--region-word)))
	  (t (signal 'wrong-type-argument (list arg))))
    (when region
      (kill-region (car region) (cadr region))
      (when (and ciel-c-mode (or (string= arg "{") (string= arg "}")))
	(newline)
	(newline)
	(indent-according-to-mode)
	(previous-line)
	(indent-according-to-mode)))))

;;;###autoload
(defun ciel-co (arg)
  "COpy inside."
  (interactive "cco: ")
  (when (integerp arg) (setq arg (char-to-string arg))) ;; char to string
  (let ((region) (init (point)))
    (cond ((or (string= arg "(") (string= arg ")")) (setq region (ciel--region-paren "(")))
	  ((or (string= arg "[") (string= arg "]")) (setq region (ciel--region-paren "[")))
	  ((or (string= arg "{") (string= arg "}")) (setq region (ciel--region-paren "{")))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq region (ciel--region-quote arg)))
	  ((string= arg "w") (setq region (ciel--region-word)))
	  (t (signal 'wrong-type-argument (list arg))))
    (when region
      (copy-region-as-kill (car region) (cadr region)))
    (goto-char init)))

;;;###autoload
(defun ciel-comment-region (arg)
  (interactive "cParentheses: ")
  (when (integerp arg) (setq arg (char-to-string arg))) ;; char to string
  (when (not (or (string= arg "(") (string= arg ")")
		 (string= arg "[") (string= arg "]")
		 (string= arg "{") (string= arg "}")))
    (signal 'wrong-type-argument (list arg)))
  (when (string= arg ")") (setq arg "("))
  (when (string= arg "]") (setq arg "["))
  (when (string= arg "}") (setq arg "{"))
  (let ((region))
    (setq region (ciel--region-paren arg))
    (message "%s" region)
    (when region
      (comment-or-uncomment-region (1- (car region)) (1+ (cadr region)))
    )))

;;;###autoload
(defun ciel-copy-to-register (arg reg)
  (interactive "cParentheses or quote: \ncRegister: ")
  (when (integerp arg) (setq arg (char-to-string arg))) ;; char to string
  (let ((region) (init (point)))
    (cond ((or (string= arg "(") (string= arg ")")) (setq region (ciel--region-paren "(")))
	  ((or (string= arg "[") (string= arg "]")) (setq region (ciel--region-paren "[")))
	  ((or (string= arg "{") (string= arg "}")) (setq region (ciel--region-paren "{")))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq region (ciel--region-quote arg)))
	  ((string= arg "w") (setq region (ciel--region-word)))
	  (t (signal 'wrong-type-argument (list arg))))
    (when region
      (copy-to-register reg (car region) (cadr region)))
    (goto-char init)))

;;;###autoload
(defun ciel-kill-region-paren (arg)
  (interactive)
  (when (not (or (string= arg "(") (string= arg ")")
		 (string= arg "[") (string= arg "]")
		 (string= arg "{") (string= arg "}")))
    (signal 'wrong-type-argument (list arg)))
  (when (string= arg ")") (setq arg "("))
  (when (string= arg "]") (setq arg "["))
  (when (string= arg "}") (setq arg "{"))
  (let ((region))
    (setq region (ciel--region-paren arg))
    (when region
      (kill-region (car region) (cadr region))
      (when (and ciel-c-mode (or (string= arg "{") (string= arg "}"))) ;; future: customizable
	(newline)
	(newline)
	(indent-according-to-mode)
	(previous-line)
	(indent-according-to-mode)))))

;;;###autoload
(defun ciel-copy-region-paren (arg)
  (when (not (or (string= arg "(") (string= arg ")")
		 (string= arg "[") (string= arg "]")
		 (string= arg "{") (string= arg "}")))
    (signal 'wrong-type-argument (list arg)))
  (when (string= arg ")") (setq arg "("))
  (when (string= arg "]") (setq arg "["))
  (when (string= arg "}") (setq arg "{"))
  (let ((region) (init (point)))
    (setq region (ciel--region-paren arg))
    (when region
      (copy-region-as-kill (car region) (cadr region)))
    (goto-char init)))

;;;###autoload
(defun ciel-kill-region-quote (arg)
  (when (not (or (string= arg "\"")
		 (string= arg "\'")
		 (string= arg "\`")))
    (signal 'wrong-type-argument (list arg)))
    (let ((region))
      (setq region (ciel--region-quote arg))
      (when region
	(kill-region (car region) (cadr region)))))

;;;###autoload
(defun ciel-copy-region-quote (arg)
  (when (not (or (string= arg "\"")
		 (string= arg "\'")
		 (string= arg "\`")))
    (signal 'wrong-type-argument (list arg)))
  (let ((region) (init (point)))
    (setq region (ciel--region-quote arg))
    (when region
      (copy-region-as-kill (car region) (cadr region)))
    (goto-char init)))

;;;###autoload
(defun ciel-kill-a-word ()
  (let ((region))
    (setq region (ciel--region-word))
    (when region
      (kill-region (car region) (cadr region)))))

;;;###autoload
(defun ciel-copy-a-word ()
  (let ((region) (init (point)))
    (setq region (ciel--region-word))
    (when region
      (copy-region-as-kill (car region) (cadr region)))
    (goto-char init)))

(defun ciel--region-paren (arg)
  (let ((init (point)) (beg (point)) (end (point)) (fw 0) (bw 0) (regexp) (pair) (target arg))
    (cond ((string= target "(") (setq regexp "[()]")) ;; for regexp
	  ((string= target "{") (setq regexp "[{}]"))
	  ((string= target "[") (setq regexp "[][]"))
	  ((string= target "<") (setq regexp "[<>]")))
    (cond ((string= target "(") (setq pair ")"))
	  ((string= target "{") (setq pair "}"))
	  ((string= target "[") (setq pair "]"))
	  ((string= target "<") (setq pair ">")))
    (cond ((string= target (char-to-string (following-char))) (setq beg (point)))
	  (t
	   (when (string= pair (char-to-string (preceding-char))) (backward-char))
	   (while (not (= bw 1))
	     (unless (re-search-backward regexp nil t)
	       (goto-char init)
	       (signal 'no-match-paren-error (list arg)))
	     (while (nth 3 (syntax-ppss)) ;; skip commented characters as much as passible
	       (unless (re-search-backward regexp nil t)
		 (goto-char init)
		 (signal 'no-match-paren-error (list arg))))
	     (setq beg (point))
	     (cond ((string= target (char-to-string (following-char))) (setq bw (1+ bw)))
		   (t (setq bw (1- bw)))))))
    (goto-char init)
    (cond ((string= pair (char-to-string (preceding-char))) (setq end (point)))
	  (t
	   (when (string= target (char-to-string (following-char))) (forward-char))
	   (while (not (= fw -1))
	     (unless (re-search-forward regexp nil t)
	       (goto-char init)
	       (signal 'no-match-paren-error (list arg)))
	     (while (nth 3 (syntax-ppss)) ;; skip commented characters as much as passible
	       (unless (re-search-forward regexp nil t)
		 (goto-char init)
		 (signal 'no-match-paren-error (list arg))))
	     (setq end (point))
	     (cond ((string= target (char-to-string (preceding-char))) (setq fw (1+ fw)))
		   (t (setq fw (1- fw)))))))
    ;; adjust pos
    (setq beg (1+ beg))
    (setq end (1- end))
    (goto-char beg)
    (list beg end)))

;; This function undetect next or previous line.
(defun ciel--region-quote (arg)
  (let ((init (point)) (beg) (end) (points-of-quote) (line-end-init (line-end-position)))
    ;; At first make a asc list that contains points of quotes in line 
    (beginning-of-line)
    (setq points-of-quote (search-points-of-quote-inline points-of-quote line-end-init))
    (when (or (null points-of-quote) (= (length points-of-quote) 1)) ;; case length = 1
      (goto-char init)
      (signal 'no-match-quote-error (list arg)))
    (goto-char init)
    (cond ((string= arg (char-to-string (following-char))) ;; init point is on the quote
	   (let ((diff))
	     (setq points-of-quote (mapcar #'1- points-of-quote)) ;; to adjust points
	     (setq diff (- (length points-of-quote) (length (member (point) points-of-quote))))
	     (cond ((= (mod diff 2) 0)
		    ;; here is odd-numbered
		    (setq beg (nth diff points-of-quote))
		    (setq end (nth (1+ diff) points-of-quote))
		    (goto-char beg) ;; adjust
		    (forward-char)
		    (setq beg (point))
		    (list beg end))
		   (t
		    ;; here is even-numbered
		    (setq beg (nth (1- diff) points-of-quote))
		    (setq end (nth diff points-of-quote))
		    (goto-char beg) ;; adjust
		    (forward-char)
		    (setq beg (point))
		    (list beg end)))))
	  (t
	   (when (or (< (point) (car points-of-quote)) (> (point) (car (last points-of-quote))))
	     ;; here isn't quoted
	     (signal 'no-match-quote-error (list arg)))
	   (setq points-of-quote (mapcar #'1- points-of-quote))
	   (setq beg (ciel--find-beg init (reverse points-of-quote))) ;; reverse to exec recursive func easily
	   (setq end (nth (1+ (- (length points-of-quote) (length (member beg points-of-quote)))) points-of-quote)) ;; next of beg in the list
	   (goto-char beg)
	   (forward-char)
	   (setq beg (point))
	   (list beg end)))))

(defun ciel--region-word ()
  ;; This function just select a word
  (let ((beg) (end) (init (point)))
    (forward-word 1)
    (setq beg (point))
    (backward-word 1)
    (setq end (point))
    (goto-char init)
    (list beg end)))

;;; Utility functions

;; defun* acts like common lisp defun. In this func, defun* to use return-from
(defun* search-points-of-quote-inline (points-of-quote line-end-init)
  ;; This function make a asc list that contains points of quote
  ;; before use this func call (beggining-of-line)
  (unless (search-forward arg nil t)
    (return-from search-points-of-quote-inline points-of-quote)) ;; couldn't find quotes anymore
  (cond ((< (point) line-end-init)
	 (search-points-of-quote-inline (append points-of-quote (list (point))) line-end-init))
	(t
	 points-of-quote)))

(defun ciel--find-beg (target points-of-quote)
  ;; find matching quote recursively
  (cond ((> init (car points-of-quote))
	 (car points-of-quote))
	(t
	 (ciel--find-beg target (cdr points-of-quote)))))

(provide 'ciel)
;;; ciel.el ends here
