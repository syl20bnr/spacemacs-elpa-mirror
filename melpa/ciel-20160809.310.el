;;; ciel.el --- A command that is clone of "ci" in vim.

;; Copyright (C) 2016 Takuma Matsushita

;; Author: Takuma Matsushita <cs14095@gmail.com>
;; Created: 2 Jul 2016
;; Version: 0.0.1
;; Package-Version: 20160809.310
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
	  )
    (when region
      (kill-region (car region) (cadr region)))
    ))

;;;###autoload
(defun ciel-co (arg)
  "COpy inside."
  (interactive "cco: ")
  (when (integerp arg) (setq arg (char-to-string arg)))
  (let ((region))
    (cond ((or (string= arg "(") (string= arg ")")) (setq region (ciel--region-paren "(")))
	  ((or (string= arg "[") (string= arg "]")) (setq region (ciel--region-paren "[")))
	  ((or (string= arg "{") (string= arg "}")) (setq region (ciel--region-paren "{")))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq region (ciel--region-quote arg)))
	  ((string= arg "w") (setq region (ciel--region-word))))
    (when region
      (copy-region-as-kill (car region) (cadr region)))
    ))

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
    	     (re-search-backward regexp)
    	     (while (nth 3 (syntax-ppss))
    	       (re-search-backward regexp))
    	     (setq beg (point))
    	     (cond ((string= target (char-to-string (following-char))) (setq bw (1+ bw)))
    		   (t (setq bw (1- bw)))))))
    (goto-char init)
    (cond ((string= pair (char-to-string (preceding-char))) (setq end (point)))
    	  (t
	   (when (string= target (char-to-string (following-char))) (forward-char))
    	   (while (not (= fw -1))
    	     (re-search-forward regexp)
    	     (while (nth 3 (syntax-ppss))
    	       (re-search-forward regexp))
    	     (setq end (point))
    	     (cond ((string= target (char-to-string (preceding-char))) (setq fw (1+ fw)))
    		   (t (setq fw (1- fw)))))))

    ;; adjust pos
    (setq beg (1+ beg))
    (setq end (1- end))
    (goto-char beg)
    (list beg end)
    ))

(defun ciel--region-quote (arg)
  (let ((init (point)) (beg nil) (end nil) (fw 0) (cur (point)))
    (search-backward arg nil t 1)
    (goto-char init)
    (cond ((string= arg (char-to-string (following-char)))
	  (search-forward arg nil t 1)
	  (goto-char init)
	  (while (> (line-end-position) (match-beginning 0))
	    (setq cur (match-end 0))
	    (setq fw (1+ fw))
	    (goto-char cur)
	    (search-forward arg nil t 1)
	    (goto-char init))
	  
	  (goto-char init)
	  (cond ((= 0 (mod fw 2))
		 (catch 'no-match-in-line-error ;; break when run into next line
		   (forward-char) ;; to avoid matching head
		   (search-forward arg)
		   (goto-char init)
		   (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
		   (setq end (match-beginning 0))

		   (forward-char)
		   (search-backward arg)
		   (goto-char init)
		   (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
		   (setq beg (match-end 0))
		   
		   (goto-char beg)
		   (list beg end)
		   ))
		(t
		 (catch 'no-match-in-line-error ;; break when run into next line
		   (search-backward arg)
		   (goto-char init)
		   (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
		   (setq beg (match-end 0))

		   (search-forward arg)
		   (goto-char init)
		   (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
		   (setq end (match-beginning 0))
		   
		   (goto-char beg)
		   (list beg end)
		   ))))
	  (t
	   (goto-char init)
	   (catch 'no-match-in-line-error ;; break when run into next line
	     (search-backward arg)
	     (goto-char init)
	     (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
	     (setq beg (match-end 0))

	     (search-forward arg)
	     (goto-char init)
	     (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
	     (setq end (match-beginning 0))
	     
	     (goto-char beg)
	     (list beg end)
	     )))
    ))

;; just select a word
(defun ciel--region-word ()
  (let ((beg) (end) (init (point)))
    (forward-word 1)
    (setq beg (point))
    (backward-word 1)
    (setq end (point))
    (goto-char init)
    (list beg end)
    ))

(provide 'ciel)
;;; ciel.el ends here
