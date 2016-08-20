;;; eloud.el --- A lightweight, interactive screen reader

;; Copyright (C) 2016  Patrick Smyth

;; Author: Patrick Smyth <patricksmyth01@gmail.com>
;; Homepage: https://github.com/smythp/eloud
;; Keywords: extensions
;; Package-Version: 20160731.857
;; Package-Requires: ((emacs "24.4"))


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

;;; Eloud is a lightweight, interactive screen reader.  It uses the espeak speech synthesizer as a backend.

;; Installation

;; 1. Install espeak

;; First, install espeak.  On Ubuntu or Debian, use:

;;     sudo apt-get install espeak

;; On OSX, use:

;;     brew install espeak

;; Or find the compiled version at http://espeak.sourceforge.net/download.html

;; 2. Install the package

;; Clone this repo:

;;     cd ~
;;     git clone https://github.com/smythp/eloud.git

;; Add the load path to your .emacs:

;;     (add-to-list 'load-path "~/eloud/")

;; Finally, set the path to espeak by adding this to your .emacs:

;;     (setq eloud-espeak-path "~/eloud/")
;; Quick install

;;     cd ~
;;     git clone https://github.com/smythp/eloud.git
;;     (add-to-list 'load-path "~/eloud/")
;;     (setq eloud-espeak-path "/usr/bin/espeak")




;;; Code:


(defgroup eloud nil
  "Customization group for the Eloud screen reader package."
  :group 'multimedia)

(defcustom eloud-speech-rate 270
  "Integer from 1 to 400. Sets speech rate for espeak."
  :type '(integer)
  :group 'eloud)

(defcustom eloud-espeak-path "/usr/bin/espeak"
  "Path to espeak as string. On OSX, likely to be /usr/local/bin/espeak instead."
  :type '(string)
  :group 'eloud)


;;; Define free variables to avoid compiler errors

(defvar dabbrev--last-expansion)


;;; Helper functions

(defun eloud-hyphen-start-p (string)
  "Test if first character of STRING is a hyphen."
  (equal (byte-to-string (aref string 0)) "-"))


(defun eloud-get-buffer-string (buffer)
  "Return a string with the contents of BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (buffer-string))))


(defun eloud-get-char-at-point (&optional offset return-edge)
  "Return string of char at point or optional OFFSET.  If optional RETURN-EDGE is non-nil, return character at point min or point max if point with offset exceeds buffer size, else return an empty string."
  (let* ((new-point (if offset
			(+ (point) offset)
		      (point)))
	 (past-max-p (>= new-point (point-max)))
	 (past-min-p (<= new-point (point-min))))
    (string (char-after (cond (past-max-p (1- (point-max)))
                              (past-min-p (point-min))
                              (t new-point))))))


;;; Main speech function

(defun eloud-speak (string &optional speed no-kill &rest args)
  "Pass STRING to the espeak asynchronous process.  Use the `eloud-speech-rate' variable if no optional integer SPEED is specified.  If NO-KILL argument non-nil, running speech processes are killed before starting new speech process.  Pass additional arguments to espeak as rest ARGS."
  ;; Defines a function that runs process on list of arguments.
  ;; Defines sensible defaults.
  ;; Run with defaults if no additional args specified in function call, else append additional arguments and run
  (let* ((string (if (equal string "") " " string))
	 (default-args `("eloud-speaking" nil ,eloud-espeak-path ,(if (eloud-hyphen-start-p string) (concat " " string) string) "-s" ,(if speed (number-to-string speed) (number-to-string eloud-speech-rate)))))
    (if (not (current-idle-time))
	(progn
	  (if (not no-kill)
	      (progn
		(start-process "kill-espeak" nil "killall" "espeak")
		(sleep-for .5)))
	  (if (not (equal string ""))
	      (apply #'start-process (if (not args)
					 default-args
				       (append default-args args))))))))


;;; Speech functions


(defun eloud-rest-of-line (&rest r)
  "Read from point to the rest of the line.  Used as advice.  Call original function with args R."
  (interactive "^p")
  (let ((move-number (cadr r))
        (old-func (car r))
        (additional-args (cdr r)))
    (progn
      (apply old-func additional-args)
      (let ((point-to-end-of-line (buffer-substring (point) (line-end-position))))
        (if (not (equal point-to-end-of-line ""))
            (eloud-speak point-to-end-of-line))))))


(defun eloud-dabbrev (&rest r)
  "Advice for reading expanded dabbrev.  Call original function with args R."
  (interactive "*P")
  (let ((old-func (car r))
        (additional-args (cdr r))
        (initial-point (point)))
    (apply old-func additional-args)
    (eloud-speak dabbrev--last-expansion)))


(defun eloud-rest-of-line-delay (&rest r)
  "Advice for reading remainder of line.  Call original function with arguments R.  Incorporates delay as workaround for moving to beginning of line while reading."
  (interactive "^p")
  (let ((move-number (cadr r))
        (old-func (car r))
        (additional-args (cdr r)))
    (progn
      (apply old-func additional-args)
      (sit-for .5)
      (let ((point-to-end-of-line (buffer-substring (point) (line-end-position))))
        (if (not (equal point-to-end-of-line ""))
            (eloud-speak point-to-end-of-line))))))


(defun eloud-kill-line (&rest r)
  "Advice to read killed text when using kil-line.  Call original function with args R.  Read only last line killed, not last item in killring."
  (interactive "P")
  (let ((move-number (if (cadr r) (cadr r) 0))
        (old-func (car r))
        (blank-line-p (equal (- (point) (line-end-position)) 0))
        (additional-args (cdr r)))
    (if (> move-number 1)
        (progn
          (apply old-func additional-args)
          (eloud-speak (car kill-ring)))
      (progn
        (let ((rest-of-line (buffer-substring (point) (line-end-position))))
          (if (not (equal rest-of-line ""))
              (eloud-speak (buffer-substring (point) (line-end-position))))
          (apply old-func additional-args))))))


(defun eloud-whole-buffer (&rest r)
  "Advice to speak whole buffer.  Call original function with args R."
  (interactive "^P")
  (let ((old-func (car r))
        (n (cadr r)))
    (progn
      (funcall  old-func n)
      (eloud-speak
       (if (> (point-max) 120000)
           (buffer-substring (point-min) 120000)
         (buffer-substring (point-min) (point-max)))))))


(defun eloud-status-info ()
  "Read status info normally on mode line."
  (interactive)
  (eloud-speak
   (concat (buffer-name) " " (symbol-name major-mode))))


(defun eloud-current-buffer (&rest r)
  "Advice to read current buffer name aloud.  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (other-args (cdr r)))
    (if r
	(apply old-func other-args))
    (progn
      (eloud-speak (concat (buffer-name) (buffer-substring (point) (point-max))))
      (buffer-name))))


(defun eloud-switch-to-buffer (&rest r)
  "Advice to read current buffer aloud after switch.  Call original function with args R."
  (let ((old-func (car r))
        (other-args (cdr r)))
    (if (called-interactively-p 'any)
        (progn
          (apply old-func other-args)
          (eloud-speak (buffer-name))
          (buffer-name))
      (apply old-func other-args))))


(defun eloud-ispell-command-loop (&rest r)
  "Advice to read the word being corrected during Ispell.  Call original function with args R."
  (let ((old-func (car r))
        (correction-list (car (cdr r)))
        (other-args (cdr r))
        (word-item-num 0)
        (word (car (cdr (cdr (cdr r)))))
        (list-to-read '()))
    (progn
      ;; (print (car correction-list)))))
      (while correction-list
        (progn
          (push (number-to-string word-item-num) list-to-read)
          (push ". " list-to-read)
          (push (pop correction-list) list-to-read)
          (push ". . . " list-to-read)
          ;; (setq correction-list (cdr correction-list))
          (setq word-item-num (1+ word-item-num))))
      (progn
        (setq list-to-read (concat word " . " (apply 'concat (reverse list-to-read))))
        (eloud-speak list-to-read)
        (let ((output (apply old-func other-args)))
          (progn
            (eloud-speak output)
            output))))))


(defun eloud-character-at-point (&rest r)
  "Advice to read aloud the character at point.  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (n (cadr r)))
    (progn
      (funcall old-func n)
      (eloud-speak
       (buffer-substring (point) (1+ (point)))
       nil t "--punct"))))


(defun eloud-character-before-point (&rest r)
  "Advice to read aloud the character before point.  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (n (cadr r))
        (other-args (cdr r)))
    (if (= (point) 1)
        (progn
          (eloud-speak "Beginning of buffer")
          (apply old-func other-args))
      (progn
        (eloud-speak
         (eloud-get-char-at-point -1)
         nil t "--punct")
        (apply old-func other-args)))))


(defun eloud-character-after-point (&rest r)
  "Advice to read aloud the character after point.  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (n (cadr r))
        (other-args (cdr r)))
    (if (called-interactively-p 'any)
        (if (= (point) (point-max))
            (progn
              (eloud-speak "end of buffer")
              (apply old-func other-args))
          (progn
            (eloud-speak
             (eloud-get-char-at-point)
             nil t "--punct")
            (apply old-func other-args))))
    (apply old-func other-args)))


(defun eloud-delete-forward (&rest r)
  "Advice to read aloud the character after point.  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (n (cadr r)))
    (if (= (point) (point-max))
        (eloud-speak "end of buffer")
      (progn
        (eloud-speak
         (eloud-get-char-at-point)
         nil t "--punct")
        (funcall old-func n)))))


(defun eloud-last-character (&rest r)
  "Advice to read last printed character.  Call original function with args R."
  (interactive "^p")
  (let* ((old-func (car r))
	 (n (cadr r))
	 (other-args (cddr r))
	 (cmd (this-command-keys))
	 (last-char-cmd (byte-to-string (car (last (string-to-list cmd))))))
    (progn

      (let ((out-char (funcall old-func n))
            (word (save-excursion (search-backward " " (line-beginning-position) t 2))))
        (if (equal cmd " ")
            (eloud-speak
             (buffer-substring (point) (if word word (line-beginning-position)))))
        (eloud-speak
         (if (> n 1)
             (concat (number-to-string n) " times " last-char-cmd)
           last-char-cmd)
         eloud-speech-rate t "--punct")
	out-char))))


(defun eloud-isearch-insert (&rest r)
  "Advice to read characters inserted into the minibuffer during isearch.  Call original function with args R."
  (let ((old-func (car r))
        (other-args (cdr r))
        (char-arg (cadr r)))
    (progn
      (eloud-speak (string char-arg))
      (apply old-func other-args))))


(defun eloud-isearch-move (&rest r)
  "Advice to read aloud search word during isearch.  If no additional match in buffer, read \"no match\" aloud.  Call original function with args R."
  (interactive)
  (let ((old-func (car r))
        (other-args (cdr r))
        (direction (cadr r)))
    (progn
      (apply old-func other-args)
      (cond (isearch-error (eloud-speak isearch-error))
            (isearch-success (eloud-speak isearch-string))
            (t (eloud-speak "no match"))))))


(defun eloud-last-kill-ring (&rest r)
  "Advice to read last item on killring aloud.  Call original function with args R."
  (interactive "^p")
  (let* ((old-func (car r))
         (n (cadr r))
         (other-args (cdr r)))
    (progn
      (apply old-func other-args)
      (eloud-speak (car kill-ring)))))


(defun eloud-moved-point (&rest r)
  "Advice to read difference between point before calling original function and point after calling original function.  Original function called with args R."
  (interactive "^p")
  (let ((move-number (cadr r))
        (old-func (car r))
        (additional-args (cddr r))
        (start-point (point)))
    (progn
      (funcall old-func move-number)
      (save-excursion
        (progn
          (eloud-speak (buffer-substring start-point (point))))))))


(defun eloud-evaluation (&rest r)
  "Advice to read the output of an interactive command aloud.  Call original command with args R."
  (interactive "P")
  (let* ((old-func (car r))
	 (n (cadr r))
	 (other-args (cdr r))
	 (output (apply old-func other-args)))
    (eloud-speak (prin1-to-string output))))


(defun eloud-completion (&rest r)
  "Advice to read completion in minibuffer.  Call original completion function with args R."
  (let* ((old-func (car r))
         (other-args (cdr r))
         (initial-point (point)))
    (if (apply old-func other-args)
        (if (equal initial-point (point))
            (eloud-speak
             (substring-no-properties (eloud-get-buffer-string "*Completions*") 96))
          (eloud-speak (current-word))))))


;;; Map speech functions to Emacs commands


(defvar eloud-around-map '((ispell-command-loop . eloud-ispell-command-loop)
			   (move-beginning-of-line . eloud-rest-of-line)
			   (org-beginning-of-line . eloud-rest-of-line)
			   (beginning-of-buffer . eloud-whole-buffer)
			   (dabbrev-expand . eloud-dabbrev)
			   (dired-next-line . eloud-rest-of-line)
			   (forward-char . eloud-character-at-point)
			   (backward-char . eloud-character-at-point)
			   (dired-previous-line . eloud-rest-of-line)
			   (next-line . eloud-rest-of-line)
			   (previous-line . eloud-rest-of-line)
			   (other-window . eloud-current-buffer)
			   (switch-to-buffer . eloud-switch-to-buffer)
			   (kill-word . eloud-last-kill-ring)
			   (backward-kill-word . eloud-last-kill-ring)
			   (kill-line . eloud-kill-line)
			   (forward-button . eloud-moved-point)
			   (backward-button . eloud-moved-point)
			   (backward-word . eloud-moved-point)
			   (forward-word . eloud-moved-point)
			   (forward-sentence . eloud-moved-point)
			   (eval-last-sexp . eloud-evaluation)
			   (delete-forward-char . eloud-character-after-point)
			   (delete-char . eloud-character-after-point)
			   (backward-sentence . eloud-moved-point)
			   (read-from-minibuffer . eloud-read-minibuffer-prompt)
			   (self-insert-command . eloud-last-character)
			   (isearch-printing-char . eloud-isearch-insert)
			   (isearch-repeat . eloud-isearch-move)
			   (minibuffer-complete . eloud-completion)
			   (backward-delete-char-untabify . eloud-character-before-point))
  "Holds a list of cons cells used to map advice onto functions.")


;;; add functions to hooks


(defvar eloud-hook-map '((gnus-summary-prepared-hook . (lambda () (progn (sit-for .3) (eloud-rest-of-line (lambda () nil))))))
  "List of concs cells to map functions onto hooks when Eloud is initialized.")


(defun eloud-map-commands-to-speech-functions (advice-map advice-type &optional unmap)
  "Map native Emacs functions to Eloud advice defined as list of cons cells in ADVICE-MAP.  ADVICE-TYPE determines whether advice is :around, :override, :after, etc., in the form of a keyword symbol.  If optional UNMAP parameter is non-nil, remove all bound advice functions instead."
  (mapcar (lambda (x)
            (let ((target-function (car x))
                  (speech-function (cdr x)))
              (if (not unmap)
                  (advice-add target-function advice-type speech-function)
                (advice-remove target-function speech-function))))
          advice-map))


(defun eloud-map-commands-to-hooks (hook-map &optional unmap)
  "Map speech functions to hooks as defined in list of cons cells HOOK-MAP.  If UNMAP is non-nil, remove the added functions."
  (mapcar (lambda (x)
            (progn
              (if (not (boundp (car x)))
                  (set (car x)  nil))
              (let ((hook-variable (car x))
                    (function-to-add (cdr x)))
                (if (not unmap)
                    (push function-to-add (symbol-value hook-variable))
                  (set hook-variable (remove function-to-add (symbol-value hook-variable)))))))
          hook-map))


(defun eloud-read-minibuffer-prompt (&rest r)
  "Advice to read the current prompt in the minibuffer.  Call original function with args R."
  (let* ((old-func (car r))
	 (prompt (cadr r))
	 (args (cdr r)))
    (progn
      (eloud-speak prompt)
      (let ((output (apply old-func args)))
					;       (eloud-speak output)
        output))))


;;; Define mode

(define-minor-mode eloud-mode "Minor mode for reading text aloud." nil " eloud" :global t
  (if eloud-mode
      (progn
        (eloud-map-commands-to-speech-functions eloud-around-map :around)
        (eloud-map-commands-to-hooks eloud-hook-map)
        (eloud-speak "eloud on"))
    (progn
      (eloud-map-commands-to-speech-functions eloud-around-map nil t)
      (eloud-map-commands-to-hooks eloud-hook-map t)
      (eloud-speak "eloud off"))))


(provide 'eloud)


;;; eloud.el ends here

;; Copyright (C) 2016  Patrick Smyth

;; Author: Patrick Smyth <patricksmyth01>
