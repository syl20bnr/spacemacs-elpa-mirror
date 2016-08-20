;;; wpuzzle.el --- find as many word in a given time  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Ivan Kanis <ivan@kanis.fr>
;; Version: 1.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Find as many word as possible in a 100 seconds.  Words are scored by
;; length and the scrablle letter value.

;; M-x 100secwp to start the game

;; You need to have aspell installed, it will check for valid words.

;;;; THANKS:

;; Inspiration from an Android game written by SpiceLabs http://spicelabs.in

;; I dedicate this code to my grandmother who taught me to play Scrabble

;;;; BUGS:

;;;; INSTALLATION:

;; Use ELPA

;; install aspell english dictionary.  On Ubuntu or Debian type the following:

;; sudo apt-get install aspell aspell-en

;;;; TODO

;;  - add other languages such as french
;;  - input letter one by one like the original game
;;  - really stop after 100 seconds
;;  - display something more fancy with letter points (SVG would be cool!)
;;  - use ispell.el
;;  - display best possible score on a given deck at the end of the game
;;  - use gamegrid.el for dealing with high score
;;  - use defcustom for variables
;;  - add unit testing
;;  - use global state less (functional style programming)
;;  - clock ticks with timer
;;  - use face to display picked letter
;;    (insert (propertize "foo" 'face 'highlight))
;;  - kill score buffer when quiting
;;  - use a list instead of a string for the deck letters
;;  - add command to shuffle the deck
;;  - navigate to source code in other window to pretend working while playing

;; search for TODO within the file

;;;; VERSION

;; version 1

;; version 1.1

;; bump version number to see if it gets published

;;; Code:

(require 'thingatpt)

(defvar 100secwp-time-limit 100
  "Number of seconds the game will last.")

(defvar 100secwp-high-score-buffer "100secwp-score"
  "File for holding high scores.")

(defvar 100secwp-high-score-directory
  (locate-user-emacs-file "games/")
  "A directory for storing game high score.")

(defvar 100secwp-high-score-file
  (expand-file-name 100secwp-high-score-buffer 100secwp-high-score-directory)
  "Full path to file used for storing game high score.")

(defvar 100secwp-buffer "*100secwp*"
  "Game buffer.")

(defvar 100secwp-state
  '((deck-letter)
    (score)
    (start-time)
    (correct-word))
  "Global game state.")

(defconst 100secwp-frequency
  '((?e . 111)
    (?a . 84)
    (?r . 75)
    (?i . 75)
    (?o . 71)
    (?t . 69)
    (?n . 66)
    (?s . 57)
    (?l . 54)
    (?c . 45)
    (?u . 36)
    (?d . 70) ; crank up for verb ending in ed (normally 33)
    (?p . 31)
    (?m . 30)
    (?h . 30)
    (?g . 70) ; same for ing (normally 33)
    (?b . 20)
    (?f . 18)
    (?y . 17)
    (?w . 12)
    (?k . 11)
    (?v . 10)
    (?x . 10) ; (normally 2) remaining letters are cranked up to
    (?z . 10) ;              add a bit of spice to the game :)
    (?j . 10) ; (normally 1)
    (?q . 10))
  "English letter frequency.")

(defconst 100secwp-scrabble
  '((?a . 1) (?b . 3) (?c . 3) (?d . 2) (?e . 1) (?f . 4) (?g . 2) (?h . 4)
    (?i . 1) (?j . 8) (?k . 5) (?l . 1) (?m . 3) (?n . 1) (?o . 1) (?p . 3)
    (?q . 10) (?r . 1) (?s . 1) (?t . 1) (?u . 1) (?v . 4) (?w . 4) (?x . 8)
    (?y . 4) (?z . 10))
  "Scrabble letter values.")

(defmacro 100secwp-state (key)
  "Return KEY stored variable state."
  `(cdr (assoc ',key 100secwp-state)))

(defmacro 100secwp-add (place number)
  "Append number PLACE with CHAR."
  `(setf ,place (+ ,place ,number)))

(defmacro 100secwp-append (place element)
  "Append to list PLACE with ELEMENT."
  `(setf ,place (append ,place (list ,element))))

(defun 100secwp-coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.
\n(fn OBJECT TYPE)"
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
        ((eq type 'string) (if (stringp x) x (concat x)))
        (t (error "Can't coerce %s to type %s" x type))))

(defun 100secwp-pick-letter ()
  "Pick a random letter."
  (string
   (let* ((start 0)
          (sum (let ((ret 0)
                     (list 100secwp-frequency))
                 (while list
                   (setq ret (+ ret (cdr (car list)))
                         list (cdr list))) ret))
          (pick (random sum))
          (ret ?e)
          (list 100secwp-frequency))
     (while list
       (when (< start pick)
         (setq ret (car (car list))))
       (setq start (+ start (cdr (car list)))
             list (cdr list))) ret)))

(defun 100secwp-generate-first-deck ()
  "Generate first deck of letters."
  (let ((word (100secwp-generate-first-deck-1)))
    (while (100secwp-insane-deck word)
      (setq word (100secwp-generate-first-deck-1))) word))

(defun 100secwp-generate-first-deck-1 ()
  "Generate a ten letter deck."
  (let ((word "")
        (index 0))
    (while (< index 10)
      (setq word (concat (100secwp-pick-letter) word)
            index (1+ index))) word))

(defun 100secwp-generate-next-deck (deck input)
  "Remove INPUT in DECK and pick a new letter.
Return new string, nil if INPUT is not in DECK."
  (let ((match (string-match input deck)))
    (when match
      (if (catch 'done
            (while t
              (aset deck match (aref (100secwp-pick-letter) 0))
              (when (not (100secwp-insane-deck deck))
                (throw 'done t)))) deck))))

(defun 100secwp-set-difference (list1 list2)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2."
  (if (or (null list1) (null list2)) list1
    (let ((res nil))
      (while list1
        (when (not (member (car list1) list2))
          (setq res (cons (car list1) res)))
        (setq list1 (cdr list1))) res)))

(defun 100secwp-insane-deck (word)
  "Return nil if deck is nice to play with."
  (let ((vowel-count 0)
        (index 0)
        (vowel '(?a ?e ?i ?o ?y))
        (three-identical-letter nil)
        (letter-count-alist
         (let ((character ?a) list)
           (while (<= character ?z)
             (setq list (append list (list (cons character 0)))
                   character (1+ character))) list)))
    ;; vowel-count vowels and consonant
    (while (< index (length word))
      (when (member (aref word index) vowel)
        (setq vowel-count (1+ vowel-count)))
      (setq index (1+ index)))
    (setq vowel-count (or vowel-count 0))
    ;; count same letter
    (setq index 0)
    (while (< index (length word))
      (when (>= (100secwp-add
                 (cdr (assoc (aref word index) letter-count-alist)) 1)
                3)
        (setq three-identical-letter t))
      (setq index (1+ index)))
    (or (< vowel-count 4) (< (- (length word) vowel-count) 3)
        three-identical-letter)))

(defun 100secwp-sum-word (word)
  "Return sum of WORD with Scrabble letter value and length."
  (let ((length (length word))
        (sum 0)
        (index 0))
    (while (< index length)
      (setq sum (+ sum (cdr (assoc (aref word index) 100secwp-scrabble))))
      (setq index (1+ index)))
    (cond ((< length 3)
           (setq sum 0))
          ((> length 10)
           (setq sum (+ sum 100)))
          (t
           (setq sum (+ sum
                        (cdr (assoc length
                                    '((3 . 5) (4 . 10) (5 . 20) (6 . 40)
                                      (7 . 50) (8 . 75) (9 . 85))))))))

    sum))

(defun 100secwp-begin-game ()
  "Reset game state. Display deck."
  (setf (100secwp-state start-time) (float-time))
  (setf (100secwp-state score) 0)
  (setf (100secwp-state deck-letter)
        (let ((word (100secwp-generate-first-deck-1)))
          (while (100secwp-insane-deck word)
            (setq word (100secwp-generate-first-deck-1))) word))
  (100secwp-generate-first-deck)
  (setf (100secwp-state correct-word) nil)
  (100secwp-display-deck nil nil 100secwp-time-limit))

(defun 100secwp-display-deck (invalid-word invalid-input time-left)
  (erase-buffer)
  (when (<= time-left 0)
    (setq time-left 0))
  (insert (format (concat "%d second"
                          (if (> time-left 1) "s")
                          " left        Score %d        High score %d\n")
                  time-left
                  (100secwp-state score)
                  (100secwp-retrieve-high-score)))
  (let ((deck (100secwp-state deck-letter)))
    (insert "\n ")
    (100secwp-display-deck-1 (upcase (substring deck 0 3)))
    (100secwp-display-deck-1 (upcase (substring deck 3 7)))
    (insert " ")
    (100secwp-display-deck-1 (upcase (substring deck 7 10))))
  (when (stringp invalid-word)
    (insert (format "\nThe word %s does not exist.\n" invalid-word)))

  (when invalid-input
    (insert (format "\nThe following letters are not in the deck: %s\n"
                    (100secwp-coerce invalid-input 'string))))
  (if (= time-left 0)
      (progn
        (100secwp-end-game)
        (insert "\nThe game is over. Press enter to play one more time.\n\n"))
    (insert "\nEnter word: ")))

(defun 100secwp-display-deck-1 (letter)
  (let ((index 0))
    (while (< index (length letter))
      (insert (substring letter index (+ 1 index)) " ")
      (setq index (1+ index)))
    (insert "\n")))

(defun 100secwp-word-exist (word)
  "Return t when WORD exists in dictionary."
  (with-temp-buffer
    (erase-buffer)
    (let ((process
           (start-process
            "100secwp" (current-buffer)
            "aspell" "-a" "-B" "--encoding=utf-8")))
      (process-send-string nil
                           (concat"%n\n^" word "\n"))
      (while (accept-process-output process 0.1))
      (goto-char (point-min))
      (re-search-forward "^\*$" nil t))))


(defun 100secwp-substitute-letter (input)
  "Pick new letter that are proposed from INPUT."
  (let ((index 0)
        (length (length input))
        exist letter)
    (while (< index length)
      (setq letter (substring input index (+ 1 index)))
      (setq exist (100secwp-generate-next-deck
                   (100secwp-state deck-letter) letter))
      (when exist
        (setf (100secwp-state deck-letter) exist))
      (setq index (1+ index)))))


(defun 100secwp-check-input (input)
  "Return list of character from INPUT that are not in the deck."
  (100secwp-set-difference
   (100secwp-coerce input 'list)
   (100secwp-coerce (100secwp-state deck-letter) 'list)))

(defun 100secwp-retrieve-high-score ()
  (when (not (file-exists-p 100secwp-high-score-directory))
    (make-directory 100secwp-high-score-directory))
  (with-current-buffer (find-file-noselect 100secwp-high-score-file)
    (goto-char (point-min))
    (prog1
        (if (word-at-point)
            (string-to-number (word-at-point))
          (erase-buffer)
          (insert "0")
          (save-buffer)
          0)
      (kill-buffer))))

(defun 100secwp-end-game ()
  (let ((max-length 1)
        (score (100secwp-state score)))
    (when (not (= (100secwp-state score) 0))
      (insert "\n\n")
      (dolist (word (100secwp-state correct-word))
        (when (> (length word) max-length)
          (setq max-length (length word))))
      (dolist (word (100secwp-state correct-word))
        (insert (format (concat "%-" (int-to-string max-length) "s %d\n")
                        word (100secwp-sum-word word))))
      (insert (make-string (+ 4 max-length) ?-) "\n")
      (insert "sum " (make-string (- max-length 3) ? )
              (int-to-string score) "\n")
      (when (> (100secwp-state score) (100secwp-retrieve-high-score))
        (insert "\nCongratulation, you beat the high score!\n")
        ;; TODO there is duplication with 100secwp-retrieve-high-score
        ;; maybe it could be refactored in one function setter and getter.
        (with-current-buffer
            (find-file-noselect 100secwp-high-score-file)
          (erase-buffer)
          (insert (int-to-string score))
          (save-buffer)
          (kill-buffer))))))

(defun 100secwp-read-input ()
  "Read input from player."
  (interactive)
  (let ((input (word-at-point))
        (time-left (- 100secwp-time-limit
                      (- (float-time) (100secwp-state start-time))))
        (invalid-word nil)
        (invalid-input nil))
    (when (< time-left 0)
      (setq time-left 0))
    (when input
      (setq invalid-input (100secwp-check-input input))
      (when (not invalid-input)
        (if (100secwp-word-exist input)
            (progn
              (100secwp-add (100secwp-state score)
                            (100secwp-sum-word input))
              ;; Update global list of correct word to be
              ;; displayed at the end of the game."
              (100secwp-append (100secwp-state correct-word) input)
              (100secwp-substitute-letter input) t)
          (setq invalid-word input))))
    (if (and (not input) (= time-left 0))
        (100secwp-begin-game)
      (100secwp-display-deck invalid-word invalid-input time-left))))

(define-derived-mode 100secwp-mode text-mode "100secwp"
  "Major mode for the word by word game."
  (100secwp-begin-game)
  (use-local-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "RET") '100secwp-read-input) map))
  (100secwp-begin-game))

;;;###autoload
(defun 100secwp ()
  "Start game."
  (interactive)
  (switch-to-buffer 100secwp-buffer)
  (erase-buffer)
  (switch-to-buffer 100secwp-buffer)
  (erase-buffer)
  (insert (format "Welcome to %d seconds word puzzle!

You have %d seconds to type as many word made out of the
letters presented. Longer words are worth more points. The letters
are scored with Scrabble value.

Press any key to start." 100secwp-time-limit 100secwp-time-limit))
  (while (not (aref (read-key-sequence nil) 0))
    (sit-for 1))
  (100secwp-mode))

(provide '100secwp)

;; Local Variables:
;; compile-command: "make"
;; End:

;;;; ChangeLog:

;; 2014-01-15  Ivan Kanis	<ivan@kanis.fr>
;; 
;; 	bump version number to see if it gets published
;; 
;; 2014-01-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* wpuzzle.el: Minor cleanup.  Run checkdoc-current-buffer.
;; 
;; 	(100secwp-high-score-file): Use expand-file-name.
;; 	(100secwp-add): Remove redundant `progn'.
;; 	(100secwp-retrieve-high-score, 100secwp-end-game): Don't use find-file.
;; 
;; 2014-01-09  Ivan Kanis	<ivan@kanis.fr>
;; 
;; 	move wpuzzle in packages directory
;; 


(provide 'wpuzzle)
;;; wpuzzle.el ends here
