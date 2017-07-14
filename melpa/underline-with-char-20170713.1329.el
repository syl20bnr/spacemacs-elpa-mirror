;;; underline-with-char.el --- Underline with a char  -*- lexical-binding: t ; eval: (view-mode 1) -*-

;; THIS FILE HAS BEEN GENERATED.

;; Program
;; :PROPERTIES:
;; :ID:       17c5897e-3413-4576-aa83-3869e0cb1053
;; :END:


;; [[id:17c5897e-3413-4576-aa83-3869e0cb1053][Program:1]]
;; THIS FILE HAS BEEN GENERATED.


;;
;; Version: 3.0.0
;; Package-Version: 20170713.1329
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience

;;; Commentary:
;;
;; This program supports underlining with a certain character.
;;
;; When point is in an empty line then fill the line with a character
;; making it as long as the line above.
;;
;; This program provides just command =underline-with-char=.
;;

;; Examples
;; ========
;;
;; Notation:
;; - | means the cursor.
;; - RET means the return key.
;;

;; Full underlining
;; ................
;;
;; Input
;; _____
;;
;; lala
;; |
;;
;; Action
;; ______
;;
;; M-x underline-with-char RET
;;
;; Output
;; ______
;;
;; lala
;; ----|
;;

;; Partial underlining
;; ...................
;;
;; Input
;; _____
;;
;; lolololo
;; //|
;;
;; Action
;; ______
;;
;; M-x underline-with-char RET
;;
;; Output
;; ______
;;
;; lolololo
;; //------|
;;

;; Use a certain char for current and subsequent underlinings (1)
;; ..............................................................
;;
;; Input
;; _____
;;
;; lala
;; |
;;
;; Action
;; ______
;;
;; C-u M-x underline-with-char X RET
;;
;; Output
;; ______
;;
;; lala
;; XXXX|
;;

;; Use a certain char for current and subsequent underlinings (2)
;; ..............................................................
;;
;; Input
;; _____
;;
;; lala
;; |
;;
;; Action
;; ______
;;
;; C-u M-x underline-with-char X RET RET M-x underline-with-char RET
;;
;; Output
;; ______
;;
;; lala
;; XXXX
;; XXXX|


;;; Code:


(defcustom underline-with-char-fill-char ?-
  "The character for the underline."
  :group 'underline-with-char
  :type 'character)


;;;###autoload
(defun underline-with-char (arg)
  "Underline the line above with a certain character.

Fill what's remaining if not at the first position.

The default character is `underline-with-char-fill-char'.

With prefix ARG use the next entered character for this and
subsequent underlining.

Example with `underline-with-char-fill-char' set to '-' and point
symbolized as | and starting with

;; Commentary:
;; |

get

;; Commentary:
;; -----------|"
  (interactive "P")
  (when (equal '(4) arg)
    (setq underline-with-char-fill-char (read-char "char: ")))
  (insert
   (make-string
    (save-excursion
      (let ((col (current-column)))
        (forward-line -1)
        (end-of-line)
        (when (< col (current-column))
          (beginning-of-line)
          (forward-char col)))
      (let ((old-point (point)))
        (- (progn (end-of-line) (point)) old-point)))
        underline-with-char-fill-char)))


(provide 'underline-with-char)
;; Program:1 ends here


;;; underline-with-char.el ends here
