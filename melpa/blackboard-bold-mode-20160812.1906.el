;;; blackboard-bold-mode.el --- Easily insert Unicode mathematical double-struck characters -*- lexical-binding: t -*-

;; Copyright (C) 2016 Grant Rettke

;; Author: Grant Rettke <gcr@wisdomandwonder.com>
;; Version: 1.0
;; Package-Version: 20160812.1906
;; Package-Requires: ((cl-lib "0.5"))
;; Maintainer: <gcr@wisdomandwonder.com>
;; Keywords: Unicode, Double Struck, Blackboard Bold, Math, Mathematical
;; URL: https://github.com/grettke/blackboard-bold-mode

;;; Commentary:

;; Transliterate ASCII a-z, A-Z, and 1-9 to their Unicode mathematical
;; double-struck equivalent.

;;; Code:

(require 'cl-lib)

(defmacro blackboard-bold-insert (bbb)
  `(lambda ()
     ,(format "Insert the %s blackboard bold character." bbb)
     (interactive)
     (insert ,bbb)))

;;;###autoload
(define-minor-mode blackboard-bold-mode
  "Easily insert Unicode mathematical double-struck characters"
  :lighter " b3"
  :keymap (let ((map (make-keymap))) 
            (cl-dolist (letter-pair '(("a" . "𝕒")
                                      ("b" . "𝕓")
                                      ("c" . "𝕔")
                                      ("d" . "𝕕")
                                      ("e" . "𝕖")
                                      ("f" . "𝕗")
                                      ("g" . "𝕘")
                                      ("h" . "𝕙")
                                      ("i" . "𝕚")
                                      ("j" . "𝕛")
                                      ("k" . "𝕜")
                                      ("l" . "𝕝")
                                      ("m" . "𝕞")
                                      ("n" . "𝕟")
                                      ("o" . "𝕠")
                                      ("p" . "𝕡")
                                      ("q" . "𝕢")
                                      ("r" . "𝕣")
                                      ("s" . "𝕤")
                                      ("t" . "𝕥")
                                      ("u" . "𝕦")
                                      ("v" . "𝕧")
                                      ("w" . "𝕨")
                                      ("x" . "𝕩")
                                      ("y" . "𝕪")
                                      ("z" . "𝕫")
                                      ("A" . "𝔸")
                                      ("B" . "𝔹")
                                      ("C" . "ℂ")
                                      ("D" . "𝔻")
                                      ("E" . "𝔼")
                                      ("F" . "𝔽")
                                      ("G" . "𝔾")
                                      ("H" . "ℍ")
                                      ("I" . "𝕀")
                                      ("J" . "𝕁")
                                      ("K" . "𝕂")
                                      ("L" . "𝕃")
                                      ("M" . "𝕄")
                                      ("N" . "ℕ")
                                      ("O" . "𝕆")
                                      ("P" . "ℙ")
                                      ("Q" . "ℚ")
                                      ("R" . "ℝ")
                                      ("S" . "𝕊")
                                      ("T" . "𝕋")
                                      ("U" . "𝕌")
                                      ("V" . "𝕍")
                                      ("W" . "𝕎")
                                      ("X" . "𝕏")
                                      ("Y" . "𝕐")
                                      ("Z" . "ℤ")
                                      ("0" . "𝟘")
                                      ("1" . "𝟙")
                                      ("2" . "𝟚")
                                      ("3" . "𝟛")
                                      ("4" . "𝟜")
                                      ("5" . "𝟝")
                                      ("6" . "𝟞")
                                      ("7" . "𝟟")
                                      ("8" . "𝟠")
                                      ("9" . "𝟡")))
              (let ((letter (car letter-pair))
                    (bbb (cdr letter-pair)))
                (define-key map letter (blackboard-bold-insert bbb))))            
            map))

(provide 'blackboard-bold-mode)
;;; blackboard-bold-mode.el ends here
