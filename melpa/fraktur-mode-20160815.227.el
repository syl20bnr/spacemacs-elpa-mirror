;;; fraktur-mode.el --- Easily insert Unicode mathematical Fraktur characters -*- lexical-binding: t -*-

;; Copyright (C) 2016 Grant Rettke

;; Author: Grant Rettke <gcr@wisdomandwonder.com>
;; Version: 1.0
;; Package-Version: 20160815.227
;; Package-Requires: ((cl-lib "0.5"))
;; Maintainer: <gcr@wisdomandwonder.com>
;; Keywords: Unicode, Fraktur, Math, Mathematical
;; URL: https://github.com/grettke/fraktur-mode

;;; Commentary:

;; Transliterate ASCII a-z and A-Z to their Unicode mathematical
;; Fraktur equivalent.  Eszett and umlaut aren't used because the Unicode
;; specification defines these characters only as a mathematical symbol via
;; `http://www.unicode.org/faq/ligature_digraph.html'.
;; Via +saizai `https://plus.google.com/+saizai/posts/V7zxyRYg2EB':
;; "𝕿𝖍𝖊𝖗𝖊 𝖎𝖘 𝖓𝖔 𝖘𝖚𝖈𝖍 𝖙𝖍𝖎𝖓𝖌 𝖆𝖘 𝖇𝖔𝖑𝖉𝖎𝖓𝖌 𝖎𝖓 𝕱𝖗𝖆𝖐𝖙𝖚𝖗.".

;;; Code:

(require 'cl-lib)

(defmacro fraktur-insert (frk)
  `(lambda ()
     ,(format "Insert the %s Fraktur character." frk)
     (interactive)
     (insert ,frk)))

;;;###autoload
(define-minor-mode fraktur-mode
  "Easily insert Unicode mathematical Fraktur characters"
  :lighter " frk"
  :keymap (let ((map (make-keymap)))
            (cl-dolist (letter-pair '(("a" . "𝖆")
                                      ("b" . "𝖇")
                                      ("c" . "𝖈")
                                      ("d" . "𝖉")
                                      ("e" . "𝖊")
                                      ("f" . "𝖋")
                                      ("g" . "𝖌")
                                      ("h" . "𝖍")
                                      ("i" . "𝖎")
                                      ("j" . "𝖏")
                                      ("k" . "𝖐")
                                      ("l" . "𝖑")
                                      ("m" . "𝖒")
                                      ("n" . "𝖓")
                                      ("o" . "𝖔")
                                      ("p" . "𝖕")
                                      ("q" . "𝖖")
                                      ("r" . "𝖗")
                                      ("s" . "𝖘")
                                      ("t" . "𝖙")
                                      ("u" . "𝖚")
                                      ("v" . "𝖛")
                                      ("w" . "𝖜")
                                      ("x" . "𝖝")
                                      ("y" . "𝖞")
                                      ("z" . "𝖟")
                                      ("A" . "𝕬")
                                      ("B" . "𝕭")
                                      ("C" . "𝕮")
                                      ("D" . "𝕯")
                                      ("E" . "𝕰")
                                      ("F" . "𝕱")
                                      ("G" . "𝕲")
                                      ("H" . "𝕳")
                                      ("I" . "𝕴")
                                      ("J" . "𝕵")
                                      ("K" . "𝕶")
                                      ("L" . "𝕷")
                                      ("M" . "𝕸")
                                      ("N" . "𝕹")
                                      ("O" . "𝕺")
                                      ("P" . "𝕻")
                                      ("Q" . "𝕼")
                                      ("R" . "𝕽")
                                      ("S" . "𝕾")
                                      ("T" . "𝕿")
                                      ("U" . "𝖀")
                                      ("V" . "𝖁")
                                      ("W" . "𝖂")
                                      ("X" . "𝖃")
                                      ("Y" . "𝖄")
                                      ("Z" . "𝖅")))
              (let ((letter (car letter-pair))
                    (frk (cdr letter-pair)))
                (define-key map letter (fraktur-insert frk))))
            map))

(provide 'fraktur-mode)
;;; fraktur-mode.el ends here
