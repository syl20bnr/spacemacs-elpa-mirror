;;; old-norse-input.el --- An input method for Old Norse  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  David Christiansen

;; Author: David Christiansen <david@davidchristiansen.dk>
;; Version: 0.9
;; Package-Version: 20170816.1842
;; Package-Requires: ((emacs "24"))
;; Keywords: languages
;; Homepage: https://github.com/david-christiansen/emacs-old-norse-input

;; This file is not part of GNU Emacs.

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

;; This is an input method for Old Norse text in Emacs, meant to work
;; with either standard US or standard Scandinavian keyboard layouts.
;; Like TeX input, characters are entered using \, but many use
;; shorter abbreviations than TeX.
;;
;; How to type letters:
;;  - The acute accent is created with \'a, \'y, etc
;;  - Þ, þ, Ð and ð are written \Th, \th, \Dh, \dh
;;  - E and o caudata are written \,e and \,o
;;  - Æ, æ, Œ, œ are written \Ae, \ae, \Oe, and \oe
;;  - Capital and lowercase o diaresis are written \"O and \"o
;;  - Slashed o is written \/o (or \'/o or \'ø with acute accent)
;; Some additional, LaTeX-inspired abbreviations are also available.

;;; Code:
(require 'quail)

;;;###autoload
(quail-define-package
 "old-norse" "Old Norse" "Þ" t
 "An input method for Old Norse text"
 nil t t nil t nil nil nil nil nil t)

;;;###autoload
(quail-define-rules
 ("\\th" ?þ)
 ("\\TH" ?Þ)
 ("\\Th" ?Þ)
 ("\\dh" ?ð)
 ("\\eth" ?ð)
 ("\\DH" ?Ð)
 ("\\Dh" ?Ð)
 ("\\Eth" ?Ð)
 ("\\ko" ?ǫ)
 ("\\,o" ?ǫ)
 ("\\kO" ?Ǫ)
 ("\\,O" ?Ǫ)
 ("\\,e" ?ę)
 ("\\,E" ?Ę)
 ("\\oslash" ?ø)
 ("\\/o" ?ø)
 ("\\Oslash" ?Ø)
 ("\\/O" ?Ø)
 ("\\OSLASH" ?Ø)
 ("\\\"o" ?ö)
 ("\\\"O" ?Ö)
 ("\\ae" ?æ)
 ("\\Ae" ?Æ)
 ("\\AE" ?Æ)
 ("\\'a" ?á)
 ("\\'A" ?Á)
 ("\\'i" ?í)
 ("\\'I" ?Í)
 ("\\'u" ?ú)
 ("\\'U" ?Ú)
 ("\\'e" ?é)
 ("\\'E" ?É)
 ("\\'o" ?ó)
 ("\\'O" ?Ó)
 ("\\'y" ?ý)
 ("\\'Y" ?Ý)
 ("\\'ø" ?ǿ)
 ("\\'Ø" ?Ǿ)
 ("\\'/o" ?ǿ)
 ("\\'/O" ?Ǿ)
 ("\\oe" ?œ)
 ("\\Oe" ?Œ)
 ("\\OE" ?Œ)
 ("\\vend" ?ꝩ)
 ("\\Vend" ?Ꝩ)
 ("\\wynn" ?ƿ)
 ("\\Wynn" ?Ƿ))

(provide 'old-norse-input)
;;; old-norse-input.el ends here
