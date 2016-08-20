;;; unipoint.el --- a simple way to insert unicode characters by TeX name
;; 
;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2010, Andrew Gwozdziewycz <git@apgwoz.com>

;; Author: Andrew Gwozdziewycz <git@apgwoz.com>
;; URL : https://github.com/apgwoz/unipoint
;; Package-Version: 20140113.1424

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version. This is
;; distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details. You
;; should have received a copy of the GNU General Public License along with 
;; Emacs; see the file `COPYING'. If not, write to the Free Software 
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; How to use:
;; Add this file to your load-path
;; (require 'unipoint)
;; turn on unipoint-mode
;; chord 

;;; Code:

(require 'thingatpt)

(defconst *unipoint-table*
  '(("Downarrow" "⇓")
    ("nwarrow" "↖")
    ("downarrow" "↓")
    ("Rightarrow" "⇒")
    ("rightarrow" "→")
    ("mapsto" "↦")
    ("searrow" "↘")
    ("swarrow" "↙")
    ("leftarrow" "←")
    ("uparrow" "↑")
    ("Leftarrow" "⇐")
    ("longrightarrow" "−")
    ("Uparrow" "⇑")
    ("Leftrightarrow" "⇔")
    ("updownarrow" "↕")
    ("leftrightarrow" "↔")
    ("nearrow" "↗")
    ("Updownarrow" "⇕")
    ("aleph" "א")
    ("prime" "′")
    ("emptyset" "∅")
    ("nabla" "∇")
    ("diamondsuit" "♦")
    ("spadesuit" "♠")
    ("clubsuit" "♣")
    ("heartsuit" "♥")
    ("sharp" "♯")
    ("flat" "♭")
    ("natural" "♮")
    ("surd" "√")
    ("neg" "¬")
    ("triangle" "△")
    ("forall" "∀")
    ("exists" "∃")
    ("infty" "∞")
    ("circ" "∘")
    ("alpha" "α")
    ("theta" "θ")
    ("tau" "τ")
    ("beta" "β")
    ("vartheta" "θ")
    ("pi" "π")
    ("upsilon" "υ")
    ("gamma" "γ")
    ("varpi" "π")
    ("phi" "φ")
    ("delta" "δ")
    ("kappa" "κ")
    ("rho" "ρ")
    ("varphi" "φ")
    ("epsilon" "ε")
    ("lambda" "λ")
    ("varrho" "ρ")
    ("chi" "χ")
    ("varepsilon" "ε")
    ("mu" "μ")
    ("sigma" "σ")
    ("psi" "ψ")
    ("zeta" "ζ")
    ("nu" "ν")
    ("varsigma" "ς")
    ("omega" "ω")
    ("eta" "η")
    ("xi" "ξ")
    ("iota" "ι")
    ("Gamma" "Γ")
    ("Lambda" "Λ")
    ("Sigma" "Σ")
    ("Psi" "Ψ")
    ("Delta" "∆")
    ("Xi" "Ξ")
    ("Upsilon" "Υ")
    ("Omega" "Ω")
    ("Theta" "Θ")
    ("Pi" "Π")
    ("Phi" "Φ")
    ("pm" "±")
    ("cap" "∩")
    ("diamond" "◇")
    ("oplus" "⊕")
    ("mp" "∓")
    ("cup" "∪")
    ("bigtriangleup" "△")
    ("ominus" "⊖")
    ("times" "×")
    ("uplus" "⊎")
    ("bigtriangledown" "▽")
    ("otimes" "⊗")
    ("div" "÷")
    ("sqcap" "⊓")
    ("triangleleft" "▹")
    ("oslash" "⊘")
    ("ast" "∗")
    ("sqcup" "⊔")
    ("vee" "∨")
    ("wedge" "∧")
    ("triangleright" "◃")
    ("odot" "⊙")
    ("star" "★")
    ("dagger" "†")
    ("bullet" "•")
    ("ddagger" "‡")
    ("wr" "≀")
    ("amalg" "⨿")
    ("leq" "≤")
    ("geq" "≥")
    ("equiv" "≡")
    ("models" "⊨")
    ("prec" "≺")
    ("succ" "≻")
    ("sim" "∼")
    ("perp" "⊥")
    ("top" "⊤")
    ("preceq" "≼")
    ("succeq" "≽")
    ("simeq" "≃")
    ("ll" "≪")
    ("gg" "≫")
    ("asymp" "≍")
    ("parallel" "∥")
    ("subset" "⊂")
    ("supset" "⊃")
    ("approx" "≈")
    ("bowtie" "⋈")
    ("subseteq" "⊆")
    ("supseteq" "⊇")
    ("cong" "≌")
    ("sqsubsetb" "⊏")
    ("sqsupsetb" "⊐")
    ("neq" "≠")
    ("smile" "⌣")
    ("sqsubseteq" "⊑")
    ("sqsupseteq" "⊒")
    ("doteq" "≐")
    ("frown" "⌢")
    ("in" "∈")
    ("ni" "∋")
    ("propto" "∝")
    ("vdash" "⊢")
    ("dashv" "⊣")
    ("sqrt" "√")
    ("skull" "☠") 
    ("smiley" "☺")
    ("blacksmiley" "☻")
    ("frownie" "☹")
    ("S" "§")
    ))

(defun unipoint-replace-symbol (word)
  (let* ((match (try-completion word *unipoint-table*)))
    (cond
     ; word is unique in table, or word is longest subsequence
     ((or (eq t match) (string-equal word match))
      (let ((cp (assoc-string word *unipoint-table*)))
        (if cp
            (progn 
              (kill-backward-chars 1)
              (kill-word 1)
              (insert (cadr cp))
              t)
          'continue)))
     ; word matches multiple targets
     ((and match (not (string-equal word match)))
      (kill-word 1)
      (insert match)
      'replace))))

(defun unipoint-read-replace-symbol ()
  (let* ((ins (completing-read "\\" 
                               (mapcar 'identity *unipoint-table*) nil nil))
         (ent (assoc-string ins *unipoint-table*)))
    (if ent
        (insert (cadr ent))
      (insert (concat "\\" ins))
      t)))

;;;###autoload
(defun unipoint-at-point ()
  "Converts word before point to unicode if appropriate"
  (interactive)
  (let ((word (word-at-point)))
     (if word
        ; check that word is really \word and that it actually
        ; exists
         (let ((result 
                (save-excursion
                  (cond
                   ;; are we at the end of the word
                   ((or (looking-at "$") (looking-at "\s+")) 
                    (if (and (backward-word) ;; move to beginning of word
                             (= (char-before) 92)) ;; previous character is '\'?
                        (unipoint-replace-symbol word)))
                   ((= (char-before) 92);; we're at the beginning of the word
                    (unipoint-replace-symbol word))
                   ;; TODO: we might be somewhere in the middle
                   ))))
           (cond
            ((eq t result) (forward-char 1) t)
            ((eq 'replace result) (forward-word 1) t)
            ((eq 'continue result) t))))))



;;;###autoload
(defun unipoint-insert ()
  "Inserts at point, a unicode codepoint by name"
  (interactive)
  (or (unipoint-at-point)
      (unipoint-read-replace-symbol)))

;;;###autoload
(define-minor-mode unipoint-mode
  "Toggle Unipoint mode."
  ;; initial value
  nil
  ;; indicator
  " UP"
  ;; keybindings
  '(("\C-\\" . unipoint-insert)))

(provide 'unipoint)

;;; unipoint.el ends here
