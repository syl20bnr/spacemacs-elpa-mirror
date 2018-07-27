;;; prettify-greek.el --- Greek letters for prettify-symbols -*- coding: utf-8; -*-

;; Copyright (C) 2016 Sam Halliday
;; License: http://www.gnu.org/licenses/gpl.html

;; Homepage: https://gitlab.com/fommil/emacs-prettify-greek
;; Keywords: faces
;; Package-Version: 20160603.908

;;; Commentary:
;;
;;  Provides a table of Greek letters and their UTF-8 symbols for
;;  use by `prettify-symbols-mode'.
;;
;;  Replaces https://www.emacswiki.org/emacs/PrettyGreek
;;
;;  `prettify-symbols-alist' is a local variable so either
;;  setq-default or add in a major mode hook.
;;
;;  (add-hook 'emacs-lisp-mode-hook
;;            (lambda ()
;;              (setq prettify-symbols-alist prettify-greek-lower)
;;              (prettify-symbols-mode t)))
;;
;;  recall that alists can be combined with `append'.
;;
;;; Code:

(defconst
  prettify-greek-lower
  '(("alpha" . ?α)
    ("beta" . ?β)
    ("gamma" . ?γ)
    ("delta" . ?δ)
    ("epsilon" . ?ε)
    ("zeta" . ?ζ)
    ("eta" . ?η)
    ("theta" . ?θ)
    ("iota" . ?ι)
    ("kappa" . ?κ)
    ("lambda" . ?λ)
    ("mu" . ?μ)
    ("nu" . ?ν)
    ("xi" . ?ξ)
    ("omicron" . ?ο)
    ("pi" . ?π)
    ("rho" . ?ρ)
    ("sigma" . ?σ)
    ("tau" . ?τ)
    ("upsilon" . ?υ)
    ("phi" . ?φ)
    ("chi" . ?χ)
    ("psi" . ?ψ)
    ("omega" . ?ω))
  "Prettify rules for lower case greek letters.")

(defconst
  prettify-greek-upper
  '(("Alpha" . ?Α)
    ("Beta" . ?Β)
    ("Gamma" . ?Γ)
    ("Delta" . ?Δ)
    ("Epsilon" . ?Ε)
    ("Zeta" . ?Ζ)
    ("Eta" . ?Η)
    ("Theta" . ?Θ)
    ("Iota" . ?Ι)
    ("Kappa" . ?Κ)
    ("Lambda" . ?Λ)
    ("Mu" . ?Μ)
    ("Nu" . ?Ν)
    ("Xi" . ?Ξ)
    ("Omicron" . ?Ο)
    ("Pi" . ?Π)
    ("Rho" . ?Ρ)
    ("Sigma" . ?Σ)
    ("Tau" . ?Τ)
    ("Upsilon" . ?Υ)
    ("Phi" . ?Φ)
    ("Chi" . ?Χ)
    ("Psi" . ?Ψ)
    ("Omega" . ?Ω))
  "Prettify rules for upper case greek letters.")

(provide 'prettify-greek)

;;; prettify-greek.el ends here
