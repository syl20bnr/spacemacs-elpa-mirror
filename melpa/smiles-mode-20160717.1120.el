;;; smiles-mode.el --- Major mode for SMILES.
;; -*- coding: utf-8 -*-

;; Keywords: SMILES
;; Package-Version: 20160717.1120
;; Version: 0.0.1
;; Author: John Kitchin [jkitchin@andrew.cmu.edu]

;;; Commentary:

;; I copy code from:
;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/26/A-molecule-link-for-org-mode

;;; Code:

;; smiles major mode
(require 'easymenu)

(defun smiles-cml ()
  "Convert the smiles string in the buffer to CML."
  (interactive)
  (let ((smiles (buffer-string)))
    (switch-to-buffer (get-buffer-create "SMILES-CML"))
    (erase-buffer)
    (insert
     (shell-command-to-string
      (format "obabel -:\"%s\" -ocml 2> /dev/null"
              smiles)))
    (goto-char (point-min))
    (xml-mode)))

(defun smiles-names ()
  (interactive)
  (browse-url
   (format "http://cactus.nci.nih.gov/chemical/structure/%s/names"
           (buffer-string))))

(defvar smiles-mode-map
  (make-sparse-keymap)
  "Keymap for smiles-mode.")

(define-key smiles-mode-map (kbd "C-c C-c") 'smiles-cml)
(define-key smiles-mode-map (kbd "C-c C-n") 'smiles-names)

(define-key smiles-mode-map [menu-bar] (make-sparse-keymap))

(let ((menu-map (make-sparse-keymap "SMILES")))
  (define-key smiles-mode-map [menu-bar smiles] (cons "SMILES" menu-map))

  (define-key menu-map [cml]
    '("CML" . smiles-cml))
  (define-key menu-map [names]
    '("Names" . smiles-names)))

;;;###autoload
(define-derived-mode smiles-mode fundamental-mode "☺"
  "Major mode for SMILES code."
  (setq buffer-invisibility-spec '(t)))



(provide 'smiles-mode)

;;; smiles-mode.el ends here
