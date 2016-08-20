;;; empos.el --- Locate bibtex citations from within emacs

;; Copyright (C) 2015, Dimitris Alikaniotis

;; Author: Dimitris Alikaniotis <da352 [at] cam.ac.uk>
;; Keywords: citations, reference, bibtex, reftex
;; Package-Version: 20151011.1216
;; URL: http://github.com/dimalik/empos/
;; Version: 0.1
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;; * Neither the name of this package nor the
;;   names of its contributors may be used to endorse or promote products
;;   derived from this software without specific prior written permission.
;;
;; This software is provided by the copyright holders and contributors "as
;; is" and any express or implied warranties, including, but not limited
;; to, the implied warranties of merchantability and fitness for a
;; particular purpose are disclaimed. In no event shall Dimitris Alikaniotis be
;; liable for any direct, indirect, incidental, special, exemplary, or
;; consequential damages (including, but not limited to, procurement of
;; substitute goods or services; loss of use, data, or profits; or business
;; interruption) however caused and on any theory of liability, whether in
;; contract, strict liability, or tort (including negligence or otherwise)
;; arising in any way out of the use of this software, even if advised of
;; the possibility of such damage.
;;
;; (also known as the New BSD License)
;;
;;; Commentary:
;;
;; Emacs wrapper for pyopl (python online paper locator) to search and fetch
;; scientific citations online and add them to a bib file.
;;
;;; Installation:
;;
;;   (require 'empos)
;;   (setq empos-available-engines '("arxiv" "crossref"))
;;   (setq empos-favorite-engines '("crossref")) <- optional
;;   (setq empos-bib-file "path/to/bibliography.bib")
;;   (setq empos-secondary-bib "path/to/a/folder")
;;
;;  empos-available-engines should contain engines that have been installed in pyopl.
;;  empos-favorite-engines contains the engines to be used. Note this is a custom variable
;;    and can be set through customization.
;;  empos-bib-file is the (absolute) path to the master bibliography file in which the
;;    references are appended.
;;  empos-secondary-bib is the (absolute) path to a folder in which the citations are going
;;    to be added.
;;
;;; Use:
;;
;; The extension is essentially a wrapper for pyopl written for emacs.
;; It works by calling pyopl with arguments specified in emacs, displaying
;; the results in a separate buffer and saving the references in a specified
;; location.
;;
;; The location of the pyopl executable is considered to be global (i.e, it can be
;; invoked like this:
;;
;; >> pyopl "you talkin to me"
;; )
;;
;; In case something goes wrong and this does not work (might be the case in
;; virtualenvs), you can respecify the variable `pyopl-path'.
;;
;; The engines which are used are specified in `empos-favorite-engines' which is a list
;; of string containing the names of the engines. If no such variable is declared then
;; the search is done on all available engines defined in `empos-available-engines'.
;;
;; The actual search is carried by an interactive function `empos-search' displaying its
;; output on a new buffer defining an minor mode called `empos-mode' to ensure better
;; interaction.
;;
;; Upon hitting <RET> the function `empos-get-identifier' is called using a regex to
;; fetch the relevant id and engine and calling pyopl executable again, this time in
;; fetch mode.
;;
;;; Code:

(defgroup empos nil
  "Search citations online."
  :group 'convenience)

(defcustom empos-available-engines nil
  "List of the available engines for pyopl.
This should be specified in the .emacs file."
  :type 'list
  :require 'empos-base
  :group 'empos)

(defcustom empos-favorite-engines empos-available-engines
  "List of your favourite engines.
When specified then empos-search uses only these to find your query.  If not
specified empos-search uses all available engines found in the
empos-available-engines variable in .emacs."
  :type 'list
  :require 'empos-base
  :group 'empos)

(defconst empos-citation-height 4
  "The number of lines each citation has when searched from empos.py.")

(defcustom empos-pyopl-path "pyopl"
  "Path to the pyopl executable.
Normally, this would be available globally (i.e. invakable as a terminal
command), however, in the case something goes wrong, you can specify the
full path in this variable."
  :group 'empos)

(defun empos-quit-window ()
  "Close the empos window."
  (interactive)
  (empos-mode -1)
  (quit-window))

(defun empos-move-up ()
  "Move the cursor up by the given height."
  (interactive)
  (forward-line (- 0 empos-citation-height)))

(defun empos-move-down ()
  "Move the cursor down by the given height."
  (interactive)
  (forward-line empos-citation-height))

(defun empos-visual-line-range ()
  "Change the line height to the height of the citation."
  (save-excursion
    (cons (progn (vertical-motion 0) (point))
	  (progn (vertical-motion empos-citation-height) (point)))))

(defvar empos-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'empos-quit-window)
    (define-key map (kbd "<down>") 'empos-move-down)
    (define-key map (kbd "<up>") 'empos-move-up)
    (define-key map (kbd "<return>")  'empos-get-identifier)
    (define-key map (kbd "RET") 'empos-get-identifier)
    map))

(define-minor-mode empos-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " Empos"
  :keymap empos-mode-map
  (if (not (string= (buffer-name) "*Empos*"))
      (progn
	(empos-mode -1)
	(error "empos-mode should be only run on an *Empos* buffer."))
    (progn
      (defvar hl-line-range-function)
      (if empos-mode
	  (progn
	    (set (make-local-variable 'hl-line-range-function) #'empos-visual-line-range)
	    (hl-line-mode 1))
	(progn
	  (makunbound 'hl-line-range-function)
	  (hl-line-mode -1))))))

(add-hook 'empos-mode-hook (lambda () (setq truncate-lines t)))

(defun empos-take-me-to-first-line ()
  "Return the cursor to the first line of the citation.
Ensures we are on the first line of the reference
which contains the identifier and the engine."
  (interactive)
  (beginning-of-line)
  (let ((current-line-num (+ 1 (count-lines 1 (point)))))
    (while (not (eq (% current-line-num empos-citation-height) 1))
      (forward-line -1)
      (setq current-line-num (+ 1 (count-lines 1 (point)))))))

(defun empos-get-identifier ()
  "Extract the paper identifier and its associated engine.
The identifier is assumed to be between angle brackets,
and the engine between parentheses."
  (interactive)
  (empos-take-me-to-first-line)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "\\[\\(.*\\)\\][[:blank:]]*\(\\(.*\\)\)" line)
	(let* ((identifier (match-string 1 line))
	       (engine (match-string 2 line))
	       (script (format "%s --fetch --engines=\"%s\""
			       empos-pyopl-path engine)))
	  (if (boundp 'empos-bib-file)
	      (setq script (concat script (format " --bib=\"%s\"" empos-bib-file))))
	  (if (boundp 'empos-secondary-bib)
	      (setq script (concat script (format " --secondary-bib=\"%s\"" empos-secondary-bib))))
	  (setq script (concat script (format " \"%s\"" identifier) " 2> /dev/null"))
	  (shell-command script nil)
	  (message "Article with id %s was successfully copied to your library." identifier)
	  (empos-mode -1)
	  (kill-buffer-and-window)))))

;;;###autoload
(defun empos-search (q &optional engines)
  "Send the query Q to pyopl.py.
If ENGINES is not provided it defaults to your favourite engines."
  (interactive "sEnter query: ")
  (unless engines (setq engines empos-favorite-engines))
  (setq engines (mapconcat 'identity engines ","))
  (let* ((scriptName (format "%s --search --engines=%s \"%s\" 2> /dev/null"
			    empos-pyopl-path engines q)))
    (save-excursion
      (switch-to-buffer-other-window "*Empos*")
      (shell-command scriptName "*Empos*")
      (empos-mode 1))))

(provide 'empos)
;;; empos.el ends here
