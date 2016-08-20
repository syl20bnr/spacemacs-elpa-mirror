;;; anything-milkode.el --- Command line search with Milkode

;; Copyright (C) 2013 ongaeshi

;; Author: ongaeshi
;; Keywords: milkode, anything, search, grep, jump, keyword
;; Package-Version: 20140518.743
;; Version: 0.3
;; Package-Requires: ((milkode "0.3") (anything "1.3.9"))

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Install:
;;   (auto-install-from-url "http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el")
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/milkode.el")
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/emacs-milkode/master/anything-milkode.el")

;;; Initlial Setting:
;; (require 'anything-milkode)
;;
;; ;; Use anything-grep single line mode
;; (setq anything-grep-multiline nil)                
;; 
;; ;; Shortcut setting
;; (global-set-key (kbd "M-g") 'anything-milkode)
;; (global-set-key (kbd "C-x a f") 'anything-milkode-files)
;;
;; ;; popwin setting (Optional)
;; (push '("*grep*" :noselect t)       popwin:special-display-config)
;; (push '("*anything milkode*")       popwin:special-display-config)
;; (push '("*anything milkode files*") popwin:special-display-config)

;;; Code:

;;; Variables:

;;; Public:
(require 'anything-grep)
(require 'milkode)

;;;###autoload
(defun anything-milkode (n)
  "Milkode search using `anything-grep`.
With C-u `milkode:search`"
  (interactive "P")
  (let ((at-point (thing-at-point 'filename))
        (is-milkode:search (consp n)))
    (if is-milkode:search
        (milkode:search)
      (if (milkode:is-directpath at-point)
        (progn
          (setq milkode:history (cons at-point milkode:history)) 
          (milkode:jump-directpath at-point))
      (let* ((input   (read-string "anything-milkode: " (thing-at-point 'symbol) 'milkode:history))
         (command (concat gmilk-command " " input))
         (pwd     default-directory))
        (if (milkode:is-directpath input)
            (milkode:jump-directpath input)
          (anything-grep-base (list (agrep-source (agrep-preprocess-command command) pwd))
                              "*anything milkode*")))))))

(defun anything-c-sources-milkode-files (pwd is-rebuild)
  (loop for elt in
        '(("milk files (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (when (or (not (anything-candidate-buffer))
                              ,is-rebuild)
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format (concat milk-command " files -r")
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

(defvar anything-c-source-milkode-packages
  '((name . "Milkode Packages")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (with-current-buffer
                  (anything-candidate-buffer 'global)
                (insert (shell-command-to-string (format "%s list -d" milk-command)))))))
    (candidates-in-buffer)
    (type . file)
    (real-to-display . (lambda (c) (file-name-nondirectory c)))))

;;;###autoload
(defun anything-milkode-files (n)
  "Jump to registered files and package directories with `anything`.
With C-u clear cache."
  (interactive "P")
  (let* ((pwd default-directory)
         (is-rebuild (consp n))
         (sources
          (list (car (anything-c-sources-milkode-files pwd is-rebuild))
                anything-c-source-milkode-packages)))
    (when is-rebuild
      (kill-buffer " *anything candidates:Milkode Packages*"))
    (anything-other-buffer sources
                           (format "*anything milkode files*"))))

;; 

(provide 'anything-milkode)
;;; anything-milkode.el ends here
