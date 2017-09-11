;;; evil-mu4e.el --- evil-based key bindings for mu4e

;; Copyright (C) 2015 Joris Engbers

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/evil-mu4e
;; Version: 0.0.5
;; Package-Version: 20170911.122
;; Package-Requires: ((emacs "24.4")(dash "2.12.0") (evil "1.2.10"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; evil-mu4e keybindings for Mu4e that make sense for Evil users. The following
;; keybindings are defined:
;;
;; General commands:
;; | Commmand        | evil-mu4e |
;; |-----------------+-----------|
;; | Jump to maildir | J         |
;; | Update          | u         |
;;
;; Commands for view and header mode:
;; | Command                         | evil-mu4e |
;; |---------------------------------+-----------|
;; | next message                    | C-j       |
;; | previous message                | C-k       |
;; | Mark the current thread as read | T         |
;;; Code:

(require 'evil)
(require 'mu4e)
(require 'dash)

(defcustom evil-mu4e-state 'motion
  "State to use in mu4e buffers where keybindings are altered."
  :group 'mu4e
  :type  'symbol)


;;; By default all mu4e modes except for mu4e-compose-mode will start in
;;; evil-emacs-state. This section makes all modes start in evil-motion-state.

(defvar evil-mu4e-emacs-to-evil-mu4e-state-modes
  '(mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    mu4e-org-mode)
  "Modes that should switch from Emacs state to `evil-mu4e-state'.")

(defun evil-mu4e-set-state ()
  "Associate all relevant modes with the evil-mu4e-state."
  (dolist (mode evil-mu4e-emacs-to-evil-mu4e-state-modes)
    (evil-set-initial-state mode evil-mu4e-state)))



;;; Define bindings

(defvar evil-mu4e-mode-map-bindings
  `((,evil-mu4e-state mu4e-main-mode-map "J"               mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-main-mode-map "j"               next-line)
    (,evil-mu4e-state mu4e-main-mode-map "k"               previous-line)
    (,evil-mu4e-state mu4e-main-mode-map "u"               mu4e-update-mail-and-index)
    (,evil-mu4e-state mu4e-main-mode-map "gr"              mu4e-update-mail-and-index)
    (,evil-mu4e-state mu4e-main-mode-map "b"               mu4e-headers-search-bookmark)
    (,evil-mu4e-state mu4e-main-mode-map "N"               mu4e-news)
    (,evil-mu4e-state mu4e-main-mode-map ";"               mu4e-context-switch)
    (,evil-mu4e-state mu4e-main-mode-map "H"               mu4e-display-manual)

    (,evil-mu4e-state mu4e-headers-mode-map "J"            mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-headers-mode-map "j"            next-line)
    (,evil-mu4e-state mu4e-headers-mode-map "k"            previous-line)
    (,evil-mu4e-state mu4e-headers-mode-map ";"            mu4e-context-switch)
    (,evil-mu4e-state mu4e-headers-mode-map ,(kbd "RET")   mu4e-headers-view-message)
    (,evil-mu4e-state mu4e-headers-mode-map "/"            mu4e-headers-search-narrow)
    (,evil-mu4e-state mu4e-headers-mode-map "?"            mu4e-headers-mark-for-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "!"            mu4e-headers-mark-for-read)
    (,evil-mu4e-state mu4e-headers-mode-map "\C-j"         mu4e-headers-next)
    (,evil-mu4e-state mu4e-headers-mode-map "\C-k"         mu4e-headers-prev)
    (,evil-mu4e-state mu4e-headers-mode-map "T"           (lambda ()
                                                          (interactive)
                                                          (mu4e-headers-mark-thread nil '(read))))

    (,evil-mu4e-state mu4e-view-mode-map "H"               mu4e-view-toggle-html)
    (,evil-mu4e-state mu4e-view-mode-map "e"               mu4e-view-save-attachment)
    (,evil-mu4e-state mu4e-view-mode-map "o"               mu4e-view-open-attachment)
    (,evil-mu4e-state mu4e-view-mode-map "A"               mu4e-view-attachment-action)
    (,evil-mu4e-state mu4e-view-mode-map "J"               mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-view-mode-map "\C-j"            mu4e-view-headers-next)
    (,evil-mu4e-state mu4e-view-mode-map "\C-k"            mu4e-view-headers-prev)
    (,evil-mu4e-state mu4e-view-mode-map "?"               mu4e-view-mark-for-unread)
    (,evil-mu4e-state mu4e-view-mode-map "!"               mu4e-view-mark-for-read)
    (,evil-mu4e-state mu4e-view-mode-map "R"               mu4e-compose-reply)
    (,evil-mu4e-state mu4e-view-mode-map "F"               mu4e-compose-forward)
    (,evil-mu4e-state mu4e-view-mode-map "\C-u"            evil-scroll-up)
    (,evil-mu4e-state mu4e-view-mode-map "T"               (lambda ()
                                                           (interactive)
                                                           (mu4e-headers-mark-thread nil '(read)))))
  "All evil-mu4e bindings.")

(defun evil-mu4e-set-bindings ()
  "Set the bindings."
  (dolist (binding evil-mu4e-mode-map-bindings)
    (evil-define-key
      (nth 0 binding) (nth 1 binding) (nth 2 binding) (nth 3 binding))))


;;; Update mu4e-main-view
;;; To avoid confusing the main-view is updated to show the keys that are in use
;;; for evil-mu4e.

(defvar evil-mu4e-begin-region-basic "\n  Basics"
  "The place where to start overriding Basic section.")

(defvar evil-mu4e-end-region-basic "a new message\n"
  "The place where to end overriding Basic section.")

(defvar evil-mu4e-new-region-basic
  (concat (mu4e~main-action-str "\t* [J]ump to some maildir\n" 'mu4e-jump-to-maildir)
          (mu4e~main-action-str "\t* enter a [s]earch query\n" 'mu4e-search)
          (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new))
  "Define the evil-mu4e Basic region.")

(defvar evil-mu4e-begin-region-misc "\n  Misc"
  "The place where to start overriding Misc section.")

(defvar evil-mu4e-end-region-misc "q]uit"
  "The place where to end overriding Misc section.")

(defvar evil-mu4e-new-region-misc
  (concat
   (mu4e~main-action-str "\t* [;]Switch focus\n" 'mu4e-context-switch)
   (mu4e~main-action-str "\t* [u]pdate email & database (Alternatively: gr)\n"
                         'mu4e-update-mail-and-index)

   ;; show the queue functions if `smtpmail-queue-dir' is defined
   (if (file-directory-p smtpmail-queue-dir)
       (mu4e~main-view-queue)
     "")
   "\n"

   (mu4e~main-action-str "\t* [N]ews\n" 'mu4e-news)
   (mu4e~main-action-str "\t* [A]bout mu4e\n" 'mu4e-about)
   (mu4e~main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
   (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit))
  "Define the evil-mu4e Misc region.")

(defun evil-mu4e-replace-region (new-region start end)
  "Insert NEW-REGION instead of the region between START and END where START end END end are regular expressions."
  ;; move to start of region
  (goto-char (point-min))
  (re-search-forward start)

  ;; insert new headings
  (insert "\n\n")
  (insert new-region)
  ;; Delete text until end of region.
  (let ((start-point (point))
        (end-point (re-search-forward end)))
    (delete-region start-point end-point)))


(defun evil-mu4e-update-main-view ()
  "Evil-mu4e-update-main-view updates both the 'Basic' and the 'Misc' region with texts that reflect the new keybindings."
  (evil-mu4e-replace-region evil-mu4e-new-region-basic evil-mu4e-begin-region-basic evil-mu4e-end-region-basic)
  (evil-mu4e-replace-region evil-mu4e-new-region-misc evil-mu4e-begin-region-misc evil-mu4e-end-region-misc)
  )



;;; Initialize evil-mu4e

(defun evil-mu4e-init ()
  "Initialize evil-mu4e if necessary. If mu4e-main-mode is in
evil-state-motion-modes, initialization is already done earlier."
    (evil-mu4e-set-state)
    (evil-mu4e-set-bindings)
    (add-hook 'mu4e-main-mode-hook 'evil-mu4e-update-main-view))

;; Evil-mu4e is only needed if mu4e is loaded.
(eval-after-load "mu4e" '(evil-mu4e-init))

(provide 'evil-mu4e)
;;; evil-mu4e.el ends here
