;;; universal-emotions-emoticons.el --- Emoticons For The Six Universal Expressions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Grant Rettke

;; Author: Grant Rettke <gcr@wisdomandwonder.com>
;; Version: 1.0
;; Package-Version: 20171210.220
;; Maintainer: <gcr@wisdomandwonder.com>
;; Keywords: convenience, docs, languages
;; URL: https://github.com/grettke/universal-emotions-emoticons
;; Package-Requires: ((emacs "24.4"))

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

;; Introduction:
;;
;; It is difficult to communicate emotions using words. There are too many.
;; They mean different things to different people. Facial expressions, however,
;; are universal.
;;
;; "Psychological research has classified six facial expressions which
;; correspond to distinct universal emotions: disgust, sadness, happiness,
;; fear, anger, surprise [Black,Yacoob,95]. [Source](https://people.ece.cornell.edu/land/OldStudentProjects/cs490-95to96/HJKIM/emotions.html)
;; They are referred to as the [Ekman Expressions](https://www.paulekman.com/universal-facial-expressions/).
;;
;; All human emotions can be expressed through the combination of
;; these six emotions.
;;
;; Use them to precisely communicate yours.

;; Usage:
;;
;; Select the emotion(s) to add them to the kill-ring, optionally including
;; their description(s).
;;
;; Hit n to go forward, p to go backward, RETURN to select a field,
;; and q to quit.

;; Installation:
;;
;; Whatever your preference.
;;
;; Add it to your load path and
;;
;; `(require 'universal-emotions-emoticons)`
;;
;; or
;;
;; `(use-package universal-emotions-emoticons :ensure t)`

;;; Code:

(require 'widget)
(require 'seq)

(eval-when-compile
  (require 'wid-edit))

(defvar universal-emotions-emoticons--joy nil)
(defconst universal-emotions-emoticons--joy-char "😄")
(defconst universal-emotions-emoticons--joy-desc "Joy")

(defvar universal-emotions-emoticons--surprise nil)
(defconst universal-emotions-emoticons--surprise-char "😮")
(defconst universal-emotions-emoticons--surprise-desc "Surprise")

(defvar universal-emotions-emoticons--sadness nil)
(defconst universal-emotions-emoticons--sadness-char "😢")
(defconst universal-emotions-emoticons--sadness-desc "Sadness")

(defvar universal-emotions-emoticons--anger nil)
(defconst universal-emotions-emoticons--anger-char "😠")
(defconst universal-emotions-emoticons--anger-desc "Anger")

(defvar universal-emotions-emoticons--disgust nil)
(defconst universal-emotions-emoticons--disgust-char "😑")
(defconst universal-emotions-emoticons--disgust-desc "Disgust")

(defvar universal-emotions-emoticons--fear nil)
(defconst universal-emotions-emoticons--fear-char "😱")
(defconst universal-emotions-emoticons--fear-desc "Fear")

(defvar universal-emotions-emoticons--include-description nil)

(defun universal-emotions-emoticons--wrap-description (desc)
  "Prepare user-facing DESC string."
  (string-join (list "(" desc ")")))

(defmacro universal-emotions-emoticons--make-checkbox-description (id desc)
  "Assemble a checkbox description using the ID and DESC."
  (let* ((wrapped-desc (universal-emotions-emoticons--wrap-description
                        (symbol-value desc)))
         (spec (string-join
                (list " "
                      (symbol-value id)
                      " "
                      wrapped-desc
                      "\n"))))
    (list 'widget-insert spec)))

(defun universal-emotions-emoticons--make-value ()
  (if (or universal-emotions-emoticons--joy
         universal-emotions-emoticons--surprise
         universal-emotions-emoticons--sadness
         universal-emotions-emoticons--anger
         universal-emotions-emoticons--disgust
         universal-emotions-emoticons--fear)
      (progn
        (let* ((candidates
                (list
                 (when universal-emotions-emoticons--joy
                   universal-emotions-emoticons--joy-char)
                 (when (and universal-emotions-emoticons--joy
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--joy-desc))
                 (when universal-emotions-emoticons--surprise
                   universal-emotions-emoticons--surprise-char)
                 (when (and universal-emotions-emoticons--surprise
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--surprise-desc))
                 (when universal-emotions-emoticons--sadness universal-emotions-emoticons--sadness-char)
                 (when (and universal-emotions-emoticons--sadness
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--sadness-desc))
                 (when universal-emotions-emoticons--anger universal-emotions-emoticons--anger-char)
                 (when (and universal-emotions-emoticons--anger
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--anger-desc))
                 (when universal-emotions-emoticons--disgust universal-emotions-emoticons--disgust-char)
                 (when (and universal-emotions-emoticons--disgust
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--disgust-desc))
                 (when universal-emotions-emoticons--fear universal-emotions-emoticons--fear-char)
                 (when (and universal-emotions-emoticons--fear
                          universal-emotions-emoticons--include-description)
                   (universal-emotions-emoticons--wrap-description universal-emotions-emoticons--fear-desc))))
               (selected (seq-filter 'stringp candidates))
               (value (string-join selected)))
          (kill-new value)
          (message (format "Added To Kill-Ring: %s" value))))
    (progn
      (message "Nothing added to the kill-ring: no emotions selected."))))
(defun universal-emotions-emoticons ()
  "Insert emoticons for the six universal expressions."
  (interactive)
  (switch-to-buffer "*Universal Emotion Emoticons*")
  (kill-all-local-variables)
  (make-local-variable 'universal-emotions-emoticons--joy)
  (make-local-variable 'universal-emotions-emoticons--surprise)
  (make-local-variable 'universal-emotions-emoticons--sadness)
  (make-local-variable 'universal-emotions-emoticons--anger)
  (make-local-variable 'universal-emotions-emoticons--disgust)
  (make-local-variable 'universal-emotions-emoticons--fear)
  (make-local-variable 'universal-emotions-emoticons--description)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert
   "It is difficult to communicate emotions using words. There are too many.\n")
  (widget-insert
   "They mean different things to different people. Facial expressions, however,\n")
  (widget-insert
   "are universal.")
  (widget-insert
   "\n\n")
  (widget-insert
   "\"Psychological research has classified six facial expressions which\n")
  (widget-insert
   "correspond to distinct universal emotions: disgust, sadness, happiness,\n")
  (widget-insert
   "fear, anger, surprise [Black,Yacoob,95]. ")
  (widget-create
   'url-link
   :tag "Source"
   "https://people.ece.cornell.edu/land/OldStudentProjects/cs490-95to96/HJKIM/emotions.html")
  (widget-insert " They are referred to\n")
  (widget-insert "as the ")
  (widget-create
   'url-link
   :tag "Ekman Expressions"
   "https://www.paulekman.com/universal-facial-expressions/")
  (widget-insert ".")
  (widget-insert "\n\n")
  (widget-insert "All human emotions can be expressed through the combination of\n")
  (widget-insert "these six emotions.")
  (widget-insert "\n\n")
  (widget-insert "Use them to precisely communicate yours.")
  (widget-insert "\n\n")
  (widget-insert "Select the emotion(s) to add them to the kill-ring, \n")
  (widget-insert "optionally including their description(s).\n\n")
  (widget-insert "Hit n to go forward, p to go backward, RETURN to select a field,\n")
  (widget-insert "and q to quit.")
  (widget-insert "\n\n")
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--joy
                         (not
                          universal-emotions-emoticons--joy))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--joy-desc
                                    universal-emotions-emoticons--joy))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--joy-char
   universal-emotions-emoticons--joy-desc)
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--surprise
                         (not
                          universal-emotions-emoticons--surprise))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--surprise-desc
                                    universal-emotions-emoticons--surprise))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--surprise-char
   universal-emotions-emoticons--surprise-desc)
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--sadness
                         (not
                          universal-emotions-emoticons--sadness))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--sadness-desc
                                    universal-emotions-emoticons--sadness))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--sadness-char
   universal-emotions-emoticons--sadness-desc)
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--anger
                         (not
                          universal-emotions-emoticons--anger))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--anger-desc
                                    universal-emotions-emoticons--anger))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--anger-char
   universal-emotions-emoticons--anger-desc)
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--disgust
                         (not
                          universal-emotions-emoticons--disgust))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--disgust-desc
                                    universal-emotions-emoticons--disgust))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--disgust-char
   universal-emotions-emoticons--disgust-desc)
  (widget-create 'checkbox
                 :notify
                 (lambda (&rest args)
                   (setq universal-emotions-emoticons--fear
                         (not
                          universal-emotions-emoticons--fear))
                   (message (format "%s: %s"
                                    universal-emotions-emoticons--fear-desc
                                    universal-emotions-emoticons--fear))
                   (universal-emotions-emoticons--make-value))
                 nil)
  (universal-emotions-emoticons--make-checkbox-description
   universal-emotions-emoticons--fear-char
   universal-emotions-emoticons--fear-desc)
  (widget-insert "\n")
  (widget-insert "Include description(s)?\n")
  (widget-create 'radio-button-choice
                 :value "No"
                 :notify
                 (lambda (widget &rest ignore)
                   (setq universal-emotions-emoticons--include-description
                         (equal (widget-value widget) "Yes"))
                   (message (format "Include description(s)? %s"
                                    universal-emotions-emoticons--include-description))
                   (universal-emotions-emoticons--make-value))
                 '(item "No")
                 '(item "Yes"))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    (define-key map "q" 'kill-this-buffer)
    (use-local-map map))
  (widget-setup)
  (progn
    (search-backward universal-emotions-emoticons--joy-char)
    (beginning-of-line)))

(provide 'universal-emotions-emoticons)
;;; universal-emotions-emoticons.el ends here
