;;; quiz.el --- Multiple choice quiz game -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.4
;; Package-Version: 1.4
;; Keywords: games, trivia, quiz
;; URL: https://github.com/davep/quiz.el
;; Package-Requires: ((cl-lib "0.5") (emacs "25"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; quiz.el implements a simple multiple-choice trivia quiz, using
;; https://opentdb.com/ as the back end.

;;; Code:

(require 'cl-lib)
(require 'url-vars)
(require 'json)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defgroup quiz nil
  "Trivia quiz game using Open Trivia DB as the back end."
  :group 'games)

(defface quiz-question-number-face
  '((t :height 1.3
       :background "black"
       :foreground "white"))
  "Face for the question number."
  :group 'quiz)

(defface quiz-question-face
  '((t :weight bold))
  "Face for the question."
  :group 'quiz)

(defconst quiz-source-url "https://opentdb.com/api.php?amount=%d&encode=base64%s%s"
  "URL for loading up questions from the Open Trivia DB.")

(defconst quiz-categories-url "https://opentdb.com/api_category.php"
  "URL for loading up the list of quiz categories.")

(defconst quiz-difficulty-levels '("any" "easy" "medium" "hard")
  "Levels of difficulty of questions.")

(defconst quiz-user-agent "quiz.el (https://github.com/davep/quiz.el)"
  "User agent to send when requesting a quiz.")

(defconst quiz-buffer-name "*Quiz*"
  "Name of the quiz buffer.")

(defun quiz-get (url)
  "Get quiz data from URL."
  (let* ((url-request-extra-headers `(("User-Agent" . ,quiz-user-agent)))
         (buffer (url-retrieve-synchronously url t)))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (setf (point) (point-min))
        (when (search-forward-regexp "^$" nil t)
          (buffer-substring (1+ (point)) (point-max)))))))

(defvar quiz-categories nil
  "Holds the list of quiz categories once they've been loaded.

Never access this directly, always call `quiz-get-categories' instead.")

(defun quiz-lispify-categories (json-categories)
  "Turn JSON-CATEGORIES into a list."
  (cl-loop with categories = (make-hash-table :test #'equal)
           for cat across (alist-get 'trivia_categories (json-read-from-string json-categories))
           do (puthash (alist-get 'name cat) (alist-get 'id cat) categories)
           finally return categories))

(defun quiz-get-categories ()
  "Return the list of quiz categories."
  (or quiz-categories
      (setq quiz-categories (quiz-lispify-categories (quiz-get quiz-categories-url)))))

(defun quiz-get-category-names ()
  "Return a list of category names."
  (cl-loop for cat being the hash-key of (quiz-get-categories)
           collect cat into categories
           finally return (sort categories #'string<)))

(defun quiz-lispify-questions (json-questions)
  "Turn JSON-QUESTIONS into a list."
  (alist-get 'results (json-read-from-string json-questions)))

(defun quiz-category-url-param (category)
  "Return CATEGORY as a parameter for the quiz URL."
  (let ((id (gethash category (quiz-get-categories))))
    (if (null id)
        ""
      (format "&category=%d" id))))

(defun quiz-difficulty-url-param (difficulty)
  "Return DIFFICULTY as a parameter for the quiz URL."
  (if (or (null difficulty) (string= difficulty (car quiz-difficulty-levels)))
      ""
    (format "&difficulty=%s" difficulty)))

(defun quiz-get-questions (&optional count category difficulty)
  "Load COUNT questions from the trivia server.

Ten questions are loaded if COUNT isn't supplied.

Specify CATEGORY to only get questions in that category.

DIFFICULTY can be used top optionally set the difficulty of the questions."
  (quiz-lispify-questions
   (quiz-get (format quiz-source-url
                     (or count 10)
                     (quiz-category-url-param category)
                     (quiz-difficulty-url-param difficulty)))))

(defun quiz-decode (s)
  "Decode S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun quiz-insert-question-text (questions i)
  "From QUESTIONS insert the text of question I."
  (insert
   (propertize
    (quiz-decode (alist-get 'question (aref questions i)))
    'font-lock-face 'quiz-question-face)))

(defun quiz-insert-answers (questions i)
  "From QUESTIONS insert the answers for question I."
  (let ((q (aref questions i)))
    (insert "  ")
    (apply #'widget-create
           'radio-button-choice
           :indent 2
           :notify (lambda (widget &rest _)
                     (setf (alist-get 'given_answer (aref questions i))
                           (base64-encode-string (widget-value widget))))
           (mapcar (lambda (answer)
                     (list 'item answer))
                   (sort
                    (mapcar #'quiz-decode
                            (append (alist-get 'incorrect_answers q)
                                    (list (alist-get 'correct_answer q))))
                    #'string<)))))

(defun quiz-insert-question (questions i)
  "From QUESTIONS insert QUESTION I."
  (insert
   (propertize (format "Question %s:\n" (1+ i)) 'font-lock-face 'quiz-question-number-face)
   "\n")
  (quiz-insert-question-text questions i)
  (insert "\n")
  (quiz-insert-answers questions i)
  (insert "\n"))

(defun quiz-insert-questions (count category difficulty)
  "Get and insert COUNT questions into the current buffer.

Optionally only show questions in the given CATEGORY.

Questions will be at most as hard as DIFFICULTY."
  (let ((questions (quiz-get-questions count category difficulty)))
    (if questions
        (cl-loop for i from 0 to (1- (length questions))
                 do (quiz-insert-question questions i))
      (insert "Sorry. Unable to load up any questions right now."))
    questions))

(defun quiz-insert-finish ()
  "Insert the finish button for the QUESTIONS."
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (quiz-check-answers))
                 :help-echo "Check how many correct answers you have"
                 "Check answers"))

(defun quiz-check-answers ()
  "Show the results of the quiz."
  (interactive)
  (message "%d out of %d questions answered correctly."
           (cl-loop for q across quiz-questions
                    if (string=
                        (alist-get 'correct_answer q)
                        (alist-get 'given_answer q ""))
                    sum 1)
           (length quiz-questions)))

(defun quiz-reload ()
  "Load a new quiz with the current settings."
  (interactive)
  (quiz (length quiz-questions) quiz-category quiz-difficulty))

(defvar quiz-mode-map
  (let ((map widget-keymap))
    (suppress-keymap map t)
    (define-key map " " #'quiz-check-answers)
    (define-key map "r" #'quiz-reload)
    map)
  "Local keymap for `quiz'.")

(defun quiz-mode-header-line ()
  "Return the header line for a `quiz-mode' buffer."
  '(:eval
    (format " Quiz | Questions: %d | Category: %s | Difficulty: %s"
            (length quiz-questions)
            (if (string-empty-p quiz-category) "Any" quiz-category)
            (capitalize quiz-difficulty))))

(define-derived-mode quiz-mode special-mode "Quiz"
  "Major mode for playing `quiz'.

The key bindings for `quiz-mode' are:

\\{quiz-mode-map}"
  (setq truncate-lines     nil
        header-line-format (quiz-mode-header-line))
  (buffer-disable-undo))

(defvar-local quiz-questions nil
  "Holds the questions for the current quiz.")

(defvar-local quiz-category nil
  "Holds the category for the current set of questions.")

(defvar-local quiz-difficulty nil
  "Holds the difficulty for the current set of questions.")

;;;###autoload
(defun quiz (count category difficulty)
  "Play a multiple choice trivia quiz with COUNT questions.

If non-blank, questions from only CATEGORY will be asked.

Questions will be at most as hard as DIFFICULTY."
  (interactive
   (list
    (read-number "Questions: " 10)
    (completing-read "Category (default any): " (quiz-get-category-names) nil t)
    (completing-read
     (format "Difficulty (default %s): " (car quiz-difficulty-levels))
     quiz-difficulty-levels nil t nil nil (car quiz-difficulty-levels))))
  (if (< 51 count 0)
      (error "Between 1 and 50 questions would seem sensible")
    (when (get-buffer quiz-buffer-name)
      (kill-buffer quiz-buffer-name))
    (let ((buffer (get-buffer-create quiz-buffer-name)))
      (with-current-buffer buffer
        (quiz-mode)
        (setq quiz-category   category
              quiz-difficulty difficulty)
        (let ((buffer-read-only nil))
          (setf (buffer-string) "")
          (save-excursion
            (setq quiz-questions (quiz-insert-questions count category difficulty))
            (quiz-insert-finish))
          (widget-forward 1))
        (switch-to-buffer buffer)))))

(provide 'quiz)

;;; quiz.el ends here
