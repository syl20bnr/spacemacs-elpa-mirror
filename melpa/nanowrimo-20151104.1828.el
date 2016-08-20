;;; nanowrimo.el --- Track progress for nanowrimo

;; Copyright (C) 2013  Ivan Andrus <darthandrus@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;; Author:	Ivan Andrus <darthandrus at gmail.com>
;; URL: https://bitbucket.org/gvol/nanowrimo-mode
;; Package-Version: 20151104.1828

;;; Commentary:

;; Adds to the modeline a wordcount, words per minute, and an estimate
;; of how long you will have to continue writing to meet the day's goal.

;; If you plan to use this with org-mode you should install the great
;; org-wc.el from https://github.com/dato/org-wc

;;; Code:

;; TODO: Perhaps make `nanowrimo-mode' hook into auto-fill-mode?  Then
;; it wouldn't be accurate after deletion...  But it might be faster.

;;{{{ Customizable variables

(defgroup nanowrimo nil
  "Word count display for nanowrimo."
  :group 'editing
  :prefix "nanowrimo-")

(defcustom nanowrimo-show-wpm t
  "If non-nil, show words per minute in the mode line."
  :group 'nanowrimo
  :type 'boolean
  :safe 'booleanp)

(defcustom nanowrimo-show-estimate t
  "If non-nil, show an estimate of how long it will take to finish today's goal."
  :group 'nanowrimo
  :type 'boolean
  :safe 'booleanp)

(defcustom nanowrimo-total-goal 50000
  "How many words you would like to write in total."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-start-date
  (format-time-string "Nov 1 00:00:00 %Y")
  "The date string for the first day of NaNoWriMo."
  :group 'nanowrimo
  :type 'string
  :safe 'stringp)

(defcustom nanowrimo-num-days 30
  "How many days you would like to write for."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-today-goal
  (ceiling nanowrimo-total-goal nanowrimo-num-days)
  "How many words you would like to write today."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-count-words-function nil
  "Function used to actually count the words.
It should return an integer, the number of words to be counted.

This can be used to filter out unwanted words, e.g. when editing
LaTeX, HTML and the markup should not be counted.  It should
count as small a portion of the buffer as possible since it will
be called after every change."
  :group 'nanowrimo
  :type 'function)

(defcustom nanowrimo-org-table-name "nanocalc"
  "Name of the table where goals are tracked."
  :group 'nanowrimo
  :type 'string
  :safe 'stringp)

(defcustom nanowrimo-today-goal-calculation-function
  nil
  "How many words you would like to write today."
  :group 'nanowrimo
  :type '(choice (const :tag "No Calculation"
                        nil)
                 (const :tag "From Org Table"
                        nanowrimo-today-goal-from-org-table)
                 (const :tag "From Org Table Quota"
                        nanowrimo-today-goal-from-org-table-quota)
                 (function :tag "Other Function"))
  :safe 'symbolp)

(defcustom nanowrimo-finish-functions nil
  "Functions to call when `nanowrimo-mode' is turned off."
  :group 'nanowrimo
  :type '(repeat (choice (const :tag "Update Org Table"
                                nanowrimo-update-org-table)
                         (function :tag "Other Function"))))

(defcustom nanowrimo-show-suggestions t
  "If non-nil, show suggestions after you haven't written anything for a while."
  :group 'nanowrimo
  :type 'boolean
  :safe 'booleanp)

(defcustom nanowrimo-suggestions-idle-time 60
  "Number of seconds without typing before displaying a suggestion."
  :type 'numberp
  :group 'achievements)

(defcustom nanowrimo-username ""
  "Username for nanowrimo.org."
  :group 'nanowrimo
  :type 'string
  :link '(url-link "http://nanowrimo.org/")
  :safe 'stringp)

(defcustom nanowrimo-api-key nil
  "Key used for updating word count on nanowrimo.org.
Found at http://nanowrimo.org/api/wordcount (must be logged in)."
  :group 'nanowrimo
  :type '(choice (const :tag "Prompt" nil)
                 (string :tag "Secret key" ""))
  :link '(url-link "http://nanowrimo.org/api/wordcount")
  :safe 'stringp)

;;}}}
;;{{{ Internal Variables

(defvar nanowrimo--cur-wc 0
  "The number of words in the current buffer.")
;; (make-variable-buffer-local 'nanowrimo-buffer)

;; (defvar nanowrimo-lines 0
;;   "The number of lines in the current buffer")
;; (make-variable-buffer-local 'nanowrimo-line)

(defvar nanowrimo--display ""
  "The string to display for the mode line.")

(defvar nanowrimo--start-time nil
  "When command `nanowrimo-mode' was turned on.
Used to compute WPM and estimates.")
;; (make-variable-buffer-local 'nanowrimo--start-time)

(defvar nanowrimo--start-wc 0
  "How many words there were at the beginning of the session.")
;; (make-variable-buffer-local 'nanowrimo--start-wc)

(defvar nanowrimo-suggestions-timer nil
  "Holds the idle timer.")

(defvar nanowrimo--org-table-skeleton
  "#+NAME: %s
#+CONSTANTS: base=250 target=1667 increment=4 logbase=100. tdlogbase=100. tclogbase=200.
| <3> | <6>    | <5>   | <7>     | <7>     | <5>   | <6>    | <6>    | <7>     |
| Day | Words  | Quota | Chain   | Cum Wds | Targ  | Cum T  | Plus/min | Score   |
|-----+--------+-------+---------+---------+-------+--------+--------+---------|
|   1 |        |       |         |         |       |        |        |         |
#+TBLFM: $3='(if (eq \"@-1$2\" \"\") \"\" (calcFunc-max $base (+ $base (* $increment @-1$4))));L::$4='(if (eq \"$2\" \"\") \"\" (if (>= $2 $base) (calcFunc-max 0. (+ 1. (+ (calcFunc-max 0. @-1$4) (string-to-number (calc-eval \"log(div($2,$3),$logbase)\"))) )) (if (> @-1$4 0) -1 (- @-1$4 1))));L::$5='(if (eq \"$2\" \"\") \"\" (+ $2 @-1$5));L::$6='(if (eq \"@-1$2\" \"\") \"\" $target);L::$7='(if (eq \"@-1$2\" \"\") \"\" (+ $target @-1$7));L::$8='(if (eq \"$2\" \"\") \"\" (- $5 $7));L::$9='(if (eq \"$2\" \"\") \"\" (if (> $4 0) (calcFunc-max 0 (+ (string-to-number (calc-eval \"log(div($5,$7),$tclogbase)\")) (string-to-number (calc-eval \"log(div($2,$6),$tdlogbase)\")) (calcFunc-max $4 1) @-1$9)) (calcFunc-max 0 (+ @-1$9 $4))));L::@3$3=$base;N::@3$4='(if (eq \"@3$2\" \"\") \"\" (if (>= @3$2 $base) (+ 1.0 (string-to-number (calc-eval \"log(div(@3$2, @3$3),$logbase)\"))) -1.));L::@3$5='(if (eq \"@3$2\" \"\") \"\" @3$2);L::@3$6=$target;N::@3$7=$target;N::@3$8='(if (eq \"@3$2\" \"\") \"\" (- @3$5 @3$7));L::@3$9='(if (eq \"@3$2\" \"\") \"\" (if (> @3$4 0) (calcFunc-max 0 (+ (string-to-number (calc-eval \"log(div(@3$5,@3$7),$tclogbase)\")) (string-to-number (calc-eval \"log(div(@3$2,@3$6),$tdlogbase)\") ) @3$4)) @3$4));L")

;;}}}
;;{{{ Basic Functions

(defun nanowrimo-count-words-region (start end)
  "Count the number of words in the region."
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      n)))

(defun nanowrimo-org-count-subtree ()
  "Count words in the current subtree.
This require the org-wc package."
  (org-word-count-aux (save-excursion (or (outline-previous-heading)
                                          (point-min)))
                      (save-excursion (or (outline-next-heading)
                                          (point-max)))))

(defun nanowrimo-org-count-document ()
  "Count words in the current subtree.
This require the org-wc package."
  (org-word-count-aux (point-min) (point-max)))

(defun nanowrimo-count-words ()
  "Count the words in the current buffer (or relevant portion).
If `nanowrimo-count-words-function' is set, call that, otherwise
call `org-word-count-aux' on the current subtree, if it's
available and the buffer is in `org-mode'.  It falls back to
simply counting words in the entire buffer."
  (save-match-data
    (cond
     ((functionp nanowrimo-count-words-function)
      (funcall nanowrimo-count-words-function))
     ((and (eq major-mode 'org-mode)
           (require 'org-wc nil t))
      (nanowrimo-org-count-document))
     (t
      (nanowrimo-count-words-region (point-min) (point-max))))))

(defun nanowrimo-mode-update (&optional first last len)
  "Update the word count in the mode line.
The optional arguments FIRST LAST and LEN are so that it can be
added to `after-change-functions'."
  (interactive)
  (let* ((wc (nanowrimo-count-words))
         (wc-cur (- wc nanowrimo--start-wc))
         (min (/ (time-to-seconds (time-subtract (current-time) nanowrimo--start-time)) 60))
         (wpm (/ wc-cur min))
         (words-left (- nanowrimo-today-goal wc))
         (estimate (/ words-left wpm)))
    (setq nanowrimo--display
          (concat (format "%sw" wc)
                  (when nanowrimo-show-wpm
                    (format " %dwpm" wpm))
                  (when (and nanowrimo-show-estimate
                             (> estimate 0)
                             ;; When starting, estimate is huge and we don't want to depress anyone
                             (< estimate 600))
                    (format " %dmin" estimate))))))

;;;###autoload
(define-minor-mode nanowrimo-mode
  "Display the number of words, WPM and estimate to finish in the mode line.

When called with prefix-argument, set today's goal to that value
instead of calling `nanowrimo-today-goal-calculation-function'."
  nil "" '()
  (if nanowrimo-mode
      (progn
        (or global-mode-string (setq global-mode-string '("")))
        (add-to-list 'global-mode-string 'nanowrimo--display t)
        (add-to-list 'after-change-functions 'nanowrimo-mode-update)
        (setq nanowrimo--start-wc (nanowrimo-count-words))
        (setq nanowrimo--start-time (current-time))
        (cond
         (current-prefix-arg
          (setq nanowrimo-today-goal (prefix-numeric-value current-prefix-arg)))
         (nanowrimo-today-goal-calculation-function
          (funcall nanowrimo-today-goal-calculation-function)))
        (when (and nanowrimo-show-suggestions
                   (not nanowrimo-suggestions-timer))
          (setq nanowrimo-suggestions-timer
                (run-with-idle-timer nanowrimo-suggestions-idle-time
                                     t #'nanowrimo-show-stuck-suggestion)))
        (nanowrimo-mode-update))
    (setq global-mode-string (delete 'nanowrimo--display global-mode-string))
    (setq nanowrimo-suggestions-timer (and (timerp nanowrimo-suggestions-timer)
                                           (cancel-timer nanowrimo-suggestions-timer)))
    (remove-hook 'after-change-functions 'nanowrimo-mode-update t)
    (run-hooks 'nanowrimo-finish-functions)))

;;}}}
;;{{{ Maintaining org table of score

(defun nanowrimo-days-into-nanowrimo ()
  "Returns how many days into NaNoWriMo today is."
  (1+ (days-between (format-time-string "%FT%T%z") nanowrimo-start-date)))

;;;###autoload
(defun nanowrimo-insert-org-table ()
  "Insert an org-mode table for keeping track of progress.
If a table with a name of `nanowrimo-org-table-name' already exists
then it is merely updated to contain the correct number of days."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (require 'calc-ext nil t))
    (let ((p (save-excursion
               (goto-char (point-min))
               (and (re-search-forward
                     (org-babel-named-data-regexp-for-name nanowrimo-org-table-name) nil t)
                    (re-search-forward "^\\s *|" nil t)
                    (point)))))
      (if p
          (goto-char p)
        (when (not (bolp)) (insert "\n"))
        (insert (format nanowrimo--org-table-skeleton nanowrimo-org-table-name))
        (re-search-backward "^|"))
      (nanowrimo-verify-org-table))))

(defun nanowrimo-verify-org-table ()
  "Ensure that the org table has the right number of rows.

Expects point to be in a table and ensures that the first column
contains 1, 2, 3, ... up to `nanowrimo-num-days'."
  (if (not (org-table-p))
      (user-error "Not at a table")
    (org-table-goto-line 1)
    (while (not (string-match "^ *[0-9]+ *$" (org-table-get-field 1)))
      (forward-line))
    (let ((i 1))
      (while (<= i nanowrimo-num-days)
        (if (org-table-p)
            (progn
              (org-table-get-field 1 (format "%s" i))
              (forward-line))
          (beginning-of-line)
          (insert (format "| %s\n" i)))
        (setq i (1+ i))))
    (forward-line -1)
    (org-table-align)))

(defun nanowrimo-todays-org-table (column replace visit)
  "Interact with today's row in the org table.
COLUMN is the column number to interact with.
If REPLACE is non-nil, the column will be replaced with this.
If VISIT is non-nil, point will be moved to the org table."
  (when (and (eq major-mode 'org-mode)
             (require 'calc-ext nil t))
    (let ((days (nanowrimo-days-into-nanowrimo))
          (p nil)
          (res nil))
      (if (< days 1)
          (user-error "Today is not a NaNoWriMo day.")
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (re-search-forward
             (org-babel-named-data-regexp-for-name nanowrimo-org-table-name))
            (re-search-forward "^\\s *|")
            (nanowrimo-verify-org-table)
            (forward-line 1)
            ;; This is a bit of a hack to find the right row
            (re-search-backward (format "^\\s *| +%d |" days))
            (setq p (point))
            (when (not replace)
              (org-table-recalculate t nil))
            ;; Replace the column or extract it
            (setq res (org-table-get-field column replace))
            (when replace
              (org-table-recalculate t nil))))
        (when (and p visit)
          (goto-char p)
          (when (not (equal p (point)))
            (widen)
            (goto-char p))))
      res)))

;;;###autoload
(defun nanowrimo-update-org-table (&optional novisit)
  "Update the org-mode table calculating the score.
Suitable for adding to `nanowrimo-finish-functions'."
  (interactive "P")
  (let ((wc (nanowrimo-count-words)))
    (nanowrimo-todays-org-table 2 (format "%s" wc) (not novisit))))

(defun nanowrimo-today-goal-from-org-table ()
  "Update today's goal from the org table."
  (let ((g (nanowrimo-todays-org-table 6 nil nil)))
    (when g (setq nanowrimo-today-goal
                  (string-to-number g)))))

(defun nanowrimo-today-goal-from-org-table-quota ()
  "Update today's goal from the org table."
  (let ((g (nanowrimo-todays-org-table 3 nil nil)))
    (when g (setq nanowrimo-today-goal
                  (string-to-number g)))))

;;}}}
;;{{{ Updating nanowrimo.org

;;;###autoload
(defun nanowrimo-update-nanowrimo-org (wc)
  "Send the current word count to nanowrimo.org.
If WC is non-nil, use the numeric value as the word count.
Otherwise, send the value returned by `nanowrimo-count-words'.
If the variables `nanowrimo-username' or `nanowrimo-api-key' are
nil, the user will be prompted for values to be used (but not
stored)."
  (interactive "P")
  (let* ((wordcount (if wc (prefix-numeric-value wc)
                      (nanowrimo-count-words)))
         (go-ahead (y-or-n-p (format
                              "Update nanowrimo.org with a word count of %s"
                              wordcount)))
         (name (or nanowrimo-username
                   (not go-ahead)
                   (completing-read "nanowrimo.org username: " nil)))
         (api-key (or nanowrimo-api-key
                      (not go-ahead)
                      (completing-read "API key: " nil)))
         (hashable (format "%s%s%s" api-key name wordcount))
         (hash (sha1 hashable))
         ;; URL request variables
         (url-request-method "PUT")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (format
                            "hash=%s&name=%s&wordcount=%s"
                            (url-hexify-string hash)
                            (url-hexify-string name)
                            wordcount)))
    (when go-ahead
      (url-retrieve
       "http://nanowrimo.org/api/wordcount"
       (lambda (status)
         (if status
             (switch-to-buffer (current-buffer))
           (message "Wordcount successfully updated.")))))))

;;}}}
;;{{{ Redacted export

;;;###autoload
(defun nanowrimo-redact-region (beg end)
  "Convert all letters in the region to x and all numbers to 9.

The result is then suitable for sending to the word count
function of nanowrimo.org without fear that someone will
intercept your masterpiece."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[[:alpha:]]" end t)
      (replace-match "x"))
    (goto-char beg)
    (while (re-search-forward "[0-9]" end t)
      (replace-match "9"))))

(defun nanowrimo-ignore-empty-org-section (headline contents info)
  (when (org-string-nw-p contents)
    (org-export-with-backend 'ascii headline contents info)))

;; This only works with org 8.0
(when (and (require 'ox-ascii nil t)
           (fboundp 'org-export-define-derived-backend))
  (org-export-define-derived-backend
      'redacted-ascii 'ascii
    :translate-alist '((headline . nanowrimo-ignore-empty-org-section))
    :menu-entry
    '(?w "Redacted NaNoWriMo"
         (lambda (a s v b)
           (org-export-to-buffer 'redacted-ascii "*NaNoWriMo Redacted Export*"
             a s v b '(:ascii-charset ascii) (lambda () (text-mode)))
           (with-current-buffer "*NaNoWriMo Redacted Export*"
             (nanowrimo-redact-region (point-min) (point-max)))))))

;;}}}
;;{{{ Oblique Strategies

(defconst nanowrimo-stuck-suggestions
  '("(Organic) machinery"
    "A line has two sides"
    "A very small object         Its center"
    "Abandon desire"
    "Abandon normal instruments"
    "Accept advice"
    "Accretion"
    "Adding on"
    "Allow an easement (an easement is the abandonment of a stricture)"
    "Always first steps"
    "Always give yourself credit for having more than personality"
    "Always the first steps"
    "Are there sections?  Consider transitions"
    "Ask people to work against their better judgement"
    "Ask your body"
    "Assemble some of the elements in a group and treat the group"
    "Back up a few steps. What else could you have done?"
    "Balance the consistency principle with the inconsistency principle"
    "Be dirty"
    "Be extravagant"
    "Be less critical more often"
    "Breathe more deeply"
    "Bridges   -build   -burn"
    "Call your mother and ask her what to do."
    "Cascades"
    "Change ambiguities to specifics"
    "Change instrument roles"
    "Change nothing and continue with immaculate consistency"
    "Change specifics to ambiguities"
    "Children's voices   -speaking   -singing"
    "Cluster analysis"
    "Consider different fading systems"
    "Consider transitions"
    "Consult other sources   -promising   -unpromising"
    "Convert a melodic element into a rhythmic element"
    "Courage!"
    "Cut a vital connection"
    "Decorate, decorate"
    "Define an area as 'safe' and use it as an anchor"
    "Describe the landscape in which this belongs. (9 August)"
    "Destroy nothing; Destroy the most important thing"
    "Discard an axiom"
    "Disciplined self-indulgence"
    "Disconnect from desire"
    "Discover the recipes you are using and abandon them"
    "Discover your formulas and abandon them"
    "Display your talent"
    "Distorting time"
    "Do nothing for as long as possible"
    "Do something boring"
    "Do something sudden, destructive and unpredictable"
    "Do the last thing first"
    "Do the washing up"
    "Do the words need changing?"
    "Do we need holes?"
    "Don't avoid what is easy"
    "Don't be afraid of things because they're easy to do"
    "Don't be frightened of cliches"
    "Don't be frightened to display your talents"
    "Don't break the silence"
    "Don't stress one thing more than another"
    "Dont be afraid of things because they're easy to do"
    "Dont be frightened to display your talents"
    "Emphasize differences"
    "Emphasize repetitions"
    "Emphasize the flaws"
    "Faced with a choice, do both"
    "Feed the recording back out of the medium"
    "Feedback recordings into an acoustic situation"
    "Fill every beat with something"
    "Find a safe part and use it as an anchor"
    "First work alone, then work in unusual pairs."
    "From nothing to more than nothing"
    "Get your neck massaged"
    "Ghost echoes"
    "Give the game away"
    "Give the name away"
    "Give way to your worst impulse"
    "Go outside.  Shut the door."
    "Go slowly all the way round the outside"
    "Go to an extreme, move back to a more comfortable place"
    "Honor thy error as a hidden intention"
    "How would someone else do it?"
    "How would you explain this to your parents?"
    "How would you have done it?"
    "Humanize something free of error"
    "Idiot glee (?)"
    "Imagine the music as a moving chain or caterpillar"
    "Imagine the piece as a set of disconnected events"
    "In total darkness, or in a very large room, very quietly"
    "Infinitesimal gradations"
    "Instead of changing the thing, change the world around it."
    "Intentions   -nobility of  -humility of   -credibility of"
    "Into the impossible"
    "Is it finished?"
    "Is something missing?"
    "Is the information correct?"
    "Is the style right?"
    "Is the tuning intonation correct?"
    "Is there something missing?"
    "It is quite possible (after all)"
    "It is simply a matter or work"
    "Just carry on"
    "Left channel, right channel, center channel"
    "List the qualities it has. List those you'd like."
    "Listen in total darkness, or in a very large room, very quietly"
    "Listen to the quiet voice"
    "Look at a very small object, look at its centre"
    "Look at the order in which you do things"
    "Look closely at the most embarrassing details and amplify."
    "Lost in useless territory"
    "Lowest common denominator"
    "Magnify the most difficult details"
    "Make a blank valuable by putting it in an exquisite frame"
    "Make a sudden, destructive unpredictable action; incorporate"
    "Make an exhaustive list of everything you might do and do the last thing on the list"
    "Make it more sensual"
    "Make what's perfect more human"
    "Mechanize something idiosyncratic"
    "Move towards the unimportant"
    "Mute and continue"
    "Not building a wall but making a brick"
    "Once the search has begun, something will be found"
    "Only a part, not the whole"
    "Only one element of each kind"
    "Overtly resist change"
    "Pae White's non-blank graphic metacard"
    "Pay attention to distractions"
    "Picture of a man spotlighted"
    "Put in earplugs"
    "Question the heroic approach"
    "Reevaluation (a warm feeling)"
    "Remember those quiet evenings"
    "Remove a restriction"
    "Remove ambiguities and convert to specifics"
    "Remove specifics and convert to ambiguities"
    "Remove the middle, extend the edges"
    "Repetition is a form of change"
    "Retrace your steps"
    "Reverse"
    "Short circuit (example: a man eating peas with the idea that they will improve his virility shovels them straight into his lap)"

    "Shut the door and listen from outside"
    "Simple subtraction"
    "Simply a matter of work"
    "Slow preparation, fast execution"
    "Spectrum analysis"
    "State the problem in words as clearly as possible"
    "State the problem in words as simply as possible"
    "Steal a solution."
    "Take a break"
    "Take away as much mystery as possible.  What is left?"
    "Take away the elements in order of apparent non-importance"
    "Take away the important parts"
    "Tape your mouth"
    "The inconsistency principle"
    "The most important thing is the thing most easily forgotten"
    "The tape is now the music"
    "Think   -inside the work   -outside the work"
    "Think of the radio"
    "Tidy up"
    "Towards the insignificant"
    "Trust in the you of now"
    "Try faking it"
    "Turn it upside down"
    "Twist the spine"
    "Use 'unqualified' people"
    "Use an old idea"
    "Use an unacceptable color"
    "Use cliches"
    "Use fewer notes"
    "Use filters"
    "Use something nearby as a model"
    "Use your own ideas"
    "Voice your suspicions"
    "Water"
    "What are the sections sections of?    Imagine a caterpillar moving"
    "What are you really thinking about just now?"
    "What context would look right?"
    "What do you do? Now, what do you do best?"
    "What else is this like?"
    "What is the reality of the situation?"
    "What is the simplest solution?"
    "What mistakes did you make last time?"
    "What most recently impressed you?  How is it similar?  What can you learn from it?  What could you take from it?"
    "What to increase?  What to reduce?  What to maintain?"
    "What were the branch points in the evolution of this entity"
    "What were you really thinking about just now?  Incorporate"
    "What would make this really successful?"
    "What would your closest friend do?"
    "What wouldn't you do?"
    "When is it for?  Who is it for?"
    "Where is the edge?"
    "Which parts can be grouped?"
    "Who would make this really successful?"
    "Work at a different speed"
    "Would anyone want it?"
    "You are an engineer"
    "You can only make one dot at a time"
    "You don't have to be ashamed of using your own ideas"
    "Your mistake was a hidden intention")
  "Oblique Strategies, Over One Hundred Worthwhile Dilemmas.
By Brian Eno and Peter Schmidt.

Although originally intended for musicians and other artists,
they can be useful for creative writers as well.")

(defun nanowrimo-show-stuck-suggestion ()
  "Show a cryptic message intended to spark your imagination.
Warning, use sparingly.  Prolonged exposure may result in
headaches.  In case of overdose, close eyes and cease thinking."
  (interactive)
  (message "%s"
           (nth (random (length nanowrimo-stuck-suggestions))
                nanowrimo-stuck-suggestions)))

;;}}}

(provide 'nanowrimo)

;;; nanowrimo.el ends here
