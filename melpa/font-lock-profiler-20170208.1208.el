;;; font-lock-profiler.el --- Coverage and timing tool for font-lock keywords.

;; Copyright (C) 2016-2017 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces, tools
;; Package-Version: 20170208.1208
;; Version: 0.0.3
;; URL: https://github.com/Lindydancer/font-lock-profiler
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
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

;; A profiler for font-lock keywords.  This package measures time and
;; counts the number of times each part of a font-lock keyword is
;; used.  For matchers, it counts the total number and the number of
;; successful matches.
;;
;; The result is presented in table that can be sorted by count or
;; time.  The table can be expanded to include each part of the
;; font-lock keyword.
;;
;; In addition, this package can generate a log of all font-lock
;; events.  This can be used to verify font-lock implementations,
;; concretely, this is used for back-to-back tests of the real
;; font-lock engine and Font Lock Studio, an interactive debugger for
;; font-lock keywords.

;; Usage:
;;
;; Use the following functions:
;;
;; - `font-lock-profiler-buffer' -- Fontify the entire buffer and
;;   present the profiling result.
;;
;; - `font-lock-profiler-region' -- Fontify the region and present the
;;   profiling result.
;;
;; - `font-lock-profiler-start' -- Enable "live" profiling.  Do
;;   whatever you want to do measure (like editing or scrolling).  When
;;   done, run `font-lock-profiler-stop-and-report'.

;; The result buffer:
;;
;; Once the profiling has been performed, the reporter buffer,
;; `*FontLockProfiler*' is displayed.  It contains a list of all
;; font-lock keywords, the number of times they matched and the total
;; time that was spent running them.
;;
;; By pressing `x', the view is expanded into displaying the
;; corresponding information for each highlight rule, including
;; anchored highlights.

;; Additional features:
;;
;; - The variable `font-lock-profiler-remaining-matches', when set to
;;   an integer, the instrumented keywords will fake a match failure
;;   after this many matches.  This is useful, for example, when
;;   working with keywords where a search would never terminate
;;   (without this, Emacs would hang).

;; Other Font Lock Tools:
;;
;; This package is part of a suite of font-lock tools.  The other
;; tools in the suite are:
;;
;;
;; Font Lock Studio:
;;
;; Interactive debugger for font-lock keywords (Emacs syntax
;; highlighting rules).
;;
;; Font Lock Studio lets you *single-step* Font Lock keywords --
;; matchers, highlights, and anchored rules, so that you can see what
;; happens when a buffer is fontified.  You can set *breakpoints* on or
;; inside rules and *run* until one has been hit.  When inside a rule,
;; matches are *visualized* using a palette of background colors.  The
;; *explainer* can describe a rule in plain-text English.  Tight
;; integration with *Edebug* allows you to step into Lisp expressions
;; that are part of the Font Lock keywords.
;;
;;
;; Highlight Refontification:
;;
;; Minor mode that visualizes how font-lock refontifies a buffer.
;; This is useful when developing or debugging font-lock keywords,
;; especially for keywords that span multiple lines.
;;
;; The background of the buffer is painted in a rainbow of colors,
;; where each band in the rainbow represent a region of the buffer
;; that has been refontified.  When the buffer is modified, the
;; rainbow is updated.
;;
;;
;; Faceup:
;;
;; Emacs is capable of highlighting buffers based on language-specific
;; `font-lock' rules.  This package makes it possible to perform
;; regression test for packages that provide font-lock rules.
;;
;; The underlying idea is to convert text with highlights ("faces")
;; into a plain text representation using the Faceup markup
;; language.  This language is semi-human readable, for example:
;;
;;     «k:this» is a keyword
;;
;; By comparing the current highlight with a highlight performed with
;; stable versions of a package, it's possible to automatically find
;; problems that otherwise would have been hard to spot.
;;
;; This package is designed to be used in conjunction with Ert, the
;; standard Emacs regression test system.
;;
;; The Faceup markup language is a generic markup language, regression
;; testing is merely one way to use it.
;;
;;
;; Font Lock Regression Suite:
;;
;; A collection of example source files for a large number of
;; programming languages, with ERT tests to ensure that syntax
;; highlighting does not accidentally change.
;;
;; For each source file, font-lock reference files are provided for
;; various Emacs versions.  The reference files contains a plain-text
;; representation of source file with syntax highlighting, using the
;; format "faceup".
;;
;; Of course, the collection source file can be used for other kinds
;; of testing, not limited to font-lock regression testing.

;;; Code:

(require 'tabulated-list)

(defvar font-lock-profiler-log '()
  "Result of profiled font-lock keywords.

Each entry in this list has the following form:

  (DURATION
   KIND
   KEYWORD-COUNT
   HIGHLIGHT-COUNT
   ANCHORED-COUNT
   EXPR-RESULT
   (PRE-POINT POST-POINT)
   (PRE-MATCH-DATA POST-MATCH-DATA))

KIND is `match', `face', `anchored-match', `pre-match',
`post-match', or `anchored-face'.

Entries in the list are stored in reversed order (newest entries
are at the beginning of the list).")


(defvar font-lock-profiler-remaining-matches nil
  "When an integer, number of matches until instrumented code fakes a miss.

This can be used to create a truncated log, e.g. when working
with font-lock keywords that never terminate.

This is typically bound using `let'.")


;; -------------------------------------------------------------------
;; Instrumenter
;;

(defun font-lock-profiler-matcherp (matcher)
  "True if MATCHER is a matcher, i.e. a string or function."
  (or (stringp matcher)
      (functionp matcher)))


(defun font-lock-profiler--normalize-keywords (&optional keywords)
  "The original font-lock KEYWORDS, without font-lock:s compilation.

When KEYWORDS is nil, use the `font-lock-keywords'."
  (unless keywords
    (setq keywords font-lock-keywords))
  (when (eq (car-safe keywords) t)
    (setq keywords (cddr keywords)))
  keywords)


(defmacro font-lock-profiler--expr (kind
                                    expr
                                    keyword-count
                                    &optional
                                    highlight-count
                                    anchored-count)
  "Wrapper macro for a font-lock expression in a keyword.

KIND is a symbol representing the kind of the expression.  EXPR is
an expression.

KEYWORD-COUNT, HIGHLIGHT-COUNT, and ANCHORED-COUNT are integers
identifying the highlight in which the face expression occurs,
typically the position in the keyword list, in the highlight
list, and (if applicable) the anchored highlight list,
respectively."
  (let ((old-point-var (make-symbol "--old-point--"))
        (old-match-data-var (make-symbol "--old-match-data--"))
        (res-var (make-symbol "--res-expr--"))
        (start-time-var (make-symbol "--start-time--")))
    `(let ((,old-point-var (point))
           (,old-match-data-var (match-data))
           (,start-time-var (current-time))
           (,res-var ,(ignore-errors expr)))
       (push (list (float-time (time-since ,start-time-var))
                   ,kind
                   ,keyword-count
                   ,highlight-count
                   ,anchored-count
                   ,res-var
                   (list ,old-point-var (point))
                   (list ,old-match-data-var (match-data)))
             font-lock-profiler-log)
       ,res-var)))


(defmacro font-lock-profiler--match (limit
                                     expr
                                     keyword-count
                                     &optional highlight-count)
  "Wrapper macro for a font-lock matcher.

LIMIT is the end of the search.  EXPR is a regexp or a
function.

KEYWORD-COUNT and HIGHLIGHT-COUNT are integers identifying the
highlight in which the face expression occurs, typically the
position in the keyword list and in the highlight list,
respectively."
  (let ((res-var (make-symbol "--res-match--")))
    `(let ((,res-var (font-lock-profiler--expr
                      (quote ,(if highlight-count
                                  'anchored-match
                                'match))
                      ,(cond ((stringp expr)
                              `(re-search-forward ,expr ,limit t))
                             ((symbolp expr)
                              `(,expr ,limit))
                             (t
                              `(funcall ,expr ,limit)))
                      ,keyword-count
                      ,highlight-count)))
       (when (and ,res-var
                  (integerp font-lock-profiler-remaining-matches))
         (if (zerop font-lock-profiler-remaining-matches)
             (setq ,res-var nil)
           (setq font-lock-profiler-remaining-matches
                 (- font-lock-profiler-remaining-matches 1))))
       ,res-var)))


(defun font-lock-profiler-instrument-matcher (matcher
                                              keyword-count
                                              &optional highlight-count)
  "Instrument MATCHER which is part of a font-lock keyword.

KEYWORD-COUNT and HIGHLIGHT-COUNT are integers identifying the
highlight in which the face expression occurs, typically the
position in the keyword list and in the highlight list,
respectively."
  `(lambda (limit)
     (font-lock-profiler--match
      limit
      ,(if (symbolp matcher)
           (list 'function matcher)
         matcher)
      ,keyword-count
      ,@(if highlight-count (list highlight-count) '()))))


(defun font-lock-profiler-instrument-match-anchored (anchored-highlight
                                                     keyword-count
                                                     highlight-count)
  "Instrument ANCHORED-HIGHLIGHT which is part of a font-lock keyword.

KEYWORD-COUNT and HIGHLIGHT-COUNT are integers identifying the
highlight in which the face expression occurs, typically the
position in the keyword list and in the highlight list,
respectively."
  (let ((res '()))
    (when anchored-highlight
      (let ((matcher (pop anchored-highlight)))
        (push (font-lock-profiler-instrument-matcher matcher keyword-count
                                                     highlight-count)
              res))
      (when anchored-highlight
        (let ((pre (pop anchored-highlight)))
          (push `(font-lock-profiler--expr 'pre-match
                                           ,pre
                                           ,keyword-count
                                           ,highlight-count)
                res))
        (when anchored-highlight
          (let ((post (pop anchored-highlight)))
            (push `(font-lock-profiler--expr 'post-match
                                             ,post
                                             ,keyword-count
                                             ,highlight-count)
                  res))
          (let ((anchored-count 0))
            (dolist (highlight anchored-highlight)
              (push (font-lock-profiler-instrument-highlight highlight
                                                             keyword-count
                                                             highlight-count
                                                             anchored-count)
                    res)
              (setq anchored-count (+ anchored-count 1)))))))
    (nreverse res)))


(defun font-lock-profiler-instrument-face (face-expr
                                           keyword-count
                                           highlight-count
                                           &optional anchored-count)
  "Instrument FACE-EXPR which is a part of a font-lock keyword.

KEYWORD-COUNT, HIGHLIGHT-COUNT, and ANCHORED-COUNT are integers
identifying the highlight in which the face expression occurs,
typically the position in the keyword list, in the highlight
list, and (if applicable) the anchored highlight list,
respectively.

The result behaves exactly like the original, but information
about the highlight is logged in `font-lock-profiler-log'."
  `(font-lock-profiler--expr (quote ,(if anchored-count 'anchored-face 'face))
                             ,face-expr
                             ,keyword-count
                             ,highlight-count
                             ,@(if anchored-count
                                   (list anchored-count)
                                 '())))


(defun font-lock-profiler-instrument-highlight (highlight
                                                keyword-count
                                                highlight-count
                                                &optional anchored-count)
  "Instrument HIGHLIGHT which is a part of a font-lock keyword.

KEYWORD-COUNT, HIGHLIGHT-COUNT, and ANCHORED-COUNT are integers
identifying the highlight, typically the position in the keyword
list, in the highlight list, and (if applicable) the anchored
highlight list, respectively.

The result behaves exactly like the original, but information
about the highlight is logged in `font-lock-profiler-log'."
  (cond ((not (consp highlight))
         ;; Broken highlight. Unfortunately, these occur in the
         ;; wild. On example is `javascript-mode' where an anchored
         ;; rule contains a nil highlight.
         highlight)
        ((numberp (car highlight))
         ;; A MATCH-HIGHLIGHT.
         ;;
         ;; Wrap second element in highlight list.
         (cons
          (car highlight)
          (cons
           (font-lock-profiler-instrument-face (nth 1 highlight)
                                               keyword-count
                                               highlight-count
                                               anchored-count)
           (cdr (cdr highlight)))))
        (t
         ;; A MATCH-ANCHORED highlight.
         (when anchored-count
           (error "Anchored rule inside anchored rule is illegal"))
         (font-lock-profiler-instrument-match-anchored highlight
                                                       keyword-count
                                                       highlight-count))))


(defun font-lock-profiler-instrument-keyword (keyword count)
  "Instrument a font-lock KEYWORD.

COUNT is an integer identifying the keyword, typically the
position number in a keyword list.

The result behaves exactly like the original, but information
about each operation is logged in `font-lock-profiler-log'."
  (cond
   ;; MATCHER
   ((font-lock-profiler-matcherp keyword)
    (font-lock-profiler-instrument-matcher keyword count))
   ;; Consistency check.
   ((not (consp keyword))
    (error "Illegal font-lock keyword: %s" keyword))
   ;; (eval . FORM)
   ((eq (car keyword) 'eval)
    (error "Not yet implemented"))
   ;; (MATCHER . SUBEXP)
   ((numberp (cdr keyword))
    (cons (font-lock-profiler-instrument-matcher (car keyword) count)
          (cdr keyword)))
   ;; (MATCHER . face)
   ;; (MATCHER . 'face)
   ((or (symbolp (cdr keyword))
        (eq (car-safe (cdr keyword)) 'quote))
    (cons (font-lock-profiler-instrument-matcher (car keyword) count)
          ;; Note: Can't instrument the face expression without
          ;; changing the form, as the instrumentation code stops
          ;; font-lock from recognizing this case.
          (cdr keyword)))
   ;; (MATCHER . HIGHLIGHT)
   ;;    (MATCHER . MATCH-HIGHLIGHT) == (MATCHER . (SUBEXP ... ))
   ((numberp (car-safe (cdr keyword)))
    (cons (font-lock-profiler-instrument-matcher (car keyword) count)
          (font-lock-profiler-instrument-highlight (cdr keyword) count 0)))
   ;; (MATCHER . HIGHLIGHT)
   ;;    (MATCHER . MATCH-ANCHORED)  == (MATCHER . (MATCHER ...))
   ((font-lock-profiler-matcherp (car-safe (cdr keyword)))
    (cons
     (font-lock-profiler-instrument-matcher (car keyword) count)
     (font-lock-profiler-instrument-match-anchored (cdr keyword) count 0)))
   ;; (MATCHER HIGHLIGHT ...) === (MATCHER . (HIGHLIGHT ...))
   (t
    (let ((sub-count 0)
          (instrumented-highlights))
      (dolist (highlight (cdr keyword))
        (push
         (font-lock-profiler-instrument-highlight highlight count sub-count)
         instrumented-highlights)
        (setq sub-count (+ sub-count 1)))
      (cons
       (font-lock-profiler-instrument-matcher (car keyword) count)
       (nreverse instrumented-highlights))))))



;; ((MATCHER0
;;   ...
;;   )
;;  (MATCHER1
;;   HIGHLIGHT1-0
;;   HIGHLIGHT1-1
;;   (HIGHLIGHT2-MATCHER
;;    PRE
;;    POST
;;    HIGHLIGHT1-2-0
;;    HIGHLIGHT1-2-1
;;    HIGHLIGHT1-2-2)))

(defun font-lock-profiler-instrument-keyword-list (keywords)
  "Return instrumented replacement for font-lock KEYWORDS.

The result behaves exactly like the original, but information
about each operation is logged in `font-lock-profiler-log'."
  ;; When t is car, font-lock has "compiled" they keywords, the second
  ;; element of the list contains the original keywords.
  (setq keywords (font-lock-profiler--normalize-keywords keywords))
  (let ((res '())
        (count 0))
    (dolist (kwd keywords)
      (push (font-lock-profiler-instrument-keyword kwd count) res)
      (setq count (+ count 1)))
    (nreverse res)))


;; -------------------------------------------------------------------
;; Boil down log.
;;

(defun font-lock-profiler-result-log (&optional exclude-timing)
  "Return the font-lock profiler log, in chronological order.

If EXCLUDE-TIMING is non-nil, exclude the timing information."
  (let ((res '()))
    ;; Effectively, this reverses the list.
    (dolist (entry font-lock-profiler-log)
      (when exclude-timing
        (setq entry (cdr entry)))
      (push entry res))
    res))


(defun font-lock-profiler--empty-highlights-statistic-table (highlight-list)
  "An empty highlight slot in the statistics table.

HIGHLIGHT-LIST is a font-lock keyword without the matcher."
  (when (not (listp highlight-list))
    (setq highlight-list (list highlight-list)))
  (when (numberp (car-safe highlight-list))
    (setq highlight-list (list highlight-list)))
  (let ((res '()))
    (dolist (highlight highlight-list)
      (if (or (not (listp highlight))
              (numberp (car highlight)))
          (push (list 0 0.0) res)
        (push (list 0 0 0.0 0.0 0.0
                    (font-lock-profiler--empty-highlights-statistic-table
                     (nthcdr 3 highlight)))
              res)))
    (nreverse res)))


(defun font-lock-profiler--empty-statistic-table (keywords)
  "An empty statistics table, on a format corresponding to KEYWORDS.

KEYWORDS is the font-lock keywords currently profiled.  It
defaults to `font-lock-keywords' of the current buffer."
  (unless keywords
    (setq keywords font-lock-keywords))
  ;; When t is car, font-lock has "compiled" they keywords, the second
  ;; element of the list contains the original keywords.
  (when (eq (car-safe keywords) t)
    (setq keywords (nth 1 keywords)))
  (let ((res '()))
    (dolist (kwd keywords)
      (push (list 0 0 0.0
                  (font-lock-profiler--empty-highlights-statistic-table
                   (cdr kwd)))
            res))
    (nreverse res)))


(defmacro font-lock-profiler--add-car (place value)
  "Increase the car of PLACE with VALUE."
  `(setcar ,place (+ (car ,place) ,value)))

(defmacro font-lock-profiler--increment-car (place)
  "Increase the car of PLACE with one."
  `(setcar ,place (+ (car ,place) 1)))



(defun font-lock-profiler-accumulate (&optional keywords)
  "Return accumulated profile information.

KEYWORDS is the font-lock keywords currently profiled.  It
defaults to `font-lock-keywords' of the current buffer.

A list is returned, with one element for each logged font-lock keyword.

Each element has the following form:

    (TOTAL-COUNT TAKEN-COUNT MATCH-TIME (HIGHLIGHT-INFO ...))

HIGHLIGHT-INFO contains information about each highlight of the
keyword.  It can either be PLAIN-INFO or ANCHORED-INFO:

PLAIN-INFO:

    (COUNT TOTAL-TIME)

ANCHORED-INFO:

    (TOTAL-COUNT TAKEN-COUNT MATCH-TIME PRE-TIME POST-TIME (PLAIN-INFO ...))"
  (let ((res (font-lock-profiler--empty-statistic-table keywords)))
    (dolist (entry (reverse font-lock-profiler-log))
      (let ((duration        (nth 0 entry))
            (kind            (nth 1 entry))
            (keyword-count   (nth 2 entry))
            (highlight-count (nth 3 entry))
            (anchored-count  (nth 4 entry))
            (expr-result     (nth 5 entry))
            ;; Not used.
            ;; (point-pair      (nth 6 entry))
            ;; (match-data-pair (nth 7 entry))
            )
        (let ((keyword-entry (nth keyword-count res)))
          (if (eq kind 'match)
              (progn
                (font-lock-profiler--increment-car keyword-entry)
                (if expr-result
                    (font-lock-profiler--increment-car (cdr keyword-entry)))
                (font-lock-profiler--add-car
                 (nthcdr 2 keyword-entry) duration))
            (let ((highlight-entry (nth highlight-count
                                        (nth 3 keyword-entry))))
              (if (eq kind 'face)
                  (progn
                    (font-lock-profiler--increment-car highlight-entry)
                    (font-lock-profiler--add-car (cdr highlight-entry)
                                                 duration))
                (cond ((eq kind 'anchored-match)
                       (font-lock-profiler--increment-car
                        (nthcdr 0 highlight-entry))
                       (when expr-result
                         (font-lock-profiler--increment-car
                          (nthcdr 1 highlight-entry)))
                       (font-lock-profiler--add-car (nthcdr 2 highlight-entry)
                                                    duration))
                      ((eq kind 'pre-match)
                       (font-lock-profiler--add-car (nthcdr 3 highlight-entry)
                                                    duration))
                      ((eq kind 'post-match)
                       (font-lock-profiler--add-car (nthcdr 4 highlight-entry)
                                                    duration))
                      ((eq kind 'anchored-face)
                       (let ((anchored-entry
                              (nth anchored-count (nth 5 highlight-entry))))
                         (font-lock-profiler--increment-car anchored-entry)
                         (font-lock-profiler--add-car (cdr anchored-entry)
                                                      duration))))))))))
    res))


(defun font-lock-profiler--sum-times-in-highlight (highlight)
  "The total sum of the highlight.

HIGHLIGHT, as in the structure returned by
`font-lock-profiler-accumulate'."
  (if (eq (length highlight) 2)
      (nth 1 highlight)
    (let ((res (+ (nth 2 highlight)
                  (nth 3 highlight)
                  (nth 4 highlight))))
      (dolist (sub-highlight (nth 5 highlight))
        (setq res (+ res (nth 1 sub-highlight))))
      res)))


(defun font-lock-profiler--sum-times-in-entry (accumulated-entry)
  "The total time of the matcher and the highlight entries.

ACCUMULATED-ENTRY is an element of the list returned by
`font-lock-profiler-accumulate'."
  (let ((res (nth 2 accumulated-entry)))
    (dolist (highlight (nth 3 accumulated-entry))
      (setq res
            (+ res (font-lock-profiler--sum-times-in-highlight highlight))))
    res))


;; ------------------------------------------------------------
;; Reporter.
;;

(defvar font-lock-profiler-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "S" #'font-lock-profiler-sort)
    (define-key map "x" #'font-lock-profiler-toggle-expand)
    (define-key map "%" #'font-lock-profiler-toggle-time-in-percent)
    map))


(defvar font-lock-profiler-report-expand nil)
(make-variable-buffer-local 'font-lock-profiler-report-expand)

(defvar font-lock-profiler-report-time-in-percent t)
(make-variable-buffer-local 'font-lock-profiler-report-time-in-percent)

(defvar font-lock-profiler--saved-keywords)
(make-variable-buffer-local 'font-lock-profiler--saved-keywords)

(defvar font-lock-profiler--summary)
(make-variable-buffer-local 'font-lock-profiler--summary)

(defvar font-lock-profiler--total-time)
(make-variable-buffer-local 'font-lock-profiler--total-time)


(define-derived-mode font-lock-profiler-report-mode tabulated-list-mode
  "Font Lock Profiler"
  "Major mode for reporting Font Long profiling results."
  (setq tabulated-list-format
        [("Id"           4 font-lock-profiler--compare-index :right-align t
          :pad-right 7)
         ("Count"        7 font-lock-profiler--compare-count :right-align t)
         ("Time"        15 font-lock-profiler--compare-times :right-align t)
         ("Keyword"      0 t)])
  (setq tabulated-list-sort-key (cons "Id" t))
  (tabulated-list-init-header))


(defun font-lock-profiler--compare-index (lhs rhs)
  "Compare the index of LHS and RHS.

Effectively, this sorts the list in original order."
  (< (length (memq (car lhs) font-lock-profiler--summary))
     (length (memq (car rhs) font-lock-profiler--summary))))


(defun font-lock-profiler--compare-count (lhs rhs)
  "Compare the count of LHS and RHS."
  (< (nth 1 (car lhs))
     (nth 1 (car rhs))))


(defun font-lock-profiler--compare-times (lhs rhs)
  "Compare the measured time of LHS and RHS."
  (< (font-lock-profiler--sum-times-in-entry (car lhs))
     (font-lock-profiler--sum-times-in-entry (car rhs))))


(defun font-lock-profiler-sort ()
  "Like `tabulated-list-sort' but position the window better.

Ensure that the list is not positioned above the top of the window."
  (interactive)
  (tabulated-list-sort)
  (set-window-start (selected-window) (point-min) t))


(defun font-lock-profiler-format-expr (expr)
  "Format EXPR for the Font-Lock Profiler report window."
  (replace-regexp-in-string
   "\t" "\\\\t"
   (replace-regexp-in-string
    "\n" "\\\\n"
    (format "%S" expr))))


(defun font-lock-profiler-toggle-expand ()
  "Expand or shrink the entries in the Font-Lock Profiler report window."
  (interactive)
  (setq font-lock-profiler-report-expand
        (not font-lock-profiler-report-expand))
  (font-lock-profiler--refresh))


(defun font-lock-profiler-toggle-time-in-percent ()
  "Toggle between show time in percent and absolute."
  (interactive)
  (setq font-lock-profiler-report-time-in-percent
        (not font-lock-profiler-report-time-in-percent))
  (font-lock-profiler--refresh))


(defun font-lock-profiler--report-format-time (time)
  "Format TIME for the Font-Lock profiler report window."
  (if font-lock-profiler-report-time-in-percent
      (if (equal font-lock-profiler--total-time 0.0)
          "n/a"
        (format "%d%% "
                (round (* (/ time
                             font-lock-profiler--total-time)
                          100))))
    (format "%g  " (or time ""))))


(defun font-lock-profiler--report-format (padding1 index count
                                                   time padding2 expr)
  "Format one font-lock keyword.

This is a normal highlight or part of an anchored highlight.

PADDING1 is a string of spaces to make the output
well formed.

INDEX is the order of the font-lock keyword.

COUNT is the hit count.

TIME is the accumulated time.

PADDING2 is a strings of spaces to make the output well formed.

EXPR is the highlight part of the font-lock keyword."
  (concat
   ;; First column is designed to be right aligned, since often there
   ;; are more than ten keywords.
   (format "%-10s " (concat "  " padding1 index))
   ;; This is left aligned, since there seldom are as many at ten
   ;; highlights.
   (format "%7s " (or count ""))
   (format "%15s " (font-lock-profiler--report-format-time time))
   (or padding2 "")
   (font-lock-profiler-format-expr expr)))


;; The non-expanded view use tabulated list mode "as intended", with
;; one line per entry. However, the expanded view does not, the
;; multiline effect is created by inserting newlines into the last
;; field, and manually formatting the fields.
(defun font-lock-profiler--refresh ()
  "Update the Font Lock profiler buffer."
  (setq tabulated-list-entries '())
  (let ((keywords font-lock-profiler--saved-keywords)
        (keyword-index 0))
    (dolist (summary-entry font-lock-profiler--summary)
      (let* ((rule (prog1 (car keywords)
                     (setq keywords (cdr keywords))))
             (extra-text (replace-regexp-in-string
                          "\t" "\\\\t"
                          (replace-regexp-in-string
                           "\n" "\\\\n"
                           (font-lock-profiler-format-expr
                            (if font-lock-profiler-report-expand
                                (car rule)
                              rule))))))
        (when font-lock-profiler-report-expand
          ;; `extra-text' will be placed in the Keyword column. But
          ;; it will also contain all information about highlights.
          (let ((rule-highlight-list (cdr rule))
                (highlight-index 0))
            (dolist (highlight-summary (nth 3 summary-entry))
              (let ((rule-highlight (pop rule-highlight-list)))
                (if (eq (length highlight-summary) 2)
                    ;; --------------------
                    ;; Normal highlight.
                    (setq extra-text
                          (concat extra-text "\n"
                                  (font-lock-profiler--report-format
                                   "  "
                                   (format "%s"
                                           highlight-index)
                                   (nth 0 highlight-summary)
                                   (nth 1 highlight-summary)
                                   " "
                                   rule-highlight)))

                  ;; --------------------
                  ;; Anchored highlight.
                  ;;
                  ;; -3: matcher
                  ;; -2: Pre-match form
                  ;; -1: Post-match form
                  ;; 0 ... N: Highlights.
                  (let ((index -3))
                    (dolist (anchored-match-entry rule-highlight)
                      (let ((summary-anchorded-highlight-or-nil
                             (and (>= index 0)
                                  (nth index (nth 5 highlight-summary)))))
                        (setq extra-text
                              (concat
                               extra-text
                               "\n"
                               (font-lock-profiler--report-format
                                (if (eq index -3)
                                    "  "
                                  "    ")
                                (if (eq index -3)
                                    (format "%s" highlight-index)
                                  (cond ((eq index -2)
                                         "Pre ")
                                        ((eq index -1)
                                         "Post")
                                        (t
                                         (format "%s" index))))
                                (if summary-anchorded-highlight-or-nil
                                    (nth 0 summary-anchorded-highlight-or-nil)
                                  "")
                                (if summary-anchorded-highlight-or-nil
                                    (nth 1 summary-anchorded-highlight-or-nil)
                                  (nth (+ 3 2 index) highlight-summary))
                                (if (eq index -3)
                                    " "
                                  "   ")
                                anchored-match-entry))))
                      (setq index (+ index 1))))))
              (setq highlight-index (+ highlight-index 1))))
          (setq extra-text
                (concat extra-text "\n--------------------")))
        (push (list summary-entry (vector
                                   (format "%s" keyword-index)
                                   (format "%s" (nth 1 summary-entry))
                                   (font-lock-profiler--report-format-time
                                    (if font-lock-profiler-report-expand
                                        (nth 2 summary-entry)
                                      (font-lock-profiler--sum-times-in-entry
                                       summary-entry)))
                                   extra-text))
              tabulated-list-entries))
      (setq keyword-index (+ keyword-index 1)))
    (tabulated-list-print)))


(defun font-lock-profiler-report (&optional keywords)
  "Display the Font-Lock profiler report.

KEYWORDS is the font-lock keywords used.  When nil, use `font-lock-keywords'."
  (interactive)
  (setq keywords (font-lock-profiler--normalize-keywords keywords))
  (let ((summary (font-lock-profiler-accumulate keywords)))
    (with-current-buffer (get-buffer-create "*FontLockProfiler*")
      (font-lock-profiler-report-mode)
      (setq font-lock-profiler--saved-keywords keywords)
      (setq font-lock-profiler--summary summary)
      (let ((total-time 0.0))
        (dolist (entry summary)
          (setq total-time (+ total-time
                              (font-lock-profiler--sum-times-in-entry entry))))
        (setq font-lock-profiler--total-time total-time))
      (font-lock-profiler--refresh)
      (goto-char (point-max))
      (display-buffer (current-buffer)))))


;;;###autoload
(defun font-lock-profiler-region (beg end)
  "Profile font-lock from BEG to END and present report."
  (interactive "r")
  (save-excursion
    (let ((original-font-lock-keywords font-lock-keywords)
          (font-lock-profiler-log '())
          (font-lock-keywords
           (font-lock-profiler-instrument-keyword-list
            font-lock-keywords)))
      (font-lock-fontify-region beg end)
      (font-lock-profiler-report original-font-lock-keywords))))


;;;###autoload
(defun font-lock-profiler-buffer ()
  "Profile font-locking buffer and present report."
  (interactive)
  (font-lock-profiler-region (point-min) (point-max)))


(defvar font-lock-profiler--original-keywords nil)


;;;###autoload
(defun font-lock-profiler-start ()
  "Start recording font-lock profiling information."
  (interactive)
  ;; If this is set, assume that the keywords already are
  ;; instrumented.
  (unless font-lock-profiler--original-keywords
    (setq font-lock-profiler--original-keywords
          font-lock-keywords)
    (setq font-lock-keywords
          (font-lock-profiler-instrument-keyword-list font-lock-keywords)))
  (set (make-local-variable 'font-lock-profiler-log) '()))


(defun font-lock-profiler-stop ()
  "Stop recording font-lock profiling information."
  (when font-lock-profiler--original-keywords
    (setq font-lock-keywords font-lock-profiler--original-keywords)
    (setq font-lock-profiler--original-keywords nil)))


(defun font-lock-profiler-stop-and-report ()
  "Stop recording font-lock profiling information and present result."
  (interactive)
  (font-lock-profiler-stop)
  (font-lock-profiler-report))


;; -------------------------------------------------------------------
;; Log printer
;;

(defun font-lock-profiler--clean-match-data (match-data)
  "Replace markers in MATCH-DATA with integers representing the position."
  (mapcar (lambda (marker-or-point)
            (if (markerp marker-or-point)
                (marker-position marker-or-point)
              marker-or-point))
          match-data))



(defun font-lock-profiler-format-log-entry (entry)
  "Print the content of ENTRY in `font-lock-profiler-log'."
  (let ((kind            (nth 0 entry))
        (keyword-count   (nth 1 entry))
        (highlight-count (nth 2 entry))
        (anchored-count  (nth 3 entry))
        (point-list      (nth 4 entry))
        (match-data-list (nth 5 entry)))
    (let ((s (concat
              (format "%-8s"
                      (concat
                       (format "%d" keyword-count)
                       (when highlight-count
                         (format ":%d" highlight-count))
                       (when anchored-count
                         (format ":%d" anchored-count))))
              " "
              (format "%-10s" kind)
              (format "%-20s"
                      (concat
                       (format " %s" (nth 0 point-list))
                       (unless (equal (nth 0 point-list)
                                      (nth 1 point-list))
                         (format " -> %s" (nth 1 point-list))))))))
      (concat
       s
       (format " MD: %s" (font-lock-profiler--clean-match-data
                          (nth 0 match-data-list)))
       (unless (equal (nth 0 match-data-list)
                      (nth 1 match-data-list))
         (format "\n %s -> %s"
                 (make-string (length s) ?\s)
                 (font-lock-profiler--clean-match-data
                  (nth 1 match-data-list))))))))


;; -------------------------------------------------------------------
;; The end
;;

(provide 'font-lock-profiler)

;;; font-lock-profiler.el ends here
