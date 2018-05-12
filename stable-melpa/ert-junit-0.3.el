;;; ert-junit.el --- JUnit XML reports from ert results

;; Copyright (C) 2014  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Created; Jul 24 2014
;; Keywords: tools test unittest ert
;; Package-Version: 0.3
;; Version: 0.1.2
;; Package-Requires: ((ert "0"))
;; URL: http://bitbucket.org/olanilsson/ert-junit

;; This file is not part of GNU Emacs.

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

;; Creates JUnit (http://junit.org) compatible XML files from ert
;; results.  Batch functions for command line use are included.
;; Useful for CI testing on http://shippable.com.

;;; Code:

(require 'ert)
(require 'xml)
(eval-when-compile (require 'cl))

(unless (fboundp 'ert-skipped-p)
  (defun ert-test-skipped-p (cl-x)
    "Dummy `ert-test-skipped' for Emacs 24.3."
    nil))

(unless (fboundp 'ert-stats-skipped)
  (defun ert-stats-skipped (stats)
    "Dummy `ert-stats-skipped' for Emacs 24.3."
    0))

(defmacro ert-junit--stats (property stats test-index)
  "Get PROPERTY from STATS test with index TEST-INDEX.
Expand to `(aref (ert--stats-test-PROPERTY STATS) TEST-INDEX)'."
  (let ((stat-fn (intern (format "ert--stats-test-%s" (symbol-name property)))))
    `(aref (,stat-fn ,stats) ,test-index)))

(defun ert-junit--infos-string (result)
  "Return `ert-info' infos from RESULT as a string.
RESULT must be an `ert-test-result-with-condition'."
  (with-temp-buffer
	(ert--insert-infos result)
	(forward-line -1)
	(delete-extract-rectangle (point-min) (+ (line-beginning-position) 4))
	(buffer-string)))

(defun ert-junit--condition-string (result)
  "Return ert condition string for RESULT.
RESULT must be an `ert-test-result-with-condition'."
  (with-temp-buffer
	(let ((print-escape-newlines t)
          (print-level 5)
          (print-length 10))
      (ert--pp-with-indentation-and-newline
       (ert-test-result-with-condition-condition result)))
	(buffer-string)))

(defun ert-junit--skip-form-as-string (result)
  "Return a suitable message string from a skipped test RESULT."
  ;; there are two possible variant to handle, `skip-unless' and
  ;; `ert-skipped' called directly.
  (let ((skip (cadr (ert-test-result-with-condition-condition result))))
    ;; skip unless looks like
    ;; ((skip-unless nil) :form nil :value nil)
    ;; while plain ert-skipped tests can be any data
    (cond ((and (consp skip)
                (consp (car skip))
                (eq 'skip-unless (caar skip)))
           (pp-to-string (cadar skip)))
          ((stringp skip) skip)
          (t (pp-to-string skip)))))

(defun ert-junit--failure-message (result)
  "Return a failure message for RESULT.
RESULT must be an `ert-test-result-with-condition'."
  (concat (ert-junit--infos-string result)
          (ert-junit--condition-string result)
          ;; Printing the backtrace requires
          ;; escapes (as the content may contain
          ;; xml-like constructs like #<bytecode>
          ;; (with-temp-buffer
          ;;   (ert--print-backtrace
          ;;    (ert-test-result-with-condition-backtrace test-status)
          ;;    nil)
          ;;   (buffer-substring-no-properties (point-min)
          ;;                                 (point-max)))
          ))

(defun ert-junit--xml-escape-and-trim (string)
  "Convert STRING into a trimmed string containing valid XML character data.
Escape with `xml-escape-string'.  Use code equivalent to
`string-trim' (not available in Emacs 24.3) to trim."
  (let ((escaped (xml-escape-string string)))
	(when (string-match "\\`[ \t\n\r]+" escaped)
	  (setq escaped (replace-match "" t t escaped)))
	(if (string-match "[ \t\n\r]+\\'" escaped)
		(replace-match "" t t escaped)
	  escaped)))

(defun ert-junit--time-subtract-float (a b)
  "Return the elapsed seconds between two time values A and B.
A nil value for either argument stands for the current time.
See ‘current-time-string’ for the various forms of a time value."
  ;; time-subtract did not handle nil parameters until Emacs 25
  (let ((current-time (current-time)))
	(float-time (time-subtract (or a current-time)
							   (or b current-time)))))

(defun ert-junit-testcase (stats test-name test-index)
  "Return a testcase XML element as a string.
STATS is the test run state.  The name of the testcase is
TEST-NAME and TEST-INDEX its index into STATS."
  (concat " "
		  (format "<testcase name=\"%s\" classname=\"ert\" time=\"%f\">"
				  test-name ;name
				  ;; time
				  (ert-junit--time-subtract-float
                   (ert-junit--stats end-times stats test-index)
                   (ert-junit--stats start-times stats test-index)))
   ;; success, failure or error
   (let ((test-status (ert-junit--stats results stats test-index))
		 (text ""))
	   (etypecase test-status
		 (ert-test-passed
          (if (ert-test-result-expected-p (aref (ert--stats-tests stats) test-index) test-status)
              (setq text "")
            (setq text "\n  <failure message=\"passed unexpectedly\" type=\"type\"></failure>\n ")))
         (ert-test-skipped
          (setq text (concat "\n  <skipped message=\""
                             (ert-junit--xml-escape-and-trim
                              (ert-junit--skip-form-as-string test-status))
                             "\" type=\"type\">"
                             (ert-junit--xml-escape-and-trim
                              (ert-junit--failure-message test-status))
                             "</skipped>\n ")))
		 (ert-test-failed
          (let ((condition (ert-test-failed-condition test-status)))
            (if (and (consp condition)
                     (symbolp (car condition))
                     (not (eq (car condition) 'ert-test-failed))
                     (get (car condition) 'error-conditions))
                ;; This is an unexpected error
                (setq text (concat "\n  <error message=\"" (nth 1 condition)
                                   "\" type=\"type\">" "</error>\n "))
              (setq text (concat "\n  <failure message=\"test\" type=\"type\">"
                                 (ert-junit--xml-escape-and-trim
                                  (ert-junit--failure-message test-status))
                                 "</failure>\n ")))))
         (ert-test-quit (setq text " <failure>quit</failure>")))
	 text)
   "</testcase>" "\n"))

(defvar ert--results-stats)

(defun ert-junit-report ()
  "Show a JUnit report for the current `ert' buffer."
  (interactive)
  (when (derived-mode-p 'ert-results-mode)
	(let ((buf (get-buffer-create "*junit*")))
	  (with-current-buffer buf
		  (erase-buffer))
	  (ert-junit-generate-report ert--results-stats buf)
	  (set-window-buffer nil buf))))

(defun ert-junit-generate-report (stats buf)
  "Generate a JUnit XML report for STATS at point in BUF."
  (let ((total (ert-stats-total stats))
        (successful 0) (failures 0) (errors 0) (skipped 0))
    (maphash
     (lambda (testname index)
       (let ((test-status (ert-junit--stats results stats index)))
         (etypecase test-status
           (ert-test-passed
            (if (ert-test-result-expected-p (aref (ert--stats-tests stats) index) test-status)
                (incf successful)
              (incf failures)))
           (ert-test-skipped (incf skipped))
           (ert-test-failed
            (let ((condition (ert-test-failed-condition test-status)))
              (if (and (consp condition)
                       (symbolp (car condition))
                       (not (eq (car condition) 'ert-test-failed))
                       (get (car condition) 'error-conditions))
                  ;; This is an unexpected error
                  (incf errors)
                (incf failures))))
           (ert-test-quit (incf failures)))))
     (ert--stats-test-map stats))
    (cl-assert (= total (+ successful failures errors skipped))
               nil "%d != (+ %d %d %d %d)"
               total successful failures errors skipped)
  (with-current-buffer buf
	(insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<testsuite name=\"ERT\" timestamp=\"%s\" hostname=\"%s\" tests=\"%d\" failures=\"%d\" errors=\"%d\" skipped=\"%d\" time=\"%f\">"
                    ;; timestamp
                    (ert--format-time-iso8601 (ert--stats-start-time stats))
					(system-name) ;hostname
                    total failures errors skipped
					(ert-junit--time-subtract-float ; time
					 (ert--stats-end-time stats)
                     (ert--stats-start-time stats)))
			"\n")
	(maphash (lambda (key value)
			   (insert (ert-junit-testcase stats key value)))
			 (ert--stats-test-map stats))
    (insert "</testsuite>" "\n"))))

(defun ert-junit-run-tests-batch (result-file &optional selector)
  "Run `ert-run-tests-batch' and generate JUnit report.
The report is written to RESULT-FILE and pertains to tests
selected by SELECTOR."
  (let ((stats (ert-run-tests-batch selector))
		(buf (find-file-noselect result-file)))
	(with-current-buffer buf
	  (erase-buffer)
	  (ert-junit-generate-report stats buf)
	  (save-buffer))
	stats))

(defun ert-junit-run-tests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch-and-exit', but write a JUnit report to file.

The report file name is read from the command line.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests)."
  (unwind-protect
      (let* ((result-file (and command-line-args-left
							   (= (length command-line-args-left) 1)
							   (car command-line-args-left)))
			 (stats (ert-junit-run-tests-batch result-file selector)))
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 2))))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 4
;; indent-tabs-mode: nil
;; End:
(provide 'ert-junit)
;;; ert-junit.el ends here
