;;; himp.el --- Automatically hide imports/documentation

;; Copyright (C) 2017 Michał Kondraciuk

;;
;; Author: Michał Kondraciuk <k.michal@zoho.com>
;; URL: http://github.com/mkcms/himp/
;; Package-Version: 20170814.1215
;; Package-Requires: ((emacs "24.3") (vimish-fold "0.1.0"))
;; Version: 0.1
;; Keywords: convenience, tools
;;
;; This file is not part of GNU Emacs.
;;
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
;; himp-mode is a minor mode for hiding imports/comments/documentation
;; at beginning of buffer.
;;
;; To enable this mode when specific mode is activated:
;; (add-hook 'python-mode-hook 'himp-mode)
;; (add-hook 'java-mode-hook 'himp-mode)
;;
;; Now imports at beginning of buffer will be hidden when himp-mode is active.
;;
;; Currently, python and java modes are supported, but the package can
;; easily be extended to support other languages.  See documentation for
;; variable `himp-matchers'.


;;; Code:

(require 'vimish-fold)

(eval-when-compile
  (require 'python)
  (require 'cc-mode))

(defgroup himp nil
  "Hide uninteresting regions (imports, comments) at beginning of buffer."
  :group 'programming)

(defcustom himp-show-line-count nil
  "If non-nil, show number of lines in hidden regions."
  :type 'boolean
  :group 'himp)

(defcustom himp-rescan-on-save t
  "If non-nil, rescan buffer after saving to file."
  :type 'boolean
  :group 'himp)

(defcustom himp-matchers
  '(
    (python-mode
     . ((group
         (("import\\s-+[^\s-]+" . python-nav-forward-sexp-safe)
          ("from\\s-+[^\s-]+\\s-+import\\s-+[^\s-]+"
           . python-nav-forward-sexp-safe)
          (himp-python-tryblock-matcher . himp-python-tryblock-matcher)))
        (himp-python-comment-matcher . python-nav-forward-sexp-safe)
        (python-info-docstring-p . python-nav-forward-sexp-safe)))
    (java-mode
     . ((himp-java-comment-matcher . c-forward-single-comment)
        ("package\\b[^;]+;" . c-end-of-statement)
        ("import\\b[^;]+;" . c-end-of-statement))))
  "Alist of matchers per major mode.
Each value of the alist cell is a list of matchers.
The key of the cell is a major mode symbol.  When there is no entry for a major
mode, entry for parent mode is used, if it exists.
A matcher can be:
    1. A regex.
    2. Cons cell (regex . function).  `regex` will match a region
        and `function` will be called to move point right after the region.
    3. Cons cell (matcher . skipper).  `matcher` will be called
        and should return non-nil if point is at a region to hide
        (like `looking-at'), `skipper` will be called to move point
        right after the region.
    4. 2-element list of the form (group matchers).  `group` is literal
        symbol 'group.  `matchers` is a list of matchers.  It can be used to
         group matchers together."
  :type (let ((choices '(choice
                         regexp
                         (cons
                          :tag "Regexp, skip function"
                          regexp (function :tag "Skip function"))
                         (cons
                          :tag "Match function, skip function"
                          (function :tag "Match function")
                          (function :tag "Skip function")))))
          (setq choices
                (append choices
                        `((list :tag "Group"
                                (const group)
                                (repeat :tag "Matchers" ,choices)))))
          `(alist :key-type (symbol :tag "Major mode")
                  :value-type (repeat :tag "Matchers" ,choices)))
  :group 'himp)

(defvar himp-keymap (make-keymap)
  "Keymap to use in himp mode, enabled with command `himp-mode'.")

(defvar-local himp--regions nil
  "Markers at hidden regions in current buffer.")

(defun himp--get-parent-modes (mode)
  "Get parent modes of MODE."
  (let (result)
    (while (let ((parent (get mode 'derived-mode-parent)))
             (when parent
               (add-to-list 'result parent t)
               (setq mode parent))))
    result))

(defun himp--get-matchers (&optional mode)
  "Get matchers for given major mode MODE from variable `himp-matchers'.
MODE defaults to current major mode.
If there is no matcher for MODE, try to find matchers for parent mode."
  (or mode (setq mode major-mode))
  (cdr
   (or (assoc mode himp-matchers)
       (catch 'parent
         (dolist (parent (himp--get-parent-modes mode))
           (when (assoc parent himp-matchers)
             (throw 'parent (assoc parent himp-matchers))))))))

(defun himp-skip-space ()
  "Skip whitespace following point."
  (while (cond
          ((looking-at-p "\\s-")
           (forward-char) t)
          ((and (eolp) (not (eobp)))
           (forward-line)
           (beginning-of-line) t)
          (t nil))))

(defun himp-match-region-advance (matcher)
  "Skip space; attempt to text match using MATCHER; move point after match.
MATCHER must be of the form described in variable `himp-matchers'.
Returns cons cell (start . end) which is the matched region
\(without preceding whitespace), or nil if there was no match."
  (himp-skip-space)
  (let ((start (point)))
    (cond
     ((stringp matcher)
      (when (looking-at matcher)
        (goto-char (match-end 0))))
     ((and (consp matcher)
           (stringp (car matcher))
           (functionp (cdr matcher)))
      (when (looking-at-p (car matcher))
        (save-restriction (funcall (cdr matcher)))))
     ((and (consp matcher)
           (functionp (car matcher))
           (functionp (cdr matcher)))
      (when (save-excursion
              (save-restriction
                (funcall (car matcher))))
        (save-restriction (funcall (cdr matcher)))))
     ((and (listp matcher)
           (eq 'group (car matcher))
           (listp (cadr matcher)))
      (when (save-excursion
              (save-restriction
                (himp-next-region-advance (cadr matcher))))
        (save-restriction
          (himp-next-region-advance (cadr matcher)))))
     (t (error "Invalid matcher: %s" matcher)))
    (unless (= start (point))
      (cons start (point)))))

(defun himp-match-region (matcher)
  "Save excursion; attempt to match text following point using MATCHER."
  (save-excursion
    (himp-match-region-advance matcher)))

(defun himp-next-region-advance (matchers)
  "For each matcher in MATCHERS, try `himp-match-region-advance' with it.
Returns a cons-cell (matcher . (start . end)) when a match was found,
nil otherwise."
  (catch 'result
    (ignore
     (dolist (matcher matchers)
       (let ((region (himp-match-region-advance matcher)))
         (when region
           (throw 'result (cons matcher region))))))))

(defun himp-skip-matches (matchers)
  "Skip forward until no matcher in MATCHERS matches text following point.
Returns number of matches skipped."
  (let ((count 0))
    (while (himp-next-region-advance matchers)
      (setq count (1+ count)))
    count))

(defun himp-find-regions ()
  "Find all regions (after point) that should be hidden in this major mode.
Returns a list of matches (like `himp-match-region-advance').
Adjacent regions matched by the same matcher are merged into one."
  (let ((matchers (himp--get-matchers)))
    (unless matchers
      (error "No matchers for %s" major-mode))
    (let (result match lastmatch)
      (while (setq match (himp-next-region-advance matchers))
        (if (eq (car match) (car lastmatch))
            (setcdr (cdr lastmatch) (cddr match))
          (add-to-list 'result (setq lastmatch match) t)))
      (mapcar 'cdr result))))

(defun himp--python-narrow-to-tryblock ()
  "Narrow to pythonic tryblock at point.  Point must be at `try:` line."
  (save-excursion
    (let ((start (line-beginning-position))
          (end)
          (indentation (current-indentation)))
      (python-nav-end-of-block)
      (python-nav-forward-statement)
      (setq end (point))
      (catch 'done
        (while (and (looking-at-p "except\\b\\|finally\\b")
                    (eq (current-indentation) indentation))
          (python-nav-end-of-block)
          (setq end (point))
          (python-nav-forward-statement)
          (when (= (line-end-position) (point-max))
            (throw 'done nil))))
      (narrow-to-region start (or end (point))))))

(defvar himp--python-inside-tryblock nil
  "Non-nil when point is inside tryblock.")

(defun himp-python-tryblock-matcher ()
  "Matcher for try statements containing imports in python.
Matches a try-except-finally block only if it contains imports, other
tryblocks or comments."
  (catch 'result
    (when (himp-match-region-advance "try\\s-*:")
      (let ((himp--python-inside-tryblock t))
        (himp--python-narrow-to-tryblock)
        (when (zerop
               (himp-skip-matches (himp--get-matchers 'python-mode)))
          (throw 'result nil))
        (let* ((matchers (append
                          '("pass\\b")
                          (himp--get-matchers 'python-mode)))
               (except
                (when (himp-match-region "except[^:]*:")
                  (while (himp-match-region-advance "except[^:]*:")
                    (when (zerop (himp-skip-matches matchers))
                      (throw 'result nil)))
                  t))
               (finally
                (when (himp-match-region-advance "finally\\s-*:")
                  (if (zerop (himp-skip-matches matchers))
                      (throw 'result nil)
                    t))))
          (or except finally))))))

(defun himp-python-comment-matcher ()
  "Comment matcher for python.
Matches comments only if they are followed by something else that
should be hidden or 2 empty lines."
  (when (python-info-current-line-comment-p)
    (forward-line)
    (or
     himp--python-inside-tryblock
     (save-excursion
       (himp-next-region-advance
        (himp--get-matchers 'python-mode)))
     (save-excursion
       (and (prog1 (python-info-current-line-empty-p)
              (forward-line))
            (python-info-current-line-empty-p))))))

(defun himp-java-comment-matcher ()
  "Non-javadoc comment matcher for java."
  (and
   (not (looking-at-p "/\\*\\*"))
   (c-forward-single-comment)))

(defun himp--delete-fold (marker)
  "Remove fold at MARKER."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char marker)
      (ignore-errors
        (vimish-fold-delete)))))

(defun himp--make-fold (beg end)
  "Make fold in range BEG END."
  (condition-case err
      (let* ((vimish-fold-show-lines himp-show-line-count)
             (header-line-length
              (save-excursion
                (goto-char beg)
                (length (thing-at-point 'line))))
             (orig-header-width
              (and (boundp 'vimish-fold-header-width)
                   vimish-fold-header-width))
             (vimish-fold-header-width
              (if vimish-fold-show-lines
                  orig-header-width
                header-line-length)))
        (vimish-fold beg end))
    (error
     (message "Himp--make-fold: Vimish-fold error:
 '%s' on region %s %s" err beg end))))

;;;###autoload
(defun himp-unhide ()
  "Unhide regions hidden with `himp-hide'."
  (interactive)
  (dolist (region himp--regions)
    (himp--delete-fold region)
    (set-marker region nil))
  (setq himp--regions nil))

;;;###autoload
(defun himp-hide ()
  "Hide uninteresting regions in current buffer."
  (interactive)
  (himp-unhide)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (dolist (region (himp-find-regions))
        (let ((start (car region))
              (end (cdr region))
              (marker (make-marker)))
          (unless (= (line-number-at-pos start)
                     (line-number-at-pos end))
            (himp--make-fold start end)
            (set-marker marker start)
            (set-marker-insertion-type marker t)
            (add-to-list 'himp--regions marker)))))))

(defun himp-handle-save ()
  "Rescan buffer if `himp-rescan-on-save' is non-nil."
  (and himp-mode himp-rescan-on-save (himp-hide)))

;;;###autoload
(define-minor-mode himp-mode
  "Hide imports/uninteresting stuff at beginning of buffer."
  :lighter " himp"
  :keymap himp-keymap
  (if himp-mode
      (progn
        (himp-hide)
        (add-hook 'after-save-hook 'himp-handle-save nil t))
    (himp-unhide)
    (remove-hook 'after-save-hook 'himp-handle-save t)))

(provide 'himp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; himp.el ends here
