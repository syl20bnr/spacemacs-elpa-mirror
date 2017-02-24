;;; zpresent.el --- Simple presentation mode based on org files.  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Version: 20170223.2227
;; This file is not part of GNU Emacs.

;; Copyright 2015-2017 Zachary Kanfer <zkanfer@gmail.com>

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


;; Homepage: https://bitbucket.org/zck/zpresent.el

;; Package-Requires: ((emacs "25.1") (org-parser "0.1") (dash "2.13.0"))

;; Keywords: comm


;;; Commentary:

;;; TODOs:
;;zck why symbols here, but keywords in org-structure?
;;zck test non-ordered non-headline lists

;;; Code:

(define-derived-mode zpresent-mode special-mode "zpresent-mode"
  (define-key zpresent-mode-map (kbd "n") #'zpresent--next-slide)
  (define-key zpresent-mode-map (kbd "C-n") #'zpresent--next-slide)
  (define-key zpresent-mode-map (kbd "<right>") #'zpresent--next-slide)
  (define-key zpresent-mode-map (kbd "<down>") #'zpresent--next-slide)
  (define-key zpresent-mode-map (kbd "p") #'zpresent--previous-slide)
  (define-key zpresent-mode-map (kbd "C-p") #'zpresent--previous-slide)
  (define-key zpresent-mode-map (kbd "<left>") #'zpresent--previous-slide)
  (define-key zpresent-mode-map (kbd "<up>") #'zpresent--previous-slide)
  (define-key zpresent-mode-map (kbd "<next>") #'zpresent--next-checkpoint-slide)
  (define-key zpresent-mode-map (kbd "N") #'zpresent--next-checkpoint-slide)
  (define-key zpresent-mode-map (kbd "<prior>") #'zpresent--previous-checkpoint-slide)
  (define-key zpresent-mode-map (kbd "P") #'zpresent--previous-checkpoint-slide)
  (define-key zpresent-mode-map (kbd "<home>") #'zpresent--first-slide)
  (define-key zpresent-mode-map (kbd "<end>") #'zpresent--last-slide)
  (define-key zpresent-mode-map (kbd "C-+") #'zpresent--increase-text-size)
  (define-key zpresent-mode-map (kbd "+") #'zpresent--increase-text-size)
  (define-key zpresent-mode-map (kbd "C-=") #'zpresent--increase-text-size)
  (define-key zpresent-mode-map (kbd "=") #'zpresent--increase-text-size)
  (define-key zpresent-mode-map (kbd "C--") #'zpresent--decrease-text-size)
  (define-key zpresent-mode-map (kbd "-") #'zpresent--decrease-text-size))


;;;; Requires:
(require 'org-parser)
(require 'subr-x)
(require 'cl-lib)

;;;; Variables:
(defvar zpresent-slides nil
  "The slides for the current presentation.")

(defvar zpresent-source nil
  "The original org structure for the presentation.")

(defvar zpresent-position 0
  "The current slide position.")

(defconst zpresent-increase-multiplier 1.25
  "The amount to increase size when increasing size.")

(defconst zpresent-decrease-multiplier 0.8
  "The amount to decrease size when decreasing size.")

(defconst zpresent-long-title-cutoff 0.66
  "The fraction of the length of a line a title can be before it's considered long.")

(defvar zpresent-bullet "▸")

(defvar zpresent-fullscreen-on-zpresentation t
  "Whether to call #'toggle-frame-fullscreen upon starting a presentation.")

(defvar zpresent-delete-other-windows t
  "Whether to delete other windows when starting a presentation.")

(defvar zpresent-align-title 'left
  "How to align lines in the title.  Possible values are 'left, 'right, 'center.")


;;;; Faces:
;;zck make scaling function just change zpresent-base, not the faces in zpresent-faces.
;;zck why doesn't this center properly anymore?
(defface zpresent-base '((t . (:height 4.0))) "The base face, so we can manage changing sizes only by changing this face." :group 'zpresent-faces)
(defface zpresent-h1 '((t . (:height 1.0 :inherit zpresent-base))) "Face for titles." :group 'zpresent-faces)
(defface zpresent-body '((t . (:height 0.66 :inherit zpresent-base))) "Face for the body." :group 'zpresent-faces)

;;;; Actual code:
;;;###autoload
(defun zpresent ()
  "Present the current .org file."
  (interactive)

  (setq zpresent-source (org-parser-parse-buffer (current-buffer)))
  (setq zpresent-position 0)
  (setq zpresent-slides (zpresent--format zpresent-source))

  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)

  (when zpresent-fullscreen-on-zpresentation
    (set-frame-parameter nil 'fullscreen 'fullboth))

  (when zpresent-delete-other-windows
    (delete-other-windows))

  (visual-line-mode)

  (zpresent--redisplay))

(defun zpresent--format (structure-list)
  "Convert an STRUCTURE-LIST into a list of slides."
  (cl-mapcan #'zpresent--format-recursively
             structure-list))

(defun zpresent--format-recursively (structure)
  "Convert STRUCTURE into a list of slides."
  (zpresent--format-recursively-helper structure
                                       (zpresent--make-top-level-slide structure)
                                       1))

(defun zpresent--format-recursively-helper (structure slide-so-far level)
  "Convert STRUCTURE into a list of slides.

SLIDE-SO-FAR is the built-up slide to append text in the body to.

STRUCTURE is at level LEVEL.  This is used for indentation.

Return the list of slides."
  (let ((most-recent-slide slide-so-far)
        (slides-list (list slide-so-far))
        (how-many-slides 0))
    (dolist (cur-child (gethash :children structure))
      (setq slides-list
            (append slides-list
                    (zpresent--format-recursively-helper cur-child (zpresent--make-following-slide most-recent-slide cur-child level how-many-slides) (1+ level))))
      (setq most-recent-slide (car (last slides-list)))
      (cl-incf how-many-slides))
    slides-list))

(defun zpresent--make-top-level-slide (structure)
  "Make a top level slide from STRUCTURE."
  (zpresent--make-slide (zpresent--extract-current-text structure)))

;;zck test how this interacts with indentation/centering, if it does
(defun zpresent--extract-current-text (structure)
  "Extracts the text that should go in the slide for STRUCTURE.

This returns a list of lines."
  (if (gethash :body structure)
      (append (list (gethash :text structure))
              (gethash :body structure))
    (list (gethash :text structure))))

(defun zpresent--make-body (structure level prior-siblings)
  "Make the body text for STRUCTURE (a single structure, not a list)
at indentation level LEVEL.

PRIOR-SIBLINGS is the number of structures before STRUCTURE with the
same parent.  This is used for ordered lists.

Body text is a list containing the text just for the headline,
ignoring any children, but handling multiline headlines.  Each item in
this list is a list of strings or structure items.

The result of this is a list, containing both text and hashes.  Hashes
indicate something other than plain text.  For example, an image."
  (cons (cons (format " %s%s "
                      (make-string (* (1- level) 2) ?\s)
                      (cond ((equal ?* (gethash :bullet-type structure))
                             zpresent-bullet)
                            ((equal ?\) (gethash :bullet-type structure))
                             (format "%d)" (1+ prior-siblings)))
                            ((equal ?. (gethash :bullet-type structure))
                             (format "%d." (1+ prior-siblings)))
                            (t "")))
              (gethash :text structure))
        (let ((body (gethash :body structure))
              (body-indentation (format "%s%s"
                                        (make-string (* level 2) ?\s)
                                        (if (equal ?* (gethash :bullet-type structure))
                                            " "
                                          ""))))
          (mapcar (lambda (body-line)
                    (cons body-indentation body-line))
                  body))))

(defun zpresent--make-slide (title &optional body)
  "Create the slide with title TITLE.

If BODY is present, add it as the body of the slide.  Otherwise, the
slide is created with an empty body."
  (let ((slide (make-hash-table)))
    (puthash 'checkpoint t slide)
    (puthash 'title title slide)
    (puthash 'body (if body (list body) nil) slide)
    slide))

(defun zpresent--make-following-slide (slide structure level &optional prior-siblings)
  "Extend SLIDE with the contents of STRUCTURE, at level LEVEL.

PRIOR-SIBLINGS is the number of structures at the same level before
STRUCTURE with the same parent."
  (let ((new-slide (copy-hash-table slide)))

    (puthash 'checkpoint
             nil
             new-slide)
    (puthash 'body
             (append (gethash 'body slide)
                     (zpresent--make-body structure level (or prior-siblings 0)))
             new-slide)
    new-slide))

;;zck eventually keywords? Or symbols? Who knows.

(defun zpresent--break-title-into-lines (title-list chars-in-line)
  "Break TITLE-LIST into a list of lines, each line shorter than CHARS-IN-LINE.

This will return a list of lists.  The sub-lists will contain a
mixture of strings and hashes, when there are formatted strings in
TITLE-LIST.

If a single word is longer than CHARS-IN-LINE, that entire word will
be on a sub-list all by itself."
  (when title-list
    (cl-multiple-value-bind (first-line rest-of-title-list)
        (zpresent--pull-single-title-line title-list chars-in-line)
      (cons first-line
            (zpresent--break-title-into-lines rest-of-title-list
                                              chars-in-line)))))


(defun zpresent--pull-single-title-line (title-list chars-in-line &optional strict-length)
  "Pull a single title line out of TITLE-LIST, a list of items.

A title line is a list of items from TITLE-LIST, or sub-items such
that the line is length CHARS-IN-LINE or less.

This method returns a list; the first item is the pulled line; the
second item is the remaining items in TITLE-LIST.

If STRICT-LENGTH is true, the line returned will be less than or equal
to CHARS-IN-LINE, even if the first word in TITLE-LIST is longer than
CHARS-IN-LINE.  In that case, the line will be empty.  If
STRICT-LENGTH is nil, this will return a list containing at least one
item, even if that single word is longer than CHARS-IN-LINE.

The only thing this should do that -helper doesn't is trim
whitespace from the first and last thing in the line."
  (cl-multiple-value-bind (this-line other-title-items)
      (zpresent--pull-single-title-line-helper (zpresent--trim-beginning-and-end-of-line title-list) chars-in-line strict-length)
    (list (zpresent--trim-beginning-and-end-of-line this-line)
          other-title-items)))

(defun zpresent--trim-beginning-and-end-of-line (title-line)
  "Trim whitespace from the beginning and end of TITLE-LINE."
  (cond ((not title-line)
         nil)
        ((equal 1 (length title-line))
         (list (zpresent--trim-item (cl-first title-line))))
        (t (cons (zpresent--trim-item-left (cl-first title-line))
                 (append (butlast (cl-rest title-line))
                         (list (zpresent--trim-item-right (cl-first (last title-line)))))))))

(defun zpresent--pull-single-title-line-helper (title-list chars-in-line &optional strict-length)
  "Helper for zpresent--pull-single-title-line.

Pull a single title line out of TITLE-LIST, a list of items.

A title line is a list of items from TITLE-LIST, or sub-items such
that the line is length CHARS-IN-LINE or less.

This method returns a list; the first item is the pulled line; the
second item is the remaining items in TITLE-LIST.

If STRICT-LENGTH is true, the line returned will be less than or equal
to CHARS-IN-LINE, even if the first word in TITLE-LIST is longer than
CHARS-IN-LINE.  In that case, the line will be empty.  If
STRICT-LENGTH is nil, this will return a list containing at least one
item, even if that single word is longer than CHARS-IN-LINE."
  (let ((title-list-with-combined-strings (zpresent--combine-consecutive-strings-in-list title-list)))
    (if (not title-list-with-combined-strings)
        (list nil nil)
      (if (>= (zpresent--item-length (cl-first title-list-with-combined-strings))
              chars-in-line)
          (cl-multiple-value-bind (before-break after-break)
              (zpresent--break-item (cl-first title-list-with-combined-strings) chars-in-line strict-length)
            (list (when before-break
                    (list before-break))
                  (if (and after-break
                           (> (zpresent--item-length after-break)
                              0))
                      (cons after-break (cdr title-list-with-combined-strings))
                    (cdr title-list-with-combined-strings))))
        (cl-multiple-value-bind (rest-of-line remaining-items)
            (zpresent--pull-single-title-line-helper (cdr title-list-with-combined-strings)
                                                     (- chars-in-line
                                                        (zpresent--item-length (cl-first title-list-with-combined-strings)))
                                                     t)
          (list (cons (cl-first title-list-with-combined-strings) rest-of-line)
                remaining-items))))))

(defun zpresent--trim-item (item)
  "Trim whitespace on both sides of ITEM."
  (zpresent--trim-item-left (zpresent--trim-item-right item)))

(defun zpresent--trim-item-left (item)
  "Trim whitespace on the left of ITEM."
  (cond ((stringp item)
         (string-trim-left item))
        ((hash-table-p item)
         (let ((copied-hash (copy-hash-table item)))
           (puthash :text
                    (string-trim-left (gethash :text item))
                    copied-hash)
           copied-hash))))

(defun zpresent--trim-item-right (item)
  "Trim whitespace on the right of ITEM."
  (cond ((stringp item)
         (string-trim-right item))
        ((hash-table-p item)
         (let ((copied-hash (copy-hash-table item)))
           (puthash :text
                    (string-trim-right (gethash :text item))
                    copied-hash)
           copied-hash))))

(defun zpresent--combine-consecutive-strings-in-list (list)
  "Return LIST, but with consecutive strings joined together."
  (cond ((< (length list)
            2)
         list)
        ((and (stringp (cl-first list))
              (stringp (cl-second list)))
         (zpresent--combine-consecutive-strings-in-list (cons (concat (cl-first list)
                                                                      (cl-second list))
                                                              (cdr (cdr list)))))
        (t (cons (cl-first list)
                 (zpresent--combine-consecutive-strings-in-list (cl-rest list))))))

(defun zpresent--break-item (item chars-in-line &optional strict-length)
  "Break ITEM at the last whitespace before or at CHARS-IN-LINE.

If the first word in ITEM is longer than CHARS-IN-LINE, and
STRICT-LENGTH is nil, this will break at the first whitespace after
CHARS-IN-LINE.  If STRICT-LENGTH is t, this will return nil for the
first part of the broken item.

This returns a list where the first item is the first part of the
broken item, and the second item is the rest of the item."
  (if (stringp item)
      (zpresent--split-once-at-space item chars-in-line strict-length)
    (cl-multiple-value-bind (pre-split post-split)
        (zpresent--split-once-at-space (gethash :text item)
                                       chars-in-line
                                       strict-length)
      (list (when pre-split (org-parser--make-link-hash (gethash :target item)
                                                        pre-split))
            (when post-split (org-parser--make-link-hash (gethash :target item)
                                                         post-split))))))

(defun zpresent--line-length (line-list)
  "Calculate the length of LINE-LIST.

LINE-LIST is a list of structure items -- either strings, or hashes
representing formatted text."
  (if (not line-list)
      0
    (+ (zpresent--item-length (car line-list))
       (zpresent--line-length (cdr line-list)))))

(defun zpresent--item-length (item)
  "Calculate the length of ITEM, which is a string or a formatted text hash."
  (cond ((stringp item) (length item))
        ((zpresent--item-is-image item)
         0)
        ((and (hash-table-p item)
              (equal (gethash :type item)
                     :link))
         (length (gethash :text item)))
        (t (error "Can't get the length of %s" item))))

(defun zpresent--format-body (body-line)
  "Format BODY-LINE appropriately for the body."
  (propertize body-line
              'face
              'zpresent-body))

(defun zpresent--split-once-at-space (string max-length &optional strict-length)
  "Split STRING at the last space at MAX-LENGTH or earlier.

If the first word is of length MAX-LENGTH or greater, that word will
be on a line by itself, unless STRICT-LENGTH is t, in which case it'll
be nil.

This returns a list with the split string as the first item, and
the rest of the string as the second."
  (let ((trimmed-string (string-trim string)))
    (if (<= (length string)
            max-length)
        (list string nil)
      (let ((pos-to-split-at (or (cl-position ?\s string :from-end t :end (truncate (1+ max-length)))
                                 (and (not strict-length)
                                      (cl-position ?\s string)))))
        (cond (pos-to-split-at
               (list (string-trim-right (substring string 0 pos-to-split-at))
                     (string-trim-left (substring string pos-to-split-at))))
              (strict-length
               (list nil string))
              (t (list string nil)))))))

(defun zpresent--split-at-space (string max-length)
  "Split STRING at a space.  Each substring must be MAX-LENGTH or shorter.

If there's a single word of length MAX-LENGTH, that word will be on a line by itself."
  (if (<= (length string)
          max-length)
      (list (string-trim string))
    (let ((pos-to-split-at (cl-position ?\s string :from-end t :end max-length)))
      (if pos-to-split-at
          (cons (string-trim (substring string 0 pos-to-split-at))
                (zpresent--split-at-space (string-trim (substring string pos-to-split-at)) max-length))
        (cons (string-trim (substring string 0 max-length))
              (zpresent--split-at-space (string-trim (substring string max-length)) max-length))))))

(defun zpresent--first-slide ()
  "Move to the first slide."
  (interactive)
  (setq zpresent-position 0)
  (zpresent--slide (elt zpresent-slides zpresent-position)))

(defun zpresent--last-slide ()
  "Move to the last slide."
  (interactive)
  (setq zpresent-position (1- (length zpresent-slides)))
  (zpresent--slide (elt zpresent-slides zpresent-position)))

(defun zpresent--next-slide ()
  "Move to the next slide."
  (interactive)
  (when (< zpresent-position
           (1- (length zpresent-slides)))
    (cl-incf zpresent-position)
    (zpresent--slide (elt zpresent-slides zpresent-position))))

(defun zpresent--previous-slide ()
  "Move to the previous slide."
  (interactive)
  (when (> zpresent-position
           0)
    (cl-decf zpresent-position)
    (zpresent--slide (elt zpresent-slides zpresent-position))))

(defun zpresent--next-checkpoint-slide ()
  "Move to the next checkpoint slide.

A checkpoint slide is one with the attribute 'checkpoint.  It's used,
for example, for the first slide of each top level org element."
  (interactive)
  (let ((checkpoint-position (zpresent--next-match (lambda (slide) (gethash 'checkpoint slide))
                                                   zpresent-slides
                                                   (1+ zpresent-position))))
    (when checkpoint-position
      (setq zpresent-position checkpoint-position)
      (zpresent--slide (elt zpresent-slides checkpoint-position)))))

(defun zpresent--previous-checkpoint-slide ()
  "Move to the previous checkpoint slide.

A checkpoint slide is one with the attribute 'checkpoint.  It's used,
for example, for the first slide of each top level org element."
  (interactive)
  (let ((checkpoint-position (zpresent--previous-match (lambda (slide) (gethash 'checkpoint slide))
                                                       zpresent-slides
                                                       (1- zpresent-position))))
    (when checkpoint-position
      (setq zpresent-position checkpoint-position)
      (zpresent--slide (elt zpresent-slides checkpoint-position)))))


(cl-defun zpresent--find-forwards (pred list &optional (starting-point 0))
  "Find the first element that PRED considers truthy in LIST at or after STARTING-POINT."
  (when-let (additional-places (-find-index pred
                                            (nthcdr starting-point list)))
    (+ starting-point additional-places)))

(cl-defun zpresent--find-backwards (pred list &optional (ending-point (length list)))
  "Find the last element that PRED considers truthy in LIST at or before ENDING-POINT."
  (-find-last-index pred
                    (cl-subseq list
                               0
                               (min (1+ ending-point) (length list)))))

(defun zpresent--slide (slide)
  "Present SLIDE."
  (interactive)
  (switch-to-buffer "zpresentation")
  (buffer-disable-undo "zpresentation")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")
    (when (gethash 'title slide)
      (zpresent--insert-title (gethash 'title slide))
      (insert "\n"))
    (when (gethash 'body slide)
      (dolist (body-item (gethash 'body slide))
        (zpresent--insert-body-item body-item)
        (insert "\n")))))

(defun zpresent--insert-title (title)
  "Insert TITLE into the buffer."
  (let* ((chars-in-line (/ (window-width)
                          (face-attribute 'zpresent-h1 :height nil t)))
         (title-lines (if (equal 1 (length title)) ;;zck do we still need to check if it's 1 or many?
                          (zpresent--break-title-into-lines (cl-first title) (* chars-in-line
                                                                                zpresent-long-title-cutoff))
                        title))
         (whitespace-for-title (zpresent--calculate-aligned-whitespace title-lines chars-in-line))
         (longest-line-length (apply #'max (mapcar #'zpresent--line-length title-lines))))
    (dolist (title-line title-lines)
      (let ((whitespace-for-this-line (cl-case zpresent-align-title
                                        ('left whitespace-for-title)
                                        ('center nil)
                                        ('right (concat whitespace-for-title
                                                        (make-string (- longest-line-length
                                                                        (zpresent--line-length title-line))
                                                                     ?\s))))))
        (zpresent--insert-title-line title-line chars-in-line whitespace-for-this-line)))))


(defun zpresent--calculate-aligned-whitespace (title chars-in-line)
  "Return the whitespace for a TITLE.

TITLE is a list of rows, and is presented aligned in a row of
length CHARS-IN-LINE.."
  ;;Add one here so that we round away from zero. We want to have more whitespace on the left than the right side.
  ;;zck is this what's wanted?
  (make-string (truncate (max 0
                              (1+ (- chars-in-line
                                     (zpresent--find-longest-line-length title))))
                         2)
               ?\s))

(defun zpresent--find-longest-line-length (lines)
  "Find the length of the longest line in LINES."
  (apply #'max
         (mapcar #'zpresent--line-length
                 lines)))

(defun zpresent--insert-title-line (title-line chars-in-line &optional precalculated-whitespace)
  "Insert TITLE-LINE into the buffer.

CHARS-IN-LINE is the length of the line.  If PRECALCULATED-WHITESPACE
is provided, pad all the lines by that amount.  Otherwise, center the
title-line."
  (if precalculated-whitespace
      (insert (propertize precalculated-whitespace 'face 'zpresent-h1))
    (insert (zpresent--whitespace-for-centered-title-line title-line chars-in-line)))
  (dolist (title-item title-line)
    (zpresent--insert-title-item title-item))
  (insert "\n"))

(defun zpresent--insert-title-item (item)
  "Insert ITEM into the buffer as part of the title."
  (cond ((stringp item)
         (insert (propertize item
                             'face
                             'zpresent-h1)))
        ((zpresent--item-is-image item)
         (zpresent--insert-image (gethash :target item)))
        (t (zpresent--insert-link item 'zpresent-h1))))

(defun zpresent--item-is-image (item)
  "T if ITEM is an image."
  (and (hash-table-p item)
       (equal :link
              (gethash :type item))
       (equal "zp-image"
              (gethash :text item))))

(defun zpresent--insert-image (image-location)
  "Insert IMAGE-LOCATION as an image."
  (let ((realpath (if (string-prefix-p "file:" image-location)
                      (expand-file-name (string-remove-prefix "file:" image-location))
                    image-location)))

    ;;zck why isn't this inserting images from the internet ok? Should it?
    (insert-image (create-image realpath))))

(defun zpresent--whitespace-for-centered-title-line (title-line chars-in-line)
  "Get whitespace for TITLE-LINE.

This method assumes TITLE-LINE will not be split, and the window to be
printed into has width CHARS-IN-LINE."
  (let* ((line-width (zpresent--line-length title-line))
         (chars-to-add (max 0
                            (truncate (- chars-in-line line-width)
                                      2))))
    (propertize (make-string chars-to-add ?\s)
                'face
                'zpresent-h1)))

(defun zpresent--insert-body-item (body-item)
  "Insert BODY-ITEM into the buffer."
  (cond ((stringp body-item)
         (insert (zpresent--format-body body-item)))
        ((listp body-item)
         (dolist (inner-item body-item)
           (zpresent--insert-body-item inner-item)))
        ((zpresent--item-is-image body-item)
         (zpresent--insert-image (gethash :target body-item)))
        (t (zpresent--insert-link body-item 'zpresent-body))))

(defun zpresent--insert-link (link-hash face)
  "Insert LINK-HASH into the buffer, as a link, with face FACE.

If you want to insert an image, use '#'zpresent--insert-image'."
  (insert-button (propertize (gethash :text link-hash)
                             'face face)
                 'action `(lambda (button) (browse-url ,(gethash :target link-hash)))))



(defun zpresent--increase-text-size ()
  "Make everything bigger."
  (interactive)
  (set-face-attribute 'zpresent-base
                      nil
                      :height
                      (* zpresent-increase-multiplier
                         (or (face-attribute 'zpresent-base :height)
                             1)))
  (zpresent--redisplay))

(defun zpresent--decrease-text-size ()
  "Make everything smaller."
  (interactive)
  (set-face-attribute 'zpresent-base
                      nil
                      :height
                      (* zpresent-decrease-multiplier
                         (or (face-attribute 'zpresent-base :height)
                             1)))
  (zpresent--redisplay))

(defun zpresent--redisplay ()
  "Redisplay the presentation at the current slide."
  (interactive)
  (zpresent--slide (elt zpresent-slides zpresent-position)))

;;the slide is stored as a hash. Key-value pairs are:
;; key: title
;; value: The title of the slide. If this is a string, automatically split it.
;;    If this is a list, assume it's been manually split by the user,
;;    so just use each line separately.
;; key: body
;; value: A list of the lines in the body of the slide.

(defun zpresent--test-presentation ()
  "Start a presentation with dummy data."
  (interactive)
  (setq zpresent-position 0)
  (setq zpresent-slides
        (list #s(hash-table data (title "one-line title" body ("body line 1" "body line 2")))
              #s(hash-table data (title ("title manually split" "onto three" "lines (this one is pretty gosh darn long, but it shouldn't be automatically split no matter how long it is.)")))
              #s(hash-table data (title "an automatically split really really really really really really really really really long title"))))

  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)

  (zpresent--redisplay))

(provide 'zpresent)

;;; zpresent.el ends here
