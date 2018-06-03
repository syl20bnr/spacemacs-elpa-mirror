;;; organize-imports-java.el --- Mimic Eclipse's Organize Imports functionality.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-04-16 13:12:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Mimic Eclipse C-S-o key. (Organeize Imports)
;; Keyword: organize imports java handy eclipse
;; Version: 0.0.1
;; Package-Version: 20180603.133
;; Package-Requires: ((emacs "24") (f "0.20.0") (s "1.12.0") (cl-lib "0.6"))
;; URL: https://github.com/jcs090218/organize-imports-java

;; This file is NOT part of GNU Emacs.

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
;;
;; Mimic Eclipse's Organize Imports functionality.
;;
;; (@* "TODO" )
;; * Performance is terrible when loading all the jar files to path.
;;   Hopefully I can find out a way to get around this issue.
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)


(defgroup organize-imports-java nil
  "Organize imports java extension"
  :prefix "organize-imports-java-"
  :group 'editing
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/organize-imports-java.git"))


(defcustom organize-imports-java-java-sdk-path "C:/Program Files/Java/jdk1.8.0_131"
  "Java SDK Path."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-inc-keyword "::SDK_PATH::"
  "Java SDK Path."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-lib-inc-file "oij.config"
  "Java library include config file."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-path-jar-lib-cache-file "paths-cache.oij"
  "File generate store all the jar/lib Java paths."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-path-local-source-cache-file "paths-cache-local.oij"
  "File generate store all the local source Java paths."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-font-lock-type-faces '("font-lock-type-face")
  "List of type font face that current Jave mode applied to use."
  :type 'list
  :group 'organize-imports-java)

(defcustom organize-imports-java-unsearch-class-type '("[Bb]oolean"
                                                       "[Bb]yte"
                                                       "Character"
                                                       "char"
                                                       "[Dd]ouble"
                                                       "[Ff]loat"
                                                       "[Ii]nteger"
                                                       "int"
                                                       "[Ll]ong"
                                                       "[Ss]tring"
                                                       "[Ss]hort"
                                                       "[Vv]oid")
  "Class types that do not need to imports any library path."
  :type 'list
  :group 'organize-imports-java)

(defcustom organize-imports-java-source-dir-name "src"
  "Source directory in the project, default is 'src'.")

(defcustom organize-imports-java-non-class-list '("Callable"
                                                  "Runnable")
  "List that are no need to import like interface, etc.")


(defvar organize-imports-java-path-buffer-jar-lib '()
  "All the available java paths store here.")

(defvar organize-imports-java-path-buffer-local-source '()
  "All the available local source java paths store here.")

(defvar organize-imports-java-serach-regexp "[a-zA-Z0-9/_-]*/[A-Z][a-zA-Z0-9_-]*\\.class"
  "Regular Expression to search for java path.")

(defvar organize-imports-java-non-src-list '("document"
                                             "internal"
                                             "sun")
  "List of non Java source keywords.")


(defvar organize-imports-java-pre-insert-path-list '()
  "Paths that are ready to insert.")

(defvar organize-imports-java-same-class-name-list '()
  "Paths will store temporary, use to check if multiple class exists in the environment.")



(defun organize-imports-java-get-alphabet-id (c)
  "Get the alphabet id.
C : character to find the alphabet id."
  (cond ((string= c "A") (progn 0))
        ((string= c "B") (progn 1))
        ((string= c "C") (progn 2))
        ((string= c "D") (progn 3))
        ((string= c "E") (progn 4))
        ((string= c "F") (progn 5))
        ((string= c "G") (progn 6))
        ((string= c "H") (progn 7))
        ((string= c "I") (progn 8))
        ((string= c "J") (progn 9))
        ((string= c "K") (progn 10))
        ((string= c "L") (progn 11))
        ((string= c "M") (progn 12))
        ((string= c "N") (progn 13))
        ((string= c "O") (progn 14))
        ((string= c "P") (progn 15))
        ((string= c "Q") (progn 16))
        ((string= c "R") (progn 17))
        ((string= c "S") (progn 18))
        ((string= c "T") (progn 19))
        ((string= c "U") (progn 20))
        ((string= c "V") (progn 21))
        ((string= c "W") (progn 22))
        ((string= c "X") (progn 23))
        ((string= c "Y") (progn 24))
        ((string= c "Z") (progn 25))
        (t (progn -1))))

(defun organize-imports-java-is-contain-list-string (in-list in-str)
  "Check if a string contain in any string in the string list.
IN-LIST : list of string use to check if IN-STR in contain one of
the string.
IN-STR : string using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun organize-imports-java-contain-string (in-sub-str in-str)
  "Check if a string is a substring of another string.
Return true if contain, else return false.
IN-SUB-STR : substring to see if contain in the IN-STR.
IN-STR : string to check by the IN-SUB-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun organize-imports-java-is-in-list-string (in-list str)
  "Check if a string in the string list.
IN-LIST : list of strings.
STR : string to check if is inside the list of strings above."
  (cl-some #'(lambda (lb-sub-str) (string-match lb-sub-str str)) in-list))

;;;###autoload
(defun organize-imports-java-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]\t]*$")))

;;;###autoload
(defun organize-imports-java-keep-one-line-between ()
  "Keep one line between the two line of code.
If you want to keep more than one line use
`organize-imports-java-keep-n-line-between' instead."
  (interactive)
  (if (organize-imports-java-current-line-empty-p)
      (progn
        (forward-line 1)

        ;; Kill empty line until there is one line.
        (while (organize-imports-java-current-line-empty-p)
          (organize-imports-java-kill-whole-line)))
    (progn
      ;; Make sure have one empty line between.
      (insert "\n"))))

(defun organize-imports-java-get-string-from-file (file-path)
  "Return file-path's file content.
FILE-PATH : file path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun organize-imports-java-parse-ini (file-path)
  "Parse a .ini file.
FILE-PATH : .ini file to parse."

  (let ((tmp-ini (organize-imports-java-get-string-from-file file-path))
        (tmp-ini-list '())
        (tmp-pair-list nil)
        (tmp-keyword "")
        (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (when (not (string-match-p "#" tmp-line))
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string= tmp-keyword ""))
                   (not (equal tmp-value nil)))
          (let ((tmp-list '()))
            (push tmp-keyword tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (push tmp-value tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun organize-imports-java-get-properties (ini-list in-key)
  "Get properties data.  Search by key and return value.
INI-LIST : ini list.  Please use this with/after using
`organize-imports-java-parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0)
        (tmp-key "")
        (tmp-value "")
        (returns-value ""))

    (while (< tmp-index (length ini-list))
      ;; Get the key and data value.
      (setq tmp-key (nth tmp-index ini-list))
      (setq tmp-value (nth (1+ tmp-index) ini-list))

      ;; Find the match.
      (when (string= tmp-key in-key)
        ;; return data value.
        (setq returns-value tmp-value))

      ;; Search for next key word.
      (setq tmp-index (+ tmp-index 2)))

    ;; Found nothing, return empty string.
    returns-value))

(defun organize-imports-java-re-seq (regexp string)
  "Get a list of all regexp match in a string.

REGEXP : regular expression.
STRING : string to do searching."
  (save-match-data
    (let ((case-fold-search t)
          (pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun organize-imports-java-flatten (l)
  "Flatten the multiple dimensional array to one dimensonal array.
'(1 2 3 4 (5 6 7 8)) => '(1 2 3 4 5 6 7 8).

L : list we want to flaaten."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (organize-imports-java-flatten a)))))

(defun organize-imports-java-get-local-source ()
  "Get the all the local source file path as a list."
  (let ((src-file-path-list '())
        (project-source-dir (concat (cdr (project-current)) organize-imports-java-source-dir-name "/")))
    (setq src-file-path-list (f--files project-source-dir (equal (f-ext it) "java") t))

    (let ((index 0))
      (dolist (src-file-path src-file-path-list)
        ;; Remove the source file path, only left the package file path.
        (setf (nth index src-file-path-list)
              (s-replace project-source-dir "" src-file-path))

        ;; Convert '/' to '.'.
        (setf (nth index src-file-path-list)
              (s-replace "/" "." (nth index src-file-path-list)))

        ;; Lastly, remove `.java' extension.
        (setf (nth index src-file-path-list)
              (s-replace ".java" "" (nth index src-file-path-list)))

        (setq index (1+ index))))

    src-file-path-list))

(defun organize-imports-java-unzip-lib ()
  "Decode it `.jar' binary to readable data strucutre."
  (let ((tmp-lib-inc-file (concat
                           (cdr (project-current))
                           organize-imports-java-lib-inc-file))
        (tmp-lib-list '())
        ;; Key read from the .ini/.properties file.
        ;;(tmp-lib-key "")
        ;; Value read from the .ini/.properties file.
        (tmp-lib-path "")
        ;; Buffer read depends on one of the `tmp-lib-path'.
        (tmp-lib-buffer nil)
        ;; After search using regular expression and add all the
        ;; paths to the list/array.
        (tmp-class-list '())
        ;; index through the lib/jar paths list.
        (tmp-index 0)
        ;; length of the lib/jar paths list.
        (tmp-lib-list-length -1)
        ;; First character of the path readed from .ini file.
        (first-char-from-path nil)
        ;; Final return path list.
        (all-lib-path-list '()))
    (when (file-exists-p tmp-lib-inc-file)
      ;; Read the ini file, in order to get all the target
      ;; lib/jar files.
      (setq tmp-lib-list (organize-imports-java-parse-ini tmp-lib-inc-file))

      ;; Get the length of the library list
      (setq tmp-lib-list-length (length tmp-lib-list))

      (while (< tmp-index tmp-lib-list-length)
        ;; Get the key of the path.
        ;;(setq tmp-lib-key (nth tmp-index tmp-lib-list))
        ;; Get the value of the path.
        (setq tmp-lib-path (nth (1+ tmp-index) tmp-lib-list))

        ;; Get the first character of the path.
        (setq first-char-from-path (substring tmp-lib-path 0 1))

        (cond (;; If the first character is not '.', then we use
               ;; absolute path instead of version control relative path.
               (string= first-char-from-path ".")
               (progn
                 ;; Modefied path to version control path.
                 (setq tmp-lib-path (concat (cdr (project-current)) tmp-lib-path))))
              ;; Swap #SDK_PATH# to valid Java SDK path, if contain.
              ((organize-imports-java-contain-string organize-imports-java-inc-keyword
                                                     tmp-lib-path)
               (progn
                 (setq tmp-lib-path (s-replace organize-imports-java-inc-keyword
                                               organize-imports-java-java-sdk-path
                                               tmp-lib-path)))))

        ;; Read the jar/lib to temporary buffer.
        (setq tmp-lib-buffer (organize-imports-java-get-string-from-file tmp-lib-path))

        ;; Get all the library path strings by using
        ;; regular expression.
        (setq tmp-class-list (organize-imports-java-re-seq
                              organize-imports-java-serach-regexp
                              tmp-lib-buffer))

        ;; Add the paths to the list.
        (push tmp-class-list all-lib-path-list)

        ;; Add up index.
        (setq tmp-index (+ tmp-index 2))))
    all-lib-path-list))

;;;###autoload
(defun organize-imports-java-erase-cache-file ()
  "Clean all the buffer in the cache files."
  (interactive)
  (organize-imports-java-erase-file organize-imports-java-path-jar-lib-cache-file)
  (organize-imports-java-erase-file organize-imports-java-path-local-source-cache-file))

(defun organize-imports-java-erase-file (in-filename)
  "Erase a file.
IN-FILENAME : filename relative to project root."
  (write-region ""  ;; Start, insert nothing here in order to clean it.
                nil  ;; End
                ;; File name (concatenate full path)
                (concat (cdr (project-current))
                        in-filename)  ;; Cache filename.
                ;; Overwrite?
                nil))

;;;###autoload
(defun organize-imports-java-reload-paths ()
  "Reload the Java include paths and local source path once."
  (interactive)
  ;; Write Java path to according cache file, both `jar/lib' and
  ;; `local-source' cache.
  (organize-imports-java-reload-jar-lib-paths)
  (organize-imports-java-reload-local-source-paths))

;;;###autoload
(defun organize-imports-java-reload-jar-lib-paths ()
  "Reload external Java paths.
For .jar files."
  (interactive)
  ;; Make sure the .ini/.properties file exists before making the cache file.
  (if (file-exists-p (concat (cdr (project-current))
                             organize-imports-java-lib-inc-file))
      (progn
        ;; Import all libs/jars.
        (setq organize-imports-java-path-buffer-jar-lib
              (organize-imports-java-unzip-lib))

        ;; Flatten it.
        (setq organize-imports-java-path-buffer-jar-lib
              (organize-imports-java-flatten organize-imports-java-path-buffer-jar-lib))

        ;; Remove duplicates value from list.
        (setq organize-imports-java-path-buffer-jar-lib
              (delete-dups organize-imports-java-path-buffer-jar-lib))

        (organize-imports-java-erase-file organize-imports-java-path-jar-lib-cache-file)
        (organize-imports-java-load-path-and-write-cache organize-imports-java-path-buffer-jar-lib
                                                         organize-imports-java-path-jar-lib-cache-file))
    (error "%s"
           (propertize (concat "Include jar path file missing : "
                               (cdr (project-current))
                               organize-imports-java-lib-inc-file)
                       'face
                       '(:foreground "cyan")))))

;;;###autoload
(defun organize-imports-java-reload-local-source-paths ()
  "Reload internal Java paths.
Usually Java files under project root 'src' directory."
  (interactive)
  ;; Import local source files. File that isn't a jar/lib file.
  (setq organize-imports-java-path-buffer-local-source
        (organize-imports-java-get-local-source))

  ;; Flatten it.
  (setq organize-imports-java-path-buffer-local-source
        (organize-imports-java-flatten organize-imports-java-path-buffer-local-source))

  ;; Remove duplicates value from list.
  (setq organize-imports-java-path-buffer-local-source
        (delete-dups organize-imports-java-path-buffer-local-source))

  (organize-imports-java-erase-file organize-imports-java-path-local-source-cache-file)
  (organize-imports-java-load-path-and-write-cache organize-imports-java-path-buffer-local-source
                                                   organize-imports-java-path-local-source-cache-file))

(defun organize-imports-java-load-path-and-write-cache (path-list in-filename)
  "Load the path and write the cache file.
PATH-LIST : content paths we will write to IN-FILENAME.
IN-FILENAME : name of the cache file."
  (let ((first-char-from-path "")
        (tmp-write-to-file-content-buffer ""))
    ;; Write into file so we don't need to do it every times.
    (dolist (tmp-path path-list)
      ;; Get the first character of the path.
      (setq first-char-from-path (substring tmp-path 0 1))

      (when (and (not (equal (upcase first-char-from-path) first-char-from-path))
                 (not (organize-imports-java-is-contain-list-string organize-imports-java-non-src-list
                                                                    tmp-path))
                 (not (organize-imports-java-is-contain-list-string organize-imports-java-non-class-list
                                                                    tmp-path))
                 (not (organize-imports-java-is-digit-string first-char-from-path))
                 (not (string= first-char-from-path "-"))
                 (not (string= first-char-from-path ".")))
        ;; Swap `/' to `.'.
        (setq tmp-path (s-replace "/" "." tmp-path))

        ;; Remove `.class'.
        (setq tmp-path (s-replace ".class" "" tmp-path))

        ;; Add line break at the end.
        (setq tmp-path (concat tmp-path "\n"))

        ;; add to file content buffer.
        (setq tmp-write-to-file-content-buffer (concat tmp-path tmp-write-to-file-content-buffer))))

    ;; Write to file all at once.
    (write-region tmp-write-to-file-content-buffer  ;; Start
                  nil  ;; End
                  ;; File name (concatenate full path)
                  (concat (cdr (project-current))
                          in-filename)  ;; Cache filename.
                  ;; Overwrite?
                  t)))

(defun organize-imports-java-get-current-point-face-p ()
  "Get current point's type face as string."
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))

(defun organize-imports-java-current-point-face-list-p (face-name-list)
  "Is the current face name same as one of the pass in string in the list?
FACE-NAME-LIST : list of face name in string."
  (cl-some #'(lambda (face-name) (string= (organize-imports-java-get-current-point-face-p) face-name)) face-name-list))

(defun organize-imports-java-get-type-face-keywords-by-face-name (face-name-list)
  "Get all the type keywords in current buffer.
FACE-NAME-LIST : face name to search."

  (let ((tmp-keyword-list '()))
    (save-excursion
      ;; Goto the end of the buffer.
      (goto-char (point-max))

      (while (< (point-min) (point))
        (backward-word 1)
        (when (organize-imports-java-current-point-face-list-p face-name-list)
          (push (thing-at-point 'word) tmp-keyword-list))))
    ;; Remove duplicate
    (setq tmp-keyword-list (delete-dups tmp-keyword-list))
    tmp-keyword-list))

(defun organize-imports-java-insert-import-lib (tmp-one-path)
  "Insert the import code line here.  Also design it here.
Argument TMP-ONE-PATH Temporary passing in path, use to insert import string/code."
  (insert "import ")
  (insert tmp-one-path)
  (insert ";\n"))

;;;###autoload
(defun organize-imports-java-kill-whole-line ()
  "Deletes a line, but does not put it in the `kill-ring'."
  (interactive)
  (let ((kill-ring))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (progn
        (move-beginning-of-line 1)
        (kill-line 1)))))

;;;###autoload
(defun organize-imports-java-clear-all-imports ()
  "Clear all imports in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (< (point-min) (point))
      (beginning-of-line)
      (when (string= (thing-at-point 'word) "import")
        (organize-imports-java-kill-whole-line))
      (forward-line -1))))

(defun organize-imports-java-is-digit-string (c)
  "Check if C is a digit."
  (string-match-p "\^[0-9]'" c))

;;;###autoload
(defun organize-imports-java-same-class-ask (type)
  "Ask the user which path should you import?
TYPE : path string will be store at."
  (interactive
   (list (completing-read
          "Choose select class: " organize-imports-java-same-class-name-list)))
  (push type organize-imports-java-pre-insert-path-list))


(defun organize-imports-java-do-imports-one-cache (in-cache)
  "Do the import by cache file.
IN-CACHE : cache file name relative to project root folder."
  (save-excursion
    (let ((tmp-config-fullpath (concat (cdr (project-current))
                                       in-cache)))
      ;; If the file does not exists, load the Java path once.
      ;; Get this plugin ready to use.
      (when (not (file-exists-p tmp-config-fullpath))
        (cond ((string= in-cache organize-imports-java-path-jar-lib-cache-file)
               (progn
                 (organize-imports-java-reload-jar-lib-paths)))
              ((string= in-cache organize-imports-java-path-local-source-cache-file)
               (progn
                 (organize-imports-java-reload-local-source-paths)))
              (t
               (progn
                 ;; Reload all the cache file as default. Even though this
                 ;; should not happens.
                 (organize-imports-java-reload-paths)))))

      (let ((tmp-type-keyword-list (organize-imports-java-get-type-face-keywords-by-face-name
                                    organize-imports-java-font-lock-type-faces))
            ;; Read file to buffer.
            (tmp-path-buffer (organize-imports-java-get-string-from-file tmp-config-fullpath))
            (tmp-path-list '())
            ;; Loop the data once and split into alphabetic order.
            ;; List-Len = (Alphabet Id - 1).
            ;; 0 = A, 1 = B, 2 = C, etc.
            (alphabet-list-first '()))

        ;; Make the path buffer back to list.
        ;;
        ;; Why I use the word 'back'? Because when we make our
        ;; list, we made it from one chunk of buffer/string.
        ;; And now we split the string back to list again.
        (setq tmp-path-list (split-string tmp-path-buffer "\n"))

        ;; Reset alphabetic list.
        (setq alphabet-list-first '())

        ;; Make 26 list for all A-Z alphabet letters.
        (let ((count 0))
          (while (< count 26)
            (push '() alphabet-list-first)
            (setq count (1+ count))))

        ;; After Loading data to list.
        ;; Store data/paths to alphabetic order depends
        ;; on the class.
        (dolist (tmp-path tmp-path-list)
          (let ((tmp-split-path-list '())
                ;; Usually the class name data.
                (tmp-last-element "")
                (first-char-from-path nil)
                (alphabet-id -1)
                (alphabet-list nil))

            ;; split the string into list
            (setq tmp-split-path-list (split-string tmp-path "\\."))

            ;; the last element is usually the class name.
            (setq tmp-last-element (nth (1- (length tmp-split-path-list)) tmp-split-path-list))

            (when (and (not (string= tmp-last-element ""))
                       ;; Exclude current buffer file name because own file
                       ;; cannot be inserted. Is illegal in Java programming.
                       (not (string= tmp-last-element
                                     ;; Filename without extension.
                                     (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))
              ;; Get the first character from class name, in order
              ;; to sort in alphabetic order.
              (setq first-char-from-path (substring tmp-last-element 0 1))

              ;; get the alphabet id, which is the same as array id.
              (setq alphabet-id (organize-imports-java-get-alphabet-id first-char-from-path))

              (when (not (= alphabet-id -1))
                ;; get the current alphabet list.
                (setf alphabet-list (nth alphabet-id alphabet-list-first))

                ;; First push it class name.
                (push tmp-last-element alphabet-list)
                ;; Then push it full path.
                (push tmp-path alphabet-list)

                ;; set to the two dimensional array.
                (setf (nth alphabet-id alphabet-list-first) alphabet-list)))))

        ;; ------------------------------------------------------------------------------
        ;; Start searching class name.
        (let ((tmp-same-class-name-list-length -1))
          (dolist (tmp-type-class-keyword tmp-type-keyword-list)
            ;; Exclude the general data type. (String, Integer, etc.)
            (when (not (organize-imports-java-is-in-list-string organize-imports-java-unsearch-class-type
                                                                tmp-type-class-keyword))
              (let (;; Choose one list from `alphabet-list-first',
                    ;; depends on alphabet id.
                    (alphabet-list nil)
                    (alphabet-id -1)
                    (tmp-class-name-first-char ""))

                ;; Get the first character from the class
                ;; name keyword.
                (setq tmp-class-name-first-char (substring tmp-type-class-keyword 0 1))

                ;; Get alphabet id.
                (setq alphabet-id (organize-imports-java-get-alphabet-id tmp-class-name-first-char))

                ;; Get the alphabet list depends on `alphabet id'
                ;; what we just get above a line of code.
                (setq alphabet-list (nth alphabet-id alphabet-list-first))

                (let ((alphabet-list-index 0)
                      (alphabet-list-length (length alphabet-list)))
                  (while (< alphabet-list-index alphabet-list-length)
                    (let ((tmp-class-name "")
                          (tmp-full-path ""))
                      ;; Get class name and full path.
                      (setq tmp-full-path (nth alphabet-list-index alphabet-list))
                      ;; Get full path.
                      (setq tmp-class-name (nth (1+ alphabet-list-index) alphabet-list))

                      ;; Compare the keyword and class name stored.
                      (when (string= tmp-type-class-keyword tmp-class-name)
                        ;; add full path to check same class name list.
                        (push tmp-full-path organize-imports-java-same-class-name-list)))

                    ;; Plus two cuz data structure look like this.
                    ;;   => Class name
                    ;;   => Full path
                    (setq alphabet-list-index (+ alphabet-list-index 2)))))

              ;; ------------------------------------------------------------------------------
              ;; Remove duplicate.
              (setq organize-imports-java-same-class-name-list (delete-dups organize-imports-java-same-class-name-list))

              ;; Get the length of the check same class list.
              (setq tmp-same-class-name-list-length (length organize-imports-java-same-class-name-list))

              (cond ((= tmp-same-class-name-list-length 1)
                     (progn
                       ;; Is exactly 1 result. Just add that to the
                       ;; final pre-insert list.
                       (push (nth 0 organize-imports-java-same-class-name-list) organize-imports-java-pre-insert-path-list)))
                    ((>= tmp-same-class-name-list-length 2)
                     (progn
                       ;; Is is more than 2 results. Meaning we
                       ;; need the user to select which class
                       ;; to import!
                       (call-interactively 'organize-imports-java-same-class-ask))))

              ;; Clean the paths
              (setq organize-imports-java-same-class-name-list '()))))

        ;; ------------------------------------------------------------------------------
        ;; Prepare to insert to current buffer.

        ;; Remove duplicate for pre insert list.
        (setq organize-imports-java-pre-insert-path-list (delete-dups organize-imports-java-pre-insert-path-list))

        ;; Sort in alphabetic order.
        (setq organize-imports-java-pre-insert-path-list (sort organize-imports-java-pre-insert-path-list
                                                               'string<))

        ;; Check package keyword exists.
        (goto-char (point-min))

        ;; Make it under `package' line. Otherwise, will just
        ;; insert at the very top of the file.
        (when (string= (thing-at-point 'word) "package")
          (end-of-line)
          (insert "\n"))

        ;; Insert all import path line.
        (let ((tmp-split-path-list '())
              (tmp-first-element "")
              (tmp-record-first-element ""))
          (dolist (tmp-in-path organize-imports-java-pre-insert-path-list)

            ;; split the path into list by using `.' delimiter.
            (setq tmp-split-path-list (split-string tmp-in-path "\\."))

            ;; the first element is always the class name.
            (setq tmp-first-element (nth 0 tmp-split-path-list))

            (when (not (string= tmp-first-element tmp-record-first-element))
              (insert "\n")

              ;; record it down.
              (setq tmp-record-first-element tmp-first-element))

            (organize-imports-java-insert-import-lib tmp-in-path)))

        ;; Clean pre insert list for next use.
        (setq organize-imports-java-pre-insert-path-list '())

        ;; keep one line.
        (organize-imports-java-keep-one-line-between)))))

;;;###autoload
(defun organize-imports-java-do-imports ()
  "Do the functionalitiies of how organize imports work."
  (interactive)

  ;; Clear all imports before insert new imports.
  (organize-imports-java-clear-all-imports)

  ;; First insert the `local-source' Java path, so once later on the `jar/lib'
  ;; Java path will be ontop of the `local-source' paths.
  (organize-imports-java-do-imports-one-cache organize-imports-java-path-local-source-cache-file)
  ;; Insert the `jar/lib' Java paths.
  (organize-imports-java-do-imports-one-cache organize-imports-java-path-jar-lib-cache-file))


(provide 'organize-imports-java)
;;; organize-imports-java.el ends here
