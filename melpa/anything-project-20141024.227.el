;;; anything-project.el --- finding any resource of a project

;; Copyright (C) 2010-2014 imakado <ken.imakado_at_gmail.com>
;; Copyright (C) 2009 KAYAC Inc

;; Prefix: ap:
;; Author: imakado <ken.imakado_at_gmail.com>
;; Maintainer: imakado
;; Created: :2014-10-23
;; Version: 0.12
;; Package-Version: 20141024.227
;; Keywords: convenience
;; URL: https://github.com/imakado/anything-project
;; Package-Requires: ((imakado "0.12") (anything "1.3.9"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; anything-project.el is pure emacs lisp version of anything-find-project-resources.el.
;; Many ideas are from
;; http://trac.codecheck.in/share/browser/lang/elisp/anything-find-project-resources/trunk/anything-find-project-resources.el
;; and
;; http://blog.jrock.us/articles/eproject.POD

;; drop this file into a directory in your `load-path',
;; and put these lines into your .emacs file.

;; (require 'anything-project)
;; (global-set-key (kbd "C-c C-f") 'anything-project)

;; type C-c C-f to invoke anything with project files.
;; project root directory is automatically detected by anything-project.el

;; clear cache, If `anything-project' function is called with prefix arg (C-u M-x anything-project)

;; you can add new project rule by `ap:add-project' function
;; keywords :look-for, :include-regexp and :exclude-regexp can be regexp or list of regexp
;; below are few samples

;; (ap:add-project
;;  :name 'perl
;;  :look-for '("Makefile.PL" "Build.PL") ; or
;;  :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$") ;or
;;  )

;; (ap:add-project
;;  :name 'perl
;;  :look-for '("Makefile.PL" "Build.PL")
;;  :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
;;  :exclude-regexp "/tmp" ; can be regexp or list of regexp
;;  )

;;; Code:

(require 'anything)
(require 'cl)
(require 'imakado)
(imakado-el-require-version->= 0.10)

(defvar ap:my-projects nil)
(defvar ap:history nil)
(defvar ap:version "0.02")
(defvar ap:global-cache-key "")

(defvar ap:default-directory-filter-regexps nil)

;; almost copied from `anything-find-resource--project-root-files'.
(defvar ap:default-project-root-files
    '("build.xml" "prj.el" ".project" "pom.xml"
      "Makefile" "configure" "Rakefile" "Info.plist"
      "NAnt.build" "xpi" "Makefile.SH" ".git"

      "CVS"
      ))

;; Internal variables
(defvar ap:projects nil)
(defvar ap:root-directory "")
(defvar ap:--cache nil)

(defvar ap:global-look-for nil
  "this variable must be let bindded!!")
(defvar ap:global-include-regexp nil
  "this variable must be let bindded!!")
(defvar ap:global-exclude-regexp nil
  "this variable must be let bindded!!")
(defvar ap:global-grep-extensions nil
  "this variable must be let bindded!!")

(defvar ap:project-files-filters  (list 'identity)
  "list of function filter project-files.
each function is called with one arg(list of project-file)")

(defvar ap:default-exclude-regexps '("~$")
  "this variable always appent to :exclude-regexp.
If you always ignore emacs backup file (that named \"hoge~\"),
please set this to '(\"~$\")")

(defvar ap:debug nil
  "If non-nil, anything-project.el says debug messages")

(defun ap:debug-message (&rest args)
  (when ap:debug
    (apply 'message args)))

(defgroup anything-project nil
  "Manage any projects." :prefix "ap:" :group 'convenience)

(defcustom ap:ignore-directory-regexps nil
  "list of regexp.
If any this value matches directory that you invoke `anything-project' command,
the command doesnt work"
  :group 'anything-project)

(defcustom ap:enable-auto-add-new-file nil
  "If non-nil, The file that you visit is automatically added to project resources"
  :group 'anything-project)

(defun ap:mk-list (a)
  (if (listp a) a (list a)))

(defun ap:apply-filters (filter-fns files)
  (let ((ret files))
    (loop for filter-fn in ap:project-files-filters
          do (setq ret (funcall filter-fn ret))
          finally return ret)))

(defun* ap:add-project (&key name look-for (include-regexp ".*") (exclude-regexp nil) (exclude-directory-regexp nil) (grep-extensions nil))
  (assert (not (null look-for)))
  (assert (and (not (null name))
               (symbolp name)))
  (assert (or (null grep-extensions)
              (listp grep-extensions)))
  (setq ap:projects (assq-delete-all name ap:projects))
  (add-to-list 'ap:projects
               `(,name . ((,:look-for . ,(ap:mk-list look-for))
                          (,:include-regexp . ,(ap:mk-list include-regexp))
                          (,:exclude-regexp . ,(ap:mk-list exclude-regexp))
                          (,:grep-extensions . ,grep-extensions)))))

(defun ap:get-project-data (name type)
  (let ((sym (intern (concat "ap:global-"
                             (replace-regexp-in-string "^:" "" (symbol-name type))))))
    (cond
     ((and (boundp sym)
           (not (null (symbol-value sym))))
      (symbol-value sym))
     (t
       (let ((alist (assoc-default name ap:projects)))
         (when alist
           (assoc-default type alist)))))))


(defun ap:get-project-keys ()
  (let* ((keys (loop for alist in ap:projects
                     collect (first alist)))
         (keys (delete 'default keys)))
    (add-to-list 'keys 'default t)))


(defun ap:root-directory-p (root-files-or-fn files)
  (cond
   ((functionp (car-safe root-files-or-fn))
    (ignore-errors
      (funcall (car root-files-or-fn) files)))
   (t
    (some
     (lambda (file)
       (find file
             files
             :test 'string=))
     root-files-or-fn))))

(defun ap:current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defmacro ap:with-current-dir (dir &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(flet ((ap:current-directory ()
           (let ((dir ,dir))
             (cond
              ((file-directory-p dir)
               (if (string-match "/$" dir)
                   dir
                 (concat dir "/")))
              (t
               (file-name-directory dir))))))
     (progn ,@body)))

(defun* ap:root-detector (current-dir &optional (project-key :default))
  (let* ((current-dir (expand-file-name current-dir))
         (files-or-fn (ap:get-project-data project-key :look-for)))
    (ap:root-directory-p files-or-fn (directory-files current-dir))))

(defvar ap:get-root-directory-limit 10)
(defun ap:get-root-directory-aux (key)
  (let ((cur-dir (ap:current-directory)))
    (ignore-errors
      (loop with count = 0
            until (ap:root-detector cur-dir key)
            if (= count ap:get-root-directory-limit)
            do (return nil)
            else
            do (progn (incf count)
                      (setq cur-dir (expand-file-name (concat cur-dir "../"))))
            finally return cur-dir))))


(defun ap:get-root-directory ()
  (let ((project-keys (ap:get-project-keys)))
    (loop for key in project-keys
          for ret = (values (ap:get-root-directory-aux key) key)
          until (car ret)
          finally return ret)))

;;; add-to-history
(defadvice ap:get-root-directory (after add-to-history activate)
  (ignore-errors
    (destructuring-bind (root-dir key) ad-return-value
      (when (and root-dir key (stringp root-dir))
        (add-to-list 'ap:history root-dir)))))

(defsubst ap:any-match (regexp-or-regexps file-name)
  (when regexp-or-regexps
    (let ((regexps (if (listp regexp-or-regexps) regexp-or-regexps (list regexp-or-regexps))))
      (some
       (lambda (re)
         (string-match re file-name))
       regexps))))

(defvar ap:directory-files-recursively-known-directories nil)
(defun* ap:directory-files-recursively 
    (regexp &optional 
            (directory (imakado-normalize-file-name default-directory))
            type (dir-filter-regexp nil) no-recursive)
  (when current-prefix-arg
    (setq ap:directory-files-recursively-known-directories nil))
  (let ((directory (imakado-normalize-file-name directory)))
    (when (or (equal ap:root-directory directory)
              (not (member directory ap:directory-files-recursively-known-directories)))
      (let* ((predfunc (case type
                         (dir 'file-directory-p)
                         (file 'file-regular-p)
                         (otherwise 'identity)))
             (files (directory-files directory t "^[^.]" t))
             (files (mapcar 'ap:follow-symlink files))
             (files (remove-if (lambda (s) (string-match (rx bol (repeat 1 2 ".") eol) s)) files)))
        (push directory ap:directory-files-recursively-known-directories)
        (loop for file in files
              when (and (funcall predfunc file)
                        (ap:any-match regexp (file-name-nondirectory file))
                        (not (ap:any-match (ap:mk-list dir-filter-regexp) file)))
              collect file into ret
              when (and (file-directory-p file)
                        (not (ap:any-match dir-filter-regexp file))
                        (not no-recursive))
              nconc (ap:directory-files-recursively regexp file type dir-filter-regexp) into ret
              finally return ret)))))

(defun ap:follow-symlink (file)
  (imakado-acond ((imakado-get-symlink-target-recursively file)
          (expand-file-name it))
         (t (expand-file-name file))))

(defun ap:truncate-file-name (root-dir files)
  (let* ((root-dir (replace-regexp-in-string "/$" "" root-dir))
         (re (concat "^" root-dir "\\(.*\\)$"))
         (files (ap:mk-list files)))
    (let* ((truncate (lambda (f)
                       (if (string-match re f)
                         (match-string-no-properties 1 f)
                         f))))
      (mapcar truncate files))))

(defun* ap:get-project-files (&optional clear-cache &key (full-path nil))
  (let* ((values (ap:get-root-directory))
         (root-dir (first values))
         (key (second values)))
    (when (and root-dir key)
      ;; clear cache if command invoked with prefix(C-u).
      (when clear-cache
        (setq ap:--cache
              (delete-if (lambda (ls) (equal (concat root-dir ap:global-cache-key) ls))
                         ap:--cache
                         :key 'car))
        (setq ap:directory-files-recursively-known-directories nil))
      (let ((files (ap:get-project-files-aux root-dir key)))
        (cond
         (full-path (mapcar 'ap:expand-file files))
         (t files))))))

(defun ap:update-project-files ()
  (interactive)
  (let ((old-los (ap:get-project-files nil)))
    (let ((los (ap:get-project-files t)))
      (message "Update Done. %s -> %s files in Project"
               (length old-los)
               (length los)))))


(defun ap:get-project-files-aux (root-dir key)
  (lexical-let ((root-dir root-dir)
                (key key))
    (setq ap:root-directory root-dir)
    (ap:cache-get-or-set
     root-dir
     (lambda ()
       (message "getting project files...")
       (let* ((include-regexp (or ap:global-include-regexp (ap:get-project-data key :include-regexp)))
              (exclude-regexp (or ap:global-exclude-regexp (ap:get-project-data key :exclude-regexp)))
              (exclude-regexp (append (ap:mk-list ap:default-exclude-regexps) (ap:mk-list exclude-regexp))))
         (let* ((files (ap:directory-files-recursively include-regexp root-dir 'identity exclude-regexp))
                (files (ap:apply-filters ap:project-files-filters files))
                (files (ap:truncate-file-name root-dir files)))
           (ap:debug-message "[ap:get-project-files-aux] files: %S"
                             files)
           files))))))


(defun ap:cache-get-or-set (root-dir get-files-fn)
  (let ((cache-key (concat root-dir ap:global-cache-key)))
    (let ((cache (assoc-default cache-key ap:--cache)))
      (if cache
          cache                         ; cache hit!!
        (let ((files (funcall get-files-fn)))
          (when files
            (add-to-list 'ap:--cache
                         `(,cache-key . ,files))
            files))))))

(defun* ap:expand-file (file &optional (root-directory ap:root-directory))
  (let ((root-dir (replace-regexp-in-string "/$" "" ap:root-directory)))
    (concat root-dir file)))


(if (fboundp 'anything-run-after-quit)
    (defalias 'ap:anything-run-after-quit 'anything-run-after-quit)
  (defun ap:anything-run-after-quit (function &rest args)
    "Perform an action after quitting `anything'.
The action is to call FUNCTION with arguments ARGS."
    (setq anything-quit t)
    (apply 'run-with-idle-timer 0 nil function args)
    (anything-exit-minibuffer)))

(defun ap:project-files-init-msg ()
  (message "Buffer is not project file. buffer: %s" (ap:current-directory)))

;;;; XXX what should i do if current directory is ignored directory.
(defun* ap:project-files-init (&optional cache-clear files)
  (cond
   ((ap:any-match ap:ignore-directory-regexps (ap:current-directory))
    (ap:anything-run-after-quit
     'ap:project-files-init-msg))
   (t
    (let ((files (or files (ap:get-project-files cache-clear)))
          (cands-buf (anything-candidate-buffer 'local)))
      (cond
       (files
        (with-current-buffer cands-buf
          (insert (mapconcat 'identity files "\n"))))
       (t
        (ap:anything-run-after-quit
         'ap:project-files-init-msg)))))))

;;;; ap:enable-auto-add-new-file
(defun ap:add-this-file-to-project-files ()
  (ignore-errors
    (when ap:enable-auto-add-new-file
      (let* ((values (ap:get-root-directory))
             (root-dir (first values))
             (key (second values)))
        (when root-dir
          (let ((path (and buffer-file-truename
                           (expand-file-name buffer-file-truename))))
            (when (and path
                       (file-exists-p path))
              (let ((path (first (ap:truncate-file-name root-dir path))))
                (let* ((cache-key (concat root-dir ap:global-cache-key))
                       (cache (assoc cache-key ap:--cache)))
                  (when cache
                    (pushnew path (cdr cache) :test 'equal)
                    (ap:debug-message "[ap:add-this-file-to-project-files] newfile:%s \ncache: %S"
                                      path (cdr cache))))))))))))
(add-hook 'find-file-hook
          'ap:add-this-file-to-project-files)
(add-hook 'after-save-hook
          'ap:add-this-file-to-project-files)

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Project Grep
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun anything-project-grep ()
  (interactive)
  (cond
   ((require 'anything-grep  nil t)
    (ap:do-project-grep))
   (t
    (message "`anything-grep' is not installed. this command requires `anything-grep'"))))

(defun ap:do-project-grep ()
  (destructuring-bind (root-dir key) (ap:get-root-directory)
    (when (and root-dir key)
      (ap:do-project-grep-aux root-dir key))))
(defun ap:do-project-grep-aux (root-dir key)
  (let* ((query (read-string "Grep query: " (or (thing-at-point 'symbol) "")))
         (command (ap:build-grep-command key)))
    (anything-grep-base
     (list
      (agrep-source (format (agrep-preprocess-command command)
                            (shell-quote-argument query))
                    root-dir)))
    ))

(defun ap:build-grep-command (key)
  (let ((grep-extensions (ap:get-grep-extensions key))
        (ack-command (ap:get-ack-command))
        (xargs-command (ap:get-xargs-command))
        (egrep-command (ap:get-egrep-command)))
    (concat
     ack-command " -afG " grep-extensions
     " | "
     xargs-command
     " "
     egrep-command " -Hin "
     "%s")))

(defun ap:get-xargs-command ()
  (or (executable-find "xargs")
           (error "can't find 'xargs' command in PATH!!")))

(defun ap:get-egrep-command ()
  (or (executable-find "egrep")
           (error "can't find 'egrep' command in PATH!!")))

(defun ap:get-ack-command ()
  (or (executable-find "ack")
      (executable-find "ack-grep")
      (error "can't find 'ack' command in PATH!!"))) ; debian

;; "ack -afG '(m|t|tt2|tt|yaml|yml|p[lm]|js|html|xs)$' | xargs egrep -Hin %s"
(defun ap:get-grep-extensions (key)
  (let ((list-of-grep-extention
         (cond
          ((or ap:global-grep-extensions
               (ap:get-project-data key :grep-extensions)))
          (t
           (ap:get-project-data key :include-regexp)))))
    ;; build, '(m|t|tt2|tt|yaml|yml|p[lm]|js|html|xs)$'
    (concat
     "'("
     (mapconcat 'identity list-of-grep-extention "|")
     ")$'")))

;;;; anything-project-moccur-grep-find
(defmacro ap:with-require-color-moccur (&rest body)
  (declare (indent 0)
           (debug (body)))
  `(cond
    ((require 'color-moccur  nil t)
     (progn ,@body))
    (t
     (message "`color-moccur.el' is not installed."))))

(defvar anything-project-moccur-grep-find-enable-extension-filter  t)
(defun anything-project-moccur-grep-find ()
  (interactive)
  (ap:with-require-color-moccur
    (ap:do-project-moccur-grep-find)))

(defun* ap:do-project-moccur-grep-find (&optional (exclude-regexps nil))
  (destructuring-bind (root-dir key) (ap:get-root-directory)
    (when (and root-dir key)
      (let* ((query (read-string "Grep query: " (or (thing-at-point 'symbol) ""))))
        (assert (and (stringp query) (not (string= query ""))))
        (moccur-search-files (ap:do-project-moccur-grep-find-make-query query) 
                             (ap:files-for-moccur-grep-find
                              (ap:do-project-moccur-grep-find-extension-filter-regexp
                               query)))))))

(defun ap:do-project-moccur-grep-find-make-query (query)
  (if anything-project-moccur-grep-find-enable-extension-filter
      (imakado-aif (imakado-=~ (rx "." (+ not-newline) eol) query
               ($sub ""))
          it
        query)
    query))


(defun ap:do-project-moccur-grep-find-extension-filter-regexp (query)
  (when anything-project-moccur-grep-find-enable-extension-filter
    (imakado-when-let (q (imakado-aand (split-string query "\\s +" t)
                           (and (> (length it) 1)
                                (last it))
                           (first it)
                           (and (string-match "\\..+" it)
                                it)))
      (concat (regexp-quote q) "$"))))

(defun* ap:files-for-moccur-grep-find (&optional (exclude-regexps nil))
  (destructuring-bind (root-dir key) (ap:get-root-directory)
    (when (and root-dir key)
      (let* ((project-files (ap:get-project-files nil :full-path t)))
        (ap:files-for-moccur-grep-find-filter project-files exclude-regexps)))))

(defun* ap:files-for-moccur-grep-find-filter (project-files &optional (exclude-regexps nil))
  (let* ((res (remove-if-not 'file-regular-p project-files))
         (res (remove-if (lambda (file)
                           (ap:any-match dmoccur-exclusion-mask file))
                         res)))
    (if exclude-regexps
        (remove-if-not (lambda (file)
                     (ap:any-match exclude-regexps file))
                   res)
      res)))

(defun* ap:generate-project-moccur-command
    (&key
     (projects nil)
     (use-extension-filter nil))
  (lexical-let
      ((projects projects)
       (use-extension-filter use-extension-filter))
    (lambda ()
      (interactive)
      (ap:with-require-color-moccur
        (let ((query (read-string "Grep query: " (or (thing-at-point 'symbol) ""))))
          (cond
           ((and use-extension-filter
                 (ap:project-moccur-command-use-extension-filter-p query))
            (ap:do-project-moccur-command-use-extension-filter
             query projects))
           (t
            (moccur-search-files query (ap:project-files-for-moccur-grep-find
                                        projects)))))))))

(defun ap:do-project-moccur-command-use-extension-filter (query projects)
  (let* ((queries (moccur-split-string query))
         (filter-re (concat (regexp-quote (car (last queries)))
                            "$"))
         (query (mapconcat 'identity
                           (butlast queries)
                           " "))
         (files (ap:project-files-for-moccur-grep-find
                 projects))
         (files (remove-if-not
                 (lambda (s)
                   (string-match filter-re s))
                 files)))
    (moccur-search-files query
                         files)))

(defun ap:project-files-for-moccur-grep-find (project-files)
  (let ((project-files (ap:mk-list project-files)))
    (loop for file in project-files
          for file = (expand-file-name file)
          append
          (ap:with-current-dir file
            (ap:files-for-moccur-grep-find)))))

(defun ap:project-moccur-command-use-extension-filter-p (query)
  (let ((words (moccur-split-string query)))
    (when (and words
               (>= (length words) 2))
      (string-match "^\\..+" (or (car (last words)) "")))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "ap:project-moccur-command-use-extension-filter-p")
      (expect (true)
        (let ((s "hoge .pm"))
          (ap:project-moccur-command-use-extension-filter-p s)))
      (expect nil
        (let ((s ".pm"))
          (ap:project-moccur-command-use-extension-filter-p s)))
      (expect nil
        (let ((s "huga"))
          (ap:project-moccur-command-use-extension-filter-p s)))
      )))






;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; copied from anything-config.el
(defun ap:anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))


(defvar anything-c-source-project
  '((name . "Project Files")
    (init . (lambda ()
              (ap:project-files-init (if (boundp 'cache-clear) ; KLUDGE!!
                                         cache-clear
                                       current-prefix-arg))))
    (candidates-in-buffer)
    (type . file)
    (action . (("Find file" .
                (lambda (c)
                  (find-file (ap:expand-file c))))
               ("Find file other window" .
                (lambda (c)
                  (find-file-other-window (ap:expand-file c))))
               ("Find file other frame" .
                (lambda (c)
                  (find-file-other-frame (ap:expand-file c))))
               ("Open dired in file's directory" .
                (lambda (c)
                  (ap:anything-c-open-dired (ap:expand-file c))))))
    ))

(defvar anything-c-source-my-projects
  `((name . "Projects")
    (candidates . (lambda () ap:my-projects))
    (action . (("anything project" .
                (lambda (c)
                  (flet ((buffer-file-name () nil))
                    (let ((default-directory c))
                      (call-interactively 'anything-project)))
                  ))))
    ))

(defvar anything-c-source-projects-history
  `((name . "Projects history")
    (candidates . (lambda () ap:history))
    (action . (("anything project" .
                (lambda (c)
                  (flet ((buffer-file-name () nil))
                    (let ((default-directory c))
                      (call-interactively 'anything-project)))
                  ))))
    ))

(defun anything-my-project ()
  (interactive)
  (anything '(anything-c-source-my-projects
              anything-c-source-projects-history)))

;; copied from anything-config.el :)
(defun ap:shorten-home-path (files)
  "Replaces /home/user with ~."
  (mapcar (lambda (file)
            (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                  (getenv "HOME"))))
              (if (string-match home file)
                  (cons (replace-match "~" nil nil file) file)
                file)))
          files))

(defun anything-project (&optional cache-clear)
  (interactive "P")
  (anything 'anything-c-source-project
            nil "Project files: "))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Default Project (Samples)
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; utils
(defun ap:all-files-exist (project-files files)
  (subsetp project-files
           files
           :test 'string=))

(ap:add-project
 :name 'perl
 :look-for '("Makefile.PL" "Build.PL")
 :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$") 
)

(ap:add-project
 :name 'default
 :look-for ap:default-project-root-files
 )

;;; PHP symfony
(ap:add-project
 :name 'symfony
 :look-for 'ap:symfony-root-detector
 :grep-extensions '("\\.php"))

(defun ap:symfony-root-detector (files)
  (let ((symfony-files '("apps" "web" "lib")))
    (every
     (lambda (file)
       (find file
             files
             :test 'string=))
     symfony-files)))

;;; PHP cake
(ap:add-project
 :name 'cake
 :look-for 'ap:cake-root-detector
 :grep-extensions '("\\.php"))

(defun ap:cake-root-detector (files)
  (let ((cur-dir (ap:current-directory)))
    (and cur-dir
         (file-readable-p (concat cur-dir "config/core.php")))))



(defun ap:abs->relatives (los)
  (assert (listp los))
  (mapcar 'file-relative-name los))

(defun ap:path->filenames (los)
  (mapcar 'file-name-nondirectory los))

;;; Python django
(ap:add-project
 :name 'django
 :look-for 'ap:django-root-detector
 :grep-extensions '("\\.py" "\\.ya?ml" "\\.html")
 :exclude-regexp '("\\.pyc$"))

(defun ap:django-root-detector (files)
  (let ((django-files '("manage.py" "settings.py" "urls.py")))
    (every
     (lambda (django-file)
       (find django-file files :test 'string=))
     django-files)))

;;;; Test
;; Prefix: ap-t:
(defun ap-t:directory-separator ()
  (substring (file-name-as-directory ".") -1))

(defun ap-t:path-to (path &rest paths)
  (assert (or (null paths)
              (and (listp paths)
                   (stringp (car-safe paths)))))
  (assert (stringp path))
  (let ((paths (append (list path) paths)))
    (concat (file-name-directory (locate-library "anything-project"))
            (mapconcat 'identity paths (ap-t:directory-separator)))))

(defun ap-t:directory-type ()
  (second (ap:get-root-directory)))

(defun ap-t:get-project-files-with-no-cache ()
  (ap:get-project-files t))

;; (ap-t:to-bool 'x) => t
;; (ap-t:to-bool 1) => t
;; (ap-t:to-bool nil) => nil
(defun ap-t:to-bool (obj)
  (not (not obj)))


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "---------------- basic ----------------")
      (desc "---------------- :exclude-regexp ----------------")
      ;; (expect '("/test-project-file" "/file2" "/file1")
      ;;   ;; define test project
      ;;   (ap:add-project
      ;;    :name 'test-project
      ;;    :look-for '("test-project-file")
      ;;    :exclude-regexp '("/exclude"))

      ;;   (ap:with-current-dir (ap-t:path-to "t" "test-project1" "file1")
      ;;     (ap-t:get-project-files-with-no-cache)))

      (desc "ap:directory-files-recursively")
      (desc "recursively projectX")
      (expect '("Makefile" "file2" "file1" "deep-file2" "deep-file1")
        (ap:path->filenames
         (ap:directory-files-recursively ".*" (ap-t:path-to "t" "projectX") 'file)))

      (desc "non recursively")
      (expect t
        (let ((file-names (ap:path->filenames
                           (ap:directory-files-recursively ".*" (ap-t:path-to "t" "projectX") nil nil t))))
          (and (not (member "deep-file2" file-names))
               (not (member "deep-file1" file-names)))))


      (desc "---------------- project-data ----------------")
      (desc "---------------- ap:get-project-data ----------------")
      (expect '("/exclude")
        (let ((ap:projects '((test-project
                              (:exclude-regexp "/exclude")))))
          (ap:get-project-data 'test-project :exclude-regexp)))

      ;; ap:global-exclude-regexp overide :exclude-regexp
      (expect "global-exclude-regexp"
        (let ((ap:global-exclude-regexp "global-exclude-regexp")
              (ap:projects '((test-project
                              (:exclude-regexp "/exclude")))))
          (ap:get-project-data 'test-project :exclude-regexp)))



      (desc "---------------- ap:get-project-keys ----------------")
      ;; 'default project should appear at last.
      (expect '(foo-project bar-project baz-project default)
        (let ((ap:projects '((foo-project ())
                             (default ()) ;; default is special.
                             (bar-project ())
                             (baz-project ()))))
          (ap:get-project-keys)))




      (desc "---------------- default-exclude-regexp ----------------")
      (expect t
        (let ((ap:default-exclude-regexps nil))
          (ap-t:to-bool
           (member "file1~"
                   (ap:path->filenames
                    (ap:with-current-dir (ap-t:path-to "t" "default-exclude-regexp-project" "file1")
                      (ap-t:get-project-files-with-no-cache)))))))
      (expect nil
        (member "file1~"
                (let ((ap:default-exclude-regexps '("~$"))
                      (ap:--cache nil))
                  (ap:path->filenames
                   (ap:with-current-dir (ap-t:path-to "t" "default-exclude-regexp-project" "file1")
                     (ap-t:get-project-files-with-no-cache))))))


      (desc "---------------- ap:get-root-directory ----------------")
      ;; deep directory
      ;; find more upper directory
      (expect t
        (ap-t:to-bool
         (let ((ap:get-root-directory-limit 10))
           (ap:with-current-dir (ap-t:path-to "t" "deep-project" "1/2/3/4/5/6/7/8/9/10/11")
             (ap:get-root-directory)))))

      (expect nil
        (ap-t:to-bool
         (let ((ap:get-root-directory-limit 5))
           (ap:with-current-dir (ap-t:path-to "t" "deep-project" "1/2/3/4/5/6/7/8/9/10/11")
             (first (ap:get-root-directory))))))


      (desc "---------------- ap:expand-file ----------------")
      (expect '("/file1" "/file2")
        (ap:truncate-file-name
         (ap-t:path-to "t" "projectX")
         (list
          (ap-t:path-to "t" "projectX" "file1")
          (ap-t:path-to "t" "projectX" "file2"))))

      (expect '("t/projectX/file1" "t/projectX/file2")
        (ap:abs->relatives
         (let ((ap:root-directory
                (ap-t:path-to "t" "projectX"))
               (files (ap:truncate-file-name
                       (ap-t:path-to "t" "projectX")
                       (list
                        (ap-t:path-to "t" "projectX" "file1")
                        (ap-t:path-to "t" "projectX" "file2")))))
           (mapcar (lambda (s) (ap:expand-file s)) files))))


      (desc "---------------- directory type ----------------")
      (desc "default")
      (expect 'default
        (ap:with-current-dir (ap-t:path-to "t" "projectX")
          (ap-t:directory-type)))

      (desc "-- perl --")
      (desc "Makefile.PL")
      (expect 'perl
        (ap:with-current-dir (ap-t:path-to "t" "perl-project1" "file1")
          (ap-t:directory-type)))

      (desc "Build.PL")
      (expect 'perl
        (ap:with-current-dir (ap-t:path-to "t" "perl-project2" "file1")
          (ap-t:directory-type)))

      (desc "---------------- cake ----------------")
      (expect 'cake
        (ap:with-current-dir (ap-t:path-to "t" "cake-project" "index.php")
          (ap-t:directory-type)
          ))

      (desc "---------------- symfony ----------------")
      (expect 'symfony
        (ap:with-current-dir (ap-t:path-to "t" "symfony-project" "apps")
          (ap-t:directory-type)))


      (desc "---------------- project grep ----------------")
      (desc "---------------- ap:build-grep-command ----------------")
      (expect t
        (ap-t:to-bool
         (progn
           (ap:add-project
            :name 'test-project
            :look-for "dummy"
            :grep-extensions '(".pm" ".pl"))
           (string-match (rx "ack -afG '(.pm|.pl)$' | " (+ not-newline) "xargs " (+ not-newline) "egrep -Hin %s")
                         (ap:build-grep-command 'test-project)))))

      (desc "---------------- django ----------------")
      ;; (expect 'django
      ;;   (ap:with-current-dir (ap-t:path-to "t" "django-project" "app")
      ;;     (ap-t:directory-type)))

      (desc "---------------- didnt work exclude-regexp bug ----------------")
      (desc "---------------- :exclude-regexp "\\.pyc$"  ----------------")
      (expect nil
        (member "manage.pyc"
                (ap:path->filenames
                 (ap:with-current-dir (ap-t:path-to "t" "django-project" "app")
                   (ap-t:get-project-files-with-no-cache)))))

      (desc "---------------- symlink ----------------")
      (expect (true)
        (ap:with-current-dir (ap-t:path-to "t" "symlink-project")
          (ap-t:get-project-files-with-no-cache)))

        ;; XXX


      )))

(provide 'anything-project)

;;; anything-project.el ends here
