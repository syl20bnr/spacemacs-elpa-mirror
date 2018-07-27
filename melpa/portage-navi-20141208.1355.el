;;; portage-navi.el --- portage viewer

;; Copyright (C) 2013, 2014  SAKURAI Masashi

;; Author:  <m.sakurai at kiwanami.net>
;; Keywords: tools, gentoo
;; Package-Version: 20141208.1355
;; Package-Requires: ((concurrent "0.3.1") (ctable "0.1.2"))
;; URL: https://github.com/kiwanami/emacs-portage-navi

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

;;; Code:

(require 'concurrent)
(require 'ctable)
(require 'xml)

;;; Customize variables

(defvar pona:portage-dir "/usr/portage" "pona:portage-dir.")

(defface pona:face-title
  '((((class color) (background light))
     :foreground "MediumBlue" :height 1.5 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "yellow" :weight bold :height 1.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for pona titles at level 1."
  :group 'pona)

(defface pona:face-subtitle
  '((((class color) (background light))
     (:foreground "Gray10" :height 1.2 :inherit variable-pitch))
    (((class color) (background dark))
     (:foreground "Gray90" :height 1.2 :inherit variable-pitch))
    (t :height 1.2 :inherit variable-pitch))
  "Face for pona titles at level 2."
  :group 'pona)

(defface pona:face-item
  '((t :inherit variable-pitch :foreground "DarkSlateBlue"))
  "Face for pona items."
  :group 'pona)

(defface pona:face-toolbar-button
  '((((class color) (background light))
     :foreground "Lightskyblue4" :background "White")
    (((class color) (background dark))
     :foreground "Gray10" :weight bold))
  "Face for button on toolbar" :group 'pona)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defmacro pona:aand (test &rest rest)
  "Anaphoric AND."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(pona:aand ,@rest)) 'it))))

(defmacro pona:aif (test-form then-form &rest else-forms)
  "Anaphoric IF."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'pona:aif 'lisp-indent-function 2)

(defun pona:rt-format (text &rest args)
  "[utility] Format strings with faces. TEXT is format
string. ARGS is a list of cons cell, ([string] . [face name])."
  (apply 'format (propertize text 'face 'pona:face-item)
         (loop for i in args
               if (consp i)
               collect (propertize (car i) 'face (cdr i))
               else
               collect (propertize i 'face 'pona:face-subtitle))))

(defun pona:define-keymap (keymap-list)
  "[internal] Key map definition utility.
KEYMAP-LIST is a source list like ((key . command) ... )."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro (car i)) (car i))
         (cdr i)))
     keymap-list)
    map))

(defun pona:add-keymap (keymap keymap-list &optional prefix)
  (loop with nkeymap = (copy-keymap keymap)
        for i in keymap-list
        do
        (define-key nkeymap
          (if (stringp (car i))
              (read-kbd-macro 
               (if prefix 
                   (replace-regexp-in-string "prefix" prefix (car i))
                 (car i)))
            (car i))
          (cdr i))
        finally return nkeymap))

(defun pona:render-button (title command)
  "[internal] Return a decorated text for the toolbar buttons.
TITLE is a button title.  COMMAND is a interactive command
function called by clicking."
  (pona:render-link (concat "[" title "]") command))

(defun pona:render-link (title command)
  "[internal] Return a decorated text for the link buttons.
TITLE is a button title.  COMMAND is a interactive command
function called by clicking."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] command)
    (define-key keymap (kbd "RET") command)
    (define-key keymap (kbd "C-m") command)
    (propertize title
                'face 'pona:face-item
                'keymap keymap
                'mouse-face 'highlight)))

(defun pona:kill-this-buffer ()
  (interactive)
  (kill-this-buffer))

(defun pona:display-message (buf message)
  "[internal] Display MESSAGE at the BUF during waiting for processing."
  (with-current-buffer buf
    (let ((buffer-read-only))
      (erase-buffer)
      (insert message "\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; portage low-level interfaces

(defvar pona:cache-category-list nil "pona:cache-category-list.")

(defun pona:category-list ()
  "Return a list of top-level categories."
  (unless pona:cache-category-list
    (setq pona:cache-category-list 
          (directory-files pona:portage-dir nil "^[a-z]+-[a-z]+")))
  pona:cache-category-list)

(defun pona:category-package-list-d (category)
  "Return a deferred DOM object of packages in the CATEGORY."
  (lexical-let ((category category))
    (deferred:$
      (deferred:process-buffer "eix" "--xml" "-C" category)
      (deferred:nextc it
        (lambda (buf)
          (let* ((dom (with-current-buffer buf
                        (xml-parse-region)))
                 (cats (xml-get-children (car dom) 'category)))
            (kill-buffer buf)
            (pona:trs-package-dom-to-alist
             category (xml-get-children (car cats) 'package))))))))

(defun pona:search-category-package-list-d (search-text)
  "Return a deferred DOM object of packages of search result for SEARCH-TEXT."
  (deferred:$
    (deferred:process-buffer "eix" "--xml" "-S" search-text)
    (deferred:nextc it
      (lambda (buf)
        (let* ((dom (with-current-buffer buf
                      (xml-parse-region)))
               (cats (xml-get-children (car dom) 'category)))
          (kill-buffer buf)
          (pona:trs-category-dom-to-alist cats))))))

;; (deferred:pp (pona:search-category-package-list-d "ocaml"))

(defun pona:set-category-package-list-d (set-name)
  "Return a deferred DOM object of packages of the SET-NAME."
  (deferred:$
    (deferred:process-buffer "eix" "--xml" (concat "--" set-name))
    (deferred:nextc it
      (lambda (buf)
        (let* ((dom (with-current-buffer buf
                      (xml-parse-region)))
               (cats (xml-get-children (car dom) 'category)))
          (kill-buffer buf)
          (pona:trs-category-dom-to-alist cats))))))

;; (deferred:pp (pona:set-category-package-list-d "world"))

(defun pona:package-info-d (category-package-name)
  "Return a deferred package object for CATEGORY-PACKAGE-NAME.
If not found, return nil."
  (deferred:try
    (deferred:nextc (deferred:process-buffer "eix" "--xml" category-package-name)
      (lambda (buf)
        (let* ((dom (with-current-buffer buf
                      (xml-parse-region)))
               (cats (xml-get-children (car dom) 'category))
               (ret (pona:trs-category-dom-to-alist cats)))
          (kill-buffer buf)
          (if ret (cdr (car (cdr (car ret))))))))
    :catch
    (lambda (err) nil)))

;; (deferred:pp (pona:package-info-d "dev-lang/ocaml"))

(defun pona:package-equery-use-d (package)
  "Return a deferred text for details of use flags of PACKAGE."
  (deferred:process "equery" "-C" "uses" 
    (format "%s/%s"
            (pona:package-category-name package)
            (pona:package-name package))))

;; ## eix model
;; ( ; ## category alist
;;  (category-name1
;;   ( ; ## package alist
;;    ("package-name1" (package object))
;;    ("package-name2" (package object))
;;     ...
;; ))

(defun pona:model-get-category (category-name category-alist)
  "Return an alist of packages for CATEGORY-NAME."
  (pona:aand (assoc category-name category-alist) (cdr it)))

(defun pona:model-get-package (package-name package-alist)
  "Return the package for PACKAGE-NAME."
  (pona:aand (assoc package-name package-alist) (cdr it)))


(defstruct pona:package
  "Package object

name: \"package-name\"
category-name: \"cat-name\"
description: \"description\"
licenses: \"licenses text\"
homepage: \"http://example.com/some/path\"
installed-version: \"2.25\"
latest-version: \"2.25\"
versions: (version list)"
  name category-name description licenses homepage 
  installed-version latest-version versions)


(defun pona:package-get-version (package id)
  (loop for i in (pona:package-versions package)
        for iid = (pona:version-id i)
        if (equal id iid) return i))

(defstruct pona:version
  "Version object

id: \"2.25\"
installed: t
mask: \"keywords\"
iuse: \"flag1 flag2\"
depend: \"dev-lang/perl virtual/perl-Term-ANSIColor\"
rdepend: \"dev-lang/perl virtual/perl-Term-ANSIColor\" "
  id installed mask iuse depend rdepend)


;; Transforming DOM to model objects.

(defun pona:trs-package-dom-to-alist (category-name packages-dom)
  "[internal] "
  (loop for package in packages-dom
        collect (pona:trs--package-item category-name package)))

(defun pona:trs-category-dom-to-alist (categories-dom)
  "[internal] "
  (loop with ret = nil
        for c in categories-dom
        for cn = (xml-get-attribute c 'name)
        collect
        (cons
         cn (pona:trs-package-dom-to-alist
             cn (xml-get-children c 'package)))))

(defun pona:trs--package-item (category-name package-dom)
  "[internal] "
  (let* ((name (xml-get-attribute package-dom 'name))
         (vers-dom (xml-get-children package-dom 'version))
         vers-alist latest installed latest-ver)
    (loop for v in vers-dom
          for id = (xml-get-attribute v 'id)
          for installed-ver = (xml-get-attribute-or-nil v 'installed)
          for mask = (pona:xml-get-attr v 'mask 'type)
          for iuse = (pona:xml-get-text v 'iuse)
          for dep  = (pona:xml-get-text v 'depend)
          for rdep = (pona:xml-get-text v 'rdepend)
          do
          (when installed-ver (setq installed id))
          (when (or (null latest) (and (string< latest-ver id) (not (string-match "9999" id))))
            (setq latest v latest-ver id))
          (push (make-pona:version
                 :id id :installed (and installed-ver t)
                 :mask mask :iuse iuse
                 :depend dep :rdepend rdep)
                vers-alist))
  (cons name
        (make-pona:package
         :name name
         :category-name category-name
         :description (pona:xml-get-text package-dom 'description)
         :licenses (pona:xml-get-text package-dom 'licenses)
         :homepage (pona:xml-get-text package-dom 'homepage)
         :installed-version installed
         :latest-version latest-ver
         :versions (nreverse vers-alist)))))

(defun pona:xml-get-elm (parent node-sym)
  "[internal] Return the first child element [NODE-SYM] at the PARENT node.
If not found, return nil."
  (pona:aand (xml-get-children parent node-sym)
             (car it)))

(defun pona:xml-get-text (parent node-sym)
  "[internal] Return the text content of the first child node [NODE-SYM] at the PARENT node.
If not found, return nil."
  (pona:aand (pona:xml-get-elm parent node-sym)
             (car (xml-node-children it))))

(defun pona:xml-get-attr (parent node-sym attr-sym)
  "[internal] Return the attribute [ATTR-SYM] text of the first child node [NODE-SYM] at the PARENT node.
If not found, return nil."
  (pona:aand (pona:xml-get-elm parent node-sym)
             (xml-get-attribute it attr-sym)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Home Buffer : top-level category buffer

;; TODO: world, system, set? global settings, global actions

(defun pona:home-buffer--get-catnam ()
  "[internal] Return a category name at the cursor point."
  (get-text-property (point) 'pona:category-name))

(defun pona:home-buffer--put-catnam (text category-name)
  "[internal] Put the category name on the given TEXT."
  (propertize text 'pona:category-name category-name))


(defun pona:home-buffer--jump-to-category ()
  (interactive)
  (pona:aand (pona:home-buffer--get-catnam)
             (pona:open-list-category-buffer it)))

(defun pona:home-buffer--refresh ()
  (interactive)
  (pona:home-buffer))

(defun pona:open-home-buffer ()
  "Open the home buffer."
  (interactive)
  (switch-to-buffer (pona:home-buffer)))

(defvar pona:home-buffer-mode-map
  (pona:define-keymap
   '(
     ;; navigation
     ("n" . next-line)
     ("j" . next-line)
     ("p" . previous-line)
     ("k" . previous-line)
     ("d" . scroll-up)
     ("u" . scroll-down)

     ;; action

     ("q" . pona:kill-this-buffer)
     ("R" . pona:home-buffer--refresh)
     ("s" . pona:open-search-results)
     )))

(defvar pona:home-buffer-mode-hook nil
  "pona:home-buffer-mode-hook.")

(defun pona:home-buffer-mode (&optional arg)
  "Set up major mode `pona:home-buffer-mode'.

\\{pona:home-buffer-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map pona:home-buffer-mode-map)
  (setq major-mode 'pona:home-buffer-mode
        mode-name "Portage Navi Home Mode")
  (setq buffer-undo-list t)
  (hl-line-mode) (hl-line-highlight)
  (run-hooks 'pona:home-buffer-mode-hook))

(defvar pona:home-buffer-name "*pona:home-buffer")

(defun pona:home-buffer ()
  "Build up home buffer and return the buffer object."
  (let ((buf (get-buffer-create pona:home-buffer-name)) (br "\n"))
    (with-current-buffer buf
      (let ((buffer-read-only))
        (erase-buffer)
        (insert (propertize "Portage Navi Home" 'face 'pona:face-title) br)
        (insert (pona:render-button "Search" 'pona:open-search-results) br br)

        (insert (propertize "Set" 'face 'pona:face-subtitle) br)
        (insert (pona:render-button "System" 'pona:open-list-system-buffer) "  "
                (pona:render-button "World"  'pona:open-list-world-buffer) br br)
        
        (insert (propertize "Categories" 'face 'pona:face-subtitle) br)
        (loop for i in (pona:category-list)
              for line = (pona:home-buffer--put-catnam i i)
              do (insert (pona:render-link line 'pona:home-buffer--jump-to-category) "\n")))
      (goto-char (point-min))
      (pona:home-buffer-mode)
      (setq buffer-read-only t))
    buf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Category Package List Buffer

(defvar pona:list-buffer-map 
  (pona:define-keymap
   '(
     ;; action
     ("q" . pona:kill-this-buffer)
     )))

(defvar pona:list-buffer-name " *pona:list-buffer")

(defun pona:make-package-table--line (package)
  (list (pona:package-name package)
        (pona:aif (pona:package-installed-version package) it "")
        (pona:package-latest-version package)
        (pona:package-description package)
        package))

(defun pona:make-package-table (buf packages)
  (let* ((param
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-models
          (list
           (make-ctbl:cmodel :title "No" :align 'right)
           (make-ctbl:cmodel :title "Name" :align 'left)
           (make-ctbl:cmodel :title "Installed" :align 'left)
           (make-ctbl:cmodel :title "Latest" :align 'left)
           (make-ctbl:cmodel :title "Description" :align 'left)))
         (data
          (loop for (pname . i) in packages
                for no from 1
                collect (cons no (pona:make-package-table--line i))))
         (model
          (make-ctbl:model
           :column-model column-models :data data))
         component)
    (setf (ctbl:param-fixed-header param) t)
    (setf (ctbl:param-bg-colors param)
          (lambda (model row-id col-id str)
            (when (= 2 col-id)
              (let* ((rows (ctbl:model-data model))
                     (row (nth row-id rows))
                     (cver (nth col-id row))
                     (lver (nth (1+ col-id) row)))
                (if (and (not (string= cver "")) 
                         (string< cver lver)) "LightPink" nil)))))
    (setq component
          (ctbl:create-table-component-buffer
           :buffer buf :model model :custom-map pona:list-buffer-map :param param))
    (ctbl:cp-add-click-hook
     component (lambda ()
                 (let* ((row (ctbl:cp-get-selected-data-row component))
                        (package (nth 5 row)))
                   (when package
                     (pona:open-package-detail-buffer package)))))
    (ctbl:cp-get-buffer component)))

(defun pona:list-package-buffer-gen (deferred-packages)
  "list-category-buffer-gen"
  (lexical-let
      ((buf (get-buffer-create pona:list-buffer-name)))
    (pona:display-message buf "[processing...]")
    (deferred:try
      (deferred:$ deferred-packages
        (deferred:nextc it
          (lambda (packages)
            (pona:make-package-table buf packages))))
      :catch
      (lambda (err)
        (pona:display-message buf (concat "[error!]\n" (pp-to-string err)))))
    buf))

(defun pona:open-list-category-buffer (cat-name)
  (pop-to-buffer
   (pona:list-package-buffer-gen
    (pona:category-package-list-d cat-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search Result Buffer

(defun pona:make-category-package-table (buf categories)
  (let* ((param
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-models
          (list
           (make-ctbl:cmodel :title "No" :align 'right)
           (make-ctbl:cmodel :title "Category" :align 'left)
           (make-ctbl:cmodel :title "Name" :align 'left)
           (make-ctbl:cmodel :title "Installed" :align 'left)
           (make-ctbl:cmodel :title "Latest" :align 'left)
           (make-ctbl:cmodel :title "Description" :align 'left)))
         (data
          (loop with ret = nil
                with no = 0
                for (cn . ps) in categories do
                (loop for (pn . p) in ps do
                      (push
                       (cons
                        (incf no)
                        (cons cn (pona:make-package-table--line p))) ret))
                finally return (nreverse ret)))
         (model
          (make-ctbl:model
           :column-model column-models :data data))
         component)
    (setf (ctbl:param-fixed-header param) t)
    (setf (ctbl:param-bg-colors param)
          (lexical-let ((cp component))
            (lambda (model row-id col-id str)
              (when (= 3 col-id)
                (let* ((rows (ctbl:component-sorted-data component))
                       (row (nth row-id rows))
                       (cver (nth col-id row))
                       (lver (nth (1+ col-id) row)))
                  (if (and (not (string= cver "")) 
                           (string< cver lver)) "LightPink" nil))))))
    (setq component
          (ctbl:create-table-component-buffer
           :buffer buf :model model :custom-map pona:list-buffer-map :param param))
    (ctbl:cp-add-click-hook
     component (lambda ()
                 (let* ((row (ctbl:cp-get-selected-data-row component))
                        (package (nth 6 row)))
                   (when package
                     (pona:open-package-detail-buffer package)))))
    (ctbl:cp-get-buffer component)))

(defun pona:list-category-package-buffer-gen (deferred-catpacks)
  "list-category-package-buffer-gen"
  (lexical-let
      ((buf (get-buffer-create pona:list-buffer-name)))
    (pona:display-message buf "[processing...]")
    (deferred:try
      (deferred:nextc deferred-catpacks
        (lambda (categories)
          (pona:make-category-package-table buf categories)))
      :catch
      (lambda (err)
        (pona:display-message buf (concat "[error!]\n" (pp-to-string err)))))
    buf))

(defun pona:open-search-results (&optional text)
  "exec-search"
  (interactive)
  (let ((search-text (or text (read-string "Search text: "))))
    (pop-to-buffer
     (pona:list-category-package-buffer-gen
      (pona:search-category-package-list-d search-text)))))

(defun pona:open-list-world-buffer ()
  "world list"
  (interactive)
  (pop-to-buffer
   (pona:list-category-package-buffer-gen
    (pona:set-category-package-list-d "world"))))

(defun pona:open-list-system-buffer ()
  "system list"
  (interactive)
  (pop-to-buffer
   (pona:list-category-package-buffer-gen
    (pona:set-category-package-list-d "system"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Detail Buffer

;; TODO: version list, depends, jump, file list

(defvar pona:package-detail-mode-map
  (pona:define-keymap
   '(
     ("q" . pona:kill-this-buffer))))

(defvar pona:package-detail-mode-hook nil
  "pona:package-detail-mode-hook.")

(defun pona:package-detail-mode (&optional arg)
  "Set up major mode `pona:package-detail-mode'.

\\{pona:package-detail-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map pona:package-detail-mode-map)
  (setq major-mode 'pona:package-detail-mode
        mode-name "Package Detail Mode")
  (setq buffer-undo-list t)
  (run-hooks 'pona:package-detail-mode-hook))

(defvar pona:package-detail-buffer-name " *pona:package-detail-buffer")

(defun pona:open-package-detail-buffer (package)
  "open-package-detail-buffer
PACKAGE"
  (let ((buf (get-buffer-create pona:package-detail-buffer-name)))
    (with-current-buffer buf
      (let ((buffer-read-only))
        (erase-buffer)
        (pona:package-detail-mode)
        (insert
         (pona:rt-format
          "%s\nLatest: %s  Installed: %s\n"
          (cons (pona:package-name package) 'pona:face-title)
          (pona:package-latest-version package)
          (pona:aif (pona:package-installed-version package) it "(not installed)")))
        (insert
         (pona:rt-format
          "Description: %s\nLicenses: %s\nHomepage: %s\n"
          (cons (pona:package-description package) nil)
          (cons (pona:package-licenses package) nil)
          (cons (pona:package-homepage package) nil)))
        (insert (propertize  "Versions:\n" 'face 'pona:face-item))
        (pona:insert-package-detail-versions-table package)
        (insert "\n")
        (insert (propertize  "Use flags:\n" 'face 'pona:face-item))
        (goto-char (point-min))
        (pona:insert-deferred
         buf (pona:package-equery-use-d package)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defvar pona:package-detail-versions-quicklook-buffer " *pona:package-detail-versions-quicklook*" "[internal] ")

(defun pona:package-detail-versions-quicklook-command ()
  "Display the cell content on the popup buffer."
  (interactive)
  (let ((cp (ctbl:cp-get-component)))
    (when cp
      (ctbl:cp-set-selected-cell cp (ctbl:cursor-to-nearest-cell))
      (let ((cell-data (ctbl:cp-get-selected-data-cell cp))
            (buf (get-buffer-create pona:package-detail-versions-quicklook-buffer)))
        (with-current-buffer buf
          (let (buffer-read-only)
            (edbi:dbview-query-result-quicklook-mode)
            (erase-buffer)
            (insert cell-data))
          (setq buffer-read-only t))
        (pop-to-buffer buf)))))

(define-derived-mode pona:
  text-mode "Result Quicklook Mode"
  "Major mode for quicklooking the result data.
\\{edbi:dbview-query-result-quicklook-mode-map}"
  (setq case-fold-search nil))

(defvar pona:package-detail-versions-table-keymap
  (pona:add-keymap
   ctbl:table-mode-map
   '(
     ("SPC" . pona:package-detail-versions-quicklook-command)
     )) "Keymap for the package detail table.")

(defun pona:insert-package-detail-versions-table (package)
  (let* ((param
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-models
          (list
           (make-ctbl:cmodel :title "Version" :align 'right :min-width 9)
           (make-ctbl:cmodel :title "Mask"    :align 'left  :min-width 6)
           (make-ctbl:cmodel :title "IUse"    :align 'left  :min-width 6 :max-width 30)
           (make-ctbl:cmodel :title "Depend"  :align 'left  :min-width 8 :max-width 40)))
         (data
          (loop for v in (reverse (pona:package-versions package))
                collect 
                (list 
                 (concat
                  (if (pona:version-installed v) "*" "") 
                  (pona:version-id v))
                 (or (pona:version-mask v) "")
                 (pona:version-iuse v)
                 (pona:version-depend v)
                 v)))
         (model
          (make-ctbl:model
           :column-model column-models :data data)))
    (setf (ctbl:param-bg-colors param)
          (lambda (model row-id col-id str)
            (when (= 0 col-id)
              (let* ((rows (ctbl:model-data model))
                     (row  (nth row-id rows))
                     (v (nth 4 row)))
                (if (pona:version-installed v) "LightPink" nil)))))
    (ctbl:create-table-component-region 
     :keymap pona:package-detail-versions-table-keymap
     :model model :param param)))

(defun pona:insert-deferred (buf d)
  (lexical-let ((buf buf))
    (deferred:nextc d
      (lambda (text)
        (with-current-buffer buf
          (let ((buffer-read-only))
            (goto-char (point-max))
            (insert text)
            (goto-char (point-min))))))))



;;;###autoload
(defun pona:portage-navi ()
  (interactive)
  (pona:open-home-buffer))

;; (progn (eval-current-buffer) (pona:open-home-buffer))
;; (pona:open-list-category-buffer "app-text")
;; (pona:open-list-world-buffer) (pona:open-list-system-buffer)
;; (pona:open-search-results "chrome")

(provide 'portage-navi)
;;; portage-navi.el ends here
