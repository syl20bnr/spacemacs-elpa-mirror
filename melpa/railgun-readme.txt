The goal of this project is to provide easy ways to get to the places you
want to be. The built in finders are

rg-find-libs          - show a list of libs
rg-find-views         - show a list of views
rg-find-controller    - jump to a given controller
rg-find-presenter     - jump to a given presenter
rg-find-helper        - jump to a given helper
rg-find-model         - jump to a given model
rg-find-schema        - find model entry in schema.rb (or structure.sql) file
rg-find-blueprint     - find the entry in blueprints.rb for a given model (if you use machinist)
rg-find-factory       - find the entry in factories.rb for a given model (if you use factory_girl)
rg-create-model       - create a model with a given name (in wide case)
rg-create-helper      - create a helper with a given name (in wide case)
rg-create-controller  - create a controller with a given name (in wide case)
rg-create-spec        - create a spec for the current file
rg-create-test        - create a test for the current file

Customizing railgun:

you can add another type by adding to rg--class-paths. it is an alist with the format of

(type<symbol> . path<string>)

the path will determine where we search for files, and how we determine the class. if those are different,
you can use the alternate syntax

(type<symbol> . (search-path<string> . path-that-doesn't-apply-re<string>))

So for example, if you add a "domain" folder in the rails-root, that has a sub-folder which is not
part of the class name (real use case)

(rg-add-class-path (domain . ("domain/" . "domain/.*/")))
(rg-define-finder domain "Entity")

which will take all rb files in domain, and given a path of domain/common/foo/foo_class.rb, will consider
that to be Foo::FooClass. It will also create a function rg-find-domain which will have a prompt of
"Entity: ".

the default case is simpler, for example, a presenters directory

(presenter . "app/presenters/")
(rg-define-finder presenter)

to reset the class paths to the default (for example, when switching projects) use rg-reset-class-paths
to clear caches (for example, when adding a new file) use rg-clear-caches

(require 'inflections)
(require 'cl)
(require 'eieio)

config

(defvar rg-entity 'model
  "entity type to be used in railgun.
   change this if you have your models as another type")

(defvar rg-factory-file-path "test/blueprints.rb")
(defvar rg-blueprints-file-path "test/blueprints.rb")

find in files

(defun rg-find-blueprint ()
  (interactive)
  (let* ((class (rg-prompt "Blueprint for" (rg-filter-by file-type rg-entity)))
         (search (concat "^" class ".blueprint")))

    (when (rg-find-file-if-it-exists rg-blueprints-file-path)
      (or (re-search-forward search nil t)
          (re-search-backward search nil t)))))

(defun rg-find-factory (&optional file)
  (interactive)
  (let* ((file (or file (rg-prompt-for-file "Factory for: " (rg-files))))
         (class (concat "factory.*" (slot-value file 'class)))
         (sym (concat "factory +:['\"]?" (rg-table-for-file file) "['\"]?"))
         ;; TODO: remove me when I stop working on packmanager
         (strange-path (concat "factory +:['\"]?"
                               (rg-replace "::" "/" (singularize-string
                                                     (rg-table-for-file file)))
                               "['\"]?")))

    (when (rg-find-file-if-it-exists rg-factory-file-path)
      (or (rg-search-in-file class)
          (rg-search-in-file spec)
          (rg-search-in-file strange-path)))))

(defun rg-find-schema (&optional table-name)
  (interactive)
  (let ((name (or table-name (rg-prompt-for-table-name "Schema for: "))))

    (cond ((rg-find-file-if-it-exists "db/schema.rb")
           (rg-search-in-file (concat "create_table \"" name "\"")))

          ((rg-find-file-if-it-exists "db/structure.sql")
           (rg-search-in-file (concat "CREATE TABLE " name " ")))

          (t nil))))


(defun rg-search-in-file (re)
  (or (re-search-forward re nil t)
      (re-search-backward re nil t)))

(defun rg-find-file-if-it-exists (file)
  (let ((path (rg-path file)))
    (if (file-exists-p path)
        (find-file path))))

finders

(defun rg-find-entity ()
  (interactive)
  (let ((prompt (capitalize (symbol-name rg-entity)))
        (list (rg-filter-by 'type rg-entity)))
    (find-file (rg-prompt-for-path (concat prompt ": ") list))))

(defun rg-find-class ()
  (interactive)
  (find-file (rg-prompt-for-path "Class: " (rg-files))))

(defun rg-find-view ()
  (interactive)
  (let* ((input (rg-prompt "View for: " (rg-filter-by file-type 'controller)))
         (path (slot-value (rg-file-for-class input) relative-path))
         (controller-name (rg-remove "_controller.rb$" path))
         (view-dir (rg-path (concat "app/views/" controller-name))))
    (find-file (ido-read-file-name "View: " view-dir))))

tests

(defun rg-toggle-test-and-implementation ()
  (interactive)
  (with-slots (type) (rg-current-file)
    (if (or (eq 'spec type)
            (eq 'unit-test type))
        (rg-find-implementation)
      (rg-find-spec-or-test))))

(defun rg-find-implementation ()
  (interactive)
  (or (let* ((target (rg-remove "\\(Spec\\|Test\\)$" (rg-current 'class)))
             (path (rg-path-for-class target)))
        (and path (find-file path)))

      (rg-wide-find-implementation)))

(defun rg-find-spec-or-test ()
  (interactive)
  (or (or (rg-find-spec)
          (rg-find-test))
      (rg-wide-find-spec-or-test)))

(defun rg-find-spec ()
  (interactive)
  (let* ((target (concat (rg-current 'class) "Spec"))
         (path (rg-path-for-class target)))
    (and path (find-file path))))

(defun rg-find-test ()
  (interactive)
  (let* ((target (concat (rg-current 'class) "Test"))
         (path (rg-path-for-class target)))
    (and path (find-file path))))

(defun rg-wide-find-spec-or-test ()
  (interactive)
  (let* ((file (rg-file-name-for-path (buffer-file-name)))
         (spec-file (rg-file-name-postfix file "_spec"))
         (test-file (rg-file-name-postfix file "_test")))
    (rg-goto-file-in-list "Spec/Tests: "
                          (append (rg-filter-by 'name spec-file)
                                  (rg-filter-by 'name test-file)))))

(defun rg-wide-find-implementation ()
  (interactive)
  (let* ((file (rg-file-name-for-path (buffer-file-name)))
         (impl (rg-remove "\\(_test\\|_spec\\)" file))
         (files (rg-filter-by 'name impl)))
    (rg-goto-file-in-list "Implementation: " files)))

(defun rg-create-spec ()
  (interactive)

  (with-slots (type relative-path) (rg-current-file)
    (let* ((search-path (rg-search-path type))
           (spec-path (rg-remove "app/\\(assets/\\)?" search-path))
           (path (rg-path (concat "spec/" spec-path relative-path))))

      (find-file (rg-replace "\.\\([a-z]+\\)$" "_spec.\\1" path))
      (save-buffer)
      (rg-clear-caches))))

(defun rg-spec? ()
  (eq 'spec (rg-current 'type)))

(defun rg-test? ()
  (let ((type (rg-current 'type)))
    (or (eq 'unit-test type)
        (eq 'func-test type))))

(defun rg-file-name-postfix (file-name postfix)
  (replace-regexp-in-string "^\\(.+\\)\\.\\([a-z]+\\)$"
                            (concat "\\1" postfix ".\\2")
                            file))

(defun rg-goto-file-in-list (prompt files)
  (let* ((len (length files))
         (search-by (mapcar (lambda (file)
                              (slot-value file 'root-path)) files)))

    (if (= len 0) (message "Could not find spec or test")

      (let ((file (if (= len 1) (car files)
                    (object-assoc (ido-completing-read prompt search-by) 'root-path files))))

        (find-file (slot-value file 'path))))))

(defun rg-build-test-path (file)
  (with-slots (relative-path type) file
   (let ((path (replace-regexp-in-string ".rb$" "_test.rb" relative-path))
         (base "test/"))

    (rg-path (cond ((eq type 'controller)
                    (concat base "functional/" path))
                   ((eq type 'helper)
                    (concat base "unit/helper" path))
                   (t
                    (concat base "unit/" path)))))))

(defun rg-create-test ()
  (interactive)
  (let ((path (rg-build-test-path (rg-current-file))))
    (find-file path)
    (save-buffer)
    (rg-clear-caches)))


finders

(defun rg-find-class ()
  (interactive)
  (find-file (rg-prompt-for-path "Class:" (rg-files))))

(defmacro rg-define-finder (type &optional prompt)
  (let ((prompt (or prompt (capitalize (symbol-name type)))))
    `(defun ,(intern (concat "rg-find-" (symbol-name type))) ()
       (interactive)
       (let ((prompt (concat ,prompt ": "))
             (list (rg-filter-by 'type (quote ,type))))
         (find-file (rg-prompt-for-path prompt list))))))

(defun rg-prompt-for-table-name (prompt)
  (let ((file (rg-prompt-for-file prompt (rg-files))))
    (rg-table-for-path (slot-value file 'relative-path))))

(defun rg-find-path-in-list (class-name list)
  (slot-value (assoc class-name list)) path)

(defun rg-prompt-for-path (prompt list)
  (slot-value (rg-prompt-for-file prompt list) 'path))

(defun rg-prompt-for-file (prompt files)
  (let ((alist (object-assoc-list 'class files)))
    (object-assoc (rg-prompt prompt alist) 'class files)))

(defun rg-prompt-for (type prompt)
  (rg-prompt prompt (rg-filter-by-type type)))

(defun rg-prompt (prompt list)
  (ido-completing-read prompt (mapcar 'car list) nil t))

creating

(defmacro rg-define-creator (type &optional suffix initial-template-fn)
  (let ((type-name (capitalize (symbol-name type)))
        (name (intern (concat "rg-create-" (symbol-name type))))
        (search-path (rg-search-path type))
        (template (or initial-template-fn (lambda (input)))))
    `(defun ,name ()
       (interactive)
       (let ((input (read-from-minibuffer (concat "Create " ,type-name ": "))))
         (find-file (concat ,search-path "/" input ,suffix))
         (unless (string= (buffer-string) "")
           (funcall ,template input)
           (save-buffer)
           (rg-clear-caches))))))

(defun rg-helper-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name "Helper"))
    (newline)
    (insert "end")))

(defun rg-model-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name " < ActiveRecord::Base"))
    (newline)
    (insert "end")))

(defun rg-controller-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name "Controller < ApplicationController"))
    (newline)
    (insert "end")))

parsing

(defvar rg--default-class-paths
  '((model      . "app/models/")
    (controller . "app/controllers/")
    (presenter  . "app/presenters/")
    (helper     . "app/helpers/")
    (service    . ("app/services/" . "app/services/[a-zA-Z-0-9_]+/"))
    (domain     . "domain/")
    (lib        . "lib/")
    (unit-test  . ("test/unit/" . "test/unit/\\(helper/\\)?"))
    (func-test  . "test/functional/")
    (spec       . ("spec/" . "spec/\\(domain/[a-zA-Z0-9_]+/\\|[a-zA-Z0-9_]+/\\)"))))

(defvar rg--class-paths (copy-list rg--default-class-paths))

(defun rg-add-class-path (path)
  (push path rg--class-paths))

(defun rg-reset-class-paths ()
  (interactive)
  (rg-clear-caches)
  (setq rg--class-paths (copy-list rg--default-class-paths)))

(defun rg-file-types ()
  (mapcar 'car rg--class-paths))

(defun rg-search-path (type)
  (let ((path (cdr (assoc type rg--class-paths))))
    (if (listp path) (car path) path)))

(defun rg-base-regexp (type)
  (let ((path (cdr (assoc type rg--class-paths))))
    (if (listp path) (cdr path) path)))


rg-files
(defvar rg--files nil)
(defun rg-files ()
  (if rg--files rg--files
    (setq rg--files (rg-make-files))))

(defun rg-clear-caches ()
  (interactive)
  (setq rg--files nil))

(defun rg-make-files ()
  (loop for type in (rg-file-types)
        append (mapcar (lambda (path) (rg-make-file type path))
                       (all-files-under-dir-recursively
                        (rg-path (rg-search-path type))))))


(defclass rg-file ()
  ((class
    :initarg :file-class
    :initform ""
    :documentation "guess at the ruby class based on path")
   (relative-path
    :initarg :relative-path
    :initform ""
    :documentation "relative path from the base of the search path")
   (root-path
    :initarg :root-path
    :initform ""
    :documentation "relative path from the base of the project root path")
   (type
    :initarg :file-type
    :initform nil
    :documentation "type of file (from rg-paths)")
   (path
    :initarg :path
    :initform ""
    :documentation "full path to the file")
   (name
    :initarg :file-name
    :initform ""
    :documentation "name of the file (without the path)"))
  "A file in the rails project")

(defun rg-make-file (type path)
  (let ((relative-path (rg-relative-path type path)))
    (rg-file (concat "from " path)
             :path path
             :file-class (rg-class-for-path relative-path)
             :root-path (rg-remove (rg-path) path)
             :file-type type
             :file-name (rg-file-name-for-path path)
             :relative-path relative-path)))

(defun rg-filter-by (slot search)
  (let ((slot-equals
         (lambda (file)
           (let ((val (slot-value file slot)))
             (if (stringp search)
                 (string= search val)
               (eq search val))))))

    (rg-filter slot-equals (rg-files))))

(defun rg-current-file ()
  (let ((path (buffer-file-name)))
    (rg-make-file (rg-type-from-path path) path)))

(defun rg-current (slot)
  (slot-value (rg-current-file) slot))

(defun rg-type-from-path (path)
  (let ((path-matches (lambda (type)
                        (string-match (rg-path (rg-search-path type)) path))))
    (find-if path-matches (rg-file-types))))

(defun rg-file-name-for-path (path)
  (replace-regexp-in-string "^/\\(.*/\\)*" "" path))

(defun rg-file-for-class (class)
  (assoc class (rg-files)))

(defun rg-relative-path (type path)
  (let ((replace (rg-path (rg-base-regexp type))))
    (replace-regexp-in-string replace "" path)))

(defun rg-class-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "::" chopped))
         (capitalized (capitalize moduled)))
    (replace-regexp-in-string "_" "" capitalized)))

(defun rg-path-for-class (file-class)
  (let ((file (object-assoc file-class 'class (rg-files))))
    (when file
      (slot-value file 'path))))

(defun rg-table-for-file (file)
  (rg-table-for-path (slot-value file 'relative-path)))

(defun rg-table-for-path (path)
  (let* ((chopped (rg-remove ".rb$" path))
         (moduled (rg-replace "/" "_" chopped)))
    (pluralize-string moduled)))


utils

(defun rg-replace (regexp replace string)
  "REALLY tired of typing replace-regexp-in-string"
  (replace-regexp-in-string regexp replace string))

(defun rg-remove (regexp string)
  "REALLY tired of typing replace-regexp-in-string"
  (replace-regexp-in-string regexp "" string))

(defun rg-path (&optional path)
  (concat (railway-root) path))

(defun rg-constantize (name)
  (replace-regexp-in-string "_" "" (capitalize name)))

(defun rg-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

defines

(rg-define-creator helper "_helper.rb" 'rg-helper-template)
(rg-define-creator model ".rb" 'rg-model-template)
(rg-define-creator controller "_controller.rb" 'rg-controller-helper)
(rg-define-creator service "_service.rb")

(rg-define-finder model)
(rg-define-finder controller)
(rg-define-finder presenter)
(rg-define-finder service)
(rg-define-finder helper)
(rg-define-finder domain "Entity")
(rg-define-finder lib)

(provide 'railgun)
