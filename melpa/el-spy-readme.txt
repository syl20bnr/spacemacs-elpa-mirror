########   Compatibility   ########################################

Works with Emacs-24.2

########   Quick start   ########################################

Add to your ~/.emacs

(require 'el-spy)

Code: section

(require 'advice)
(eval-when-compile (require 'cl))
for should in el-spy:make-args-keylist
(require 'ert)

(defun el-spy:setup-mock (funcsym)
  (el-spy:put-args funcsym nil)
  (when (fboundp funcsym)
    (put funcsym 'el-spy:original-func (symbol-function funcsym))))

(defun el-spy:get-original-func (symbol)
  (get symbol 'el-spy:original-func))

(defun el-spy:teardown-mock (funcsym)
  (el-spy:put-args funcsym nil)
  (let ((func (el-spy:get-original-func funcsym)))
    (if func
        (ad-safe-fset funcsym func)
      (fmakunbound funcsym))))

(defmacro with-el-spy (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     ;; macrolet
     (letf (((symbol-function 'defmock) (symbol-function 'el-spy:defmock)))
       (let ((el-spy:original-func nil))
         (unwind-protect
             (progn ,@body)
           (mapc #'el-spy:teardown-mock el-spy:original-func)
           )))))

(defalias 'with-mock2 'with-el-spy)

user level
(defun el-spy:get-args (symbol)
  (get symbol 'el-spy:args))
(defalias 'el-spy:args-for-call 'el-spy:get-args)

(defun el-spy:put-args (symbol args)
  (put symbol 'el-spy:args args))

(defun el-spy:append-args (symbol arglist)
  (el-spy:put-args
   symbol
   (append (get symbol 'el-spy:args) arglist)))

(defun el-spy:not-called (symbol)
  (eq (length (el-spy:get-args symbol)) 0))

(defun el-spy:called-count (symbol)
  (length (el-spy:get-args symbol)))

(defvar el-spy:func-name nil)

(defmacro el-spy:defmock (symbol arglist &rest body)
  (declare (indent defun) (debug t))
  `(progn
     (unless (boundp 'el-spy:original-func)
       (error "not in with-mock"))
     (push ',symbol el-spy:original-func)
     (el-spy:setup-mock ',symbol)
     (ad-safe-fset
      ',symbol
      (lambda ,arglist
        (setq el-spy:func-name ',symbol)
        (el-spy:append-args ',symbol (list (mapcar 'symbol-value ',arglist)))
        ;; Even if body include interactive, it works well.
        ;; Undocumented behavior?
        ,@body)
      )))
user level
(defmacro defproxy (symbol arglist &rest body)
  ;; todo
  )
user level
(defun el-spy:make-returns-keylist (list default)
  (append
   (let ((i 0))
     (mapcar (lambda (elem) (list (incf i) elem)) list))
   `((t ,default))))

(defun el-spy:make-args-keylist (arg list default)
  (el-spy:make-returns-keylist
   (mapcar (lambda (elem) (list 'should `(equal ,arg ,elem))) list) 6)
  )
user level
(defmacro el-spy:returns (list default)
  `(case (el-spy:called-count el-spy:func-name)
     ,@(el-spy:make-returns-keylist list default)))
user level
(defmacro el-spy:args (symbol list default)
  `(case (el-spy:called-count el-spy:func-name)
     ,@(el-spy:make-args-keylist symbol list default)))

el-mock limitation
(with-mock
  (mock (test1) :times 0)
  (call-interactively 'test1);; error
 )
(provide 'el-spy)

el-spy.el ends here
