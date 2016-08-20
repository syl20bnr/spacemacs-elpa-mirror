;;; dbus-codegen.el --- Lisp code generation for D-Bus. -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: comm, dbus, convenience
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.1
;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides macros and functions to make D-Bus
;; client/server implementation easy, inspired by the `gdbus-codegen'
;; utility in GLib.  To get it work, `lexical-binding' must be
;; enabled.
;;
;; * Client support
;;
;; A proxy object representing a D-Bus client can be defined with
;; either `dbus-codegen-define-proxy' or `dbus-codegen-make-proxy'.
;;
;; `dbus-codegen-define-proxy' takes a static XML definition of a
;; D-Bus service and generates code at compile time.  This is good for
;; stable D-Bus services.  On the other hand,
;; `dbus-codegen-make-proxy' uses D-Bus introspection and retrieves a
;; D-Bus service definition from a running service itself.  This is
;; good for debugging or for unstable D-Bus services.
;;
;; ** Example
;;
;; Suppose the following code:
;;
;; (dbus-codegen-define-proxy test-proxy
;;                            "\
;; <node>
;;   <interface name='org.example.Test'>
;;     <method name='OpenFile'>
;;       <arg type='s' name='path' direction='in'/>
;;     </method>
;;     <signal name='Changed'>
;;       <arg type='s' name='a_string'/>
;;     </signal>
;;     <property type='s' name='Content' access='read'/>
;;   </interface>
;; </node>"
;;                            "org.example.Test")
;;
;; The `dbus-codegen-define-proxy' macro expands to a definition a
;; struct `test-proxy' with a slot `content', which corresponds to the
;; "Content" property.  The slot value will be initialized when the
;; proxy is created and updated when the server sends a notification.
;; The proxy can always retrieve the value with the function
;; `PROXY-retrieve-PROPERTY-property'.
;;
;; The macro also defines the following wrapper functions:
;;
;; - `test-proxy-create'
;;   constructor of the proxy
;; - `test-proxy-destroy'
;;   destructor of the proxy
;; - `test-proxy-open-file'
;;   wrapper around calling the "OpenFile" method
;; - `test-proxy-open-file-asynchronously'
;;   asynchronous wrapper around calling the "OpenFile" method
;; - `test-proxy-send-changed-signal'
;;   wrapper around sending the "Changed" signal
;; - `test-proxy-register-changed-signal'
;;   wrapper around registering a handler for the "Changed" signal
;; - `test-proxy-retrieve-content-property'
;;   retrieve the value of the "Content" property
;;
;; In addition to those, the macro also defines a generic function
;; `test-proxy-handle-changed-signal' to allow a class-wide signal
;; handler definition.
;;
;; To register a class-wide signal handler, define a method
;; `test-proxy-handle-changed-signal' with `cl-defmethod', like this:
;;
;; (cl-defmethod test-proxy-handle-changed-signal ((proxy test-proxy) string)
;;   ... do something with PROXY and STRING ...)
;;
;; * Server support
;;
;; A skeleton object representing a D-Bus server can be defined with
;; `dbus-codegen-define-skeleton'.
;;
;; `dbus-codegen-define-skeleton' takes a static XML definition of a
;; D-Bus service and generates code at compile time.
;;
;; ** Example
;;
;; Suppose the following code:
;;
;; (dbus-codegen-define-skeleton test-skeleton
;;                            "\
;; <node>
;;   <interface name='org.example.Test'>
;;     <method name='OpenFile'>
;;       <arg type='s' name='path' direction='in'/>
;;     </method>
;;     <signal name='Changed'>
;;       <arg type='s' name='a_string'/>
;;     </signal>
;;     <property type='s' name='Content' access='read'/>
;;   </interface>
;; </node>"
;;                            "org.example.Test")
;;
;; The `dbus-codegen-define-skeleton' macro expands to a definition a
;; struct `test-skeleton' with a slot `content', which corresponds to the
;; "Content" property.
;;
;; The macro also defines the following wrapper functions:
;;
;; - `test-skeleton-create'
;;   constructor of the skeleton
;; - `test-skeleton-destroy'
;;   destructor of the skeleton
;; - `test-skeleton-register-open-file-method'
;;   wrapper around registering a handler for the "OpenFile" method
;; - `test-skeleton-send-changed-signal'
;;   wrapper around sending the "Changed" signal
;; - `test-skeleton-register-changed-signal'
;;   wrapper around registering a handler for the "Changed" signal
;; - `test-skeleton-register-content-property'
;;   wrapper around registering a value of the "Content" property
;;
;; In addition to those, the macro also defines a generic function
;; `test-skeleton-handle-open-file-method' to allow a class-wide method
;; handler definition.
;;
;; To register a class-wide method handler, define a method
;; `test-skeleton-handle-open-file-method' with `cl-defmethod', like this:
;;
;; (cl-defmethod test-skeleton-handle-open-file-method ((skeleton test-skeleton)
;;                                                      string)
;;   ... do something with SKELETON and STRING ...)
;;
;; * TODO
;;
;; - function documentation generation from annotations

;;; Code:

(require 'dbus)
(require 'cl-lib)

(eval-when-compile
  (require 'xml)
  (require 'subword))

;; Base type of a D-Bus proxy and a skeleton.
(cl-defstruct (dbus-codegen-object
	       (:constructor nil))
  (bus :read-only t)
  (service :read-only t)
  (path :read-only t)
  (interface :read-only t)
  registration-list)

;; Base type of a D-Bus proxy.
(cl-defstruct (dbus-codegen-proxy
	       (:include dbus-codegen-object)
	       (:constructor nil)))

;; Base type of a D-Bus skeleton
(cl-defstruct (dbus-codegen-skeleton
	       (:include dbus-codegen-object)
	       (:constructor nil)))

;; Return a list of elements in the form: (LISP-NAME ORIG-NAME MEMBER).
(defun dbus-codegen--apply-transform-name (elements transform-name)
  (mapcar (lambda (elements)
	    (let ((name (xml-get-attribute-or-nil elements 'name)))
		(unless name
		  (error "missing \"name\" attribute of %s"
			 (xml-node-name elements)))
		(list (funcall transform-name name)
		      name
		      elements)))
	  elements))

;; Return a list of symbols.
(defun dbus-codegen--collect-arglist (args transform-name)
  (delq nil
	(mapcar
	 (lambda (arg)
	   (let ((direction
		  (xml-get-attribute-or-nil (nth 2 arg) 'direction)))
	     (if (or (null direction)
		     (equal direction "in"))
		 (intern (car arg)))))
	 (dbus-codegen--apply-transform-name args transform-name))))

(defconst dbus-codegen--basic-type-to-symbol-alist
  '((?y . :byte)
    (?b . :boolean)
    (?n . :int16)
    (?q . :uint16)
    (?i . :int32)
    (?u . :uint32)
    (?x . :int64)
    (?t . :uint64)
    (?d . :double)
    (?s . :string)
    (?o . :object-path)
    (?g . :signature))
  "Mapping from D-Bus type-codes to Lisp symbols.")

;; Read a single type from SIGNATURE.  Returns a cons cell of
;; (NEXT-OFFSET . TYPE).
(defun dbus-codegen--read-signature (signature offset)
  (let* ((c (aref signature offset))
	 (entry (assq c dbus-codegen--basic-type-to-symbol-alist)))
    (if entry
	(cons (1+ offset) (cdr entry))
      (pcase c
	(?{
	 (let* ((type1 (dbus-codegen--read-signature signature (1+ offset)))
		(type2 (dbus-codegen--read-signature signature (car type1))))
	   (unless (eq (aref signature (car type2)) ?})
	     (error "Unterminated dict-entry"))
	   (cons (car type2) (list :dict-entry (cdr type1) (cdr type2)))))
	(?\(
	 (let ((next-offset (1+ offset))
	       types
	       type)
	   (while (and (< next-offset (length signature))
		       (not (eq (setq c (aref signature next-offset)) ?\))))
	     (setq type (dbus-codegen--read-signature signature next-offset)
		   next-offset (car type))
	     (push (cdr type) types))
	   (unless (eq (aref signature (car type)) ?\))
	     (error "Unterminated struct"))
	   (cons next-offset (list :struct (nreverse types)))))
	(?a
	 (unless (< (1+ offset) (length signature))
	   (error "Unterminated array"))
	 (let ((type (dbus-codegen--read-signature signature (1+ offset))))
	   (cons (car type) (list :array (cdr type)))))
	(?v
	 (cons (1+ offset) (list :variant)))))))

(defun dbus-codegen--byte-p (value)
  (and (integerp value)
       (<= 0 value #xFF)))

(defun dbus-codegen--int16-p (value)
  (and (integerp value)
       (<= (- (- #x7FFF) 1) value #x7FFF)))

(defun dbus-codegen--uint16-p (value)
  (and (integerp value)
       (<= 0 value #xFFFF)))

(defun dbus-codegen--object-path-p (value)
  (and (stringp value)
       (string-match "\\`/\\'\\|\\`\\(?:/\\(?:[A-Za-z0-9_]+\\)\\)+\\'" value)))

(defconst dbus-codegen--basic-type-check-alist
  '((:byte . dbus-codegen--byte-p)
    (:boolean . booleanp)
    (:int16 . dbus-codegen--int16-p)
    (:uint16 . dbus-codegen--uint16-p)
    (:int32 . integerp)
    (:uint32 . natnump)
    (:int64 . integerp)
    (:uint64 . natnump)
    (:double . floatp)
    (:string . stringp)
    (:object-path . dbus-codegen--object-path-p)
    (:signature . stringp)
    (:unix-fd . natnump))
  "An alist mapping from Lisp symbols to predicates that check value types.")

(defun dbus-codegen--annotate-arg (type arg)
  (pcase type
    ((and basic (or :byte :boolean :int16 :uint16 :int32 :uint32 :int64 :uint64
		    :double :string :object-path :signature :unix-fd))
     (let ((entry (assq basic dbus-codegen--basic-type-check-alist)))
       (when (and entry
		  (not (funcall (cdr entry) arg)))
	 (signal 'wrong-type-argument (list (cdr entry) arg))))
     (list basic arg))
    (`(:array ,elttype)
     ;; FIXME: an empty array must have a `:signature' element to
     ;; denote the element type.
     (list (cons :array
		 (apply #'nconc
			(mapcar (lambda (subarg)
				  (dbus-codegen--annotate-arg elttype subarg))
				arg)))))
    (`(:struct . ,memtypes)
     (list (cons :struct (apply #'nconc
				(cl-mapcar
				 (lambda (memtype subarg)
				   (dbus-codegen--annotate-arg memtype subarg))
				 memtypes arg)))))
    (`(:variant)
     (list (cons :variant (apply #'nconc
				 (mapcar (lambda (subarg) (list subarg))
					 arg)))))
    (`(:dict-entry ,keytype ,valtype)
     (list (cons :dict-entry
		 (nconc (dbus-codegen--annotate-arg keytype (car arg))
			(dbus-codegen--annotate-arg valtype (cdr arg))))))
    (_ (error "Unknown type specification: %S" type))))

(defun dbus-codegen--collect-arglist-with-type-annotation (args transform-name)
  (delq nil (mapcar
	     (lambda (arg)
	       (let ((direction
		      (xml-get-attribute-or-nil (nth 2 arg) 'direction))
		     (type
		      (xml-get-attribute-or-nil (nth 2 arg) 'type)))
		 (if (or (null direction)
			 (equal direction "in"))
		     (let ((signature (dbus-codegen--read-signature type 0)))
		       `(dbus-codegen--annotate-arg ,(cdr signature)
						    ,(intern (car arg)))))))
	     (dbus-codegen--apply-transform-name args transform-name))))

(declare-function subword-forward "subword.el" (&optional arg))
(defun dbus-codegen-transform-name (name)
  "Transform NAME into suitable Lisp function name."
  (require 'subword)
  (with-temp-buffer
    (let (words)
      (insert name)
      (goto-char (point-min))
      (while (not (eobp))
	;; Skip characters not recognized by subword-mode.
	(if (looking-at "[^[:lower:][:upper:][:digit:]]+")
	    (goto-char (match-end 0)))
	(push (downcase (buffer-substring (point) (progn (subword-forward 1)
							 (point))))
	      words))
      (mapconcat #'identity (nreverse words) "-"))))

;; Emit wrappers around `dbus-call-method'.
(defun dbus-codegen--emit-call-method (name methods transform-name)
  (apply
   #'nconc
   (mapcar
    (lambda (method)
      (let ((arglist (dbus-codegen--collect-arglist
		      (xml-get-children
		       (car (xml-get-children method 'method))
		       'arg)
		      transform-name))
	    (annotated-arglist
	     (dbus-codegen--collect-arglist-with-type-annotation
	      (xml-get-children
	       (car (xml-get-children method 'method)) 'arg)
	      transform-name)))
	`((cl-defgeneric
	      ,(intern (format "%s-%s" name (car method)))
	      (object ,@arglist &rest args)
	    ,(format "Call the \"%s\" method of OBJECT."
		     (nth 1 method)))
	  (cl-defmethod
	      ,(intern (format "%s-%s" name (car method)))
	      ((object ,name) ,@arglist &rest args)
	    (apply #'dbus-call-method
		   (,(intern (format "%s-bus" name )) object)
		   (,(intern (format "%s-service" name)) object)
		   (,(intern (format "%s-path" name)) object)
		   (,(intern (format "%s-interface" name)) object)
		   ,(nth 1 method)
		   (append ,@annotated-arglist args))))))
    methods)))

;; Emit wrappers around `dbus-call-method-asynchronously'.
(defun dbus-codegen--emit-call-method-asynchronously (name methods
							   transform-name)
  (apply
   #'nconc
   (mapcar
    (lambda (method)
      (let ((arglist (dbus-codegen--collect-arglist
		      (xml-get-children
		       (car (xml-get-children method 'method))
		       'arg)
		      transform-name))
	    (annotated-arglist
	     (dbus-codegen--collect-arglist-with-type-annotation
	      (xml-get-children
	       (car (xml-get-children method 'method))
	       'arg)
	      transform-name)))
	`((cl-defgeneric
	      ,(intern (format "%s-%s-asynchronously"
			       name (car method)))
	      ((object ,name) ,@arglist handler &rest args)
	    ,(format "Asynchronously call the \"%s\" method of OBJECT."
		     (nth 1 method)))
	  (cl-defmethod
	      ,(intern (format "%s-%s-asynchronously"
			       name (car method)))
	      ((object ,name) ,@arglist handler &rest args)
	  (apply #'dbus-call-method-asynchronously
		 (,(intern (format "%s-bus" name )) object)
		 (,(intern (format "%s-service" name)) object)
		 (,(intern (format "%s-path" name)) object)
		 (,(intern (format "%s-interface" name)) object)
		 ,(nth 1 method)
		 handler
		 (append ,@annotated-arglist args))))))
   methods)))

;; Emit wrappers around `dbus-register-signal'.
(defun dbus-codegen--emit-register-signal (name signals)
  (apply
   #'nconc
   (mapcar
    (lambda (signal)
      `((cl-defgeneric
	    ,(intern (format "%s-register-%s-signal" name (car signal)))
	    (object handler &rest args)
	  ,(format "Register HANDLER to the \"%s\" signal of OBJECT."
		   (nth 1 signal)))
	(cl-defmethod
	    ,(intern (format "%s-register-%s-signal" name (car signal)))
	    ((object ,name) handler &rest args)
	  (push (apply #'dbus-register-signal
		     (,(intern (format "%s-bus" name )) object)
		     (,(intern (format "%s-service" name)) object)
		     (,(intern (format "%s-path" name)) object)
		     (,(intern (format "%s-interface" name)) object)
		     ,(nth 1 signal)
		     (lambda (&rest args)
		       (apply handler object args))
		     args)
	      (,(intern (format "%s-registration-list" name)) object)))))
    signals)))

;; Emit wrappers around `dbus-send-signal'.
(defun dbus-codegen--emit-send-signal (name signals transform-name)
  (apply
   #'nconc
   (mapcar
    (lambda (signal)
      (let ((arglist (dbus-codegen--collect-arglist
		      (xml-get-children
		       (car (xml-get-children signal 'signal))
		       'arg)
		      transform-name))
	    (annotated-arglist
	     (dbus-codegen--collect-arglist-with-type-annotation
	      (xml-get-children
	       (car (xml-get-children signal 'signal))
	       'arg)
	      transform-name)))
	`((cl-defgeneric
	      ,(intern (format "%s-send-%s-signal"
			       name (car signal)))
	      (object ,@arglist &rest args)
	    ,(format "Send the \"%s\" signal of OBJECT."
		     (nth 1 signal)))
	  (cl-defmethod
	      ,(intern (format "%s-send-%s-signal"
			       name (car signal)))
	      ((object ,name) ,@arglist &rest args)
	    (apply #'dbus-send-signal
		   (,(intern (format "%s-bus" name )) object)
		   (,(intern (format "%s-service" name)) object)
		   (,(intern (format "%s-path" name)) object)
		   (,(intern (format "%s-interface" name)) object)
		   ,(nth 1 signal)
		   (append ,@annotated-arglist args))))))
    signals)))

;; Emit generic functions for signal handlers.
(defun dbus-codegen--emit-signal-defgeneric (name signals transform-name)
  (mapcar
   (lambda (signal)
     (let ((arglist (dbus-codegen--collect-arglist
		     (xml-get-children
		      (car (xml-get-children signal 'signal))
		      'arg)
		     transform-name)))
       `(cl-defgeneric
	    ,(intern (format "%s-handle-%s-signal" name (car signal)))
	    (object ,@arglist)
	  ,(format "Generic function called upon receiving the \"%s\" signal."
		   (nth 1 signal))
	  (list object ,@arglist)
	  nil)))
   signals))

;; Emit wrappers around `dbus-get-property'.
(defun dbus-codegen--emit-retrieve-property (name properties)
  (apply
   #'nconc
   (mapcar
    (lambda (property)
      `((cl-defgeneric
	    ,(intern (format "%s-retrieve-%s-property"
			     name (car property)))
	    (object)
	  ,(format "Retrieve the value of the \"%s\" property of OBJECT."
		   (nth 1 property)))
	(cl-defmethod
	    ,(intern (format "%s-retrieve-%s-property"
			     name (car property)))
	    ((object ,name))
	  (setf (,(intern (format "%s-%s" name (car property)))
		 object)
		(dbus-get-property
		 (,(intern (format "%s-bus" name )) object)
		 (,(intern (format "%s-service" name)) object)
		 (,(intern (format "%s-path" name)) object)
		 (,(intern (format "%s-interface" name)) object)
		 ,(nth 1 property))))))
    properties)))

;; Emit generic functions for method handlers.
(defun dbus-codegen--emit-method-defgeneric (name methods transform-name)
  (mapcar
   (lambda (method)
     (let ((arglist (dbus-codegen--collect-arglist
		     (xml-get-children
		      (car (xml-get-children method 'method))
		      'arg)
		     transform-name)))
     `(cl-defgeneric
	  ,(intern (format "%s-handle-%s-method" name (car method)))
	  (object ,@arglist)
	,(format "Generic function called when the \"%s\" method is called."
		 (nth 1 method))
	(list object ,@arglist)
	nil)))
   methods))

;; Emit wrappers around `dbus-register-method'.
(defun dbus-codegen--emit-register-method (name methods)
  (apply
   #'nconc
   (mapcar
    (lambda (method)
      `((cl-defgeneric
	    ,(intern (format "%s-register-%s-method" name (car method)))
	    (object handler &rest args)
	  ,(format "Register HANDLER to the \"%s\" method of OBJECT."
		   (nth 1 method)))
	(cl-defmethod
	    ,(intern (format "%s-register-%s-method" name (car method)))
	    ((object ,name) handler &rest args)
	  (push (apply #'dbus-register-method
		       (,(intern (format "%s-bus" name )) object)
		       (,(intern (format "%s-service" name)) object)
		       (,(intern (format "%s-path" name)) object)
		       (,(intern (format "%s-interface" name)) object)
		       ,(nth 1 method)
		       (lambda (&rest args)
			 (apply handler object args))
		       args)
		(,(intern (format "%s-registration-list" name)) object)))))
    methods)))

;; Emit wrappers around `dbus-register-property'.
(defun dbus-codegen--emit-register-property (name properties)
  (apply
   #'nconc
   (mapcar
    (lambda (property)
      (let* ((annotations
	      (delq nil
		   (mapcar
		    (lambda (annotation)
		      (if (equal
			   (xml-get-attribute-or-nil annotation 'name)
			   "org.freedesktop.DBus.Property.EmitsChangedSignal")
			  annotation))
		    (xml-get-children (nth 2 property) 'annotation))))
	    (emits-signal
	     (or (null annotations)
		 (not (equal (xml-get-attribute-or-nil (car annotations)
						       'value)
			     "false")))))
	`((cl-defgeneric
	      ,(intern (format "%s-register-%s-property" name (car property)))
	      (object value &rest args)
	    ,(format "Register VALUE of the \"%s\" property of OBJECT."
		     (nth 1 property)))
	  (cl-defmethod
	      ,(intern (format "%s-register-%s-property" name (car property)))
	      ((object ,name) value &rest args)
	    (setf (,(intern (format "%s-%s" name (car property))) object) value)
	    (push (apply #'dbus-register-property
			 (,(intern (format "%s-bus" name )) object)
			 (,(intern (format "%s-service" name)) object)
			 (,(intern (format "%s-path" name)) object)
			 (,(intern (format "%s-interface" name)) object)
			 ,(nth 1 property)
			 value
			 ,@(if emits-signal
			       (list :emits-signal t))
			 args)
		  (,(intern (format "%s-registration-list" name)) object))))))
    properties)))

;;;###autoload
(defmacro dbus-codegen-define-proxy (name xml interface &rest args)
  "Define a new D-Bus proxy NAME.
This defines a new struct type for the proxy and convenient
functions for D-Bus method calls and signal registration.

XML is either a string which defines the interface of the D-Bus
proxy, or a Lisp form which returns a string.  The format of the
string must comply with the standard D-Bus introspection data
format as described in:
`http://dbus.freedesktop.org/doc/dbus-specification.html#introspection-format>'.

INTERFACE is a name of interface which is represented by this
proxy.

ARGS are keyword-value pair.  Currently only one keyword is
supported:

:transform-name FUNCTION -- FUNCTION is a function which converts
D-Bus method/signal/property names, into another representation.
By default `dbus-codegen-transform-name' is used."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  ;; Accept a Lisp form as well as a string.
  (unless (stringp xml)
    (setq xml (eval xml)))
  (unless (stringp xml)
    (signal 'wrong-type-argument (list 'stringp xml)))
  (let ((node (car (with-temp-buffer
		     (insert xml)
		     (xml-parse-region (point-min) (point-max)))))
	(transform-name (or (plist-get args :transform-name)
			    #'dbus-codegen-transform-name)))
    (unless (eq (xml-node-name node) 'node)
      (error "Root is not \"node\""))
    ;; Accept a quoted form of a function, such as #'func.
    (unless (functionp transform-name)
      (setq transform-name (eval transform-name)))
    (let ((interface-node
	   (cl-find-if (lambda (element)
			 (equal (xml-get-attribute-or-nil element 'name)
				interface))
		       (xml-get-children node 'interface))))
      (unless interface-node
	(error "Interface %s is missing" interface))
      (let ((methods (dbus-codegen--apply-transform-name
		      (xml-get-children interface-node 'method)
		      transform-name))
	    (properties (dbus-codegen--apply-transform-name
			 (xml-get-children interface-node 'property)
			 transform-name))
	    (signals (dbus-codegen--apply-transform-name
		      (xml-get-children interface-node 'signal)
		      transform-name)))
	`(progn
	   ;; Define a new struct.
	   (cl-defstruct (,name (:include dbus-codegen-proxy)
				(:constructor nil)
				(:constructor ,(intern (format "%s--make" name))
					      (bus service path interface)))
	     ;; Slots for cached property values.
	     ,@(mapcar
		(lambda (property)
		  (intern (car property)))
		properties))

	   ;; Define a constructor.
	   (defun ,(intern (format "%s-create" name)) (bus service path)
	     ,(format "Create a new D-Bus proxy for %s.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE."
		  interface)
	     (let ((proxy (,(intern (format "%s--make" name))
			   bus service path ,interface)))
	       ,(when properties
		  ;; Initialize slots.
		  `(let ((properties (dbus-get-all-properties bus service path
							      ,interface)))
		     ,@(mapcar
			(lambda (property)
			  `(setf (,(intern (format "%s-%s" name (car property)))
				  proxy)
				 (cdr (assoc ,(nth 1 property) properties))))
			properties)
		     (push (dbus-register-signal
			    bus service path dbus-interface-properties
			    "PropertiesChanged"
			    (lambda (interface changed invalidated)
			      (,(intern (format "%s--handle-properties-changed"
						name))
			       proxy
			       interface changed invalidated))
			    :arg0 ,interface)
			   (,(intern (format "%s-registration-list" name))
			    proxy))))
	       ;; Register signal handlers.
	       ,@(mapcar
		  (lambda (signal)
		    `(push (dbus-register-signal
			    bus service path ,interface
			    ,(nth 1 signal)
			    (lambda (&rest args)
			      (apply #',(intern (format "%s-handle-%s-signal"
							name (car signal)))
				     proxy args)))
			   (,(intern (format "%s-registration-list" name))
			    proxy)))
		  signals)
	       proxy))

	   ,(when properties
	      ;; Define a handler of PropertiesChanged signal.
	      `(defun ,(intern (format "%s--handle-properties-changed" name))
		   (proxy interface changed invalidated)
		 (when (equal interface ,interface)
		   ,@(mapcar
		      (lambda (property)
			`(let ((changed-value
				(cdr (assoc ,(nth 1 property) changed)))
			       (invalidated-property
				(car (member ,(nth 1 property) invalidated)))
			       invalidated-value)
			   (when changed-value
			     (setf (,(intern (format "%s-%s"
						     name (car property)))
				    proxy)
				   (car (car changed-value))))
			   (when invalidated-property
			     (setq invalidated-value
				   (dbus-get-property
				    (,(intern (format "%s-bus" name)) proxy)
				    (,(intern (format "%s-service" name)) proxy)
				    (,(intern (format "%s-path" name)) proxy)
				    ,interface
				    ,(car property)))
			     (when invalidated-value
			       (setf (,(intern (format "%s-%s"
						       name (car property)))
				      proxy)
				     invalidated-value)))))
		      properties))))

	   ;; Define a destructor.
	   (cl-defgeneric ,(intern (format "%s-destroy" name)) (proxy)
	     "Destroy a D-Bus proxy PROXY.")

	   (cl-defmethod ,(intern (format "%s-destroy" name)) ((proxy ,name))
	     (dolist (registration (,(intern (format "%s-registration-list"
						     name))
				    proxy))
	       (dbus-unregister-object registration))
	     (setf (,(intern (format "%s-registration-list" name)) proxy) nil))

	   ;; Emit common helper functions.
	   ,@(dbus-codegen--emit-signal-defgeneric name signals transform-name)
	   ,@(dbus-codegen--emit-send-signal name signals transform-name)
	   ,@(dbus-codegen--emit-register-signal name signals)
	   ;; Emit helper functions for proxy.
	   ,@(dbus-codegen--emit-call-method name methods transform-name)
	   ,@(dbus-codegen--emit-call-method-asynchronously name methods
							    transform-name)
	   ,@(dbus-codegen--emit-retrieve-property name properties))))))

;;;###autoload
(defun dbus-codegen-make-proxy (name bus service path interface &rest args)
  "Create a new D-Bus proxy based on the introspection data.

If the data type of the D-Bus proxy is not yet defined, this will
define it with `dbus-codegen-define-proxy', under a type name NAME.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.

INTERFACE is an interface which is represented by this proxy.

ARGS are keyword-value pair.  Currently only one keyword is
supported:

:redefine FLAG -- if FLAG is non-nil, redefine the data type and
associated functions.

Other keywords are same as `dbus-codegen-define-proxy'."
  (require 'xml)
  (require 'subword)
  (let ((constructor (intern (format "%s-make" name))))
    (if (or (plist-get args :redefine)
	    (not (fboundp constructor)))
	(eval `(define-dbus-proxy ,(intern name)
		 ,(dbus-introspect bus service path)
		 ,interface
		 ,@args)))
    (funcall constructor bus service path)))

(defmacro dbus-codegen-define-skeleton (name xml interface &rest args)
  "Define a new D-Bus skeleton NAME.
This defines a new struct type for the skeleton and convenient
functions for D-Bus method calls and signal registration.

XML is either a string which defines the interface of the D-Bus
skeleton, or a Lisp form which returns a string.  The format of the
string must comply with the standard D-Bus introspection data
format as described in:
`http://dbus.freedesktop.org/doc/dbus-specification.html#introspection-format>'.

INTERFACE is a name of interface which is represented by this
skeleton.

ARGS are keyword-value pair.  Currently only one keyword is
supported:

:transform-name FUNCTION -- FUNCTION is a function which converts
D-Bus method/signal/property names, into another representation.
By default `dbus-codegen-transform-name' is used."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  ;; Accept a Lisp form as well as a string.
  (unless (stringp xml)
    (setq xml (eval xml)))
  (unless (stringp xml)
    (signal 'wrong-type-argument (list 'stringp xml)))
  (let ((node (car (with-temp-buffer
		     (insert xml)
		     (xml-parse-region (point-min) (point-max)))))
	(transform-name (or (plist-get args :transform-name)
			    #'dbus-codegen-transform-name)))
    (unless (eq (xml-node-name node) 'node)
      (error "Root is not \"node\""))
    ;; Accept a quoted form of a function, such as #'func.
    (unless (functionp transform-name)
      (setq transform-name (eval transform-name)))
    (let ((interface-node
	   (cl-find-if (lambda (element)
			 (equal (xml-get-attribute-or-nil element 'name)
				interface))
		       (xml-get-children node 'interface))))
      (unless interface-node
	(error "Interface %s is missing" interface))
      (let ((methods (dbus-codegen--apply-transform-name
		      (xml-get-children interface-node 'method)
		      transform-name))
	    (properties (dbus-codegen--apply-transform-name
			 (xml-get-children interface-node 'property)
			 transform-name))
	    (signals (dbus-codegen--apply-transform-name
		      (xml-get-children interface-node 'signal)
		      transform-name)))
	`(progn
	   ;; Define a new struct.
	   (cl-defstruct (,name (:include dbus-codegen-skeleton)
				(:constructor nil)
				(:constructor ,(intern (format "%s--make" name))
					      (bus service path interface)))
	     ;; Slots for cached property values.
	     ,@(mapcar
		(lambda (property)
		  (intern (car property)))
		properties))

	   ;; Define a constructor.
	   (defun ,(intern (format "%s-create" name))
	       (bus service path &rest args)
	     ,(format "Create a new D-Bus skeleton for %s.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE."
		  interface)
	     (let ((skeleton (,(intern (format "%s--make" name))
			      bus service path ,interface)))
	       (apply #'dbus-register-service bus service args)
	       ;; Register method handlers.
	       ,@(mapcar
		  (lambda (method)
		    `(push (dbus-register-method
			    bus service path ,interface
			    ,(nth 1 method)
			    (lambda (&rest args)
			      (apply #',(intern (format "%s-handle-%s-method"
							name (car method)))
				     skeleton args)))
			   (,(intern (format "%s-registration-list" name))
			    skeleton)))
		  methods)
	       skeleton))

	   ;; Define a destructor.
	   (cl-defmethod ,(intern (format "%s-destroy" name)) (skeleton)
	     "Destroy a D-Bus skeleton SKELETON.")

	   (cl-defmethod ,(intern (format "%s-destroy" name)) ((skeleton ,name))
	     (dolist (registration (,(intern (format "%s-registration-list"
						     name))
				    skeleton))
	       (dbus-unregister-object registration))
	     (setf (,(intern (format "%s-registration-list" name)) skeleton)
		   nil)
	     (dbus-unregister-service bus service))

	   ;; Emit common helper functions.
	   ,@(dbus-codegen--emit-signal-defgeneric name signals transform-name)
	   ,@(dbus-codegen--emit-send-signal name signals transform-name)
	   ,@(dbus-codegen--emit-register-signal name signals)
	   ;; Emit helper functions for skeleton.
	   ,@(dbus-codegen--emit-method-defgeneric name methods transform-name)
	   ,@(dbus-codegen--emit-register-method name methods)
	   ,@(dbus-codegen--emit-register-property name properties))))))

;;;; ChangeLog:

;; 2015-03-27  Daiki Ueno	<ueno@gnu.org>
;; 
;; 	Merge commit 'f28382c9577b50dc7250c0f55c8b34270f0ba691' as
;; 	'packages/dbus-codegen'
;; 


(provide 'dbus-codegen)

;;; dbus-codegen.el ends here
