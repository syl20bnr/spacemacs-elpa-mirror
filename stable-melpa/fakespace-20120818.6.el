;;; fakespace.el --- fake namespaces with defpackage

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/elisp-fakespace
;; Package-Version: 20120818.6
;; Version: 1.0

;;; Commentary:

;; Provides fake namespaces through a very rudimentary `defpackage',
;; familiar to Common Lisp users. The developer declares which symbols
;; are to be exported outside the package. All other symbols are
;; hidden away (by `end-package') where no other package can trample
;; on them.

;; See example.el for an example of using this package.

;; It works by comparing the symbol table before any declarations to
;; the symbol table afterward, then uninterning any symbols that were
;; created and not exported. This will work even on byte-compiled
;; files. In fact, everything is determined at compile-time, so there
;; is practically no load-time penalty from using these fake
;; namespaces.

;; Generally you will not want to actively use these fake namespaces
;; during development because there is no `in-package' function. You
;; will always been in the main namespace unable to access your
;; private functions. Later, when you're finishing up and want to test
;; out your namespace, make sure your code is unloaded from Emacs
;; (i.e. none of your symbols is in the symbol table) before applying
;; your namespace. The same applies to compilation: the package must
;; not be loaded before compilation or the `defpackage' will not hide
;; any symbols.

;; Unfortunately, due to the downright ugly implementation of obarrays
;; in Emacs, it's currently not possible to implement
;; `in-package'. Uninterned symbols cannot be added back to the global
;; obarray (or any other obarray). Symbols are invisibly chained as
;; linked lists in the obarray so it's not possible to put a symbol
;; into two obarrays at the same time -- the chains would
;; conflict. Not only is the required functionality not provided
;; (intentionally), trying to hack it in would break everything.

;;; Code:

(defun fakespace--atom-list (&optional ob)
  "Return given obarray OB as a list. Defaults to obarray."
  (let ((lst ()))
    (mapatoms (lambda (s) (push s lst)) ob)
    lst))

(defun fakespace--atom-difference (a b)
  "Like set-difference, but, for performance reasons, requires
specially formed lists (i.e. from `fakespace--atom-list'). Returns
items that are in B and not A."
  (let ((diff))
    (while (and (not (null a)) (not (null b)))
      (while (not (eq (car a) (car b)))
        (push (car b) diff)
        (setq b (cdr b)))
      (setq a (cdr a))
      (setq b (cdr b)))
    diff))

(defvar fakespace--obarray ()
  "Snapshot of the obarray before interning the package's symbols.")

(defmacro defpackage (name &rest args)
  (dolist (arg args)
    (let ((type (car arg)))
      (cond ((eq type :exports) t) ; interning the symbols is enough
            ((eq type :use) (mapc (lambda (s) (require s)) (cdr arg))))))
  (setq fakespace--obarray (fakespace--atom-list) )
  `(provide (quote ,name)))

(defmacro end-package ()
  (cons 'progn
        (mapcar (lambda (s) `(unintern (quote ,s) nil))
                (fakespace--atom-difference fakespace--obarray
                                            (fakespace--atom-list)))))

(provide 'fakespace)

;;; fakespace.el ends here
