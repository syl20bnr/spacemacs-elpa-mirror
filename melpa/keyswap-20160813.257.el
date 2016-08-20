;;; keyswap.el --- swap bindings between key pairs

;; Copyright (C) 2016 Matthew Malcomson

;;; Licence:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Matthew Malcomson <hardenedapple@gmail.com>
;; Maintainer: Matthew Malcomson <hardenedapple@gmail.com>
;; Created: 23 Jul 2016
;; Keywords: convenience
;; Version: 0.1.1
;; Package-Version: 20160813.257
;; Package-X-Original-Version: 20160730.1620
;; URL: http://github.com/hardenedapple/keyswap.el
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; keyswap is a minor mode that allows swapping the commands of two keys.
;; It comes with a default set of keys to swap of the number keys and their
;; shifted counterparts along with the '-' and '_' key.
;; This is different to the function `keyboard-translate' as swaps may be done
;; on a per-major-mode basis.
;; This is generally useful in programming languages where symbols are more
;; often used than numbers.
;;
;; To use keyswap-mode, make sure this file is in the Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require keyswap:
;;   (require 'keyswap)

;; To toggle between swapped and not-swapped sets of keys, use the command
;; (keyswap-mode) or M-x keyswap-mode
;;
;; Keys are swapped on a major-mode basis.
;; If you change the swapped keys in one buffer these changes are propagated to
;; all other major modes.
;;
;; The set of keys to swap is stored in the buffer local `keyswap-pairs'
;; variable.
;; This variable is an alist of vectors ready for passing to `define-key' that
;; should be swapped when `keyswap-mode' is turned on.
;; Its default is to swap all number keys and their shifted alternatives, along
;; with the - and _ keys.
;;
;; In order to change the current swapped keys one should modify this list with
;; `keyswap-add-pairs' or `keyswap-remove-pairs', and then run
;; `keyswap-update-keys' like so
;; (keyswap-add-pairs ?\: ?\;)
;; (keyswap-remove-pairs ?\- ?\_)
;; (keyswap-update-keys)
;;
;; Without running `keyswap-update-keys' the changes in `keyswap-pairs' will not
;; be propagated into the action of `keyswap-mode'.
;;
;; There are some provided hooks for common modifications of `keyswap-pairs'
;; that modify the pairs to swap and call `keyswap-update-keys' accordingly.
;; These are `keyswap-include-braces' to swap [ and ] with { and },
;; `keyswap-include-quotes' to swap ' with ", `keyswap-tac-underscore-exception'
;; to *not* swap - and _, and finally `keyswap-colon-semicolon' to swap : and ;.
;;
;; It is recommended to turn on `keyswap-mode' by default in programming buffers
;; with
;; (add-hook 'prog-mode-hook 'keyswap-mode)
;;
;; and then add modifications for each major-mode you desire accordingly, e.g.
;;
;; (with-eval-after-load 'cc-vars
;;   (add-hook 'c-mode-common-hook 'keyswap-include-quotes))
;;
;; (with-eval-after-load 'lisp-mode
;;   (add-hook 'emacs-lisp-mode-hook 'keyswap-tac-underscore-exception)
;;   (add-hook 'lisp-mode-hook 'keyswap-tac-underscore-exception))
;;
;; To toggle between having keys swapped and not, just turn on and off
;; `keyswap-mode'.
;;
;; Some common packages like `paredit' change bindings on some keys.
;; In order to keep the `keyswap-mode' mappings in sync it is recommended you
;; add `keyswap-update-keys' to the relevant hook.
;; (add-hook 'paredit-mode-hook 'keyswap-update-keys)
;;
;; One package that requires more than the normal amount of configuration is the
;; `wrap-region' package.
;; Because this changes the bindings on certain keys, it requires
;; `keyswap-update-keys' to be in its hook.
;; (add-hook 'wrap-region-mode-hook 'keyswap-update-keys)
;; Due to the way that it falls back to inserting a single character when the
;; region is not active, you need an advice around `wrap-region-fallback' that
;; ensures `keyswap-mode' is not on at the time it is called.
;;   (defadvice wrap-region-fallback (around keyswap-negate protect activate)
;;     "Ensure that `keyswap-mode' is not active when
;;     `wrap-region-fallback' is getting called."
;;     (let ((currently-on keyswap-mode))
;;       (when currently-on (keyswap-mode 0))
;;       ad-do-it
;;       (when currently-on (keyswap-mode 1))))
;;
;;
;; Though the conveniance functions don't account for key chords (e.g. C-x j r),
;; the utility functions work well with them.
;; Hence you can manually swap these with code similar to the below.
;; (push (cons [?\ ?r ?j] [?\ ?r ?\ ]) keyswap-pairs)
;; (keyswap-update-keys)
;;
;;
;; In order to have the same swapped keys in `isearch-mode' as in the buffer
;; you're currently editing, you can add
;; (add-hook 'isearch-mode-hook 'keyswap-isearch-start-hook)
;; into your config.
;;
;; To have swapped keys when using `avy', you can have
;; (with-eval-after-load 'avy (keyswap-avy-integrate))
;; in your config.


;;; Code:

;; TODO XXX Do I want to add full support for key chords instead of just single
;; keys?

(require 'cl-lib)
(require 'edmacro)

(defconst keyswap-command-docstring "CHAR COMMAND WRAPPER ")

(defun keyswap--print-key-vector (keyvec)
  (edmacro-format-keys
   (apply #'concat (mapcar (lambda (arg) (format "%c" arg)) keyvec))))

;; We can't just use a keyboard macro as we want to swap the commands between
;; two keys. A keyboard macro on both would put us into an infinite loop.
(defun keyswap--equivalent-command (key command)
  "Return wrapped COMMAND so it's called as if bound to KEY.

Generate a useful docstring for this command so it may be identified both
programmatically and from inspection.

Neither KEY nor COMMAND may be `nil'. "
  ;; I need to create the form when this function is used in order to
  ;; programatically create the docstring for the `lambda' form.
  ;;
  ;; In order to keep the value of command in the `lambda' form that
  ;; results I either need lexical binding or to store the value in a hidden
  ;; symbol.
  ;;
  ;; Using lexical binding requires evaluating the `lambda' form in the current
  ;; lexical environment, which means we have to create the form and then pass
  ;; it to `eval', while using a hidden symbol doesn't require such indirection.
  ;;
  ;; On the other hand, using lexical binding and creating a closure, means I
  ;; can compare two functions created with `keyswap--equivalent-command' by
  ;; using the `equal' function making things much easier to test.
  ;;
  ;; While in later versions of emacs the `eval' function can take a lexical
  ;; environment, I know that this can't be done on at least emacs 24.3.1, and
  ;; so am using the more compatible call method.
  (eval
   `(let ((key key) (old-binding command)
          (current-key (aref key (- (length key) 1))))
      (lambda (&optional arg return-command)
        ,(concat keyswap-command-docstring "\""
                 (keyswap--print-key-vector key)
                 "\""
                 "\n\nWrapping the command\n\n"
                 (format "%S" command)
                 "\n\nWith the key vector\n\n"
                 (format "%S" key))
        (interactive "p")
        (if return-command
            old-binding
          ;; Set `last-command-event' so that `self-insert-char' behaves as
          ;; expected.
          ;; Attempt to use `call-interactively' to set the values that
          ;; `this-command-keys-vector' will find, but it doesn't seem to be
          ;; working at the moment, and I can't figure out why.
          (let ((last-command-event current-key))
            (call-interactively old-binding nil key)))))
   t))

(defun keyswap--unbound-key-mock ()
  "Mock function to pretend that this key is undefined.

This function is only designed for `keyswap-mode', to pretend to
swap the action between keys where one is unbounded."
  (interactive)
  ;; TODO XXX -- This only works because `call-interactively' doesn't actually
  ;; change what is returned from the function `this-command-keys-vector'.
  ;; If/When that's fixed, I'll have to figure something else out.
  (error "%s is undefined" (keyswap--print-key-vector (this-command-keys-vector))))

;; This function is a little misleadingly named.
;; When it is called on a normal command it does in fact return the equivalent
;; command that would be run were that key to be pressed.
;; However, when it's run on a function that it created it returns the original
;; function.
;; This could then be bound to a third key, and that key would then act
;; differently to how the original one did (e.g. with `self-insert-command').
(defun keyswap-equivalent-binding (key &optional keymap)
  "Return a command that is equivalent to pressing KEY.

If the optional argument KEYMAP is provided, wrap the command
bound to KEY in KEYMAP. Otherwise wrap whatever command would be
run if KEY were pressed in the current state of the editor.

If the command that would be run is a `lambda' function created
by this function then return the original command that was
wrapped.

NOTE -- this function is hacky but useful.

I use the `documentation' of a function to recognise whether a
command has been created using this function, the docstring would
then begin with `keyswap-command-docstring'.

Otherwise create a `lambda' function that runs that command under
the false environment where `last-command-event' is KEY

When asked to find the equivalent binding of `nil', we create an
artificial function that complains about that binding being
unavailable.
"
  (let* ((current-binding
          (let ((direct-command
                 (if keymap (lookup-key keymap key) (key-binding key))))
            (or (command-remapping direct-command nil keymap)
                direct-command
                'keyswap--unbound-key-mock)))
         (current-docstring (documentation current-binding)))
    (if (eq current-binding 'keyswap--unbound-key-mock)
        'keyswap--unbound-key-mock
        (if (and (listp current-binding)
                 current-docstring
                 (string-prefix-p keyswap-command-docstring current-docstring))
            (funcall current-binding nil t)
          (keyswap--equivalent-command key current-binding)))))

(defun keyswap-swap-these (left-key right-key keymap &optional base-map)
  "Puts swapped bindings of LEFT-KEY and RIGHT-KEY into KEYMAP.

If the optional BASE-MAP is defined, only search for original
bindings there.  Otherwise, original bindings are found from what
would be run in the current editor state.

LEFT-KEY and RIGHT-KEY should be two objects valid in a call to
`key-binding'.

If a mapping has a normal function, we bind the other key to a
`lambda' function that calls the original with a masked
`last-command-event' pretending to be the first key.

These `lambda' functions are marked by their documentation
string (*very* hacky -- I know), and if this documentation string
is noticed, they are called with arguments so they return their
wrapped function."
  (let ((left-function (keyswap-equivalent-binding left-key base-map))
        (right-function (keyswap-equivalent-binding right-key base-map)))
    (define-key keymap left-key right-function)
    (define-key keymap right-key left-function)))

;;;###autoload
(defvar-local keyswap-pairs
  (mapcar (lambda (pair) (cons (vector (car pair)) (vector (cdr pair))))
          (list '(?1 . ?!) '(?2 . ?@) '(?3 . ?#) '(?4 . ?$) '(?5 . ?%)
                '(?6 . ?^) '(?7 . ?&) '(?8 . ?*) '(?9 . ?\() '(?0 . ?\))
                '(?- . ?_)))
  "Pairs of characters to swap in `keyswap-mode'.")

(defun keyswap-swapped-keymap ()
  "Create a swapped keymap for this buffer.
Take the keys currently active, and create a keymap that takes
inverts the bindings of those key pairs in `keyswap-pairs'.
Returns the resulting keymap with these bindings, but doesn't do
anything other than create and return the keymap."
  (let ((return-map (make-sparse-keymap)))
    (dolist (key-pair keyswap-pairs return-map)
      (keyswap-swap-these (car key-pair) (cdr key-pair) return-map))))

;;;###autoload
(define-minor-mode keyswap-mode
  "Minor mode for programming where number keys are swapped with their shifted
counterparts.

This effectively makes the keyboard a \"programmers\" version of the keyboard.

In order to to have a different set of keys swapped for each
buffer, I abuse `minor-mode-overriding-map-alist' and never
actually have a minor mode map in the main `minor-mode-map-alist'
variable.

When this mode is activated, it first checks whether the current
buffer already has a local overriding keymap in
`minor-mode-overriding-map-alist', and if so does nothing but
activate that keymap.

If there is no relevant keymap in
`minor-mode-overriding-map-alist' it finds the mappings in the
`current-buffer' that relate to the keys to be swapped in
`keyswap-pairs', creates a keymap that has functionaly swapped
these keys, and stores that as the keymap in `minor-mode-overriding-map-alist'
to be used in all future invocations of this minor-mode.

If this minor mode is activated before others that change the
maps, then the bindings on each keys will not represent the
bindings of those other minor modes.  In this case
`keyswap-update-keys' should be called to update the bindings on
the current set of `keyswap-pairs'.
"
  nil
  " keyswap"
  nil
  ;; Body is executed every time the mode is toggled
  ;; If keyswap-mode is not in the `minor-mode-overriding-map-alist' variable,
  ;; then create a new map with `keyswap-swapped-keymap', and add that to the list of
  ;; `minor-mode-overriding-map-alist'.
  (unless (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (push (cons 'keyswap-mode (keyswap-swapped-keymap))
          minor-mode-overriding-map-alist)))

(defvar keyswap-update-keys-hook nil
  "Hook run just after calling `keyswap-update-keys'.")

;;;###autoload
(defun keyswap-update-keys ()
  "Update the buffer-local keymap currently used for function `keyswap-mode'."
  (interactive)
  (when (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (let ((currently-on keyswap-mode))
      ;; Make sure to turn `keyswap-mode' off when calculating the swapped
      ;; keymap, otherwise we swap back elements previously swapped.
      (when currently-on (keyswap-mode 0))
      (setf (cdr (assoc 'keyswap-mode minor-mode-overriding-map-alist))
            (keyswap-swapped-keymap))
      (when currently-on (keyswap-mode t)))
    (run-hooks 'keyswap-update-keys-hook)))

(defun keyswap--act-on-pairs (action-fn keyswaps)
  "Call ACTION-FN on successive pairs of KEYSWAPS."
  (cl-loop for remaining-keyswaps on keyswaps by #'cddr
        do (let ((left-key (car remaining-keyswaps))
                 (right-key (cadr remaining-keyswaps)))
             (if (and left-key right-key)
                 (funcall action-fn (cons (vector left-key)
                                          (vector right-key)))))))

;;;###autoload
(defun keyswap-add-pairs (&rest keyswaps)
  "Add KEYSWAPS into `keyswap-pairs'."
  (keyswap--act-on-pairs
   (lambda (pair)
     (setq-local
      keyswap-pairs
      (cl-adjoin pair keyswap-pairs
                 :test #'(lambda (left right)
                           (or (equal left right)
                               (equal (cons (cdr left) (car left)) right))))))
   keyswaps))

;;;###autoload
(defun keyswap-remove-pairs (&rest keyswaps)
  "Remove KEYSWAPS from `keyswap-pairs'."
  (keyswap--act-on-pairs
   (lambda (pair)
     (setq-local keyswap-pairs
                 (remove (cons (cdr pair) (car pair))
                         (remove pair keyswap-pairs))))
   keyswaps))

;;;###autoload
(defun keyswap-set-pairs (&rest keyswaps)
  (setq-local keyswap-pairs nil)
  (apply #'keyswap-add-pairs keyswaps))

;;;###autoload
(defun keyswap-include-braces ()
  "Hook to make function `keyswap-mode' swap {,[, and },]."
  (keyswap-add-pairs ?\[ ?\{   ?\] ?\} )
  (keyswap-update-keys))

;;;###autoload
(defun keyswap-include-quotes ()
  "Hook to make function `keyswap-mode' swap \" and '."
  (keyswap-add-pairs ?\' ?\")
  (keyswap-update-keys))

;;;###autoload
(defun keyswap-tac-underscore-exception ()
  "Hook to make function `keyswap-mode' ignore - and _."
  (keyswap-remove-pairs ?- ?_)
  (keyswap-update-keys))

;;;###autoload
(defun keyswap-colon-semicolon ()
  "Hook to make function `keyswap-mode' swap : and ;."
  (keyswap-add-pairs ?: ?\;)
  (keyswap-update-keys))


;; `isearch-mode' is a little tricky to set up.
;;
;; Instead of adding a keymap somewhere it makes `isearch-mode-map' the
;; `overriding-terminal-local-map'.  This means we can't put the `keyswap-mode'
;; mappings in any variable to override what `isearch-mode' has defined.
;;
;; Instead, we overwrite the mappings in `isearch-mode-map' and put them back
;; when `isearch-mode' finishes.
;;
;; When `isearch-mode' starts, we check whether `keyswap-mode' is currently
;; active and if so iterate through all keys in `keyswap-pairs' replacing those
;; keys directly in `isearch-mode-map'.
;;
;; When `isearch-mode' finishes, we do the same thing, using the fact that
;;`keyswap-equivalent-binding' returns the original wrapped command when called
;; on a `lambda' function it created itself.
;;
;; Hence after our hook has been called twice, the bindings in
;;`isearch-mode-map' are the same as they were originally.
;;
;; XXX TODO -- Get pairs to swap from currently shifted keymap instead of
;; `keyswap-pairs'.
;; `keyswap-pairs' is local to one buffer, but all observable actions are
;; across all buffers in the same major-mode.
;; I should have the same split when propagating changes out to
;; `isearch-mode', in that the keys that get swapped are the same keys that
;; are already swapped in the current buffer.
;;
;; XXX TODO -- Account for someone updating `keyswap-pairs' while in
;; `isearch-mode' (i.e. wanting to change the mappings while in
;; `isearch-mode').
;;
;; XXX TODO -- Account for someone turning `keyswap-mode' off or on while in
;; `isearch-mode'.
;;
;; These last two shouldn't really happen, whenever someone does anything
;; non-standard in `isearch-mode' it just takes them out of that mode and puts
;; them back in the normal buffer.  If anyone is doing anything special with the
;; combination of these modes then they may not want automatic updating (who am
;; I to assume).
;; If I do want to automatically update the `isearch-mode' stuff upon calling
;;`keyswap-update-keys' then I just need to add `keyswap-isearch-start-hook' and
;;`keyswap-isearch-end-hook' to `keyswap-update-keys' hook and that should work.
;;
(defvar-local keyswap-isearch-swapped-pairs nil
  "An indicator of whether the `isearch-mode-map' keymap has been
  swapped to match the current buffers swapped keymap.

This buffer local variable contains the pairs that were swapped
in `isearch-mode-map' when `isearch-mode' was activated.")

(defun keyswap-isearch-end-hook ()
  "Remove keys swapped with `keyswap-isearch-start-hook' from
  `isearch-mode-map'."
  (when keyswap-isearch-swapped-pairs
    (dolist (key-pair keyswap-isearch-swapped-pairs)
      (keyswap-swap-these (car key-pair) (cdr key-pair)
                          isearch-mode-map isearch-mode-map))
    (setq-local keyswap-isearch-swapped-pairs nil)
    (remove-hook 'isearch-mode-end-hook 'keyswap-isearch-end-hook)))

(defun keyswap-isearch-start-hook ()
  "Hook to keep the same toggled keys from `keyswap-mode' in the
current buffer when searching with `isearch-mode'."
  (when keyswap-mode
    (dolist (key-pair keyswap-pairs)
      (keyswap-swap-these (car key-pair) (cdr key-pair)
                          isearch-mode-map isearch-mode-map))
    (setq-local keyswap-isearch-swapped-pairs (copy-sequence keyswap-pairs))
    ;; I have to remove the swapped keys from `isearch-mode-map' because it is an
    ;; editor global map, and I need to not affect other modes.
    ;; I can't just have `keyswap-isearch-end-hook' stored in
    ;; `isearch-mode-end-hook' because other packages do similar to me by
    ;; storing the original `isearch-mode-map' and replacing it in a function
    ;; placed in `isearch-mode-end-hook'.
    ;;
    ;; Because they add this hook at the same time they're storing the original
    ;; value, and they add the hook to the start of `isearch-mode-end-hook', in
    ;; order to call our teardown hooks in the correct order I have to do the same.
    ;; (either that or get them to agree on appending our hooks to
    ;; `isearch-mode-end-hook').
    (add-hook 'isearch-mode-end-hook 'keyswap-isearch-end-hook)))


;; TODO XXX -- Add support for avy and ace-jump (may just be finding the correct
;; hook, it may be something else)

;; Simply make a function out of the `keyswap-pairs' in the buffer, and assign
;; that function to `avy-translate-char-function'.

;; `avy-translate-char-function' swaps the characters you use when selecting a
;; choice, not the characters used when telling avy which letter to start with.
(defun keyswap--avy-char-translate (original-key)
  (if (and keyswap-mode keyswap-pairs)
      (let ((key-pair
             (assoc (vector original-key)
                    (append keyswap-pairs
                            (mapcar (lambda (val) (cons (cdr val) (car val)))
                                    keyswap-pairs)))))
        (if key-pair (aref (cdr key-pair) 0) original-key))
    original-key))

(defun keyswap-avy-integrate ()
  "Set it up so that keys are swapped when searching for a char and when
  selecting an option in `avy'."
  ;; This is a little dangerous -- I put an advice around `read-char', in order
  ;; to swap the characters that are read as arguments in many `avy' functions.
  ;;
  ;; I don't know whether this will create any adverse affects because I don't
  ;; know where else this function is used.
  ;;
  ;; On the other hand, if the goal is integration, then this function will only
  ;; help with that, so it may be a good thing.
  (advice-add 'read-char :filter-return 'keyswap--avy-char-translate
              '((name . keyswap-read-char-transform)))
  ;; Make it so that letters to jump to are also swapped according to
  ;; `keyswap-pairs'
  (setq avy-translate-char-function #'keyswap--avy-char-translate))

(defun keyswap-avy-remove-integration ()
  "Remove the settings such that `keyswap-mode' affects `avy'."
  (advice-remove 'read-char 'keyswap-read-char-transform)
  (setq avy-translate-char-function #'identity))

(provide 'keyswap)

;;; keyswap.el ends here
