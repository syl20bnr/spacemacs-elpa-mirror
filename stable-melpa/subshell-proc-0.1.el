;;; subshell-proc.el --- Functions for working with comints
;; Author: Andrew Mains
;; URL: https://github.com/andrewmains12/subshell-proc
;; Package-Version: 0.1
;; Version: 0.1
;;
;; Sample usage:
;;
;; ;;In elisp code
;; (defproc "noir-server" "lein" '("run")
;; "Run a noir server")  
;;
;; While editing:
;;   M-x run-proc 
;;     bash
;;   will bring up a comint running bash  
;;
;;

(require 'cl)


(defmacro defproc (fn-name command command-args &optional docstring)
  "Defines an interactive function which creates a comint subprocess using command"
  (let* ((fn-sym (intern fn-name)))

    `(defun ,fn-sym (&rest extra-args)       
       ,docstring
       (interactive)
       (funcall 
        (make-proc-run-fn ,command ,command-args ,(format "*%s*" fn-name)))
     )))


(defun make-proc-run-fn (command command-args &optional buffer-name)
  (lexical-let ((buffer-name buffer-name)
                (command command)
                (command-args command-args)
                )
    (function (lambda (&rest extra-args)                
                (let* ((buffer-name (or buffer-name (format "*%s*" command)))
                       (buffer (get-buffer-create buffer-name))
                       )
                  (pop-to-buffer buffer)
                  (apply 'make-comint-in-buffer 
                         (append (list buffer-name buffer command nil) command-args))
                  )))))


(defun run-proc (command &optional buffer-name)
  (interactive "MCommand to run: ")
  (let ((command-list (split-string command)))
    (funcall (make-proc-run-fn (car command-list) (cdr command-list) buffer-name))))

(provide 'subshell-proc)


;;; subshell-proc.el ends here
