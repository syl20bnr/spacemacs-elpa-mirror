;;; exsqlaim-mode.el --- Use variables inside sql queries -*- lexical-binding: t -*-

;; Author: Ahmad Nazir Raja <ahmadnazir@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20170418.124
;; Package-Requires: ((s "1.10.0"))
;; URL: https://github.com/ahmadnazir/exsqlaim-mode

;;; Commentary:
;;
;; Variables can be defined as:
;;
;; @db   = `test`
;; @id   =  1234
;; @name = "John Doe"
;;
;; and the query can be:
;;
;; SELECT * FROM @db.users WHERE id = @id OR name = @name;
;;
;; NOTE: The parameters are not escaped as the values are define by the user and
;; hence it is a trusted source. However, functionality for evaluating a value
;; will probably be added in the future which would require the values to be
;; escaped (hence, a breaking change is expected in the future).

(require 's)
(require 'sql)

;;; Code:

(defconst exsqlaim-mode--regexp-stmt-var-assign "^\\(@[^@ ]+\\)[ \t]*=[ \t]*\\(.*\\)$")

;; Inspired and modified from restclient.el: restclient-find-vars-before-point
(defun exsqlaim-mode--find-vars-before-point ()
  "Find the mapping between variables and their values before point."
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp exsqlaim-mode--regexp-stmt-var-assign bound t)
        (let ((name (match-string-no-properties 1))
              (value (match-string-no-properties 2)))
          (setq vars (cons (cons name value) vars))))
      vars)))

(defun exsqlaim-mode--get-vars ()
  "Get a map of all variables and values."
  (cons
   '(";"."\\p;") ;; echo the query to the terminal
   (exsqlaim-mode--find-vars-before-point)))

(defun exsqlaim-mode--get-raw-query (start end)
  "Get the raw query with variables.
Argument START Point where the query starts.
Argument END Point where the query ends."
  (interactive "r")
  (buffer-substring-no-properties start end))

(defun exsqlaim-mode--build-query (query vars)
  "Build the sql QUERY using defined variables.
Argument VARS Map of variables and values."
  (s-replace-all vars query))

(defun exsqlaim-mode--build-query-at-point()
  "Build the query to be executed at point"
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (exsqlaim-mode--build-query (exsqlaim-mode--get-raw-query start end) (exsqlaim-mode--get-vars))
    ))

(defun exsqlaim-mode--update-query-at-point ()
  "Update the query at point with the values from the variables."
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (let ((query (exsqlaim-mode--build-query-at-point)))
      (kill-region start end)
      (insert query))))

(defun exsqlaim-mode--send ()
  "Build a query at point and send it to the sql process."
  (interactive)
  (sql-send-string (exsqlaim-mode--build-query-at-point))
  )

;; Minor Mode
(defvar exsqlaim-mode-map (make-sparse-keymap)
  "Exsqlaim-mode keymap.")

(define-key exsqlaim-mode-map
  (kbd "C-c C-c") 'exsqlaim-mode--send)

(define-key exsqlaim-mode-map
  (kbd "C-c C-i") 'exsqlaim-mode--update-query-at-point)

(defconst exsqlaim-mode--regexp-var "@[^@= \n\"'\.]+")

(defun exsqlaim-mode--fontify(mode)
  "Fontify for mode."
  (interactive)
  (font-lock-add-keywords mode `((,exsqlaim-mode--regexp-var 0 font-lock-variable-name-face t))))

(defun exsqlaim-mode--unfontify(mode)
  "Unfontify for mode."
  (font-lock-remove-keywords mode `((,exsqlaim-mode--regexp-var 0 font-lock-variable-name-face t))))

;;;###autoload
(define-minor-mode exsqlaim-mode
  "Exsqlaim mode" nil " Exsqlaim" exsqlaim-mode-map
  (progn ()
         (if exsqlaim-mode
             (exsqlaim-mode--fontify nil)
           (exsqlaim-mode--unfontify nil))
         (font-lock-fontify-buffer)))

(provide 'exsqlaim-mode)

;;; exsqlaim-mode.el ends here
