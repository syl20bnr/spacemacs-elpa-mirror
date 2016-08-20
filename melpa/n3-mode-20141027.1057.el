;;; n3-mode.el --- mode for Notation 3

;; Version: 0.2
;; Package-Version: 20141027.1057

;; Copyright (c) 2003-2007 Hugo Haas <hugo@larve.net>
;; re-worked and re-published by kurtjx (c) 2010 <kurtjx@gmail.com>

;; For documentation on Notation 3, see:
;; http://www.w3.org/DesignIssues/Notation3.html

;;; Comentary:

;; Goals:
;; - sytax highlighting
;; - completion
;; - indentation

;; What it does now:
;; - Syntax highlighting
;; - comment/uncomment block with M-;

;;; Code:

;; the command to comment/uncomment text
(defun n3-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
     (comment-dwim arg)))

;; run a process over a buffer
(defun n3-process-buffer (program &optional infile destination &rest args)
  "Run a process over a buffer."
  (let* ((destination (or destination "*n3-process-output*"))
         (in  (get-buffer infile))
         (buf (get-buffer-create destination))
         output prepoint)
    (save-excursion
      (set-buffer infile)
      (apply 'call-process-region
             (append
              (list (point-min) (point-max) program nil destination t) args))
      (set-buffer buf)
      (setq output (buffer-substring (point-min) (point-max)))
      (kill-buffer buf)
      )
    output))

;; run a process over a file
(defun n3-process-file (program &optional infile destination &rest args)
  "Run a process over a file."
  (let* ((destination (or destination "*n3-process-output*"))
         (buf (get-buffer-create destination))
         output prepoint)
    (when (not (file-readable-p infile))
      (error 
       (concat "Attempt to run process over an unreadable file:" infile)))
    (message infile)
    (save-excursion
      (set-buffer buf)
      (setq prepoint (point))
      (let ((code (apply 'call-process
                          (append (list program infile destination nil) args))))
        (when (and (/= code 0) (/= code 2))
          (warn (format "Process %s exited with %d" program code))))
      (setq output (buffer-substring prepoint (point-max)))
      (kill-buffer buf)
      )
    output))

(defun n3-process-file-or-buffer
  (program &optional infile destination &rest args)
  "Run a process over either a filename or a buffer, depending on
what it is. Returns the process's output as a string."

  ; if it is a buffer but not a readable file, then warn
  ; if it is a string but not a readable file, then error
  (let* ((buf (or (get-buffer infile) (current-buffer)))
         (filename (or (buffer-file-name buf) infile)))
    (cond ((file-readable-p filename)
           (apply 'n3-process-file
                  (append (list program filename destination) args)))
          ((or buf infile)
           (display-warning :error "An unsaved buffer may not parse correctly.")
           (apply 'n3-process-buffer
                  (append (list program buf destination) args)))
          (t (display-warning
              :error
              "I need a filename (preferred) or buffer to work with.")))))

;; 
(defun n3-make-sparql-query (string)
  "Return a SPARQL query that will return subject-literal pairs."
  (interactive "sValue: ")
  ; XXX this will fail if you put quotation marks in it, obvs
  (concat "SELECT DISTINCT ?s ?o "
          "WHERE { ?s ?p ?o FILTER (isLITERAL(?o) && regex(str(?o), \"^.*"
          string ".*$\", \"i\")) }"))

;;
(defun n3-get-subject-list (literal &optional buffer)
  "Return an alist of the form (subject . literal) for subjects
that match a given literal."
  (interactive "sValue: \nbBuffer: ")
  (let* ((my-buffer (current-buffer))
         (work-buffer (get-buffer (or buffer my-buffer)))
         (roqet (or (executable-find "roqet")
                    (error "Can't find roqet executable!")))
         (output (n3-process-file-or-buffer
                   roqet work-buffer nil ;"-W" "100" "-d" "debug"
                   "-q" "-D" "file:/dev/stdin" "-e"
                   (n3-make-sparql-query literal)))
         (end (length output))
         (pos 0)
         options)
    ; return an alist of the match pairs
    (while
        (string-match
         "result: \\[s=uri\\(<[^>]*>\\), *o=string(\"+\\(.*?\\)\"+[^\"]*)\\]"
         output pos)
      (setq pos (match-end 2))
      ;(message (match-string 1 output))
      ; this will build the list backwards but whatever
      (setq options
            (cons (cons (match-string 1 output) (match-string 2 output))
                  options)))
    options))

;; 
(defun n3-prepare-options (options)
  "Given an alist of strings, return a string of enumerated options."
  (let ((nargs (length options)) (out "") (pos 1))
    (while options
      (setq out (concat out (format "%d) %s\n" pos (cdr (car options)))))
      (setq options (cdr options))
      (setq pos (1+ pos)))
    out))

;; not sure how else to curry an interactive function
(defun n3-make-select-option (options)
  "Generate an interactive command to choose from multiple subjects."
  (lambda (which)
    (interactive (list (read-string (n3-prepare-options options))))
    (let ((which (string-to-number which)))
      (when (= which 0)
        (setq which 1)
        (message "Using the first element as a default"))
      (cond ((> which (length options))
             (message (format "Index %d longer than list (%d)"
                              which (length options))))
            (t (car (nth (- which 1) options)))))))

;; a more generic version
(defun n3-subject-for (literal &optional buffer)
  "Return the subject found for the given literal from the given buffer."
  (interactive "sLiteral: \nbBuffer: ")
  (let ((options (n3-get-subject-list literal buffer)))
    (if (null options)
        (progn (message (format "No results found for %s" literal)) nil)
      (if (= (length options) 1)
          (car (car options))
        (call-interactively (n3-make-select-option options))))))

;; 
(defun n3-insert-subject-for-this-buffer (literal)
  "Insert the subject found for the given literal into the current buffer."
  (interactive "sLiteral: ")
  (let ((subject (n3-subject-for literal)))
    (when (not (null subject))
      (insert subject))))

(setq n3-highlights
  '(("\\(@prefix\\)\\>" 1 font-lock-keyword-face t)
    ("\\(a\\)\\>" 1 font-lock-keyword-face t)
    ("\\(\\S-*?:\\)" 1 font-lock-type-face t)
    (":\\(.+?\\)[ ;.]" 1 font-lock-constant-face t)
    ("\\(<.*?>\\)" 1 font-lock-function-name-face t)
    ("\\(\\\".*?\\\"\\)" 1 font-lock-string-face t)
    ; Bug: some trailing characters are highlighted; restricting comments regexp
    ; ("\\(#.*\\)" 1 font-lock-comment-face t)
    ("^\\s-*\\(#.*\\)" 1 font-lock-comment-face t)
    )
)

;;(define-generic-mode 'n3-mode
(define-derived-mode n3-mode fundamental-mode
  ;; setup tab key not working :/
  ;;(setq c-basic-offset 4)

  ;; syntax highlighting
  (setq font-lock-defaults '(n3-highlights))

  ;; mode name
  (setq mode-name "N3")

  ;; modify the keymap M-; comments/uncomments region
  (define-key n3-mode-map [remap comment-dwim] 'n3-comment-dwim)
  (define-key n3-mode-map (kbd "C-c c") 'n3-insert-subject-for-this-buffer)
  ;; comments: “# ...” 
  (modify-syntax-entry ?# "< b" n3-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" n3-mode-syntax-table)

  ;; description
  "Mode for Notation 3 documents."
)

(provide 'n3-mode)
;;; n3-mode.el ends here
