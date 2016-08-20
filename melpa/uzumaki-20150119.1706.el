;;; uzumaki.el --- A simple buffer cycler

;; Copyright (C) 2014 Geyslan G. Bem <geyslan@gmail.com>

;; Author: Geyslan G. Bem <geyslan@gmail.com>
;; URL: http://github.com/geyslan/uzumaki
;; Version: 0.1
;; Package-Version: 20150119.1706
;; Package-X-Original-version: 0.1
;; Keywords: buffer, convenience
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:

;;   Setup:
;;
;;      (require 'uzumaki)
;;      (uzumaki-minor-mode 1)
;;
;;
;;   The default cycling is done through "major-mode" buffers.
;;   But its behavior can be changed:
;;
;;      (uzumaki-set-cycle-mode 'all-no-hidden)
;;   or
;;      (setq uzumaki-cycle-mode 'system)
;;   or
;;      (uzumaki-set-cycle-mode 'regex)
;;      (uzumaki-add-regex "^\\*CEDET.*\\*")
;;      (uzumaki-add-regex "^\\*Back.*\\*")
;;
;;
;;   Is possible to add regexes to never show buffers in accordance
;;   with its criteria.  But remember that these regexes will override
;;   any mode buffers, so if a never-show criteria is found in whatever mode,
;;   the respective buffer will not be showed.
;;   The uzumaki-never-show-list is exclusive.
;;
;;      (uzumaki-add-never-show-regex "^\\*CEDET.*\\*")
;;      (uzumaki-add-never-show-regex "^\\*Back.*\\*")
;;
;;
;;   Without change the configured cycler mode, the user can set other
;;   key-bindings specifying the desired options:
;;
;; (defun my:uzumaki-mode-keys ()
;;   "my keybindings for 'uzumaki-minor-mode'."
;;   (define-key uzumaki-minor-mode-map (kbd "C-c <left>") 'uzumaki-cycle-to-prev-buffer 'all)
;;   (define-key uzumaki-minor-mode-map (kbd "C-c <right>") 'uzumaki-cycle-to-next-buffer 'all)
;;   (define-key uzumaki-minor-mode-map (kbd "C-c <") 'uzumaki-cycle-to-prev-buffer 'all-no-hidden)
;;   (define-key uzumaki-minor-mode-map (kbd "C-c >") 'uzumaki-cycle-to-next-buffer 'all-no-hidden)
;;
;;   ;; If there is a conflicting key, unbind the uzumaki-minor-mode respective one.
;;   (define-key uzumaki-minor-mode-map (kbd "C-,") nil)
;;   (define-key uzumaki-minor-mode-map (kbd "C-.") nil))
;;
;; (add-hook 'uzumaki-minor-mode-hook 'my:uzumaki-mode-keys)
;;
;;
;;   The same can be said about uzumaki code reuse, once the major part of
;;   functions has an &optional argument.

;;; Code:

(require 'ido)
(require 'cl-lib)

(defgroup uzumaki nil
  "Cycle buffers easily using a predefined setup"
  :version "0.1"
  :group 'convenience)

(defcustom uzumaki-cycle-mode 'major-mode
  "Variable that holds the cycler default mode."
  :type '(choice (const :tag "Major mode" :value major-mode)
		 (const :tag "All no hidden" :value all-no-hidden)
		 (const :tag "All" :value all)
		 (const :tag "Hidden" :value hidden)
		 (const :tag "System" :value system)
		 (const :tag "Regex restricted" :value regex))
  :group 'uzumaki)

(defcustom uzumaki-regex-mode-list '()
  "Regex list used by regex mode."
  :type '(alist :key-type regexp)
  :group 'uzumaki)

(defcustom uzumaki-never-show-list '()
  "Exclusive regex list that overrides all modes."
  :type '(alist :key-type regexp)
  :group 'uzumaki)

(defcustom uzumaki-ido-decorations '("< "
				     " >"
				     " | "
				     " | ..."
				     "< "
				     " >"
				     " [No match]"
				     " [Matched]"
				     " [Not readable]"
				     " [Too big]"
				     " [Confirm]")
  "Ido decorations used by uzumaki-minor-mode."
  :type '(alist :key-type string)
  :group 'uzumaki)

(defun uzumaki-set-cycle-mode (mode)
  "Function that set the cycler default MODE."
  (setq uzumaki-cycle-mode mode))

(defun uzumaki-add-regex (regex)
  "Function that add REGEX to 'uzumaki-regex-mode-list'.  REGEX will be evaluated only when in regex mode."
  (add-to-list 'uzumaki-regex-mode-list regex))

(defun uzumaki-add-never-show-regex (regex)
  "Function that add REGEX to 'uzumaki-never-show-list'.  REGEX will be evaluated before all modes."
  (add-to-list 'uzumaki-never-show-list regex))

(defun uzumaki-clear-regex-list ()
  "Function that clear 'uzumaki-regex-mode-list'."
  (setq uzumaki-regex-mode-list '()))

(defun uzumaki-clear-never-show-list ()
  "Function that clear 'uzumaki-never-show-list'."
  (setq uzumaki-never-show-list '()))

(defun uzumaki-buffers (&optional mode)
  "Function that retrieves all MODE buffers that wasn't matched in never-show criteria."
  (unless mode
    (setq mode uzumaki-cycle-mode))
  (let ((this-buf-mode major-mode))
    (sort
     (delq
      nil
      (mapcar (lambda (buf)
		(when (buffer-live-p buf)
		  (catch 'show
		    (let ((bufname (buffer-name buf)))
		      (dolist (regex (sort uzumaki-never-show-list 'string-lessp))
			(if (not (eq nil (string-match-p regex bufname)))
			    (throw 'show nil)))
		      (cl-case mode
			('major-mode
			 (and (eq this-buf-mode (with-current-buffer buf major-mode))
			      bufname))
			('all-no-hidden
			 (and (not (eq nil (string-match-p "^[^ .]" bufname)))
			      bufname))
			('all
			 bufname)
			('hidden
			 (and (not (eq nil (string-match-p "^ " bufname)))
			      bufname))
			('system
			 (and (not (eq nil (string-match-p "^\\*.*\\*" bufname)))
			      bufname))
			('regex
			 (dolist (regex (sort uzumaki-regex-mode-list 'string-lessp))
			   (if (not (eq nil (string-match-p regex bufname)))
			       (throw 'show bufname)))
			 nil))))))
		(buffer-list)))
      'string-lessp)))

(defun uzumaki-cycle-right (list)
  "Cycle LIST to the right by one element."
  (append (last list) (butlast list)))

(defun uzumaki-cycle-left (list)
  "Cycle LIST to the left by one element."
  (append (cdr list) (list (nth 0 list))))

(defun uzumaki-buffers-ordered-by-current (&optional mode)
  "Function that rotate MODE buffers ordering them by the current buffer when the latter is member of former."
  (interactive)
  (let ((buffers (uzumaki-buffers mode)))
    (if (member (buffer-name) buffers)
	(while (not (eq (nth 0 buffers) (buffer-name)))
	  (setq buffers (uzumaki-cycle-right buffers))))
    buffers))

(defun uzumaki-ido-buffers-cycle-by-mode (&optional mode)
  "Function that open MODE buffers via ido cycler."
  (interactive)
  (unless mode
    (setq mode uzumaki-cycle-mode))
  (setq ido-decorations
	(let ((prev-ido-decor ido-decorations))
	  (setq ido-decorations uzumaki-ido-decorations)
	  (switch-to-buffer (ido-completing-read
			     (format "Buffers [%s]: " mode)
			     (uzumaki-buffers-ordered-by-current mode)))
	  prev-ido-decor)))

(defun uzumaki-current-buffer-position (&optional mode)
  "Function that return current buffer position in MODE buffers.  If not found, return nil."
  (interactive)
  (let* ((buffers (uzumaki-buffers mode))
	(pos (cl-position (buffer-name) buffers :start 0 :test #'equal)))
    pos))

(defun uzumaki-next-buffer-position (&optional mode)
  "Function that return next buffer position in MODE buffers."
  (interactive)
  (let ((curpos (uzumaki-current-buffer-position mode))
	(len (length (uzumaki-buffers mode))))
    (if (not (eq curpos nil))
	(if (= curpos (1- len))
	    0
	  (1+ curpos))
      (if (> len 0)
    	  0
	nil))))

(defun uzumaki-prev-buffer-position (&optional mode)
  "Function that return previous buffer position in MODE buffers."
  (interactive)
  (let ((curpos (uzumaki-current-buffer-position mode))
	(len (length (uzumaki-buffers mode))))
    (if (not (eq curpos nil))
	(if (= curpos 0)
	    (1- len)
	  (1- curpos))
      (if (> len 0)
    	  0
	nil))))

(defun uzumaki-next-buffer-name (&optional mode)
  "Function that return next buffer name in MODE buffers."
  (interactive)
  (let ((nextpos (uzumaki-next-buffer-position mode))
	(buffers (uzumaki-buffers mode)))
    (if (not (eq nextpos nil))
	(nth nextpos buffers)
      nil)))

(defun uzumaki-prev-buffer-name (&optional mode)
  "Function that return previous buffer name in MODE buffers."
  (interactive)
  (let ((prevpos (uzumaki-prev-buffer-position mode))
	(buffers (uzumaki-buffers mode)))
    (if (not (eq prevpos nil))
	(nth prevpos buffers)
      nil)))

(defun uzumaki-cycle-to-next-buffer (&optional mode)
  "Function that cycle to the next MODE buffer."
  (interactive)
  (let ((nextbuffer (uzumaki-next-buffer-name mode)))
    (if (or (eq nextbuffer nil)
	    (eq nextbuffer (buffer-name)))
	(progn
	  (message "[Uzumaki]: there is no other buffer given the criteria")
	  nil)
      (switch-to-buffer nextbuffer nil 1))))

(defun uzumaki-cycle-to-prev-buffer (&optional mode)
  "Function that cycle to the previous MODE buffer."
  (interactive)
  (let ((prevbuffer (uzumaki-prev-buffer-name mode)))
    (if (or (eq prevbuffer nil)
	    (eq prevbuffer (buffer-name)))
	(progn
	  (message "[Uzumaki]: there is no other buffer given the criteria")
	  nil)
      (switch-to-buffer prevbuffer nil 1))))

;;;###autoload
(define-minor-mode uzumaki-minor-mode
  "Minor-mode that allows to cycle buffers easily using a predefined setup"
  :lighter " Uzumaki"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c z") 'uzumaki-ido-buffers-cycle-by-mode)
	    (define-key map (kbd "C-.")   'uzumaki-cycle-to-next-buffer)
	    (define-key map (kbd "C-,")   'uzumaki-cycle-to-prev-buffer)
	    (define-key map (kbd "C->")   '(lambda ()
					     (interactive)
					     (uzumaki-cycle-to-next-buffer
					      'all-no-hidden)))
	    (define-key map (kbd "C-<")   '(lambda ()
					     (interactive)
					     (uzumaki-cycle-to-prev-buffer
					      'all-no-hidden)))
	    (define-key map (kbd "C-M->") '(lambda ()
					     (interactive)
					     (uzumaki-cycle-to-next-buffer
					      'system)))
	    (define-key map (kbd "C-M-<") '(lambda ()
					     (interactive)
					     (uzumaki-cycle-to-prev-buffer
					      'system)))
	    map)
  :global 1)

(provide 'uzumaki)
;;; uzumaki.el ends here
