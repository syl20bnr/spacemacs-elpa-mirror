;;; css-comb.el --- Sort CSS properties in a particular order using CSS Comb

;; Copyright (C) 2015 Charanjit Singh <ckhabra@gmail.com>

;; Author: Charanjit Singh <ckhabra@gmail.com>
;; Version: 0.2
;; Package-Version: 20160415.2259
;; URL: https://github.com/channikhabra/css-comb.el

;;; Commentary:

;; Wraps csscomb (https://github.com/csscomb/csscomb.js) for
;; convenient use in Emacs.  Provides an interactive command `css-comb'.
;; Beautifies combed css if web-beautify package is installed (recommended)

;;; Code:

(defvar css-comb-executable "csscomb"
  "Executable to use for combing the CSS.")

(defconst css-comb-args '())

(defun css-comb-command-not-found-message (program)
  "Construct a message about PROGRAM not found."
  (format
   "%s not found. Install it with: \"npm -g install csscomb\" "
   program))

(defun css-comb-format-error-message (bufname)
  "Construct a format error message with BUFNAME."
  (format
   "Could not apply csscomb. See %s to check errors for details"
   bufname))

(defun css-comb-format-buffer (program extension)
  "Using PROGRAM, format current buffer with EXTENSION."
  (if (executable-find program)
      (css-comb-format-buffer-1 program extension)
    (error (css-comb-command-not-found-message program))))


(defun css-comb-format-buffer-1 (program extension)
  "Internal function of `css-comb-format-buffer'.

Using PROGRAM, format current buffer with EXTENSION."
  (let* ((tmpfile (make-temp-file "css-comb" nil
                                  (format ".%s" extension)))
         (outputbufname (format "*css-comb-%s*" extension))
         (outputbuf (get-buffer-create outputbufname))
         (args (append css-comb-args (list tmpfile))))
    (unwind-protect
        (progn
          (with-current-buffer outputbuf (erase-buffer))
          (write-region nil nil tmpfile)

          (if (zerop (apply 'call-process program nil nil nil args))
              (progn
                (with-current-buffer outputbuf
                  (insert-file-contents tmpfile)
                  (when (require 'web-beautify nil 'noerror)
                    (web-beautify-css-buffer)))
                (let ((p (point)))
                  (save-excursion
                    (with-current-buffer (current-buffer)
                      (erase-buffer)
                      (insert-buffer-substring outputbuf)))
                  (goto-char p)
                  (message "Applied css-comb")
                  (kill-buffer outputbuf)))
            (error (css-comb-format-error-message outputbufname))
            (display-buffer outputbuf)))
      (progn
        (delete-file tmpfile)))))

(defun css-comb-format-region (program extension beg end)
  "Using PROGRAM, format each line in the BEG .. END region."
  (let* ((regionbufname "*css-comb-region*")
         (regionbuf (get-buffer-create regionbufname)))
    (copy-to-buffer regionbuf beg end)
    (with-current-buffer regionbuf
      (css-comb-format-buffer program extension))
    (delete-region beg end)
    (insert-buffer-substring regionbuf)
    (indent-region beg end)
    (kill-buffer regionbuf)))


;;;###autoload
(defun css-comb (&optional beg end)
  "Comb region from BEG to END if active, otherwise the entire buffer.

Formatting is done according to the csscomb command."
  (interactive "r")
  (if (use-region-p)
      (css-comb-format-region
       css-comb-executable
       "css"
       beg end)
    (css-comb-format-buffer css-comb-executable "css")))

(provide 'css-comb)

;; Local Variables:
;; coding: utf-8
;; End:

;;; css-comb.el ends here
