Extend M-x all (older than M-x occur-edit-mode) to be replacement of it.
  - Show line number before line content (using overlay)
  - Can navigate with M-x next-error / M-x previous-error
  - Use C-x h in *All* to get all matched lines.

Call M-x all from anything/helm:
  1. Call anything/helm command showing lineno and content
     such as M-x anything-occur / anything-browse-code /
             helm-occur / helm-swoop / helm-browse-code etc
  2. Press C-c C-a to show anything/helm contents into *All* buffer
  3. You can edit *All* buffer!

Multiple-cursors in *All*:
  - M-x mc/edit-lines-in-all sets one cursor to all lines in *All* buffer.

*All* is undo-able!

Installation:

Put all-ext.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'all-ext)
optional
(require 'helm-config) ;; or (require 'anything-config)
(define-key all-mode-map (kbd "C-c C-m") 'mc/edit-lines-in-all)

No need more.
