This package advises the `enable-theme' function and convert non-terminal colors
to their closest approximation. Inspired by VIM's CSApprox plugin
http://www.vim.org/scripts/script.php?script_id=2390

Normally Emacs does have non-terminal color themes degrade, however in some
system the effect is pretty poor. I've encountered this on Arch Linux and
Ubuntu (probably caused by some libvte weirdness).

Installation:
Add the to your .emacs or similar:

(autoload 'color-theme-approximate-on "color-theme-approximate")
(color-theme-approximate-on)

Changelog

v0.4, Feb 28 2014
- Revamp and simplify

v0.3, Feb 23 2014
- Fix for non-standard color names ("Grey07" in Magit)

v0.2, Mar 29 2013
- Fix error that degrades colors on graphical frame running the same Emacs server
- Fix error that `ca-defined-rgb-map' is wrong when start Emacs with graphical frame

v0.1, Jan 14 2013
- Initial version


(require 'color)

(defvar ca-defined-rgb-list nil
  "RGB values of defined colors.")

(defun ca-make-defined-rgb-list ()
  (mapcar #'ca-color-to-rgb (defined-colors)))

(defun ca-color-to-rgb (color)
  "Convert color to RGB without implied approximation.
Only fallback to `color-name-to-rgb' for named colors."
  (cond
   ;; Hex string of 6 chars
   ((string-match "^#[[:xdigit:]]\\{6\\}$" color)
    (let ((r (substring color 1 3))
          (g (substring color 3 5))
          (b (substring color 5 7)))
      (mapcar (lambda (c) (string-to-number c 16))
              (list r g b))))
   ;; Hex string of 3 chars
   ((string-match "^#[[:xdigit:]]\\{3\\}$" color)
    (let ((r (substring color 1 2))
          (g (substring color 2 3))
          (b (substring color 3 4)))
      (mapcar (lambda (c) (string-to-number (format "%s%s" c c) 16))
              (list r g b))))
   ;; Irregular name, .i.e "Grey07" => "grey7"
   ((string-match "^\\([a-zA-Z]+\\)\\([0-9]+\\)$" color)
    (mapcar (lambda (c) (round (* c 255)))
            (color-name-to-rgb
             (format "%s%d"
                     (match-string 1 color)
                     (string-to-number (match-string 2 color) 10)))))
   ;; Anything else
   (t (mapcar (lambda (c) (round (* c 255)))
              (color-name-to-rgb color)))))

(defun ca-rgb-to-color (rgb)
  "Format RGB values into hex string."
  (format "#%02X%02X%02X"
          (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))

(defun ca-distance (red green blue)
  (+ (* red red) (* green green) (* blue blue)))

(defun ca-rgb-diff (rgb1 rgb2)
  "Distance in RGB colorspace."
  (ca-distance
   (- (nth 0 rgb1) (nth 0 rgb2))
   (- (nth 1 rgb1) (nth 1 rgb2))
   (- (nth 2 rgb1) (nth 2 rgb2))))

(defun ca-rgb-diff-real (rgb1 rgb2)
  "Like `ca-rgb-diff' but the components are scaled according to eye sensitivity."
  (ca-distance
   (* 0.3 (- (nth 0 rgb1) (nth 0 rgb2)))
   (* 0.59 (- (nth 1 rgb1) (nth 1 rgb2)))
   (* 0.11 (- (nth 2 rgb1) (nth 2 rgb2)))))

(defvar ca-approximator #'ca-rgb-diff-real
  "Function used to calculate the different between colors.
The approximator is called with two lists of RGB values, for
the color pre-defined and currently processed.")

(defun ca-approximate (color)
  "Find the closest defined color. Use our custom `ca-color-to-rgb'
because `color-name-to-rgb' already returns the wrong approximation.
The approximation can be customized by `ca-approximator'."
  ;; (ca-color-to-rgb "#ff8") => (1.0 1.0 0.5333333333333333)
  ;; (color-name-to-rgb "#ff8") => (0.8951965065502183 0.8951965065502183 0.0)
  ;; (color-name-to-rgb "#ffff88") => (0.8951965065502183 0.8951965065502183 0.0)
  (let ((min-diff (* 3 256 256))
        (best nil))
    (dolist (candidate ca-defined-rgb-list best)
      (let ((diff (funcall ca-approximator color candidate)))
        (when (< diff min-diff)
          (setq min-diff diff
                best candidate))))))

(defun ca-process-face (face)
  (let ((background (face-background face))
        (foreground (face-foreground face))
        (frame (selected-frame)))
    (when (and background (ca-color-to-rgb background))
      (set-face-attribute
       face frame
       :background
       (ca-rgb-to-color (ca-approximate (ca-color-to-rgb background)))))
    (when (and foreground (ca-color-to-rgb foreground))
      (set-face-attribute
       face frame
       :foreground
       (ca-rgb-to-color (ca-approximate (ca-color-to-rgb foreground)))))))

(defadvice enable-theme (after ca-apply-approximation)
  (unless (display-graphic-p (selected-frame))
    (setq ca-defined-rgb-list (ca-make-defined-rgb-list))
    (mapc #'ca-process-face (reverse (face-list)))))

###autoload
(defun color-theme-approximate-on ()
  (interactive)
  (ad-enable-advice 'enable-theme 'after 'ca-apply-approximation)
  (ad-activate 'enable-theme))

###autoload
(defun color-theme-approximate-off ()
  (interactive)
  (ad-disable-advice 'enable-theme 'after 'ca-apply-approximation)
  (ad-activate 'enable-theme))

(provide 'color-theme-approximate)

color-theme-approximate.el ends here
