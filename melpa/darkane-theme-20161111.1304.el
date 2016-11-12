;;; darkane-theme.el --- A dark theme with strong colors
;;
;; Filename: darkane-theme.el
;; Author: Adam Wenham <adamwenham64@gmail.com>
;; Version: 0.1
;; Package-Version: 20161111.1304
;; URL: https://github.com/felixfortis/emacs-darkane-theme
;; Package-Requires: ((emacs "24"))
;; Keywords: theme, dark, strong colors
;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Darkane, A Dark Theme with strong colors for Emacs24+
;;
;; Change History:
;;   Adam Wenham, 2016-11:
;;     * Changed colors for personal taste
;;
;;
;; This theme is based on Andre Richter's Lush theme.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(deftheme darkane
  "Dark color theme adapted from Andre Richter's Lush theme.
  See  https://github.com/felixfortis/emacs-darkane-theme")

(let* ((darkane/background "#00001a")
       (darkane/foreground "#E0E0E0")
       (darkane/turquoise  "#2AA198")
       (darkane/orange     "#FF951B")
       (darkane/pink       "#FF88FF")
       (darkane/yellow     "#FFE329")
       (darkane/green      "#61CE3C")
       (darkane/light-blue "#82A6DF")
       (darkane/dark-blue  "#284050")
       (darkane/light-red  "#FA583F")
       (darkane/hl-line    "#333333"))

  (custom-theme-set-faces
   `darkane
   `(bold                         ((t (:bold t))))
   `(bold-italic                  ((t (:bold t))))
   `(border-glyph                 ((t (nil))))
   `(default                      ((t (:foreground ,darkane/foreground :background ,darkane/background))))
   `(fringe                       ((t (:background ,darkane/background))))
   `(buffers-tab                  ((t (:foreground ,darkane/foreground :background ,darkane/background))))
   `(font-lock-builtin-face       ((t (:foreground "Khaki"))))
   `(font-lock-comment-face       ((t (:foreground ,darkane/turquoise :italic t))))
   `(font-lock-constant-face      ((t (:foreground ,darkane/orange))))
   `(font-lock-doc-face           ((t (:foreground "SlateGray"))))
   `(font-lock-doc-string-face    ((t (:foreground ,darkane/orange))))
   `(font-lock-string-face        ((t (:foreground ,darkane/green))))
   `(font-lock-function-name-face ((t (:foreground ,darkane/pink))))
   `(font-lock-keyword-face       ((t (:foreground ,darkane/yellow))))
   `(font-lock-preprocessor-face  ((t (:foreground "Aquamarine"))))
   `(font-lock-type-face          ((t (:foreground ,darkane/light-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,darkane/light-red))))
   `(font-lock-negation-char-face ((t (:foreground ,darkane/pink))))
   `(font-lock-warning-face       ((t (:foreground "Pink"     :bold t))))
   `(gui-element                  ((t (:foreground "black"    :background "#D4D0C8"))))
   `(mode-line                    ((t (:foreground "#F0F0F0"  :background "#444444" :box nil))))
   `(mode-line-highlight          ((t (:foreground ,darkane/pink :box nil))))
   `(hl-line                      ((t (:background ,darkane/hl-line))))
   `(text-cursor                  ((t (:foreground "black"    :background "yellow"))))
   `(region                       ((t (:background ,darkane/dark-blue))))
   `(highlight                    ((t (:background "#222222"))))
   `(highline-face                ((t (:background "SeaGreen"))))
   `(italic                       ((t (nil))))
   `(left-margin                  ((t (nil))))
   `(toolbar                      ((t (nil))))

   `(magit-item-highlight         ((t (:inherit region))))

   `(underline                    ((nil (:underline nil))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun darkane-theme()
  "Load darkane-theme."
  (interactive)
  (load-theme 'darkane t))

(provide-theme 'darkane)

;;; darkane-theme.el ends here
