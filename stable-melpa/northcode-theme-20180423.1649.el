;;; northcode-theme.el --- A dark theme focused on blue and orange colors.

;;; Commentary:
;; Dark theme with most colors roughly hand-picked in blue and orange shades.
;; I will keep adding as I go, but try not to drastically change anything.
;; If you want me to add colors to support a specific package, submit an issue on the github repo.

;; Title: Northcode Theme
;; Project: northcode-theme
;; Version: 0.1
;; Package-Version: 20180423.1649
;; URL: https://github.com/Northcode/northcode-theme.el
;; Author: Andreas Larsen <andreas@northcode.no>
;; Package-Requires: ((emacs "24"))
;; License: GPL-3.0

;;; Code:

(deftheme northcode
  "Dark theme focused on blue and orange colors.")

(custom-theme-set-faces
 'northcode
 '(default ((t (:inherit nil :stipple nil :background "#1c1c1c" :foreground "#f6f3e8"))))
 '(font-lock-constant-face ((t (:foreground "#5971B2"))))
 '(font-lock-keyword-face ((t (:foreground "#6E8EE5"))))
 '(font-lock-variable-name-face ((t (:foreground "#CBD4EA"))))
 '(font-lock-string-face ((t (:foreground "#C27127"))))
 '(font-lock-type-face ((t (:foreground "#7392E5"))))
 '(font-lock-function-name-face ((t (:foreground "#E9D2AF"))))
 '(font-lock-comment-face ((t (:foreground "#656C7D"))))
 '(fringe ((t (:background "#1c1c1c" :foreground "#D8DEE9"))))
 '(widget-field ((t (:background "#303030" :foreground "#D8DEE9"))))
 '(custom-variable-tag ((t (:inherit font-lock-keyword-face :weight bold))))
 '(helm-match ((t (:inherit font-lock-variable-name-face))))
 '(helm-candidate-number ((t (:background "#212736" :foreground "#C27127"))))
 '(helm-selection ((t (:background "#E9D2AF" :distant-foreground "black"))))
 '(helm-source-header ((t (:background "#212736" :foreground "white" :weight bold :height 1.3))))
 '(helm-M-x-key ((t (:underline t))))
 '(header-line ((t (:inherit mode-line :background "#26314C" :foreground "grey90" :box nil))))
 '(mode-line ((t (:background "#2A2E38" :foreground "#88C0D0"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#1e1e1e" :weight light))))
 '(minibuffer-prompt ((t (:foreground "#55C2BE"))))
 '(error ((t (:foreground "#993937" :weight bold))))
 '(highlight ((t (:background "#333" :weight bold))))
 '(region ((t (:background "#E9D2AF" :foreground "black"))))
 '(link ((t (:foreground "#4C97E0" :underline t))))
 '(eshell-prompt ((t (:inherit font-lock-keyword-face :weight bold))))
 '(org-todo ((t (:foreground "#B0412A" :weight bold))))
 '(org-done ((t (:foreground "#377D32" :weight bold))))
 '(org-date ((t (:foreground "#E9D2AF"))))
 '(company-scrollbar-bg ((t (:background "#1e1e1e"))))
 '(company-template-field ((t (:inherit company-tooltip))))
 '(company-tooltip ((t (:background "#2A2E38" :foreground "white"))))
 )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'northcode)
;;; northcode-theme.el ends here
