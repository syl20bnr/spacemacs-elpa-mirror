;;; melancholy-theme.el --- A dark theme for dark minds

;; Copyright (C) 2016 Sod Oscarfono

;; Author: Sod Oscarfono <sod@oscarfono.com>
;; URL: http://github.com/techquila/melancholy-theme.el
;; Package-Version: 20160929.43
;; Version: 1.6
;; Package-requires:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A dark theme  for dark minds.  > Emacs 24

;;; Code:

(deftheme melancholy  "A dark theme for dark minds")

;;; wip
;;
;; (defvar melancholy-default-colors-alist
;;  '(("melancholy-black"           . "#161A1F")
;;     ("melancholy-white"           . "#DBDBDB")
;;     ("melancholy-light-gray"    .  "8C8C8C")
;;     ("melancholy-gray"             . "#555555")
;;     ("melancholy-dark-gray"    . "#000000")
;;     ("melancholy-green"           . "#96BF33")
;;     ("melancholy-blue"             . "#00BFFF")
;;     ("melancholy-purple"         . "#F37DEE")
;;     ("melancholy-pink"             . "#F92769")
;;     ("melancholy-red"              . "red")
;;     ("melancholy-orange"        . "orange")
;;     ("melancholy-yellow"         . "yellow")))
  
;;; Theme Faces
(custom-theme-set-faces
 'melancholy

 ;;; Global
 '(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DBDBDB" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "DejaVu sans"))))
 '(cursor ((t (:background "light blue"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#00BFFF"))))
 '(highlight ((t (:background "#151515"))))
 '(region ((t (:background "#555555"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#96BF33"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#8C8C8C"))))
 '(font-lock-comment-face ((t (:foreground "#8C8C8C"))))
 '(font-lock-constant-face ((t (:foreground "#DFAF8F"))))
 '(font-lock-doc-face ((t (:foreground "#FFB728"))))
 '(font-lock-function-name-face ((t (:foreground "#00BFFF"))))
 '(font-lock-keyword-face ((t (:foreground "#F92672" :height 160 :weight extra-bold))))
 '(font-lock-negation-char-face ((t (:foreground "#F37DEE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#F92672"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#A63A62"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#A63A62"))))
 '(font-lock-string-face ((t (:foreground "#F37DEE" :slant italic :weight extra-light))))
 '(font-lock-type-face ((t (:foreground "#96BFF0"))))
 '(font-lock-variable-name-face ((t (:foreground "#96BF33"))))
 '(font-lock-warning-face ((t (:foreground "#FF6969"))))
 '(button ((t (:underline (:color foreground-color :style line) :foreground "#F92672"))))
 '(link ((t (:foreground "#96BF33"))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "#161A1F"))))
 '(header-line ((t (:foreground "#DEDEDE" :background "#333333"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "light yellow")) (t (:inherit (variable-pitch)))))

;;; org-mode
 '(org-level-8 ((t  )))
 '(org-level-7 ((t  )))
 '(org-level-6 ((t  )))
 '(org-level-5 ((t  )))
 '(org-level-4 ((t  )))
 '(org-level-3 ((t  :foreground "#888888" )))
 '(org-level-2 ((t  :foreground "#888888" :height 1.15 )))
 '(org-level-1 ((t  :height 1.25 :weight bold)))
 '(org-block ((t (:family "DejaVu sans mono" :foreground "#00BFFF" :box nil))))
 '(org-block-begin-line ((t (:background "#444444" :foreground "#00BFFF"))))
 '(org-block-end-line ((t (:background "#444444" :foreground "#00BFFF"))))
 '(org-document-title ((t (:foreground "#00BFFF" :height 1.75 :weight extra-bold ))))
 '(org-document-info ((t (:foreground "#00BFFF" :height 1.25 ))))
 '(org-headline-done ((t (:foreground "#96BF33" :strike-through t))))
 '(org-done ((t (:foreground "#96BF33" :strike-through t))))

;;; mode-line
 '(mode-line ((t (:foreground "#00BFFF" :background "#333333"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:foreground "#555555" :background "#222222"))))

;;; isearch
 '(isearch ((t (:background "#96BF33" :foreground "#444444"))))
 '(isearch-fail ((t (:background "#00BFFF"))))
 '(lazy-highlight ((t (:foreground "#666" :background "#96BFF0"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "#00BFFF")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit isearch)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'melancholy)
;;; melancholy-theme.el ends here
