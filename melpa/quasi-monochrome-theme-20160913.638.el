;;; quasi-monochrome-theme.el --- High contrast quasi monochrome color theme

;; Copyright (C) 2015 Lorenzo Bolla

;; Author: Lorenzo Bolla <lbolla@gmail.com>
;; URL: https://github.com/lbolla/emacs-quasi-monochrome
;; Package-Version: 20160913.638
;; Created: 28th July 2015
;; Version: 1.0
;; Keywords: color-theme, monochrome, high contrast

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A high contrast quasi-monochrome color theme.

;;; Code:

(deftheme quasi-monochrome
  "quasi-monochrome emacs theme")

(custom-theme-set-faces
 'quasi-monochrome
 '(button ((t (:inherit (link)))))
 '(cursor ((t (:background "yellow"))))
 '(default ((t (:family "Terminus" :foundry "xos4" :width normal :height 120 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "light gray" :background "black" :stipple nil :inherit nil))))
 '(escape-glyph ((t (:foreground "light gray"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(font-lock-builtin-face ((t (:foreground "light gray"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "light slate gray"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "light gray"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "white"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "white"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "dim gray"))))
 '(font-lock-type-face ((t (:weight bold :foreground "light gray"))))
 '(font-lock-variable-name-face ((t (:foreground "light gray"))))
 '(font-lock-warning-face ((t (:foreground "gold"))))
 '(fringe ((t (:background "gray20"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(highlight ((t (:background "gray20"))))
 '(hl-line ((t (:background "grey20"))))
 '(isearch ((t (:foreground "black" :background "spring green"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:foreground "black" :background "lime green"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(magit-diff-hunk-heading ((t (:background "gray20"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "gray30"))))
 '(magit-hash ((t (:foreground "gray60"))))
 '(magit-section-highlight ((t (:background "gray20"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "light gray"))))
 '(mode-line ((t (:box nil :foreground "black" :background "#e5e5e5"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box nil)) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "#b0b0b0" :foreground "black" :box nil))))
 '(mu4e-flagged-face ((t (:inherit font-lock-constant-face :foreground "firebrick" :weight bold))))
 '(mu4e-header-face ((t (:foreground "grey50" :weight normal))))
 '(mu4e-header-key-face ((t (:inherit font-lock-type-face :underline nil))))
 '(mu4e-header-value-face ((t (:inherit font-lock-type-face :foreground "yellow"))))
 '(mu4e-replied-face ((t (:foreground "grey70" :weight normal))))
 '(mu4e-unread-face ((t (:foreground "white" :weight normal))))
 '(next-error ((t (:inherit (region)))))
 '(org-done ((t (:foreground "forest green"))))
 '(org-scheduled-previously ((t (:foreground "gray50" :weight normal))))
 '(org-scheduled-today ((t (:foreground "gray80" :weight normal))))
 '(org-scheduled ((t (:foreground "gray80" :weight normal))))
 '(org-todo ((t (:foreground "gray30"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:foreground "black" :background "gold"))))
 '(secondary-selection ((t (:background "gray15"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(trailing-whitespace ((t (:background "firebrick2"))))
 '(variable-pitch ((t (:family "Sans Serif")))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'quasi-monochrome)

;;; quasi-monochrome-theme.el ends here
