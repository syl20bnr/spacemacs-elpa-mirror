;;; green-screen-theme.el --- A nice color theme for those who miss green CRTs

;; Author: Ricardo Banffy <rbanffy@gmail.com>
;; Maintainer: Ricardo Banffy <rbanffy@gmail.com>
;; URL: https://github.com/rbanffy/green-screen-emacs
;; Package-Version: 20170209.1208
;; Keywords: faces, theme
;; Version: 1.0.0a

;; Copyright (C) 2017  Ricardo Bánffy

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides a theme that uses various shades of green.

;;; Code:

(deftheme green-screen
  "A nice color theme for those who miss green CRTs")

(custom-theme-set-faces
 'green-screen
 '(cursor ((t (:background "green"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "green4"))))
 '(minibuffer-prompt ((t (:foreground "green3" :weight normal))))
 '(highlight ((t (:background "green3" :foreground "lawn green" :weight normal))))
 '(region ((t (:background "green3"))))
 '(shadow ((t (:foreground "green4"))))
 '(secondary-selection ((t (:background "dark green"))))
 '(trailing-whitespace ((t (:background "spring green" :foreground "black"))))
 '(font-lock-builtin-face ((t (:foreground "medium spring green" :weight semi-bold))))
 '(font-lock-comment-face ((t (:foreground "forest green" :weight normal))))
 '(font-lock-constant-face ((t (:foreground "green" :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#00DE00" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "green" :weight extra-bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#38ff00" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#c0ff00" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "medium spring green" :weight normal))))
 '(font-lock-warning-face ((t (:weight bold :foreground "chartreuse"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "#00ff7f" :underline t))))
 '(link-visited ((t (:inherit link))))
 '(fringe ((t (:background "black" :foreground "green"))))
 '(mode-line-buffer-id ((t (:underline (:color foreground-color :style line)))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "dark green" :foreground "lawn green" :box (:line-width -1 :color "forest green") :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(hl-line ((t (:inherit highlight :background "dark green" :foreground "green"))))
 '(linum ((t (:height 100 :weight normal :box nil :foreground "SpringGreen3" :background "#002200" :inherit default))))
 '(mode-line ((t (:background "#009922" :foreground "#002200" :box (:line-width 1 :color "#009944") :weight normal))))
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "IBM 3270 Narrow"))))
 )

(provide-theme 'green-screen)
;;; green-screen-theme.el ends here
