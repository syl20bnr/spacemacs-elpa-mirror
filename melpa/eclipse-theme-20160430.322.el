;;; eclipse-theme.el --- Theme based on Eclipse circa 2010

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/eclipse-theme
;; Package-Version: 20160430.322
;; Version: 0.1.0
;; Keywords: themes

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This theme assumes light background.  To load it, use:
;;
;;     (require 'eclipse-theme)

;;; Code:

(deftheme eclipse
    "Color theme from Eclipse.")

(let ((class '((class color) (min-colors 88) (background light)))
      (eclipse-bg "#ffffff")
      (eclipse-fg "#000000")
      (eclipse-const "#110099")
      (eclipse-comment "#3F7F5F")
      (eclipse-error "#FF0000")
      (eclipse-builtin "#7F0055")
      (eclipse-string "#2A00FF")
      (eclipse-blue-3 "#758BC6")
      (eclipse-region "#f9b593")
      (eclipse-shadow "grey50"))
  (apply 'custom-theme-set-faces 'eclipse
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default
              :foreground ,eclipse-fg
              :background ,eclipse-bg)
            (cursor :background ,eclipse-fg)
            (shadow :foreground ,eclipse-shadow)
            (success :foreground ,eclipse-error)
            (error :foreground ,eclipse-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (highlight :background "darkseagreen2")
            (fringe :background ,eclipse-bg)
            (region :background ,eclipse-region :foreground ,eclipse-bg)
            (secondary-selection :background "#333366" :foreground "#f6f3e8")
            (whitespace-indentation :background "LightYellow" :foreground "lightgray")
            (term)
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,eclipse-builtin :bold t)
            (font-lock-comment-face :foreground ,eclipse-comment :slant normal)
            (font-lock-comment-delimiter-face :foreground ,eclipse-comment :slant normal)
            (font-lock-constant-face :foreground ,eclipse-const)
            (font-lock-doc-face :foreground ,eclipse-string)
            (font-lock-doc-string-face :foreground ,eclipse-string)
            (font-lock-function-name-face :foreground ,eclipse-fg :bold t)
            (font-lock-keyword-face :foreground ,eclipse-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,eclipse-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,eclipse-builtin)
            (font-lock-regexp-grouping-construct :foreground ,eclipse-builtin)
            (font-lock-string-face :foreground ,eclipse-string)
            (font-lock-type-face :foreground ,eclipse-fg :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,eclipse-fg)
            (font-lock-warning-face :foreground ,eclipse-error)
            (org-code :foreground ,eclipse-builtin :weight bold)
            (org-verbatim :foreground ,eclipse-const)
            (org-level-1 :weight bold :foreground "black")
            (org-level-2 :weight bold :foreground ,eclipse-builtin)
            (org-level-3 :foreground "#123555")
            (org-level-4 :weight bold :slant normal :foreground "#E3258D")
            (org-level-5 :weight bold :slant normal :foreground "#0077CC")
            (org-level-6 :weight bold :slant italic :foreground "#EA6300")
            (org-level-7 :weight bold :slant italic :foreground "#2EAE2C")
            (org-level-8 :weight bold :slant italic :foreground "#FD8008")
            (org-block-begin-line :foreground ,eclipse-const)
            (org-block-end-line :foreground ,eclipse-const)
            (org-scheduled-previously :foreground ,eclipse-comment)
            (ido-subdir :weight bold)
            (mode-line :foreground "black" :background "#f9b593" :box nil)
            (mode-line-inactive :foreground "grey20" :background "grey90" :box nil)
            (minibuffer-prompt :foreground "medium blue")
            (hl-line :background "#e5e4e2")
            ;; defaults
            (mode-line-buffer-id)
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "RoyalBlue3" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,eclipse-const)
            (aw-mode-line-face :foreground ,eclipse-string)
            (swiper-match-face-1 :background "#FEEA89")
            (swiper-match-face-2 :background "#fb7905")
            (swiper-match-face-3 :background "#F9A35A")
            (swiper-match-face-4 :background "#F15C79")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            (magit-section-heading :inherit header-line)
            (magit-section-highlight :weight bold)
            (magit-diff-context :foreground "grey20")
            (magit-diff-context-highlight :weight bold :foreground "grey20")
            (magit-diff-added :inherit diff-added)
            (magit-diff-added-highlight :inherit diff-added :weight bold)
            (magit-diff-removed :inherit diff-removed)
            (magit-diff-removed-highlight :inherit diff-removed :weight bold)
            (magit-diff-file-heading)
            (magit-diff-file-heading-highlight :weight bold)
            (magit-diff-file-heading-selection :foreground "red")
            (magit-diff-hunk-heading :inherit diff-hunk-header)
            (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
            (magit-hash :foreground "firebrick")
            (magit-branch-remote :background "Grey85" :foreground "OliveDrab4" :box t)
            (magit-branch-local :background "Grey85" :foreground "LightSkyBlue4" :box t)
            (cider-instrumented-face)))))

(custom-theme-set-variables
 'eclipse
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'eclipse-theme)

;;; eclipse-theme.el ends here
