;;; exotica-theme.el --- A dark theme with vibrant colors

;; Copyright (C) 2017 Bharat Joshi

;; Author: Bharat Joshi <jbharat@outlook.com>
;; Maintainer: Bharat Joshi <jbharat@outlook.com>
;; URL: https://github.com/jbharat/exotica-theme
;; Package-Version: 20170818.432
;; Created: 22th July 2017
;; Keywords: faces, theme, dark, vibrant colors
;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;; License: GPL3

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

;; Bright colors over dark background with italics.
;; Inspired by Molokai-theme, Dracula-theme

;;; Code:

(deftheme exotica
  "A dark theme with vibrant colors")

(let
    (
     (bg                     "#0B080F")
     (fg                     "#E8F0FF")
     (seperator              "#271C33")
     (cursor                 "#F8F8F0")
     (face1                  "#66D9EF")
     (face2                  "#A6E22E")
     (face3                  "#FF3DB1")
     (face4                  "#AE81FF")
     (face5                  "#4577D7")
     (face6                  "#2ee267")
     (face7                  "#FF5996")
     (face8                  "#60FCEC")
     (face9                  "#433C4C")
     (bg1                    "#403D3D")
     (m1                     "#C1CAFF")
     (m2                     "#FD971F")
     (m3                     "#EF5939")
     (m4                     "#960050")
     (m5                     "#BCA3A3")
     (m6                     "#272822")
     (m7                     "#FF0000")
     (diff1                  "#232526")
     (ml-inactive-face       "#BCBCBC")
     (ml-active-face         "#050302")
     (comment-face           "#465457")
     (warning-bg-face        "#333333")
     (fullWhite              "#FFFFFF")
     (fullBlack              "#000000")
     (whiteSmoke         "white-smoke")
     (highlighter            "#E7F221")
     (line-highlight         "#271C33")
   )


(custom-theme-set-faces
 'exotica

 ;; default stuff
 `(default ((t (:background ,bg :foreground ,fg))))
 `(vertical-border ((t (:foreground ,seperator))))
 `(fringe ((t (:background ,bg))))
 `(cursor ((t (:background ,cursor))))
 `(bold ((t (:weight bold))))
 `(bold-italic ((t (:weight bold :slant italic))))
 `(custom-face-tag ((t (:foreground ,face1 :weight bold))))
 `(custom-state ((t (:foreground ,face2))))
 `(italic ((t (:slant italic))))
 `(region ((t (:background ,face9))))
 `(underline ((t (:underline t))))

 ;; diff
 `(diff-added ((t (:foreground ,face2 :weight bold))))
 `(diff-context ((t (:foreground ,fg))))
 `(diff-file-header ((t (:foreground ,face1 :background nil))))
 `(diff-indicator-added ((t (:foreground ,face2))))
 `(diff-indicator-removed ((t (:foreground ,face3))))
 `(diff-header ((t (:foreground ,fg :background ,diff1))))
 `(diff-hunk-header ((t (:foreground ,face4 :background ,diff1))))
 `(diff-removed ((t (:foreground ,face3 :weight bold))))


 `(escape-glyph ((t (:foreground ,m1))))
 `(minibuffer-prompt ((t (:foreground ,face1))))

 ;; powerline/modeline
 `(mode-line ((t (:foreground ,fg :background ,ml-active-face
                              :box (:line-width 1 :color ,seperator :style released-button)))))
 `(mode-line-inactive ((t (:foreground ,ml-inactive-face :background ,bg ))))
 `(powerline-active1 ((t (:inherit mode-line :background ,bg))))
 `(powerline-active2 ((t (:inherit mode-line :background ,bg))))
 `(powerline-inactive1 ((t (:inherit mode-line-inactive :background ,bg))))
 `(powerline-inactive2 ((t (:inherit mode-line-inactive :background ,bg))))

 ;; font
 `(font-lock-builtin-face ((t (:foreground ,face2))))
 `(font-lock-comment-face ((t (:foreground ,comment-face :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,comment-face :slant italic))))
 `(font-lock-constant-face ((t (:foreground ,face4))))
 `(font-lock-doc-face ((t (:foreground ,m1 :slant italic))))
 `(font-lock-function-name-face ((t (:foreground ,face3 :slant italic))))
 `(font-lock-keyword-face ((t (:foreground ,face1))))
 `(font-lock-negation-char-face ((t (:weight bold))))
 `(font-lock-preprocessor-face ((t (:foreground ,face2))))
 `(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 `(font-lock-regexp-grouping-construct ((t (:weight bold))))
 `(font-lock-string-face ((t (:foreground ,m1))))
 `(font-lock-type-face ((t (:foreground ,face1 :italic slant))))
 `(font-lock-variable-name-face ((t (:foreground ,face3))))
 `(font-lock-warning-face ((t (:foreground ,fullWhite (quote :background) ,warning-bg-face))))

 ;; Basic face
 `(success ((t (:foreground ,face2))))

 ;; js2-mode
 `(js2-function-call ((t (:inherit default :foreground ,face4 :slant italic))))
 `(js2-function-param ((t (:inherit default :foreground ,face7))))
 `(js2-external-variable ((t (:foreground ,face6))))
 
 ;; highlighting
 `(highlight ((t (:foreground ,highlighter :background ,face9))))
 `(hl-line ((t (:background ,line-highlight))))
 `(lazy-highlight ((t (:foreground ,comment-face :background ,fullBlack))))

 ;; isearch
 `(isearch ((t (:foreground ,highlighter :background ,face9))))
 `(isearch-fail ((t (:foreground ,fullWhite :background ,warning-bg-face))))

 ;; org
 `(outline-1 ((t (:foreground ,face1))))
 `(outline-2 ((t (:foreground ,face2))))
 `(outline-3 ((t (:foreground ,face3))))
 `(outline-4 ((t (:foreground ,face4))))
 `(outline-5 ((t (:foreground ,face5))))
 `(outline-6 ((t (:foreground ,face6))))
 `(outline-7 ((t (:foreground ,face7))))
 `(outline-8 ((t (:foreground ,face8))))
 `(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.0))))
 `(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.0))))
 `(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.0))))
 `(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.0))))
 `(org-level-5 ((t (:inherit outline-5 :weight bold :height 1.0))))
 `(org-level-6 ((t (:inherit outline-6 :weight bold :height 1.0))))
 `(org-level-7 ((t (:inherit outline-7 :weight bold :height 1.0))))
 `(org-level-8 ((t (:inherit outline-8 :weight bold :height 1.0))))
 `(rainbow-delimiters-depth-1-face ((t (:inherit org-level-1))))
 `(rainbow-delimiters-depth-2-face ((t (:inherit org-level-2))))
 `(rainbow-delimiters-depth-3-face ((t (:inherit org-level-3))))
 `(rainbow-delimiters-depth-4-face ((t (:inherit org-level-4))))
 `(rainbow-delimiters-depth-5-face ((t (:inherit org-level-5))))
 `(rainbow-delimiters-depth-6-face ((t (:inherit org-level-6))))
 `(rainbow-delimiters-depth-7-face ((t (:inherit org-level-7))))
 `(rainbow-delimiters-depth-8-face ((t (:inherit org-level-8))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground ,face1))))

 ;; others
 `(secondary-selection ((t (:background ,face9))))
 `(shadow ((t (:foreground ,comment-face))))
 `(widget-inactive ((t (:background ,m7))))

 ;; undo-tree
 `(undo-tree-visualizer-active-branch-face ((t (:inherit default))))
 `(undo-tree-visualizer-current-face ((t (:foreground ,m3))))
 `(undo-tree-visualizer-default-face ((t (:inherit shadow))))
 `(undo-tree-visualizer-register-face ((t (:foreground ,m1))))
 `(undo-tree-visualizer-unmodified-face ((t (:foreground ,face1))))

 ;; helm-buffer
 `(helm-buffer-file ((t (:foreground ,face1))))
 `(helm-ff-executable ((t (:foreground ,fullWhite))))
 `(helm-ff-file ((t (:foreground ,fullWhite))))
 `(helm-prefarg ((t (:foreground ,face4))))
 `(helm-selection ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
 `(helm-buffer-directory ((t (:foreground ,face4))))
 `(helm-ff-directory ((t (:foreground ,face4))))
 `(helm-source-header ((t (:background ,fullBlack :foreground ,fullWhite
                                       :weight bold :height 1.3 :family "Sans Serif"))))
 `(helm-swoop-target-line-block-face ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
 `(helm-swoop-target-line-face ((t (:background ,line-highlight :foreground ,face3 :slant italic))))

 ;; ivy
 `(ivy-current-match ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
 `(ivy-highlight-face ((t (:background ,fullBlack :foreground ,face3 :slant italic))))
 `(ivy-modified-buffer ((t (:inherit default :foreground ,m2))))
 `(ivy-virtual ((t (:inherit default ))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit default :foreground ,face7))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit default :foreground ,face7))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit default :foreground ,face7))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit default :foreground ,face7))))
 `(swiper-line-face ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
 `(swiper-match-face-2 ((t (:foreground ,face7))))


  ;; company
  `(company-tooltip ((t (:background ,bg :foreground ,fullWhite))))
  `(company-template-field ((t (:background: ,bg :foreground ,fullWhite))))
  `(company-tooltip-selection ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
  `(company-echo-common ((t (:foreground ,face3))))
  `(company-scrollbar-bg ((t (:background ,seperator))))
  `(company-scrollbar-fg ((t (:background ,line-highlight))))
  `(company-tooltip-annotation ((t (:foreground ,face3))))
  `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
  `(company-tooltip-common ((t (:foreground ,face8))))
  `(company-preview ((t (:background ,line-highlight :foreground ,face3 :slant italic))))
  '(company-preview-common ((t (:inherit company-preview ))))

  ;; neotree
  `(neo-dir-link-face ((t (:foreground ,face7 :slant italic))))
  `(neo-root-dir-face ((t (:foreground ,face1 :weight demibold))))

  ;; treemacs
  `(treemacs-directory-face ((t (:foreground ,face7 :slant italic))))

  ;; parentheses matching
  `(show-paren-match ((t (:background ,face7 :foreground ,fullBlack))))
  `(show-paren-mismatch ((t (:background ,m7 :foreground ,fullWhite))))

  ;; dired
  `(dired-directory ((t (:foreground ,face7 :slant italic))))

  ;; Web-mode
  `(web-mode-html-attr-custom-face ((t (:foreground ,face3))))
  `(web-mode-html-attr-equal-face ((t (:foreground ,fullWhite))))
  `(web-mode-html-attr-name-face ((t (:foreground ,face3))))
  `(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face ))))
  `(web-mode-html-tag-bracket-face ((t (:foreground ,fullWhite))))
  `(web-mode-html-tag-face ((t (:foreground ,face3 :slant italic))))
  `(web-mode-html-tag-custom-face ((t (:inherit web-mode-html-tag-face))))

  ;; linum relative line number face
  `(linum-relative-current-face ((t (:inherit linum :foreground ,face4 :weight normal))))

  ;; imenu-list
  `(imenu-list-entry-subalist-face-0 ((t (:foreground ,face2))))
  `(imenu-list-entry-subalist-face-1 ((t (:foreground ,face4))))
  `(imenu-list-entry-subalist-face-2 ((t (:foreground ,face6))))
  `(imenu-list-entry-subalist-face-3 ((t (:foreground ,face8))))
  `(imenu-list-entry-face-0 ((t (:foreground ,face1))))
  `(imenu-list-entry-face-1 ((t (:foreground ,face3))))
  `(imenu-list-entry-face-2 ((t (:foreground ,face5))))
  `(imenu-list-entry-face-3 ((t (:foreground ,face7))))

 ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'exotica-theme)
;;; exotica-theme.el ends here
