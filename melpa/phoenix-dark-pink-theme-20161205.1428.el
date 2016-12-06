;;; phoenix-dark-pink-theme.el --- Port of the Sublime Text 2 theme of the same name

;; Copyright 2013-2016 J Irving

;; Author: J Irving <j@lollyshouse.ca>
;; URL: http://github.com/j0ni/phoenix-dark-pink
;; Package-Version: 20161205.1428
;; Version: 1.4

;; Org-mode mods from Rikard Glans - https://github.com/darrik/phoenix-dark-pink

;; Code:

(unless (>= emacs-major-version 24)
  (error "phoenix-dark-pink-theme requires Emacs 24 or later."))

;; some old p7s:

(defmacro define-phoenix-dark-pink-theme (&rest body)
  `(let ((p0 "#ffcfff")
         (p1 "#efbfef")
         (p2 "#e1b1ed")
         (p3 "#dfafdf")
         (p4 "#d1afdd")
         (p5 "#d1a1dd")
         (p6 "#c19fcf")
         (p7 "#bf8fbf")
         (p8 "#b294bb")
         (p9 "#a582a3")
         (p10 "#9f6f9f")
         (p11 "#8f5f8f")
         (p12 "#815f8d")
         (p13 "#755273")

         (fg "#cccccc")
         (bg "#101010")

         (alarmed "#f582a3")
         (warned "#87003f")
         (weirdyellow "#c0af7f")

         (dp1 "#31182d")
         (dp2 "#412b3f")
         (dp3 "#714161")

         (offpink1 "#f0dfff")
         (offpink2 "#d0bfdf")

         (offwhite1 "#efefef")
         (offwhite2 "#e0e0e0")
         (offwhite3 "#dddddd")
         (silverfox "#787878")
         (sadsilverfox "#585858")

         (b1 "#202020")
         (b2 "#2d2d2d")
         (b3 "#393939"))

     (deftheme phoenix-dark-pink
       "Phoenix Dark Pink color theme")

     (custom-theme-set-faces 'phoenix-dark-pink ,@body)))

(define-phoenix-dark-pink-theme
  `(default ((t (
                 :inherit nil
                 :stipple nil
                 :background ,bg
                 :foreground ,fg
                 :inverse-video nil
                 :box nil
                 :strike-through nil
                 :overline nil
                 :underline nil
                 :slant normal
                 :weight normal
                 :width normal))))

  ;; Another special face is the cursor face. On graphical displays, the
  ;; background color of this face is used to draw the text cursor. None of the
  ;; other attributes of this face have any effect
  ;;
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html#Faces
  `(cursor ((t (:background ,fg))))
  `(fixed-pitch ((t (:underline nil :weight normal))))
  `(variable-pitch ((t (:family "Sans Serif" :weight normal :underline nil))))
  `(escape-glyph ((t (:weight normal :underline nil :foreground ,p3))))
  `(minibuffer-prompt ((t (:weight normal :underline nil :foreground ,p3))))
  `(highlight ((t (:background ,dp1 :underline nil :weight normal))))
  `(region ((t (:weight normal :underline nil :background ,dp2))))
  `(shadow ((t (:weight normal :underline nil :foreground "grey70"))))
  `(secondary-selection ((t (:weight normal :underline nil :background ,b3))))
  `(trailing-whitespace ((t (:background ,p3 :underline nil :weight normal))))

  `(font-lock-builtin-face ((t (:weight normal :underline nil :foreground ,offwhite3))))
  `(font-lock-comment-face ((t (:foreground ,p9 :underline nil :slant italic :weight normal))))
  `(font-lock-comment-delimiter-face ((t (:weight normal :underline nil :foreground ,p12 :inherit font-lock-comment-face))))
  `(font-lock-doc-face ((t (:weight normal :underline nil :foreground ,alarmed :inherit (font-lock-string-face)))))
  `(font-lock-string-face ((t (:foreground "Thistle" :underline nil :weight normal))))
  `(font-lock-constant-face ((t (:foreground ,p8 :underline nil :weight normal))))
  `(font-lock-function-name-face ((t (:foreground ,offpink1 :underline nil :weight normal))))
  `(font-lock-keyword-face ((t (:foreground ,p3 :underline nil :weight normal))))
  `(font-lock-negation-char-face ((t (:weight normal :underline nil :foreground ,fg))))
  `(font-lock-preprocessor-face ((t (:weight normal :underline nil :foreground ,p3 :inherit (font-lock-builtin-face)))))
  `(font-lock-regexp-grouping-backslash ((t (:weight normal :underline nil :inherit (bold)))))
  `(font-lock-regexp-grouping-construct ((t (:weight normal :underline nil :inherit (bold)))))
  `(font-lock-type-face ((t (:foreground ,offwhite2 :underline nil :weight normal))))
  `(font-lock-variable-name-face ((t (:foreground ,offwhite1 :underline nil :weight normal))))
  `(font-lock-warning-face ((t (:weight normal :underline nil :foreground ,weirdyellow :inherit (error)))))

  `(compilation-info ((t (:weight normal :foreground ,offpink1))))
  `(compilation-mode-line-exit ((t (:weight normal :foreground ,offpink1))))
  `(compilation-mode-line-fail ((t (:weight normal :foreground ,p2))))
  `(compilation-mode-line-run ((t (:weight normal :foreground ,silverfox))))

  `(warning ((t (:weight normal :foreground ,offpink1))))

  `(link ((t (:weight normal :underline nil :foreground ,offpink1))))
  `(link-visited ((t (:weight normal :underline nil :foreground ,offpink2 :inherit (link)))))
  `(button ((t (:foreground ,offpink1 :underline nil :weight normal))))
  `(fringe ((t (:background "#191919" :foreground ,sadsilverfox :underline nil :weight normal))))
  `(header-line ((t (:weight normal :underline nil :inherit (mode-line)))))
  `(tooltip ((t (:weight normal :underline nil :foreground ,p3 :background ,b2 :inherit (variable-pitch)))))

  `(mode-line ((t (:weight normal :underline nil :box nil :foreground ,p3 :background ,b2))))
  `(mode-line-buffer-id ((t (:weight normal :underline nil :foreground ,offpink1))))
  `(mode-line-emphasis ((t (:weight normal :underline nil))))
  `(mode-line-highlight ((t (:weight normal :underline nil :box nil))))
  `(mode-line-inactive ((t (:weight normal :underline nil :box nil :foreground ,p7 :background ,b1 :inherit (mode-line)))))

  `(cider-error-highlight-face ((t (:underline (:style wave :color ,alarmed)))))
  `(cider-warning-highlight-face ((t (:underline (:style wave :color ,warned)))))
  `(cider-repl-input-face ((t (:weight normal))))

  `(isearch ((t (:weight normal :underline nil :background ,p3 :foreground ,b2))))
  `(isearch-fail ((t (:weight normal :underline nil :foreground ,p3 :background ,alarmed))))
  `(lazy-highlight ((t (:weight normal :underline nil :foreground ,p2 :background ,b2))))

  `(highlight-symbol-face ((t (:underline t :background ,b2))))

  `(grep-context-face ((t (:foreground ,fg))))
  `(grep-error-face ((t (:foreground ,p2 :underline t))))
  `(grep-hit-face ((t (:foreground ,p2))))
  `(grep-match-face ((t (:foreground ,p2))))
  `(match ((t (:weight normal :underline nil :foreground ,p2 :background ,b2))))

  `(next-error ((t (:weight normal :underline nil :inherit (region)))))
  `(query-replace ((t (:weight normal :underline nil :inherit (isearch)))))

  `(ido-first-match ((t (:foreground ,p2 :weight bold))))
  `(ido-only-match ((t (:foreground ,p2 :weight bold))))
  `(ido-subdir ((t (:foreground ,p7))))

  `(flx-highlight-face ((t (:foreground ,offpink1 :weight bold))))

  `(linum ((t (:foreground ,silverfox))))

  `(ac-candidate-face ((t (:background ,b2 :foreground ,p7))))
  `(ac-candidate-mouse-face ((t (:background ,b3 :foreground ,p3))))
  `(ac-selection-face ((t (:background ,b3 :foreground ,p3))))
  `(ac-yasnippet-selection-face ((t (:background ,b3 :foreground ,p3))))
  `(ac-yasnippet-candidate-face ((t (:background ,b2 :foreground ,p7))))
  `(popup-tip-face ((t (:background ,b2 :foreground ,p7))))
  `(popup-scroll-bar-foreground-face ((t (:background ,b3))))
  `(popup-scroll-bar-background-face ((t (:background ,bg))))
  `(company-tooltip ((t (:background ,b2 :foreground ,p7))))
  `(company-tooltip-selection ((t (:background ,b3 :foreground "white"))))
  `(company-tooltip-mouse ((t (:inherit (company-tooltip-selection)))))
  `(company-tooltip-common ((t (:background ,b3 :foreground ,p3))))
  `(company-tooltip-common-selection ((t (:background ,b3 :foreground "white"))))
  `(company-tooltip-annotation ((t (:foreground ,offpink1 :inherit (company-tooltip)))))
  `(company-scrollbar-fg ((t (:inherit (company-tooltip)))))
  `(company-scrollbar-bg ((t (:inherit (company-tooltip)))))
  `(company-preview ((t (:inherit (company-tooltip-selection)))))
  `(company-preview-common ((t (:inherit (company-tooltip-common-selection)))))
  `(company-preview-search ((t (:foreground ,offpink1 :inherit (company-preview)))))
  `(company-echo ((t (:foreground ,p3))))
  `(company-echo-common ((t (:foreground ,offpink1))))

  `(speedbar-button-face ((t (:foreground ,p3))))
  `(speedbar-directory-face ((t (:foreground ,offwhite1))))
  `(speedbar-file-face ((t (:foreground ,fg))))
  `(speedbar-highlight-face ((t (:foreground ,b1 :background ,p8))))
  `(speedbar-selected-face ((t (:foreground ,offpink1))))
  `(speedbar-tag-face ((t (:foreground ,p7))))

  `(eval-sexp-fu-flash ((t (:background ,bg :foreground "white"))))
  `(nrepl-eval-sexp-fu-flash ((t (:background ,bg :foreground "white"))))

  `(magit-header ((t (:foreground ,p2 :background ,b2 :box (:line-width 1 :color "grey20")))))
  `(magit-log-sha1 ((t (:foreground ,p2 :background ,b2))))
  `(magit-section-title ((t (:foreground ,p2 :background ,bg))))
  `(magit-branch ((t (:foreground ,p3))))
  `(magit-item-highlight ((t (:inherit (highlight-parentheses)))))
  `(magit-diff-add ((t (:foreground ,p3 :background ,b3))))
  `(magit-diff-del ((t (:foreground ,p9 :background ,b1))))
  `(magit-diff-none ((t (:background ,bg))))
  `(magit-diff-hunk-header ((t (:background ,b2))))
  `(magit-diff-file-header ((t (:background ,b3))))
  `(magit-log-author ((t (:foreground ,offpink1))))
  `(magit-log-head-label-remote ((t (:foreground ,offpink1 :box t))))
  `(magit-log-head-label-local ((t (:foreground ,p1 :box t))))

  `(window-number-face ((t (:background ,b2 :foreground ,offpink1))))

  `(git-gutter:separator ((t (:foreground ,bg :background ,bg))))
  `(git-gutter:modified ((t (:foreground ,p3 :background ,bg))))
  `(git-gutter:added ((t (:foreground ,offpink1 :background ,bg))))
  `(git-gutter:deleted ((t (:foreground ,alarmed  :background ,bg))))
  `(git-gutter:unchanged ((t (:foreground ,bg :background ,bg))))

  `(hl-paren-face ((t (:inherit highlight))))
  `(show-paren-match ((t (:inherit highlight :foreground ,p0 :background ,p7))))

  `(erb-face ((t (:foreground ,fg :background ,bg))))
  `(erb-exec-face ((t (:inherit erb-face))))
  `(erb-out-face ((t (:inherit erb-face))))
  `(erb-delim-face ((t (:inherit erb-face :foreground ,p1 :background ,bg))))
  `(erb-exec-delim-face ((t (:inherit erb-delim-face))))
  `(erb-out-delim-face ((t (:inherit erb-delim-face :foreground ,p1 :background ,bg))))
  `(erb-comment-face ((t (:inherit erb-face :foreground ,p13 :background ,bg))))
  `(erb-comment-delim-face ((t (:inherit erb-face :foreground ,sadsilverfox :background ,bg))))

  `(rainbow-delimiters-depth-9-face ((t (:foreground ,p1))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,p2))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,p3))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,p4))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,p5))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,p6))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,p7))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,p8))))
  `(rainbow-delimiters-depth-1-face ((t (:foreground ,p9))))

  `(js2-warning ((t (:foreground ,p2))))
  `(js2-error ((t (:foreground ,p1))))
  `(js2-jsdoc-tag ((t (:foreground ,sadsilverfox))))
  `(js2-jsdoc-type ((t (:foreground ,silverfox))))
  `(js2-jsdoc-value ((t (:foreground ,silverfox))))
  `(js2-function-param ((t (:foreground ,p8))))
  `(js2-external-variable ((t (:foreground ,p0))))

  `(erc-action-face ((t (:inherit erc-default-face))))
  `(erc-bold-face ((t (:weight bold))))
  `(erc-current-nick-face ((t (:foreground ,fg :weight bold))))
  `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
  `(erc-default-face ((t (:foreground ,fg))))
  `(erc-direct-msg-face ((t (:inherit erc-default))))
  `(erc-error-face ((t (:inherit font-lock-warning))))
  `(erc-fool-face ((t (:inherit erc-default))))
  `(erc-highlight-face ((t (:inherit hover-highlight))))
  `(erc-input-face ((t (:foreground ,p3))))
  `(erc-keyword-face ((t (:foreground ,p2 :weight bold))))
  `(erc-nick-default-face ((t (:foreground ,p2 :weight bold))))
  `(erc-my-nick-face ((t (:foreground ,offwhite1 :weight bold))))
  `(erc-nick-msg-face ((t (:inherit erc-default))))
  `(erc-notice-face ((t (:foreground ,p7 :background ,bg))))
  `(erc-pal-face ((t (:foreground ,p1 :weight bold))))
  `(erc-prompt-face ((t (:foreground ,p3 :background ,bg :weight bold))))
  `(erc-timestamp-face ((t (:foreground ,silverfox))))
  `(erc-underline-face ((t (:underline t))))

  `(circe-prompt-face ((t (:weight bold :foreground ,p3))))
  `(circe-server-face ((t (:foreground ,p7))))
  `(circe-highlight-nick-face ((t (:weight bold :inherit hover-highlight))))
  `(circe-my-message-face ((t (:foreground "Thistle"))))
  `(circe-originator-face ((t (:foreground ,p2))))
  `(circe-topic-diff-new-face ((t (:inherit git-gutter:added))))
  `(circe-topic-diff-removed-face ((t (:inherit git-gutter:deleted))))
  `(circe-fool-face ((t (:foreground "grey40"))))
  `(lui-button-face ((t (:inherit link))))
  `(lui-time-stamp-face ((t (:inherit erc-timestamp-face))))
  `(lui-highlight-face ((t (:inherit hover-highlight))))

  `(w3m-anchor ((t (:inherit link))))
  `(w3m-arrived-anchor ((t (:foreground ,p0))))
  `(w3m-form ((t (:foreground ,p7 :underline t))))
  `(w3m-header-line-location-title ((t (:foreground ,offwhite1 :underline t :weight bold))))
  `(w3m-history-current-url ((t (:inherit match))))
  `(w3m-lnum ((t (:foreground ,silverfox))))
  `(w3m-lnum-match ((t (:background ,p7 :foreground ,bg))))
  `(w3m-lnum-minibuffer-prompt ((t (:foreground ,p1))))

  `(highlight-indentation-face ((t (:inherit highlight))))
  `(highlight-indentation-current-column-face ((t (:inherit highlight))))

  `(org-level-1 ((t (:foreground ,p1))))
  `(org-level-2 ((t (:foreground ,p2))))
  `(org-level-3 ((t (:foreground ,p3))))
  `(org-level-4 ((t (:foreground ,p4))))
  `(org-level-5 ((t (:foreground ,p5))))
  `(org-level-6 ((t (:foreground ,p6))))
  `(org-level-7 ((t (:foreground ,p7))))
  `(org-level-8 ((t (:foreground ,p8))))
  `(org-level-9 ((t (:foreground ,p9))))
  `(org-meta-line ((t (:foreground ,dp3))))
  `(org-table ((t (:foreground ,p5))))
  `(org-document-info-keyword ((t (:foreground ,p7))))
  `(org-document-title ((t (:foreground ,p2))))
  `(org-date ((t (:foreground ,p7))))

  `(ivy-current-match ((t (:background ,p2 :foreground ,bg))))
  `(ivy-minibuffer-match-face-1 ((t (:background ,p10 :foreground ,p0))))
  `(ivy-minibuffer-match-face-2 ((t (:background ,p10 :foreground ,p0))))
  `(ivy-minibuffer-match-face-3 ((t (:background ,p10 :foreground ,p0))))
  `(ivy-minibuffer-match-face-4 ((t (:background ,p10 :foreground ,p0))))
  `(ivy-confirm-face ((t (:foreground ,p0))))
  `(ivy-match-required-face ((t (:foreground ,p0))))
  `(ivy-subdir ((t (:foreground ,fg))))
  `(ivy-remote ((t (:foreground "pink"))))
  `(ivy-virtual ((t (:foreground "pink"))))

  ;; `(mu4e-unread-face ((t ())))
  ;; `(mu4e-moved-face ((t ())))
  ;; `(mu4e-trashed-face ((t ())))
  ;; `(mu4e-draft-face ((t ())))
  ;; `(mu4e-flagged-face ((t ())))
  ;; `(mu4e-replied-face ((t ())))
  ;; `(mu4e-forwarded-face ((t ())))
  ;; `(mu4e-header-face ((t ())))
  ;; `(mu4e-header-title-face ((t ())))
  ;; `(mu4e-header-highlight-face ((t ())))
  ;; `(mu4e-header-marks-face ((t ())))
  `(mu4e-header-key-face ((t (:foreground ,p6))))
  `(mu4e-header-value-face ((t (:foreground ,offpink2))))
  `(mu4e-special-header-value-face ((t (:foreground ,offpink1))))
  `(mu4e-link-face ((t (:foreground ,p0))))
  ;; `(mu4e-contact-face ((t ())))
  ;; `(mu4e-highlight-face ((t ())))
  ;; `(mu4e-title-face ((t ())))
  ;; `(mu4e-context-face ((t ())))
  ;; `(mu4e-modeline-face ((t ())))
  ;; `(mu4e-view-body-face ((t ())))
  ;; `(mu4e-footer-face ((t ())))
  `(mu4e-url-number-face ((t (:foreground ,p6))))
  `(mu4e-attach-number-face ((t (:foreground ,p6))))
  `(mu4e-cited-1-face ((t (:foreground ,p0))))
  `(mu4e-cited-2-face ((t (:foreground ,p2))))
  `(mu4e-cited-3-face ((t (:foreground ,p4))))
  `(mu4e-cited-4-face ((t (:foreground ,p6))))
  `(mu4e-cited-5-face ((t (:foreground ,p8))))
  `(mu4e-cited-6-face ((t (:foreground ,p10))))
  `(mu4e-cited-7-face ((t (:foreground ,p12))))
  ;; `(mu4e-system-face ((t ())))
  ;; `(mu4e-ok-face ((t ())))
  ;; `(mu4e-warning-face ((t ())))
  ;; `(mu4e-compose-separator-face ((t ())))
  ;; `(mu4e-compose-header-face ((t ())))
  ;; `(mu4e-region-code ((t ())))
  ;; `(mu4e-compose-header-face ((t ())))

  `(indent-guide-face ((t (:foreground ,dp3)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'phoenix-dark-pink)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:

;;; phoenix-dark-pink-theme.el ends here
