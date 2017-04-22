;;; flatui-dark-theme.el --- Dark color theme with colors from https://flatuicolors.com/

;; Copyright 2017, Andrew Phillips

;; Author: Andrew Phillips <theasp@gmail.com>
;; Keywords: color theme dark flatui faces
;; Package-Version: 20170421.1718
;; URL: https://github.com/theasp/flatui-dark-theme
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Dark color theme with flatui colors from https://flatuicolors.com/.
;; The theme structure has been borrowed from grandshell-theme, which
;; borrowed it from color-theme-sanityinc-solarized.

;; URLs:
;; grandshell-theme: https://github.com/steckerhalter/grandshell-theme
;; color-theme-sanityinc-solarized: https://github.com/purcell/color-theme-sanityinc-solarized
;; 

;;; Requirements:

;; Emacs 24.

;;; Code:

(deftheme flatui-dark "Dark color theme with colors from https://flatuicolors.com/")

(let ((class '((class color) (min-colors 89)))
      (c1a "#1abc9c")
      (c1b "#16a085")
      (c2a "#2ecc71")
      (c2b "#27ae60")
      (c3a "#3498db")
      (c3b "#2980b9")
      (c4a "#9b59b6")
      (c4b "#8e44ad")
      (c5a "#34495e")
      (c5b "#2c3e50")
      (c6a "#f1c40f")
      (c6b "#f39c12")
      (c7a "#e67e22")
      (c7b "#d35400")
      (c8a "#e74c3c")
      (c8b "#c0392b")
      (c9a "#ecf0f1")
      (c9b "#bdc3c7")
      (c0a "#95a5a6")
      (c0b "#7f8c8d")
      (black "#000")
      (white "#fff"))

  (custom-theme-set-faces
   'flatui-dark

   ;; standard faces
   `(default ((,class (:foreground ,c9b :background ,black))))
   `(bold ((,class (:weight bold))))
   `(italic ((,class (:slant italic))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(shadow ((,class (:foreground ,c9b))))
   `(link ((,class (:foreground ,c1a :underline t))))

   `(highlight ((,class (:inverse-video nil :background ,c5b))))
   `(isearch ((,class (:foreground ,c6a :background ,black :inverse-video t))))
   `(isearch-fail ((,class (:background ,black :inherit font-lock-warning-face :inverse-video t))))
   `(match ((,class (:foreground ,c6b :background ,black :inverse-video t))))
   `(lazy-highlight ((,class (:foreground ,c6b :background ,black :inverse-video t))))
   `(region ((,class (:inverse-video t))))
   `(secondary-selection ((,class (:background ,c5b))))
   `(trailing-whitespace ((,class (:background ,c8b :underline nil))))

   `(mode-line ((t (:foreground ,black :background ,c9b))))
   `(mode-line-inactive ((t (:foreground ,c5b :background ,c0b :weight light :box nil :inherit (mode-line )))))
   `(mode-line-buffer-id ((t (:foreground ,black))))
   `(mode-line-emphasis ((,class (:foreground ,c4a))))
   `(which-func ((,class (:foreground ,c3b :background nil :weight bold))))

   `(header-line ((,class (:inherit mode-line :foreground ,c4a :background nil))))
   `(minibuffer-prompt ((,class (:foreground ,c9a))))
   `(fringe ((,class (:background ,c5b))))
   `(cursor ((,class (:background ,c9b))))
   `(border ((,class (:background ,c5b))))
   `(widget-button ((,class (:underline t))))
   `(widget-field ((,class (:background ,c5b :box (:line-width 1 :color ,c9b)))))

   `(success ((,class (:foreground ,c2b))))
   `(warning ((,class (:foreground ,c6b))))
   `(error ((,class (:foreground ,c8b))))

   `(show-paren-match ((,class (:inverse-video t :weight bold))))
   `(show-paren-mismatch ((,class (:background ,black :inherit font-lock-warning-face :inverse-video t))))

   `(custom-variable-tag ((,class (:foreground ,c3b))))
   `(custom-group-tag ((,class (:foreground ,c3b))))
   `(custom-state-tag ((,class (:foreground ,c2b))))

   ;; general font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,c7a))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,c6a :inherit 'fixed-pitch))))
   `(font-lock-comment-face ((,class (:foreground ,c6a :inherit 'fixed-pitch :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,c1a))))
   `(font-lock-doc-face ((,class (:foreground ,c7a))))
   `(font-lock-doc-string-face ((,class (:foreground ,c6a))))
   `(font-lock-function-name-face ((,class (:foreground ,c3a))))
   `(font-lock-keyword-face ((,class (:foreground ,c4a))))
   `(font-lock-negation-char-face ((,class (:foreground ,c2b))))
   `(font-lock-preprocessor-face ((,class (:foreground ,c4b))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,c4b))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,c4a))))
   `(font-lock-string-face ((,class (:foreground ,c6a))))
   `(font-lock-type-face ((,class (:foreground ,c3b))))
   `(font-lock-variable-name-face ((,class (:foreground ,c6a))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,c8b))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; mode specific faces

   ;; asorted faces
   `(csv-separator-face ((,class (:foreground ,c6a))))
   `(border-glyph ((,class (nil))))
   `(gui-element ((,class (:background ,c5b :foreground ,c9b))))
   `(hl-sexp-face ((,class (:background ,c5b))))
   `(highlight-80+ ((,class (:background ,c5b))))
   `(rng-error-face ((,class (:underline ,c8b))))
   `(py-builtins-face ((,class (:foreground ,c6b :weight normal))))

   ;; auto-complete
   `(ac-completion-face ((,class (:foreground ,c9a, :underline t))))
   `(ac-candidate-face ((,class (:background ,c4b :foreground ,c9a))))
   `(ac-selection-face ((,class (:background ,c4a :foreground ,c4b))))
   `(ac-yasnippet-candidate-face ((,class (:background ,c7b :foreground ,c4b))))
   `(ac-yasnippet-selection-face ((,class (:background ,c7a :foreground ,c4b))))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((,class (:background "#0c0c0c"))))

   ;; clojure
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,c2b))))
   `(clojure-keyword ((,class (:foreground ,c6a))))
   `(clojure-parens ((,class (:foreground ,c9a))))
   `(clojure-braces ((,class (:foreground ,c2b))))
   `(clojure-brackets ((,class (:foreground ,c6a))))
   `(clojure-double-quote ((,class (:foreground ,c4b :background nil))))
   `(clojure-special ((,class (:foreground ,c3b))))
   `(clojure-java-call ((,class (:foreground ,c4a))))

   ;; company
   `(company-preview ((,class (:foreground ,c9a))))
   `(company-preview-common ((,class (:foreground ,c9a :underline t))))
   `(company-preview-search ((,class (:foreground ,c4b :background ,c6a))))
   `(company-tooltip ((,class (:background ,c6a  :foreground "#000"))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground "#000"))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((,class (:inherit company-tooltip-selection :background ,c6b :weight bold))))
   `(company-scrollbar-bg ((,class (:background ,c6a))))
   `(company-scrollbar-fg ((,class (:background ,c6b))))

   ;; compilation
   `(compilation-column-number ((,class (:foreground ,c6a))))
   `(compilation-line-number ((,class (:foreground ,c6a))))
   `(compilation-message-face ((,class (:foreground ,c3b))))
   `(compilation-mode-line-exit ((,class (:foreground ,c2b))))
   `(compilation-mode-line-fail ((,class (:foreground ,c8b))))
   `(compilation-mode-line-run ((,class (:foreground ,c3b))))
   `(compilation-info ((,class (:foreground ,c1a))))

   ;; diff
   `(diff-added ((,class (:foreground ,c2b))))
   `(diff-changed ((,class (:foreground ,c4a))))
   `(diff-removed ((,class (:foreground ,c6b))))
   `(diff-header ((,class (:foreground ,c4b :background nil))))
   `(diff-file-header ((,class (:foreground ,c3b :background nil))))
   `(diff-hunk-header ((,class (:foreground ,c4a))))
   `(diff-refine-removed ((,class (:inherit magit-diff-removed-highlight :foreground ,c8a))))
   `(diff-refine-added ((,class (:inherit magit-diff-added-highlight :foreground ,c3a))))

   ;; diff-hl
   `(diff-hl-change ((,class (:foreground ,c3b :background ,c3b))))
   `(diff-hl-delete ((,class (:foreground ,c7a :background ,c7b))))
   `(diff-hl-insert ((,class (:foreground ,c2b :background ,c2b))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,c6b))))
   `(diredp-date-time ((,class (:foreground ,c6a))))
   `(diredp-deletion ((,class (:foreground ,c8a :weight bold :slant italic))))
   `(diredp-deletion-file-name ((,class (:foreground ,c8a :underline t))))
   `(diredp-dir-heading ((,class (:foreground ,c7a :underline t :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,c4a :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,c2a :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,c2a :background nil))))
   `(diredp-file-name ((,class (:foreground ,c9b))))
   `(diredp-file-suffix ((,class (:foreground ,c4b))))
   `(diredp-flag-mark ((,class (:foreground ,c8a :weight bold))))
   `(diredp-flag-mark-line ((,class (:inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,c0a))))
   `(diredp-link-priv ((,class (:background nil :foreground ,c7a))))
   `(diredp-mode-line-flagged ((,class (:foreground ,c6b))))
   `(diredp-mode-line-marked ((,class (:foreground ,c4a))))
   `(diredp-no-priv ((,class (:foreground ,c0a :background nil))))
   `(diredp-number ((,class (:foreground ,c6b))))
   `(diredp-other-priv ((,class (:background nil :foreground ,c6b))))
   `(diredp-rare-priv ((,class (:foreground ,c8b :background nil))))
   `(diredp-read-priv ((,class (:foreground ,c3b :background nil))))
   `(diredp-symlink ((,class (:foreground ,c7a))))
   `(diredp-write-priv ((,class (:foreground ,c4a :background nil))))

   ;; ediff
   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,c0a :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,c0a :background nil :inverse-video t))))

   ;; eldoc
   `(eldoc-highlight-function-argument ((,class (:foreground ,c2b :weight bold))))

   ;; erb
   `(erb-delim-face ((,class (:background ,c5b))))
   `(erb-exec-face ((,class (:background ,c5b :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,c5b))))
   `(erb-out-face ((,class (:background ,c5b :weight bold))))
   `(erb-out-delim-face ((,class (:background ,c5b))))
   `(erb-comment-face ((,class (:background ,c5b :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,c5b))))

   ;; erc
   `(erc-direct-msg-face ((,class (:foreground ,c6a))))
   `(erc-error-face ((,class (:foreground ,c8b))))
   `(erc-header-face ((,class (:foreground ,c9a :background ,c5b))))
   `(erc-input-face ((,class (:foreground ,c6a))))
   `(erc-current-nick-face ((,class (:foreground ,c3b :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,c3b))))
   `(erc-nick-default-face ((,class (:weight normal :foreground ,c4a))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,c6a))))
   `(erc-notice-face ((,class (:foreground ,c0b))))
   `(erc-pal-face ((,class (:foreground ,c6b))))
   `(erc-prompt-face ((,class (:foreground ,c3b))))
   `(erc-timestamp-face ((,class (:foreground ,c4b))))
   `(erc-keyword-face ((,class (:foreground ,c2b))))

   ;; eshell
   `(eshell-ls-archive ((,class (:foreground ,c4b :weight normal))))
   `(eshell-ls-backup ((,class (:foreground ,c6a))))
   `(eshell-ls-clutter ((,class (:foreground ,c6b :weight normal))))
   `(eshell-ls-directory ((,class (:foreground ,c3b :weight normal))))
   `(eshell-ls-executable ((,class (:foreground ,c8b :weight normal))))
   `(eshell-ls-missing ((,class (:foreground ,c4a :weight normal))))
   `(eshell-ls-product ((,class (:foreground ,c6a))))
   `(eshell-ls-readonly ((,class (:foreground ,c0b))))
   `(eshell-ls-special ((,class (:foreground ,c2b :weight normal))))
   `(eshell-ls-symlink ((,class (:foreground ,c4a :weight normal))))
   `(eshell-ls-unreadable ((,class (:foreground ,c9b))))
   `(eshell-prompt ((,class (:foreground ,c2b :weight normal))))

   ;; eval-sexp-fu
   `(eval-sexp-fu-flash ((,class (:background ,c4b))))

   ;; fic-mode
   `(font-lock-fic-face ((,class (:background ,c8b :foreground ,c8b :weight bold))))

   ;; flycheck
   `(flycheck-error-face ((t (:foreground ,c8b :background ,c8b :weight bold))))
   `(flycheck-error ((,class (:underline (:color ,c8b)))))
   `(flycheck-warning ((,class (:underline (:color ,c6b)))))

   ;; flymake
   `(flymake-warnline ((,class (:underline ,c6b :background ,black))))
   `(flymake-errline ((,class (:underline ,c8b :background ,black))))

   ;; git-commit
   `(git-commit-summary ((,class (:foreground ,c9b))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,c4a :weight bold))))
   `(git-gutter:added ((,class (:foreground ,c2b :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,c8b :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,c6a))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,c4a :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,c2b :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,c8b :weight bold))))

   ;; gnus
   `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,c6b))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-button ((,class (:inherit link :foreground nil))))
   `(gnus-signature ((,class (:inherit font-lock-comment-face))))
   `(gnus-summary-normal-unread ((,class (:foreground ,c9a :weight normal))))
   `(gnus-summary-normal-read ((,class (:foreground ,c9b :weight normal))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,c4b :weight normal))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,c6b :weight normal))))
   `(gnus-summary-low-unread ((,class (:foreground ,c0a :weight normal))))
   `(gnus-summary-low-read ((,class (:foreground ,c0b :weight normal))))
   `(gnus-summary-low-ancient ((,class (:foreground ,c0b :weight normal))))
   `(gnus-summary-high-unread ((,class (:foreground ,c6a :weight normal))))
   `(gnus-summary-high-read ((,class (:foreground ,c2b :weight normal))))
   `(gnus-summary-high-ancient ((,class (:foreground ,c2b :weight normal))))
   `(gnus-summary-high-ticked ((,class (:foreground ,c6b :weight normal))))
   `(gnus-summary-cancelled ((,class (:foreground ,c8b :background nil :weight normal))))
   `(gnus-group-mail-low ((,class (:foreground ,c0b))))
   `(gnus-group-mail-low-empty ((,class (:foreground ,c0b))))
   `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,c0a))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,c0a))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,c0a))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,c0a))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,c0a))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,c0a))))
   `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,c0a))))
   `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,c0a))))
   `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,c0a))))
   `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,c0a))))
   `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,c0a))))
   `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,c0a))))

   ;; grep
   `(grep-context-face ((,class (:foreground ,c0a))))
   `(grep-error-face ((,class (:foreground ,c8b :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,c3b))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   ;; helm
   `(helm-M-x-key ((,class (:foreground ,c7a :underline t))))
   `(helm-buffer-size ((,class (:foreground ,c6b))))
   `(helm-buffer-not-saved ((,class (:foreground ,c6b))))
   `(helm-buffer-saved-out ((,class (:foreground ,c8b :background ,black :inverse-video t))))
   `(helm-candidate-number ((,class (:background ,black :foreground ,c6a :bold t))))
   `(helm-visible-mark ((,class (:background ,c0b :foreground ,c4a :bold t))))
   `(helm-header ((,class (:inherit header-line))))
   `(helm-selection ((,class (:background ,c0b :underline t))))
   `(helm-selection-line ((,class (:background ,c9b :foreground ,c6a :underline nil))))
   `(helm-separator ((,class (:foreground ,c8b))))
   `(helm-source-header ((,class (:background ,black, :foreground ,c7a, :underline t, :weight bold))))
   `(helm-ff-directory ((t (:foreground ,c4a))))
   `(helm-ff-symlink ((t (:foreground ,c6a))))
   `(helm-buffer-directory ((t (:foreground ,c4a))))
   `(helm-match ((t (:foreground ,c6a))))
   `(helm-ff-prefix ((t (:foreground ,c6a :weight bold))))

   ;; highlight-symbol
   `(highlight-symbol-face ((,class (:background ,c6b))))

   ;; icomplete
   `(icomplete-first-match ((,class (:foreground ,white :bold t))))

   ;; ido
   `(ido-subdir ((,class (:foreground ,c4a))))
   `(ido-first-match ((,class (:foreground ,c6a))))
   `(ido-only-match ((,class (:foreground ,c2b))))
   `(ido-indicator ((,class (:foreground ,c8b :background ,black))))
   `(ido-virtual ((,class (:foreground ,c0b))))

   ;; jabber
   `(jabber-chat-prompt-local ((,class (:foreground ,c6a))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,c6b))))
   `(jabber-chat-prompt-system ((,class (:foreground ,c6a :weight bold))))
   `(jabber-chat-text-local ((,class (:foreground ,c6a))))
   `(jabber-chat-text-foreign ((,class (:foreground ,c6b))))
   `(jabber-chat-text-error ((,class (:foreground ,c8b))))
   `(jabber-roster-user-online ((,class (:foreground ,c2b))))
   `(jabber-roster-user-xa ((,class :foreground ,c0a)))
   `(jabber-roster-user-dnd ((,class :foreground ,c6a)))
   `(jabber-roster-user-away ((,class (:foreground ,c6b))))
   `(jabber-roster-user-chatty ((,class (:foreground ,c4a))))
   `(jabber-roster-user-error ((,class (:foreground ,c8b))))
   `(jabber-roster-user-offline ((,class (:foreground ,c0a))))
   `(jabber-rare-time-face ((,class (:foreground ,c0a))))
   `(jabber-activity-face ((,class (:foreground ,c4a))))
   `(jabber-activity-personal-face ((,class (:foreground ,c4b))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,c6a))))
   `(js2-error-face ((,class (:foreground nil :underline ,c8b))))
   `(js2-external-variable-face ((,class (:foreground ,c4a))))
   `(js2-function-param-face ((,class (:foreground ,c3b))))
   `(js2-instance-member-face ((,class (:foreground ,c3b))))
   `(js2-private-function-call-face ((,class (:foreground ,c8b))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,c6a))))
   `(js3-error-face ((,class (:foreground nil :underline ,c8b))))
   `(js3-external-variable-face ((,class (:foreground ,c4a))))
   `(js3-function-param-face ((,class (:foreground ,c3b))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,c4a))))
   `(js3-jsdoc-type-face ((,class (:foreground ,c4b))))
   `(js3-jsdoc-value-face ((,class (:foreground ,c4a))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,c3b))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,c2b))))
   `(js3-instance-member-face ((,class (:foreground ,c3b))))
   `(js3-private-function-call-face ((,class (:foreground ,c8b))))

   ;; linum
   `(linum ((,class (:background ,c5b))))

   ;; magit
   `(magit-branch ((,class (:foreground ,c2b))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-item-highlight ((,class (:inherit highlight :background nil))))
   `(magit-log-graph ((,class (:foreground ,c0b))))
   `(magit-log-sha1 ((,class (:foreground ,c6a))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,c8b))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,c2b))))
   `(magit-log-head-label-default ((,class (:foreground ,c6a :box nil :weight bold))))
   `(magit-log-head-label-local ((,class (:foreground ,c4a :box nil :weight bold))))
   `(magit-log-head-label-remote ((,class (:foreground ,c4a :box nil :weight bold))))
   `(magit-log-head-label-tags ((,class (:foreground ,c4b :box nil :weight bold))))
   `(magit-section-title ((,class (:foreground ,c3b :weight bold))))

   ;; magit `next'
   `(magit-section ((,class (:inherit nil))))
   `(magit-section-highlight ((,class (:background ,c5b))))
   `(magit-section-heading ((,class (:foreground ,c3a))))
   `(magit-branch-local ((,class (:foreground ,c1a))))
   `(magit-branch-remote ((,class (:foreground ,c6a))))
   `(magit-hash ((,class (:foreground ,white))))
   `(magit-diff-file-heading ((,class (:foreground ,c6a))))
   `(magit-diff-hunk-heading ((,class (:foreground ,c4a))))
   `(magit-diff-hunk-heading-highlight ((,class (:inherit magit-diff-hunk-heading :weight bold))))
   `(magit-diff-context ((,class (:foreground ,c9b))))
   `(magit-diff-context-highlight ((,class (:foreground ,c9a :background ,c5b))))
   `(magit-diff-added ((,class (:foreground ,c2b))))
   `(magit-diff-added-highlight ((,class (:foreground ,c2a :background ,c5b))))
   `(magit-diff-removed ((,class (:foreground ,c8b))))
   `(magit-diff-removed-highlight ((,class (:foreground ,c8a :background ,c5b))))

   ;; markdown
   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,c3b :underline t))))
   `(markdown-header-face-1 ((,class (:inherit org-level-1))))
   `(markdown-header-face-2 ((,class (:inherit org-level-2))))
   `(markdown-header-face-3 ((,class (:inherit org-level-3))))
   `(markdown-header-face-4 ((,class (:inherit org-level-4))))
   `(markdown-header-delimiter-face ((,class (:foreground ,c6b))))
   `(markdown-pre-face ((,class (:foreground ,white))))
   `(markdown-inline-code-face ((,class (:foreground ,white))))
   `(markdown-list-face ((,class (:foreground ,c4b))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   ;; message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,c6a))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,c6b))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,c2b :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,c4b :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,c4a))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; mmm-mode
   `(mmm-code-submode-face ((,class (:background ,c5b))))
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((,class (:background ,c5b))))

   ;; nrepl-eval-sexp-fu
   `(nrepl-eval-sexp-fu-flash ((,class (:background ,c4b))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))

   ;; org
   `(org-agenda-structure ((,class (:foreground ,c3a))))
   `(org-agenda-date ((,class (:foreground ,white))))
   `(org-agenda-done ((,class (:foreground ,c2a))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,c0a))))
   `(org-block ((,class (:background ,c5b :inherit 'fixed-pitch))))
   `(org-code ((,class (:foreground ,c9a :inherit 'fixed-pitch))))
   `(org-column ((,class (:inherit default))))
   `(org-column-title ((,class (:inherit mode-line :foreground ,c4a :weight bold :underline t))))
   `(org-date ((,class (:foreground ,c3b :underline t))))
   `(org-document-info ((,class (:foreground ,c7a))))
   `(org-document-info-keyword ((,class (:foreground ,c7b))))
   `(org-document-title ((,class (:weight bold :foreground ,c6a :height 1.44))))
   `(org-done ((,class (:foreground ,c2a))))
   `(org-ellipsis ((,class (:foreground ,c0a))))
   `(org-footnote ((,class (:foreground ,c4b))))
   `(org-formula ((,class (:foreground ,c6b))))
   `(org-hide ((,class (:foreground ,black :background ,black))))
   `(org-level-1 ((,class (:foreground ,c9a :height 1.4))))
   `(org-level-2 ((,class (:foreground ,c6a :height 1.3))))
   `(org-level-3 ((,class (:foreground ,c4a :height 1.2))))
   `(org-level-4 ((,class (:foreground ,c3b :height 1.1))))
   `(org-link ((,class (:foreground ,c1a :underline t))))
   `(org-scheduled ((,class (:foreground ,c6b))))
   `(org-scheduled-previously ((,class (:foreground ,c6b))))
   `(org-scheduled-today ((,class (:foreground ,c3b))))
   `(org-special-keyword ((,class (:foreground ,c6b))))
   `(org-table ((,class (:inherit org-block))))
   `(org-tag ((,class (:foreground ,c4a))))
   `(org-target ((,class (:foreground ,c2b))))
   `(org-time-grid ((,class (:inherit default))))
   `(org-todo ((,class (:foreground ,c8a))))
   `(org-upcoming-deadline ((,class (:foreground ,c6a))))
   `(org-verbatim ((,class (:inherit org-code))))
   `(org-warning ((,class (:foreground ,c6a))))
   `(org-checkbox ((,class (:inherit 'fixed-pitch))))

   ;; outline
   `(outline-1 ((,class (:inherit org-level-1))))
   `(outline-2 ((,class (:inherit org-level-2))))
   `(outline-3 ((,class (:inherit org-level-3))))
   `(outline-4 ((,class (:inherit org-level-4))))

   ;; parenface
   `(paren-face ((,class (:foreground ,c0b :background nil))))

   ;; powerline
   `(powerline-active1 ((t (:foreground ,c9b :background ,c5a))))
   `(powerline-active2 ((t (:foreground ,c9b :background ,c5b))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,c8a))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,c3a))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,c7a))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,c2a))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,c6a))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,c1a))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,c9a))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,c4a))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,c0a))))
   `(rainbow-delimiters-unmatched-face ((,class (:background ,c8a :weight bold))))

   ;; regex-tool
   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))
   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; sh-script
   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))

   ;; shr
   `(shr-link ((,class (:foreground ,c3b :underline t))))

   ;; slime
   `(slime-highlight-edits-face ((,class (:foreground ,c9a))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,c4a))))
   `(slime-repl-result-face ((,class (:foreground ,c2b))))
   `(slime-repl-output-face ((,class (:foreground ,c3b :background ,black))))

   ;; smart-mode-line
   `(sml/prefix ((,class (:foreground ,c2a))))
   `(sml/folder ((,class (:foreground ,c4a))))
   `(sml/filename ((,class (:foreground ,c6a))))
   `(sml/vc-edited ((,class (:foreground ,c7a))))

   ;; term
   ;; `(term-color-black ((,class (:background ,c5b :foreground ,c5b))))
   ;; `(term-color-blue ((,class (:background ,c3b :foreground ,c3b))))
   ;; `(term-color-cyan ((,class (:background ,c4b :foreground ,c4b))))
   ;; `(term-color-green ((,class (:background ,c1a :foreground ,c1a))))
   ;; `(term-color-magenta ((,class (:background ,c4a :foreground ,c4a))))
   ;; `(term-color-red ((,class (:background ,c8b :foreground ,c8b))))
   ;; `(term-color-white ((,class (:background ,c5a :foreground ,c5a))))
   ;; `(term-color-yellow ((,class (:background ,c6a :foreground ,c6a))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,c9b))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,c2b :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,c8b))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,c6a))))

   ;; web-mode
   `(web-mode-html-tag-face ((,class (:foreground ,c9a))))
   `(web-mode-html-attr-name-face ((,class (:inherit font-lock-doc-face))))
   `(web-mode-doctype-face ((,class (:inherit font-lock-builtin-face))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'flatui-dark)

;;; flatui-dark-theme.el ends here
