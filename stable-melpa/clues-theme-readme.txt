Clues was initially based on a Visual Studio theme called 'Blues 'n
Roots' however it's a long way from looking much like it, aside
from the occasional color accent, Blues (despite it's name) has a
more toasted caramel flavor. Clues on the other hand is made up of
cooling colors with a couple of flecks of light cream/brown/orange
to break up any monotony, with yellow/gold rainbow-delimiters.

Clues has become my working theme of choice since about 7/20/2013,
and when used with xterm-frobs.el, it looks great in the terminal
as well as the GUI.


(deftheme clues
  "clues theme - an Emacs 24 theme which may well be fully awesome...")

(custom-theme-set-variables
 'clues
 '(fringe-mode 10 nil (fringe))
 '(linum-format     " %6d "  )
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343"))

(custom-theme-set-faces
 'clues
 '(cursor                              ((t (                       :background "orange"                                                ))))
 '(default
   (
    (((class color) (min-colors 16777216)) (:foreground "#C0E9F6" :background "#110B14"))
    (((class color) (min-colors 88))       (:foreground "#fff" :background "#000"))
    (((class color) (min-colors 16))       (:foreground "#fff" :background "#000"))
    (((class color) (min-colors 8))        (:foreground "#fff" :background "#000")))
   )
 '(linum                               ((t (:foreground "#6a6a6a"  :background "#110B14"                                               ))))
 '(minibuffer-prompt                   ((t (:foreground "#1278A8"  :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground "orange"   :background nil                                                     ))))
 '(region                              ((t (                       :background "#342858"                                               ))))
 '(shadow                              ((t (:foreground "#777777"  :background nil                                                     ))))
 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(font-lock-builtin-face              ((t (:foreground "#55B1E2"  :background nil                                                     ))))
 '(font-lock-comment-face              ((t (:foreground "#90A0A0"  :background nil       :slant italic                                 ))))
 '(font-lock-constant-face             ((t (:foreground "#CFC5A9"  :background nil                                                     ))))
 `(font-lock-doc-string-face           ((t (:foreground "#DFD3E9"  :background nil                                                     ))))
 '(font-lock-function-name-face        ((t (:foreground "#BFC3A9"  :background nil                                                     ))))
 '(font-lock-keyword-face              ((t (:foreground "#55C0D2"  :background nil                                                     ))))
 '(font-lock-negation-char-face        ((t (:foreground "#C75311"  :background nil                                                     ))))
 '(font-lock-string-face               ((t (:foreground "#94D4D9"  :background nil                                                     ))))
 '(font-lock-variable-name-face        ((t (:foreground "#BDBA9F"  :background nil                                                     ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa"  :background nil       :inherit (font-lock-comment-face)             ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#A16C26"  :background nil       :inherit (font-lock-builtin-face)             ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500"  :background nil       :inherit (bold)                               ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"      :background nil       :inherit (bold)                               ))))
 '(font-lock-doc-face                  ((t (:foreground "#90A0A0"  :background nil       :inherit (font-lock-string-face)              ))))
 '(font-lock-warning-face              ((t (:foreground "#008000"  :background nil       :inherit (error)                              ))))
 '(font-lock-type-face                 ((t (:foreground "#55aadd"  :background nil       :inherit (default)                            ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                       :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(fringe                              ((t (                       :background "#1B0F1f"                                               ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))
 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))
 '(mode-line-inactive                  ((t (:foreground "#555555"  :background "#111111" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#777777"  :background "#111111" :box nil :inherit (variable-pitch) ))))
 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))
 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))
 '(highlight-indentation-face          ((t (                       :background "#122930"                                               ))))
 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))
 ;; flyspell-mode
 `(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 `(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))
  ;; flymake-mode
 `(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 `(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))
 ;; Magit hightlight
 `(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))
 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))
 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-changed                        ((t (:background "#103090"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))

 '(rainbow-delimiters-depth-1-face     ((t (:foreground "#99998A" ))))
 '(rainbow-delimiters-depth-2-face     ((t (:foreground "#776644" ))))
 '(rainbow-delimiters-depth-3-face     ((t (:foreground "#888866" ))))
 '(rainbow-delimiters-depth-4-face     ((t (:foreground "#AAAA88" ))))
 '(rainbow-delimiters-depth-5-face     ((t (:foreground "#CCCCAA" ))))
 '(rainbow-delimiters-depth-6-face     ((t (:foreground "#DEDEAA" ))))
 '(rainbow-delimiters-depth-7-face     ((t (:foreground "#EFEFBB" ))))
 '(rainbow-delimiters-depth-8-face     ((t (:foreground "#FFFFCC" ))))
 '(rainbow-delimiters-depth-9-face     ((t (:foreground "#FFFFEE" ))))
 '(rainbow-delimiters-depth-unmatched-face     ((t (:foreground "#AA0000" ))))
 )

###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'clues)

Local Variables:
eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
End:

clues-theme.el ends here
