Circadian provides automated theme switching based on daytime.

Example usage (with `use-package') featuring `nyx-theme' and `hemera-theme':

(use-package circadian
  :config
  (setq circadian-themes '(("8:00" . hemera)
                           ("19:30" . nyx)))
  (circadian-setup))
