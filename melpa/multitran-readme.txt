Multitran is a zero-dependancy interface to http://multitran.com
online dictionary.

Multitran supports *tons* of languages, including such languages
as: Esperanto, Latin and Luxembourgish.  See http://multitran.com
for full list and feel free to add new languages to
`multitran-languages-map' if you missing one.

Variables to customize:
~~~~~~~~~~~~~~~~~~~~~~

* `multitran-languages' - Pair of languages for translation, for
  example ("ru" . "en") for russian <-> english translation

* `multitran-header-formatters' - Header line formatters
  You might want to add your custom formatters, like:

   (defun my-multitran--hf-wordfreq ()
     "Show word's frequency rank."
     (let ((wfreq (wordfreq-find (or multitran-word ""))))
       (and wfreq (format "FRank: %S" (cadr wfreq)))))

   (setq multitran-header-formatters
         '(miltitran--hf-word multitran--hf-languages
           my-multitran--hf-wordfreq multitran--hf-history))

  Where `wordfreq-find' is from
  https://raw.githubusercontent.com/zevlg/emacs-stuff/master/wordfreq.el

* `multitran-mode-hook' - hook is run after entering multitran-mode
