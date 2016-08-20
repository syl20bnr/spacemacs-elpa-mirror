This extension mainly adds the following extra features:
  - Chinese localizations
  - Display holiday, lunar, horoscope, zodiac, solar term info on mode line
  - Define holidays using `holiday-lunar', `holiday-solar-term'
  - Highlight holidays based on different priorities
  - Add `cal-china-x-chinese-holidays', `cal-china-x-japanese-holidays'.
  - custom week diary(like weeks in school)

To use, add something like the following to your .emacs:
    (require 'cal-china-x)
    (setq mark-holidays-in-calendar t)
    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq calendar-holidays cal-china-x-important-holidays)

Note: for emacs22, please use version 1.1.

History

This is an early derived work from `chinese-calendar.el' written by
Charles Wang <charleswang@peoplemail.com.cn>.
