Installation:

To use `russian-holidays-holidays' exclusively for the calendar

 (setq calendar-holidays russian-holidays)

To use 'russian-holidays-holidays' additionally to other calenders

 (setq calendar-holidays (append calendar-holidays russian-holidays))

If you'd like to add regional holidays, you can use it like in this
example for Chuvash respublic

 (setq calendar-holidays (append calendar-holidays russian-holidays russian-holidays-ch-holidays))

This works for for all regions noted in
https://ru.wikipedia.org/wiki/Праздники_России article.

 `russian-holidays-ad-holidays' for Adygea
 `russian-holidays-ba-holidays' for Bashkiria
 `russian-holidays-ta-holidays' for Tatarstan
 etc.

The code inspired by german-holidays.el and ukrainian-holidays.el.
