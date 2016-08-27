This package provides a single function -- holiday-pascha-etc --
which calculates the date of Eastern Orthodox Christianity's date
for celebrating holidays based on distance from Pascha.


(defun holiday-pascha-etc (&optional n string)
  "Date of Easter according to the rule of the Council of Nicaea."
  ;; Backwards compatibility layer.
  (when (not n)
    (apply 'append
           (mapcar (lambda (e)
                     (apply 'holiday-easter-etc e))
                   ;; The combined list is not in order.
                   (append
                    (when calendar-christian-all-holidays-flag
                      '((-70 "The Publican and Pharisee")
                        (-63 "The Prodigal Son")
                        (-56 "The Last Judgement (Meatfare Sunday)")
                        (-49 "Sunday of Forgiveness (Cheesefare Sunday)")
                        (-42 "Sunday of Orthodoxy")
                        (-35 "St Gregory Palamas")
                        (-28 "Veneration of the Holy Cross")
                        (-21 "The Ladder of Divine Ascent")
                        (-14 "Saint Mary of Egypt")
                        (-8 "Lazarus Saturday")
                        (-7 "Palm Sunday")
                        (-3 "Holy Thursday")
                        (7 "Sunday of Thomas")
                        (14 "Holy Myrrhbearers")
                        (21 "Sunday of the Paralytic")
                        (28 "Sunday of the Samaratan Woman")
                        (35 "Sunday of the Blind Man")
                        (39 "Ascension")
                        (42 "Sunday of the Fathers of the First Ecumenical Council")
                        (49 "Pentecost")))
                    '((-48 "Clean Monday")
                      (-2 "Holy Friday")
                      (0 "Easter Sunday"))))))
  (let* ((m displayed-month)
         (y displayed-year)
         (julian-year (progn
                        (calendar-increment-month m y 1)
                        (calendar-extract-year
                         (calendar-julian-from-absolute
                          (calendar-absolute-from-gregorian
                           (list m (calendar-last-day-of-month m y) y))))))
         (shifted-epact                 ; age of moon for April 5
          (% (+ 14
                (* 11 (% julian-year 19)))
             30))
         (paschal-moon      ; day after full moon on or after March 21
          (- (calendar-julian-to-absolute (list 4 19 julian-year))
             shifted-epact))
         (nicaean-easter           ; Sunday following the Paschal moon
          (calendar-gregorian-from-absolute
           (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))))
    (if (calendar-date-is-visible-p nicaean-easter)
        (list (list nicaean-easter "Pascha")))))

(provide 'holiday-pascha-etc)
Local Variables:
time-stamp-pattern: "20/^;; Last Modified: <%%>$"
End:

holiday-pascha-etc.el ends here
