Remove default holidays, then append mexican calendar

(customize-set-variable 'holiday-bahai-holidays nil)
(customize-set-variable 'holiday-hebrew-holidays nil)
(customize-set-variable 'holiday-islamic-holidays nil)
(setq calendar-holidays (append calendar-holidays holiday-mexican-holidays))


(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar holiday-mexican--statutory-holidays
  '((holiday-fixed 1 1 "Año Nuevo")
    (holiday-fixed 2 5 "Día de la Constitución")
    (holiday-fixed 3 21 "Natalicio de Benito Juárez")
    (holiday-fixed 4 1 "Día del Trabajo")
    (holiday-fixed 9 16 "Día de la Independencia de México")
    (holiday-fixed 11 20 "Día de la Revolución Mexicana")
    (holiday-fixed 12 25 "Navidad")
    ))

(defvar holiday-mexican--civic-holidays
  '((holiday-fixed 2 19 "Día del Ejército")
    (holiday-fixed 2 24 "Día de la Bandera")
    (holiday-fixed 3 18 "Aniversario de la Expropiación petrolera")
    (holiday-fixed 4 21 "Heroica Defensa de Veracruz")
    (holiday-fixed 5 5 "Aniversario de la Batalla de Puebla")
    (holiday-fixed 6 1 "Día de la Marina")
    (holiday-fixed 9 13 "Día de los Niños Héroes")
    (holiday-fixed 9 15 "Grito de Dolores")
    (holiday-fixed 9 27 "Cnosumación de la Independencia de México")
    (holiday-fixed 10 12 "Descubrimiento de América")
    ))

(defvar holiday-mexican--festivities
  '((holiday-fixed 1 6 "Día de los Reyes Magos")
    (holiday-fixed 2 14 "Día de San Valentín")
    (holiday-fixed 4 30 "Día del Niño")
    (holiday-fixed 5 10 "Día de las Madres")
    (holiday-fixed 5 15 "Día del Docente")
    (holiday-fixed 5 23 "Día del estudiante")
    (holiday-float 6 0 3 "Día del Padre")
    (holiday-fixed 8 28 "Día del Abuelo")
    (holiday-fixed 10 31 "Halloween")
    (holiday-fixed 11 1 "Día de Todos los Santos")
    (holiday-fixed 11 2 "Día de Muertos")
    (holiday-fixed 12 24 "Nochebuena")
    (holiday-fixed 12 28 "Día de los Inocentes")
    (holiday-fixed 12 31 "Vispera de Año Nuevo")
    ))

(defvar holiday-mexican--christian
  '(
    (holiday-fixed 2 2 "Día de la Candelaria")
    (holiday-fixed 12 12 "Día de la Virgen de Guadalupe")
    (holiday-easter-etc 0 "Día de Pascua")
    (holiday-easter-etc -46 "Miércoles de Ceniza")
    (holiday-easter-etc -7 "Domingo de Ramos")
    (holiday-easter-etc -1 "Sábado Santo")
    (holiday-easter-etc 39 "Día de la Ascención")
    (holiday-easter-etc 49 "Pentecostés")
    (holiday-easter-etc 60 "Corpus Christi")
    (holiday-fixed 8 15 "Día de la Asunción de María")
    ))

(defvar holiday-mexican-holidays
  (append holiday-mexican--statutory-holidays holiday-mexican--christian holiday-mexican--festivities holiday-mexican--civic-holidays)
)

(provide 'mexican-holidays)
mexican-holidays.el ends here
