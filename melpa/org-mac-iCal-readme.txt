This file provides the import of events from Mac OS X 10.5 iCal.app
into the Emacs diary (it is not compatible with OS X < 10.5). The
function org-mac-iCal will import events in all checked iCal.app
calendars for the date range org-mac-iCal-range months, centered
around the current date.

CAVEAT: This function is destructive; it will overwrite the current
contents of the Emacs diary.

Installation: add (require 'org-mac-iCal) to your .emacs.

If you view Emacs diary entries in org-agenda, the following hook
will ensure that all-day events are not orphaned below TODO items
and that any supplementary fields to events (e.g. Location) are
grouped with their parent event

(add-hook 'org-agenda-cleanup-fancy-diary-hook
	  (lambda ()
	    (goto-char (point-min))
	    (save-excursion
	      (while (re-search-forward "^[a-z]" nil t)
		(goto-char (match-beginning 0))
		(insert "0:00-24:00 ")))
	    (while (re-search-forward "^ [a-z]" nil t)
	      (goto-char (match-beginning 0))
	      (save-excursion
		(re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
	      (insert (match-string 0)))))
