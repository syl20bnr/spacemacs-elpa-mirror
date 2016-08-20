* I want to keep my crontabs under rcs to keep a history of
  the file.  Editing them with 'crontab -e' is rather
  cumbersome.  My method is to keep the crontab as a file,
  under rcs, and check in the changes with 'C-c C-c' after
  editing.

* The remote systems are expected to share a filesystem.
  If they dont, modify crontab-shell or crontab-apply to
  suit your needs.

* You may want to add one of these to your startup:
  (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
  (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
