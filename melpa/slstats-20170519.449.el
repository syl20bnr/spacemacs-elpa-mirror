;;; slstats.el --- Acquire and display stats about Second Life -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.5
;; Package-Version: 20170519.449
;; Keywords: games
;; URL: https://github.com/davep/slstats.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; slstats.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; slstats.sl provides commands that make it easy to load and view the basic
;; stats about the Second Life grid and its economy. Included commands are:
;;
;; `slstats-signups' - Displays the sign-up total.
;;
;; `slstats-exchange-rate' - Displays the most recent L$ -> $ exchange rate.
;;
;; `slstats-inworld' - Displays how many avatars are in-world.
;;
;; `slstats-concurrency' - Displays concurrency data.
;;
;; `slstats-grid-size' - Displays stats about the size of the Second Life grid.
;;
;; `slstats' - Opens a window and displays all the above.
;;
;; `slstats-region-info' - Prompts for a Second Life region name and
;; displays information about it in a window.

;;; Code:

(require 'url)
(require 'cl-lib)

(defgroup slstats nil
  "Show stats and information about Second Life."
  :group 'games)

(defface slstats-caption
  '((t :inherit (bold font-lock-function-name-face)))
  "Face used on captions in the slstats output windows."
  :group 'slstats)

(defcustom slstats-cache-timeout (* 60 5)
  "Seconds to wait before deciding data in the cache is \"stale\"."
  :type 'integer
  :group 'slstats)

(defconst slstats-lab-url "http://secondlife.com/httprequest/homepage.php"
  "The URL that contains the SL statistics.")

(defconst slstats-concurrency-url "http://api.gridsurvey.com/metricquery.php?metric=concurrency"
  "The URL that contains the grid's concurrency data.")

(defconst slstats-grid-size-url "http://api.gridsurvey.com/metricquery.php?metric=grid_size"
  "The URL that contains grid size data.")

(defconst slstats-region-info-url "http://api.gridsurvey.com/simquery.php?region=%s"
  "The URL that gets data about a region.")

(defconst slstats-texture-url "http://secondlife.com/app/image/%s/2"
  "URL for viewing a Second Life texture.")

(defun slstats-get (key stats)
  "Get a value associated with KEY from STATS."
  (cdr (assoc key stats)))

(defun slstats-to-alist (stats)
  "Turn raw STATS list into an alist."
  (when stats
    (cons
     (cons (intern (concat ":" (car stats))) (cadr stats))
     (slstats-to-alist (cddr stats)))))

(defun slstats-load-data (url &optional sep)
  "Load data about SL from URL.

SEP is an optional separator that is passed to `split-string'."
  (with-current-buffer (url-retrieve-synchronously url t)
    (setf (point) (point-min))
    (when (search-forward-regexp "^$" nil t)
      (slstats-to-alist
       (cl-remove-if
        (lambda (s)
          (zerop (length s)))
        (split-string
         (buffer-substring-no-properties (point) (point-max))
         sep))))))

(defvar slstats-cache (make-hash-table :size 5)
  "Data cache.")

(defun slstats-from-cache (id)
  "Get stats with ID from the cache."
  (let ((cached (gethash id slstats-cache)))
    (when cached
      (let ((when-cached (car cached)))
        (when (< (- (time-to-seconds) when-cached) slstats-cache-timeout)
          (cdr cached))))))

(defun slstats-cache (id value)
  "Cache stats with ID and VALUE."
  (cdr (puthash id (cons (time-to-seconds) value) slstats-cache)))

(defun slstats-cached (id getter)
  "Get stats with ID from the cache, or use and cache result of GETTER."
  (or (slstats-from-cache id) (slstats-cache id (funcall getter))))

(defun slstats-load-lab-data ()
  "Load the raw statistics about Second Life from Linden Lab."
  (slstats-cached :lab-data (lambda () (slstats-load-data slstats-lab-url "\n"))))

(defun slstats-load-concurrency-data ()
  "Load the concurrency data."
  (slstats-cached :concurrency-data (lambda () (slstats-load-data slstats-concurrency-url))))

(defun slstats-load-grid-size-data ()
  "Load the grid size data."
  (slstats-cached :grid-size-data (lambda () (slstats-load-data slstats-grid-size-url))))

(defun slstats-load-region-data (region)
  "Load data about REGION."
  (let ((data (slstats-load-data (format slstats-region-info-url (url-hexify-string region)))))
    (unless (slstats-get :Error data)
      data)))

(defun slstats-format-time (time stats)
  "Format TIME from STATS as a string."
  (format-time-string "%F %T%z" (string-to-number (slstats-get time stats))))

(defun slstats-texture-url (uuid)
  "Return a Second Life texture URL for UUID."
  (format slstats-texture-url uuid))

(defun slstats-message (name data time)
  "Show a Second Life statistic as a message.

NAME is the title to give the statistic. DATA is the keyword for
finding the statistic. TIME is the keyword for finding the
last-update time for the statistic."
  (let ((stats (slstats-load-lab-data)))
    (message "%s: %s (as of %s)"
             name
             (slstats-get data stats)
             (slstats-format-time time stats))))

;;;###autoload
(defun slstats-signups ()
  "Display the Second Life sign-up count."
  (interactive)
  (slstats-message "Sign-ups" :signups :signups_updated_unix))

;;;###autoload
(defun slstats-exchange-rate ()
  "Display the L$ -> $ exchange rate."
  (interactive)
  (slstats-message "L$/$" :exchange_rate :exchange_rate_updated_unix))

;;;###autoload
(defun slstats-inworld ()
  "Display how many avatars are in-world in Second Life."
  (interactive)
  (slstats-message "Avatars in-world" :inworld :inworld_updated_unix))

;;;###autoload
(defun slstats-concurrency ()
  "Display the latest-known concurrency stats for Second Life."
  (interactive)
  (let ((stats (slstats-load-concurrency-data)))
    (message "As of %s: Min: %s, Max: %s, Mean: %s, Median: %s"
             (slstats-get :date stats)
             (slstats-get :min_online stats)
             (slstats-get :max_online stats)
             (slstats-get :mean_online stats)
             (slstats-get :median_online stats))))

;;;###autoload
(defun slstats-grid-size ()
  "Display the grid size data for Second Life."
  (interactive)
  (let ((stats (slstats-load-grid-size-data)))
    (message "Regions: Total: %s, Private: %s, Linden: %s, Adult: %s, Mature: %s, PG: %s, Linden Homes: %s"
             (slstats-get :total stats)
             (slstats-get :private stats)
             (slstats-get :linden stats)
             (slstats-get :adult stats)
             (slstats-get :mature stats)
             (slstats-get :pg stats)
             (slstats-get :linden_homes stats))))

(defun slstats-caption (s)
  "Add properties to S to make it a caption for the slstats outout."
  (propertize (concat s ": ") 'font-lock-face 'package-help-section-name))

(defun slstats-format-grid-size-total (title size stats)
  "Format a grid size total.

TITLE is the title to give the size. SIZE is the keyword of the
size we're going to format, and STATS is the stats list we'll
pull it from."
  (format "%s%s\n" (slstats-caption title) (slstats-get size stats)))

;;;###autoload
(defun slstats ()
  "Display available statistics about Second Life.

This includes information available about the state of the grid and the SL economy."
  (interactive)
  (let ((lab-stats (slstats-load-lab-data))
        (grid-size (slstats-load-grid-size-data))
        (grid-conc (slstats-load-concurrency-data)))
    (with-help-window "*Second Life Stats*"
      (with-current-buffer standard-output
        (insert
         (slstats-caption "Total sign-ups..")
         (slstats-get :signups lab-stats)
         "\n"
         (slstats-caption "Last updated....")
         (slstats-format-time :signups_updated_unix lab-stats)
         "\n\n"
         (slstats-caption "Exchange rate...")
         (slstats-get :exchange_rate lab-stats)
         "\n"
         (slstats-caption "Last updated....")
         (slstats-format-time :exchange_rate_updated_unix lab-stats)
         "\n\n"
         (slstats-caption "Avatars in-world")
         (slstats-get :inworld lab-stats)
         "\n"
         (slstats-caption "Last updated....")
         (slstats-format-time :inworld_updated_unix lab-stats)
         "\n\n"
         (slstats-caption "Grid size")
         "\n"
         (slstats-format-grid-size-total "Total......." :total grid-size)
         (slstats-format-grid-size-total "Private....." :private grid-size)
         (slstats-format-grid-size-total "Linden......" :linden grid-size)
         (slstats-format-grid-size-total "Adult......." :adult grid-size)
         (slstats-format-grid-size-total "Mature......" :mature grid-size)
         (slstats-format-grid-size-total "PG.........." :pg grid-size)
         (slstats-format-grid-size-total "Linden Homes" :linden_homes grid-size)
         "\n"
         (slstats-caption "Grid concurrency")
         "\n"
         (slstats-caption "As of..") (slstats-get :date grid-conc)
         "\n"
         (slstats-caption "Minimum") (slstats-get :min_online grid-conc)
         "\n"
         (slstats-caption "Maximum") (slstats-get :max_online grid-conc)
         "\n"
         (slstats-caption "Median.") (slstats-get :median_online grid-conc)
         "\n"
         (slstats-caption "Mean...") (slstats-get :mean_online grid-conc))))))

(defun slstats-insert-map (uuid)
  "Given a UUID, insert a map texture into the current buffer."
  (if (image-type-available-p 'jpeg)
      (let ((map (make-temp-file "slstats-map-" nil ".jpg")))
        (unwind-protect
            (progn
              (url-copy-file (slstats-texture-url uuid) map t)
              (insert-image-file map))
          (delete-file map)))
    (insert "This build of Emacs can't display jpeg files.")))

;;;###autoload
(defun slstats-region-info (region)
  "Display information for REGION."
  (interactive "sRegion: ")
  (if (zerop (length region))
      (error "Please provide a region name")
    (let ((region-info (slstats-load-region-data region)))
      (if region-info
          (with-help-window "*Second Life Region Information*"
            (with-current-buffer standard-output
              (insert
               (slstats-caption (concat "Information for " region))
               "\n\n"
               (slstats-caption "Grid position.....") (format "%s, %s"
                                                              (slstats-get :x region-info)
                                                              (slstats-get :y region-info))
               "\n"
               (slstats-caption "Status............") (slstats-get :status region-info)
               "\n"
               (slstats-caption "Maturity level....") (slstats-get :access region-info)
               "\n"
               (slstats-caption "Estate type.......")
               (slstats-get :estate region-info)
               "\n"
               (slstats-caption "First seen on grid")
               (slstats-get :firstseen region-info)
               "\n"
               (slstats-caption "Last seen on grid.")
               (slstats-get :lastseen region-info)
               " (as seen by GridSurvey.com)\n"
               (slstats-caption "Object map UUID..."))
              (help-insert-xref-button
               (slstats-get :objects_uuid region-info)
               'help-url
               (slstats-texture-url (slstats-get :objects_uuid region-info)))
              (insert
               "\n"
               (slstats-caption "Terrain map UUID.."))
              (help-insert-xref-button
               (slstats-get :terrain_uuid region-info)
               'help-url
               (slstats-texture-url (slstats-get :terrain_uuid region-info)))
              (insert
               "\n"
               (slstats-caption "Region UUID.......")
               (slstats-get :region_uuid region-info))
              (when (display-images-p)
                (insert
                 "\n\n"
                 (slstats-caption "Object map")
                 "\n")
                (slstats-insert-map (slstats-get :objects_uuid region-info))
                (setf (point) (point-max))
                (insert
                 "\n\n"
                 (slstats-caption "Terrain map")
                 "\n")
                (slstats-insert-map (slstats-get :terrain_uuid region-info)))))
        (error "%s is not a known region on the Second Life grid" region)))))

(provide 'slstats)

;;; slstats.el ends here
