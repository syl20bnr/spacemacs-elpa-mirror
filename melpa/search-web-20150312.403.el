;;; search-web.el --- Post web search queries using `browse-url'.
;; -*- coding: utf-8; -*-

;; Copyright (C) 2009 Tomoya Otake
;;               2014 nomaddo

;; Author: Tomoya Otake <tomoya.ton@gmail.com>
;; Version: 1.1
;; Package-Version: 20150312.403

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; * Description

;; Post web search queries using `browse-url'.
;;
;; * Installation:
;;
;; (require 'search-web)
;;
;; You should change following variables to fit your environment
;; by `M-x customize-group search-web`.
;; - search-web-default-browser
;; - search-web-in-emacs-browser
;; - search-web-external-browser
;;
;; The default values are the same as browse-url-default-browser.
;; This probably points external browser of emacs if you didn't change it.
;;
;; * usage:
;;
;; search-web-dwim: a main function
;;
;; Search selected words in a browser if you select a region.
;; Search at point if not.
;;
;; I recommend you add following code into your .emacs
;; if you use `popwin and text-browser `emacs-w3m or `eww
;; to popup *w3m* and *eww*.
;;
;; (defadvice w3m-browse-url (around w3m-browse-url-popwin activate)
;;    (save-window-excursion ad-do-it)
;;    (unless (get-buffer-window "*w3m*")
;;       (pop-to-buffer "*w3m*")))
;;
;; (defadvice eww-render (around eww-render-popwin activate)
;;   (save-window-excursion ad-do-it)
;;   (unless (get-buffer-window "*eww*")
;;     (pop-to-buffer "*eww*")))
;;
;; (push "*eww*" popwin:special-display-config)
;; (push "*w3m*" popwin:special-display-config)
;;
;;; Code:

(eval-when-compile (require 'cl))
;; for destructuring-bind and case

(defvar search-web-browser-functions
  ;; this originally came from browse-url-browser-function's :type
  '(choice
    (function-item :tag "Emacs W3" :value  browse-url-w3)
    (function-item :tag "W3 in another Emacs via `gnudoit'"
                   :value  browse-url-w3-gnudoit)
    (function-item :tag "Emacs W3M" :value w3m-browse-url)
    (function-item :tag "eww" :value  eww-browse-url)
    (function-item :tag "Mozilla" :value  browse-url-mozilla)
    (function-item :tag "Firefox" :value browse-url-firefox)
    (function-item :tag "Chromium" :value browse-url-chromium)
    (function-item :tag "Galeon" :value  browse-url-galeon)
    (function-item :tag "Epiphany" :value  browse-url-epiphany)
    (function-item :tag "Netscape" :value  browse-url-netscape)
    (function-item :tag "Mosaic" :value  browse-url-mosaic)
    (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
    (function-item :tag "Text browser in an xterm window"
                   :value browse-url-text-xterm)
    (function-item :tag "Text browser in an Emacs window"
                   :value browse-url-text-emacs)
    (function-item :tag "KDE" :value browse-url-kde)
    (function-item :tag "Elinks" :value browse-url-elinks)
    (function-item :tag "Specified by `Browse Url Generic Program'"
                   :value browse-url-generic)
    (function-item :tag "Default Windows browser"
                   :value browse-url-default-windows-browser)
    (function-item :tag "Default Mac OS X browser"
                   :value browse-url-default-macosx-browser)
    (function-item :tag "GNOME invoking Mozilla"
                   :value browse-url-gnome-moz)
    (function-item :tag "Default browser"
                   :value browse-url-default-browser)
    (function :tag "Your own function")
    (alist :tag "Regexp/function association list"
           :key-type regexp :value-type function))
  "Browsers you can choose as default"
  )

(defcustom search-web-default-browser 'browse-url-default-browser
  "Default Browser Function"
  :group 'search-web
  :type search-web-browser-functions)

(defcustom search-web-in-emacs-browser 'browse-url-default-browser
  "Default Browser Function Displaying In Emacs"
  :group 'search-web
  :type search-web-browser-functions)

(defcustom search-web-external-browser 'browse-url-default-browser
  "Default External Browser Function"
  :group 'search-web
  :type search-web-browser-functions)

(defcustom search-web-engines
  '(("sitepoint" "http://reference.sitepoint.com/?s=%s" nil)
    ("google" "http://www.google.com/search?q=%s" nil)
    ("google ja" "http://www.google.com/search?hl=ja&q=%s" nil)
    ("google en" "http://www.google.com/search?hl=en&q=%s" nil)
    ("google maps" "http://maps.google.co.jp/maps?hl=ja&q=%s" External)
    ("google scholar" "https://scholar.google.co.jp/scholar?q=%s" nil)
    ("youtube" "http://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
    ("twitter" "http://search.twitter.com/search?q=%s" External)
    ("goo" "http://dictionary.goo.ne.jp/srch/all/%s/m0u/" nil)
    ("answers" "http://www.answers.com/topic/%s" nil)
    ("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s&sa=Search" nil)
    ("eijiro" "http://eow.alc.co.jp/%s/UTF-8/" In-Emacs)
    ("cinii" "http://ci.nii.ac.jp/search?q=%s" nil)
    ("amazon" "http://www.amazon.com/s/url=search-alias%%3Daps&field-keywords=%s" External)
    ("amazon jp" "http://www.amazon.co.jp/gp/search?index=blended&field-keywords=%s" External)
    ("yahoo" "http://search.yahoo.com/search?p=%s" nil)
    ("yahoo jp" "http://search.yahoo.co.jp/search?p=%s" nil)
    ("wikipedia en" "http://www.wikipedia.org/search-redirect.php?search=%s&language=en" nil)
    ("wikipedia ja" "http://www.wikipedia.org/search-redirect.php?search=%s&language=ja" nil)
    ("stackoveflow en" "http://stackoverflow.com/search?q=%s" nil)
    ("stackoveflow ja" "http://ja.stackoverflow.com/search?q=%s" nil)
    ("duck" "https://duckduckgo.com/?q=%s" nil)
    )

  "A list of search engines. This is a list of lists which elements are
engine nick, url and browser function symbol.
Url have to includes %s. We replace %s by query words.
You can choose `External, `In-Emacs or nil as a browser function.
nil represents default browser."

  :type '(repeat
          (list :tag "Config"
                (string :tag "Engine Name")
                (string :tag "Url")
                (radio :tag "Choose Browsing Function"
                       (const :tag "Use Default" nil)
                       (choice :tag "Choose Function"
                               (const :tag "In-Emacs" In-Emacs)
                               (const :tag "External" External)
                               (function :tag "Other Function")))))
  :group 'search-web)

(defvar search-web-word-history '())
(defvar search-web-engine-history '())

(defun search-web (engine word)
  (interactive (list
                (search-web-query-engine)
                (read-string "Search Word: " nil 'search-web-word-history)))
  (destructuring-bind (engine url render)
      (assoc engine search-web-engines)
    (let* ((render
            (case render
              ((nil) search-web-default-browser)
              (In-Emacs search-web-in-emacs-browser)
              (External search-web-external-browser)
              (t render)))
           (temp browse-url-browser-function))
      (setq browse-url-browser-function render)
      (browse-url (format url (url-hexify-string word)))
      (setq browse-url-browser-function temp))
    ))

(defun search-web-query-engine ()
  (let* ((initial-engine (nth 0 search-web-engine-history))
         (prompt (case initial-engine
                   ((nil) "Search Engine: ")
                   (t (message "Search Engine [default: %s]: " initial-engine)))
                 ))
    (completing-read prompt search-web-engines
                     nil t nil 'search-web-engine-history initial-engine)))

(defmacro search-web-interactive ()
  '(interactive (list (search-web-query-engine))))

(defun search-web-at-point (engine)
  (search-web-interactive)
  (search-web engine (substring-no-properties (thing-at-point 'word))))

(defun search-web-region (engine)
  (search-web-interactive)
  (let* ((beg (mark)) (end (point)))
    (search-web engine (buffer-substring-no-properties beg end)))
  )

(defun search-web-dwim (engine)
  "Seach words you select as region or at point."
  (search-web-interactive)
  (cond
   ((region-active-p) (search-web-region engine))
   (t (search-web-at-point engine))))

(provide 'search-web)

;;; search-web.el ends here
