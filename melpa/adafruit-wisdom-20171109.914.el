;;; adafruit-wisdom.el --- Get/display adafruit.com quotes

;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Version: 0.1
;; Package-Version: 20171109.914
;; Keywords: games
;; URL: https://github.com/gonewest818/adafruit-wisdom.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; I've always enjoyed the engineering quotes found at the footer of
;; each page on adafruit.com ... now you can too!  This code is
;; derived from Dave Pearson's dad-joke.el, except adafruit.com
;; publishes their quotes as rss so we have to deal with that.

;;; Code:

(require 'url-vars)
(require 'xml)

(defconst adafruit-wisdom-quote-url "https://www.adafruit.com/feed/quotes.xml"
  "URL for the RSS quote feed on Adafruit.com.")

(defun adafruit-wisdom-get ()
  "Fetch the quotes.xml from adafruit.com and select one at random."
  (let* ((root (with-temp-buffer
                 (url-insert-file-contents adafruit-wisdom-quote-url)
                 (xml-parse-region (point-min) (point-max))))
         ;; parse the following form:
         ;; ((rss (channel (item ...) (item ...) (item ...) ...)))
         ;; where each item contains (item (title nil "the quote") ...)
         ;; and we need just "the quote"
         (rss   (car root))
         (chan  (car (xml-get-children rss 'channel)))
         (items (xml-get-children chan 'item))
         (pick  (nth (random (length items)) items))
         (title (car (xml-get-children pick 'title))))
    (car (last title))))

;;;###autoload
(defun adafruit-wisdom (&optional insert)
  "Display one of Adafruit's quotes in the minibuffer.
If INSERT is non-nil the joke will be inserted into the current
buffer rather than shown in the minibuffer."
  (interactive "P")
  (let ((quote (adafruit-wisdom-get)))
    (if (zerop (length quote))
        (error "Couldn't retrieve a quote from adafruit")
      (if insert
          (insert quote)
        (message "%s" quote)))))

(provide 'adafruit-wisdom)

;;; adafruit-wisdom.el ends here
