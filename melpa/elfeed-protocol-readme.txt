elfeed-protocol provide extra protocols to make self-hosting RSS
readers like ownCloud News, Tiny TIny RSS and NewsBlur works with
elfeed.  See the README for full documentation.

Usage:

  ;; curl recommend
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

  ;; setup extra protocol feeds
  (require 'elfeed-protocol)
  (setq elfeed-feeds (list
                      "owncloud+https://user1:pass1@myhost.com"
                      (list "owncloud+https://user2@myhost.com"
                            :password "password/with|special@characters:"
                            :autotags '(("example.com" comic)))))
  (elfeed-protocol-enable)
