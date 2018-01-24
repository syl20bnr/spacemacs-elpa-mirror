Bind shrink-whitespace to a key, and start spamming it in places where you
want to remove whitespace. M-\ is not a bad key for this, as it does
something analagous, but isn't quite as smart.

(global-set-key (kbd "M-\\") 'shrink-whitespace)
----------------------------------------------------------
