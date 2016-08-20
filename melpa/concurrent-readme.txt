'concurrent.el' is a higher level library for concurrent tasks
based on 'deferred.el'. This library has following features:

- Generator
- Green thread
- Semaphore
- Dataflow
- Signal/Channel

(require 'cl)

(require 'deferred)

(defvar cc:version nil "version number")
(setq cc:version "0.3")
