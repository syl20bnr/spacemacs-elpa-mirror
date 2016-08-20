Usage

(eval-after-load 'flycheck
  '(require 'flycheck-protobuf))
(add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t)
