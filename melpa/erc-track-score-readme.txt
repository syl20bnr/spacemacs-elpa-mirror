Change `erc-track-showcount' value to transform it to a score.

(require 'erc-track-score)
(erc-track-mode 1)
(erc-track-score-mode 1)
(setq erc-track-showcount t)

(eval-when-compile (require 'cl))
(require 'erc)
(require 'erc-track)
(require 'erc-match)
(require 'timer)
