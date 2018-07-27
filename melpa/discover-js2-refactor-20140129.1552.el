;;; discover-js2-refactor.el --- Adds discover context menu for js2-refactor

;; Copyright (C) 2014 Nicolas Petton
;;
;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;; Keywords: js2-refactor discover
;; Package-Version: 20140129.1552
;; Package: discover-js2-refactor
;; Package-Requires: ((js2-refactor "20131221.501") (discover "20140103.1339"))

;; Version: 0.6

;; discover-js2-refactor.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; discover-js2-refactor.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary: Adds discover context menu for js2-refactor.
;;; Learn more about js2-refactor at https://github.com/magnars/js2-refactor.el and
;;; discover at https://github.com/mickeynp/discover.el

;;; Code:

(require 'js2-refactor)
(require 'discover)

(discover-add-context-menu
 :context-menu '(js2-refactor
		(description "JS2 Refactor")
		(actions
		 ("Functions"
		  ("ef" "extract function" js2r-extract-function)
		  ("em" "extract method" js2r-extract-method)
		  ("ip" "introduce parameter" js2r-introduce-parameter)
		  ("lp" "localize parameter" js2r-localize-parameter)
		  ("ao" "Arguments to object" js2r-arguments-to-object))
		 ("Variables"
		  ("ev" "Extract variable" js2r-extract-var)
		  ("iv" "Inline variable" js2r-inline-var)
		  ("rv" "Rename variable" js2r-rename-var)
		  ("vt" "var to this" js2r-var-to-this)
		  ("sv" "split var declaration" js2r-split-var-declaration))
		 ("Contract/Expand"
		  ("cu" "contract function" js2r-contract-function)
		  ("eu" "expand function" js2r-expand-function)
		  ("ca" "contract array" js2r-contract-array)
		  ("ea" "expand array" js2r-expand-array)
		  ("co" "contract object" js2r-contract-object)
		  ("eo" "expand object" js2r-expand-object))
		 ("Structure"
		  ("3i" "ternary to if" js2r-ternary-to-if)
		  ("uw" "unwrap" js2r-unwrap)
		  ("ig" "inject global in iife" js2r-inject-global-in-iife)
		  ("wi" "wrap buffer in iife" js2r-wrap-buffer-in-iife))
		 ("Misc"
		  ("lt" "log this" js2r-log-this)
		  ("sl" "forward slurp" js2r-forward-slurp)
		  ("ba" "forward barf" js2r-forward-barf))))
 :mode 'js2-mode
 :mode-hook 'js2-mode-hook
 :bind "C-c C-r")


(provide 'discover-js2-refactor)

;;; discover-js2-refactor.el ends here
