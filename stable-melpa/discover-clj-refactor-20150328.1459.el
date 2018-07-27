;;; discover-clj-refactor.el --- Adds discover context menu for clj-refactor

;; Copyright (C) 2015 Marian Schubert
;;
;; Author: Marian Schubert <marian.schubert@gmail.com>
;; Keywords: clj-refactor discover convenience
;; Package-Version: 20150328.1459
;; Package-Requires: ((clj-refactor "0.14.0") (discover "0.3"))

;; Version: 0.1

;; discover-clj-refactor.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; discover-clj-refactor.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary: Adds discover context menu for clj-refactor.
;;; Learn more about clj-refactor at https://github.com/clojure-emacs/clj-refactor.el and
;;; discover at https://github.com/mickeynp/discover.el

;;; Code:

(require 'clj-refactor)
(require 'discover)

(discover-add-context-menu
 :context-menu '(clj-refactor
                 (description "Clojure Refactor")
                 (actions
                  ("Basic"
                   ("rs" "rename symbol" cljr-rename-symbol)
                   ("ef" "extract function" cljr-extract-function)
                   ("rf" "rename file" cljr-rename-file)
                   ("mf" "move form to namespace" cljr-move-form))

                  ("Let"
                   ("il" "introduce" cljr-introduce-let)
                   ("el" "expand" cljr-expand-let)
                   ("rl" "remove" cljr-remove-let)
                   ("ml" "move to" cljr-move-to-let))

                  ("Threading"
                   ("th" "thread" cljr-thread)
                   ("tf" "first all" cljr-thread-first-all)
                   ("tl" "last all" cljr-thread-last-all)
                   ("uw" "unwind" cljr-unwind)
                   ("ua" "unwind all" cljr-unwind-all)
                   ("ct" "cycle" cljr-cycle-thread))

                  ("Namespace"
                   ("ad" "add declaration" cljr-add-declaration)
                   ("ai" "add import" cljr-add-import-to-ns)
                   ("ap" "add dependency" cljr-add-project-dependency)
                   ("am" "add missing libspec" cljr-add-missing-libspec)
                   ("ar" "add require" cljr-add-require-to-ns)
                   ("au" "add use" cljr-add-use-to-ns)
                   ("ru" "replace use" cljr-replace-use)
                   ("sn" "sort" cljr-sort-ns)
                   ("cn" "clean" cljr-clean-ns)
                   ("sr" "stop referring" cljr-stop-referring)
                   ("rr" "remove unused requires" cljr-remove-unused-requires))

                  ("Project"
                   ("hd" "hotload dependency" cljr-hotload-dependency)
                   ("sp" "sort dependencies" cljr-sort-project-dependencies)
                   ("pc" "clean project" cljr-project-clean))

                  ("Other"
                   ("fu" "find usages" cljr-find-usages)
                   ("cc" "cycle coll" cljr-cycle-coll)
                   ("ci" "cycle if" cljr-cycle-if)
                   ("cp" "cycle privacy" cljr-cycle-privacy)
                   ("cs" "cycle stringlike" cljr-cycle-stringlike)
                   ("dk" "destructure keys" cljr-destructure-keys)
                   ("pf" "promote function" cljr-promote-function)
                   ("rd" "remove debug fns" cljr-remove-debug-fns))))

 :mode 'clojure-mode
 :mode-hook 'clojure-mode-hook
 :bind "C-c j")


(provide 'discover-clj-refactor)

;;; discover-clj-refactor.el ends here
