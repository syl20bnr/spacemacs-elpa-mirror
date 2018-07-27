;;; company-childframe.el --- Please use company-posframe instead.

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel, Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/company-mode/company-mode
;; Package-Version: 20180705.546
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company-posframe "0.1.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; * company-childframe README                                :README:

;; NOTE: this package will be renamed to
;; [[https://github.com/tumashu/company-posframe][company-posframe]]



;;; Code:
;; * company-childframe's code
(require 'cl-lib)
(require 'company-posframe)

;;;###autoload
(define-minor-mode company-childframe-mode
  "Company-childframe minor mode."
  :global t
  :require 'company-childframe
  :group 'company-childframe
  :lighter " company-childframe"
  (if company-childframe-mode
      (progn
        (company-posframe-mode 1)
        (message "NOTE: company-childframe has been renamed to company-posframe, please edit your config!"))
    (company-posframe-mode -1)))

(provide 'company-childframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-childframe.el ends here
