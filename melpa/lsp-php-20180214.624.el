;;; lsp-php.el --- php support for lsp-mode
;;
;; Copyright (C) 2017 zg
;;
;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/lsp-php
;; Package-Version: 20180214.624
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0"))
;; Keywords: convenience, php
;;
;;; Commentary:
;; (require 'lsp-php)
;; (add-hook 'php-mode-hook #'lsp-php-enable)
;;
;;; Code:

(require 'lsp-mode)

;;;###autoload
(defcustom lsp-php-server-install-dir (file-truename (locate-user-emacs-file "php-language-server/"))
  "Install directory for php-language-server.
The slash is expected at the end."
  :group 'lsp-mode
  :risky t
  :type 'directory)

(defun lsp-php--ls-command ()
  "Lsp php command."
  (let ((php-language-server-file
	 (concat lsp-php-server-install-dir "vendor/felixfbecker/language-server/bin/php-language-server.php")))
    `("php"
      ,php-language-server-file)))

(defun lsp-php--get-root ()
  "Php project root directory."
  (cond
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("composer.json" ".dir-locals.el" ".project")))
	(or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
	    default-directory)))))

(lsp-define-stdio-client lsp-php "php" #'lsp-php--get-root  (lsp-php--ls-command))

(provide 'lsp-php)

;;; lsp-php.el ends here
