;;; org-chinese-utils.el --- A org-mode utils manager for Chinese users

;; * Header
;; Copyright (c) 2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/org-chinese-utils.git
;; Package-Version: 20160811.217
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; * org-chinese-utils README                                 :README:
;; ** 介绍
;; org-chinese-utils 是一个 org-mode 小工具管理器，可以方便 org-mode 中文用户：
;; 1. 将 org 文件导出为 HTML/ODT 文件时，删除不必要的空格。
;; 2. 按 'C-c C-c', 根据当前内容智能折行。
;; 3. 如果 org-babel 结果中包含表格时，对表格进行对齐处理。
;; 4. ...

;; [[./snapshots/org-chinese-utils.png]]

;; ** 安装
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET org-chinese-utils RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (require 'org)
;;    (require 'org-chinese-utils)
;;    (ocus-enable)
;;    #+END_EXAMPLE

;; ** 设置
;; 运行下面的命令后，会弹出一个选择器，用户用鼠标或者回车选择需要激活的 utils 就可以了。

;; #+BEGIN_EXAMPLE
;; M-x org-chinese-utils
;; #+END_EXAMPLE

;; ** 管理个人 utils
;; 用户可以使用 org-chinese-utils 管理自己的小工具，比如：

;; #+BEGIN_EXAMPLE
;; (add-hook 'org-mode-map 'my-hello-world)
;; (defun my-hello-world ()
;;   (message "Hello world!"))
;; #+END_EXAMPLE

;; 可以转换为 org-chinese-utils 配置：

;; #+BEGIN_EXAMPLE
;; (push '(hello-world
;;         :document "Hello world!"
;;         :function my-hello-world
;;         :hook org-mode-hook)
;;       ocus-config)
;; #+END_EXAMPLE

;; 如果用户想将自己的小工具集成到 org-chinese-utils, 可以到 [[https://github.com/tumashu/org-chinese-utils/issues][这里]]
;; 提交 Issue. 并贴出小工具的的配置。

;;; Code:
;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'cus-edit)

(defgroup org-chinese-utils nil
  "Some org-mode utils for Chinese users."
  :group 'org)

(defcustom ocus-config nil
  "A utils list that can be enabled or disabled
by `org-chinese-utils'.

A utils is a plist, which form is like:

  (NAME :document DOC :function FUNC :hook HOOK)

1. NAME is a symbol, which can be passed to `ocus-activate'
   or `ocus-deactivate'.
2. FUNC is a function which will be added to HOOK.
3. DOC will be showed in org-chinese-utils chooser."
  :group 'org-chinese-utils)

(defconst ocus-buildin-config
  '((clean-paragraph-space
     :document "Org 文件导出为 HTML 或 odt 文件时，删除中文段落中多余的空格。"
     :function ocus:clean-useless-space
     :hook org-export-filter-paragraph-functions)
    (clean-headline-space
     :document "Org 文件导出为 HTML 或 odt 文件时，删除中文标题中多余的空格。"
     :function ocus:clean-useless-space
     :hook org-export-filter-headline-functions)
    (align-babel-table
     :document "让 org-babel 运行结果中包含的 org 表格对齐。"
     :function ocus:align-babel-table
     :hook org-babel-after-execute-hook)
    (smart-truncate-lines
     :document "按 'C-c C-c' 快捷键时，根据光标处的内容智能折行，（禁用后需重启 emacs）。"
     :function ocus:smart-truncate-lines
     :hook org-mode-hook)
    (show-babel-image
     :document "让 org-babel 运行结果中包含的图片链接自动显示。"
     :function ocus:show-babel-image
     :hook org-babel-after-execute-hook)
    (visual-line-mode
     :document "打开 org 文件时，激活 visual-line-mode, （禁用后需重启 emacs）。"
     :function ocus:visual-line-mode
     :hook org-mode-hook)
    (org-cdlatex
     :document "打开 org 文件时，激活 cdlatex 功能。"
     :function ocus:org-cdlatex
     :hook org-mode-hook))
  "This variable include buildin utils, which is similar to `ocus-config'.")

(defvar ocus-enabled-utils
  '(clean-paragraph-space
    align-babel-table
    smart-truncate-lines
    show-babel-image
    visual-line-mode)
  "The utils of org-chinese-utils which will be activated.")

(defvar ocus-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "\C-x\C-s" 'ocus-save-setting)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `ocus-mode'.")

(define-derived-mode ocus-mode special-mode "OCUS"
  "Major mode for selecting org-chinese-utils.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map ocus-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (_ignore-auto noconfirm)
         (when (or noconfirm (y-or-n-p "Discard current choices? "))
           (ocus (current-buffer))))))
(put 'ocus-mode 'mode-class 'special)

;;;###autoload
(defalias 'org-chinese-utils 'ocus)
;;;###autoload
(defun ocus (&optional buffer)
  (interactive)
  (switch-to-buffer (get-buffer-create (or buffer "*Org-chinese-utils chooser*")))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (ocus-mode)
  (setq truncate-lines t)
  (widget-insert "Type RET or click to enable/disable utils of org-chinese-utils.\n\n")
  (widget-create 'push-button
                 :tag " Save settings! "
                 :help-echo "Save the selected utils for future sessions."
                 :action 'ocus-save-setting)
  (widget-insert "\n\nAvailable utils of org-chinese-utils:\n\n")
  (let ((help-echo "mouse-2: Enable this utils for this session")
        (config-list (ocus--return-all-config))
        widget)
    (dolist (utils (mapcar 'car config-list))
      (setq widget (widget-create 'checkbox
                                  :value (memq utils ocus-enabled-utils)
                                  :utils-name utils
                                  :help-echo help-echo
                                  :action 'ocus-checkbox-toggle))
      (widget-create-child-and-convert widget 'push-button
                                       :button-face-get 'ignore
                                       :mouse-face-get 'ignore
                                       :value (format " %s " utils)
                                       :action 'widget-parent-action
                                       :help-echo help-echo)
      (widget-insert " -- " (plist-get (cdr (assq utils config-list))
                                       :document)
                     ?\n)))
  (goto-char (point-min))
  (widget-setup))

(defun ocus--return-all-config ()
  `(,@ocus-config ,@ocus-buildin-config))

(defun ocus-save-setting (&rest _ignore)
  (interactive)
  (customize-save-variable
   'ocus-enabled-utils ocus-enabled-utils)
  (message "Setting of org-chinese-utils is saved."))

(defun ocus-checkbox-toggle (widget &optional event)
  (let ((utils (widget-get widget :utils-name)))
    (widget-toggle-action widget event)
    (if (widget-value widget)
        (progn (push utils ocus-enabled-utils)
               (ocus-activate (list utils)))
      (setq ocus-enabled-utils
            (remove utils ocus-enabled-utils))
      (ocus-deactivate (list utils)))))

;;;###autoload
(defun ocus-activate (utils-list)
  "Activate certain utils of org-chinese-utils.

UTILS-LIST should be a list of utils which should be activated."
  (dolist (utils utils-list)
    (let* ((plist (cdr (assq utils (ocus--return-all-config))))
           (fn (plist-get plist :function))
           (hook (plist-get plist :hook)))
      (when (and fn hook)
        (add-hook hook fn)))))

;;;###autoload
(defun ocus-deactivate (&optional utils-list)
  "Deactivate certain utils of org-chinese-utils.

This function is the opposite of `ocus-deactive'.  UTILS-LIST
should be a list of utils which should be activated."
  (let* ((config-list (ocus--return-all-config))
         (utils-list (or utils-list (mapcar 'car config-list))))
    (dolist (utils utils-list)
      (let* ((plist (cdr (assq utils config-list)))
             (fn (plist-get plist :function))
             (hook (plist-get plist :hook)))
        (when (and fn hook)
          (remove-hook hook fn))))))

;;;###autoload
(defalias 'org-chinese-utils-enable 'ocus-enable)

;;;###autoload
(defun ocus-enable ()
  "Enable all org-chinese-utils, when DISABLE is t, disable all utils."
  (interactive)
  (if (and (featurep 'org)
           (featurep 'ox))
      (add-hook 'org-mode-hook
                (lambda ()
                  (ocus-deactivate)
                  (ocus-activate ocus-enabled-utils)))
    (message "Package 'org' or 'ox' is unavailable.")))

(defun ocus:clean-useless-space (text backend info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (or (org-export-derived-backend-p backend 'html)
            (memq backend '(odt)))
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)[ ]+\\(<\\)" regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(>\\)[ ]+\\(%s\\)" regexp)
             "\\1\\2" string))
      string)))

(defun ocus:smart-truncate-lines (&optional arg)
  (interactive)
  (org-defkey org-mode-map "\C-c\C-c" 'ocus:ctrl-c-ctrl-c))

(defun ocus:ctrl-c-ctrl-c (&optional arg)
  "根据光标处内容，智能折行，比如，在表格中禁止折行。"
  (interactive "P")
  (cond ((or (and (boundp 'org-clock-overlays)
                  org-clock-overlays)
             org-occur-highlights)
         (and (boundp 'org-clock-overlays)
              (org-clock-remove-overlays))
         (org-remove-occur-highlights)
         (org-remove-latex-fragment-image-overlays)
         (message "Temporary highlights/overlays removed from current buffer"))
        (t (let* ((context (org-element-context))
                  (type (org-element-type context)))
             (cl-case type
               ((table table-cell table-row item plain-list)
                (toggle-truncate-lines 1))
               (t (toggle-truncate-lines -1))))))
  (org-ctrl-c-ctrl-c arg))

(defun ocus:align-babel-table (&optional info)
  "Align all tables in the result of the current babel source."
  (interactive)
  (when (not org-export-current-backend)
    (let ((location (org-babel-where-is-src-block-result nil info)))
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
              (when (org-at-table-p)
                (toggle-truncate-lines 1)
                (org-table-align)
                (goto-char (org-table-end)))
              (forward-line))))))))

(defun ocus:visual-line-mode ()
  (setq visual-line-fringe-indicators '(nil nil))
  (visual-line-mode)
  (if visual-line-mode
      (setq word-wrap nil)))

(defun ocus:show-babel-image ()
  (when (not org-export-current-backend)
    (org-display-inline-images)))

(defun ocus:org-cdlatex ()
  (if (featurep 'cdlatex)
      (turn-on-org-cdlatex)
    (message "Fail to active org-cdlatex, you should load cdlatex first.")))

;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'org-chinese-utils)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-chinese-utils.el ends here
;; #+END_SRC
