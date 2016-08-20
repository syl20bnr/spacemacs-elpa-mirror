;;; chinese-remote-input.el --- Input Chinese with a remote input method (e.g. Android Voice Input Method)

;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/chinese-remote-input
;; Package-Version: 20150110.2103
;; Version: 0.0.1
;; Keywords: convenience, Chinese

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Chinese-remote-input 可以让用户通过智能手机输入法（比如 Andorid 语音输入法）来远程输入中文。
;; 其工作原理是：
;;
;; 1. 在当前工作的计算机上安装ssh服务器。
;; 2. 在Android手机中安装ssh客户端，比如：JuiceSSH 或者 ConnectBot等。
;; 3. 在计算机上开启 emacs-daemon，打开待录入文件，并通过命令 `remote-input-toggle' 激活 Chinese-remote-input。
;; 4. 从手机上远程登录计算机，并运行一个 emacsclient，然后通过命令 `remote-input-terminal' 开启一个中文远程输入终端。
;; 5. 在中文远程输入终端中输入中文后按回车键，当前行对应的中文字符串就会插入到待编辑文件的光标处。
;;

;;; Code:

(require 'comint)

(defgroup chinese-remote-input nil
  "Input Chinese with a remote input method"
  :group 'leim)

(defcustom remote-input-terminal-return-key-regexp "。。\\|，，\\|！！\\|？？"
  "Remote-Input-Terminal 提交时，将与之匹配的字符串替换为换行符。"
  :group 'chinese-remote-input
  :type 'regexp)

(defcustom remote-input-terminal-prompt-regexp "> "
  "Regexp to match prompts in Remote-Input-Terminal"
  :group 'chinese-remote-input
  :type 'string)

(defcustom remote-input-terminal-buffer-name " *Remote-Input-Terminal*"
  "Remote-Input-Terminal 对应的buffer名称。"
  :group 'chinese-remote-input
  :type 'string)

(defvar remote-input-terminal-buffer nil)
(defvar remote-input-origin-monitor-timer nil)
(defvar remote-input-origin-buffer nil)
(defvar remote-input-origin-point nil)

(make-variable-buffer-local 'remote-input-origin-point)

;;;###autoload
(defun remote-input-activate ()
  "激活remote-input，通过一个timer，不断的获取(1秒1次)待编辑
文件对应的buffer以及光标位置。"
  (interactive)
  (when remote-input-origin-monitor-timer
    (cancel-timer remote-input-origin-monitor-timer))
  (setq remote-input-origin-monitor-timer
        (run-with-timer 0 0.5 'remote-input-get-origin-buffer-info))
  (message "Remote-Input activate."))

;;;###autoload
(defun remote-input-deactivate ()
  "禁用remote-input."
  (interactive)
  (when remote-input-origin-monitor-timer
    (cancel-timer remote-input-origin-monitor-timer))
  (setq remote-input-origin-monitor-timer nil)
  (message "Remote-Input deactivate."))

;;;###autoload
(defun remote-input-toggle ()
  (interactive)
  (if remote-input-origin-monitor-timer
      (remote-input-deactivate)
    (remote-input-activate)))

(defun remote-input-get-origin-buffer-info (&optional enable)
  "得到待编辑文件对应的buffer和光标位置。用变量：`remote-input-origin-buffer'
和 `remote-input-origin-point' 保存对应值。忽略 Remote-Input-Terminal 对应
的buffer。"
  (when (and (> (display-pixel-width) 50)
             (not (string= remote-input-terminal-buffer-name (buffer-name (current-buffer)))))
    (setq remote-input-origin-buffer (current-buffer))
    (setq remote-input-origin-point (point))))

(defun remote-input-terminal--input-sender (proc input)
  (let* ((buffer remote-input-origin-buffer)
         (timer remote-input-origin-monitor-timer))
    (cond
     ((not timer)
      (message "Remote-Input 没有激活, 请运行命令： `remote-input-activate'"))
     ((not buffer)
      (message "请确定待输入buffer是否打开。"))
     (t
      (with-current-buffer buffer
        (when remote-input-origin-point
          (goto-char remote-input-origin-point)
          ;; 将连续两个句号替换为换行符。
          (insert (replace-regexp-in-string
                   remote-input-terminal-return-key-regexp
                   "\n" input))
          (setq remote-input-origin-point (point))
          (message "在 buffer: %s 中插入已经输入的文本。" (buffer-name buffer)))))))
  (comint-output-filter proc remote-input-terminal-prompt-regexp))

;;;###autoload
(defun remote-input-terminal ()
  "中文远程输入终端，将输入的每一行文字发送到待编辑文件。"
  (interactive)
  (setq remote-input-terminal-buffer (get-buffer-create remote-input-terminal-buffer-name))
  (switch-to-buffer remote-input-terminal-buffer)
  (set-buffer remote-input-terminal-buffer)
  (remote-input-terminal-mode))

;;;###autoload
(define-derived-mode remote-input-terminal-mode comint-mode "Remote-Input-Terminal"
  (setq comint-prompt-regexp (concat "^" (regexp-quote remote-input-terminal-prompt-regexp)))
  (setq comint-input-sender 'remote-input-terminal--input-sender)
  (setq comint-prompt-read-only t)
  (setq remote-input-terminal-buffer (get-buffer-create remote-input-terminal-buffer-name))
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc
           (condition-case nil
               (start-process "Remote-Input-Terminal"
                              (current-buffer) "hexl")
             (file-error (start-process "Remote-Input-Terminal" (current-buffer) "cat")))))
      (set-process-query-on-exit-flag fake-proc nil)
      (insert "** Remote-input-terminal started **\n")
      (set-marker
       (process-mark fake-proc) (point))
      (comint-output-filter fake-proc remote-input-terminal-prompt-regexp))))

(provide 'chinese-remote-input)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-remote-input.el ends here
