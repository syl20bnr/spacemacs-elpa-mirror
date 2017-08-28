;;; chinese-yasdcv.el --- Yet another frontend for the StarDict command-line program

;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/chinese-yasdcv
;; Package-Version: 20170827.1650
;; Package-Requires: ((cl-lib "0.5") (pyim "1.6.0"))
;; Version: 0.0.1
;; Keywords: convenience, Chinese, dictionary

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
;; # 简介 #
;; Chinese-yasdcv 是 sdcv 的一个 emacs 前端，其工作原理是：
;;
;; 1. 调用 sdcv 程序，将翻译得到的结果定向到 *Stardict Output* buffer。
;; 2. 调用对应的elisp函数，清理上述 buffer 中的内容，并将其转化为 org-mode 格式。
;; 3. 弹出一个窗口显示上述 buffer 内容。
;;
;; 注：sdcv 是 StarDict 的 Console 版本，yasdcv 表示：Yet Another Sdcv。
;;
;; # 安装 #
;; 1. 安装 sdcv 程序（比如： sudo apt-get install sdcv）。
;; 2. 从网上寻找 StarDict 字典文件，按需下载。
;; 3. 配置melpa源，参考：http://melpa.org/#/getting-started。
;; 4. M-x package-install RET chinese-yasdcv RET
;; 5. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;
;; ```lisp
;; (require 'chinese-yasdcv)
;; ```
;; # 配置 #
;;
;; 1. 设置 `yasdcv-sdcv-command' (具体细节见变量说明)
;; 2. 设置 `yasdcv-sdcv-dicts'   (具体细节见变量说明)
;;
;; # 使用 #
;;
;; 将光标移动到需要查询的单词上（点词翻译），然后运行命令 `yasdcv-translate-at-point'，
;; 或者选择某一个单词（划词翻译），然后运行上述命令。
;;
;; 查询中文时，划词翻译可以正常使用，点词翻译要用到 Chinese-pyim 包中的命令
;;`pyim-cwords-at-point', 需要用户正确安装 Chinese-pyim 并添加配置拼音词库。
;; 具体细节请阅读 Chinese-pyim 的相关文档：http://tumashu.github.io/chinese-pyim/

;;; Code:
(require 'cl-lib)
(require 'pyim)

(defgroup chinese-yasdcv nil
  "Yet another sdcv emacs frontend (sdcv: Console version of StarDict program)"
  :group 'leim)

(defcustom yasdcv-sdcv-command
  "sdcv --non-interactive --utf8-output --utf8-input --use-dict \"%dict\" \"%word\""
  "设置sdcv命令，命令调用之前，%dict 将会替换为字典名称，%word 替换为需要查询的 word。"
  :group 'chinese-yasdcv
  :type 'string)

(defcustom yasdcv-sdcv-dicts
  '(("jianminghy" "简明汉英词典" "powerword2007" nil)
    ("jianmingyh" "简明英汉词典" "powerword2007" nil)

    ("lanconghy"  "懒虫简明汉英词典" nil nil)
    ("lancongyh"  "懒虫简明英汉词典" nil nil)


    ("xdictyh"    "XDICT英汉辞典" nil t)
    ("xdicthy"    "XDICT汉英辞典" nil t)

    ("xiandai"    "现代英汉综合大辞典" "powerword2007" t)

    ("niujing"    "牛津高阶英汉双解"  "oald" nil)
    (""           "英文相关词典" "powerword2007" nil)

    ("langdaohy"  "朗道汉英字典5.0" "langdao" nil)
    ("langdaoyh"  "朗道英汉字典5.0" "langdao" nil)

    ("21shiji"    "21世纪英汉汉英双向词典" "21cen" nil)
    ("21shjikj"   "21世纪双语科技词典"" nil" nil)

    (""           "新世纪英汉科技大词典" nil nil)
    (""           "新世纪汉英科技大词典" nil nil)

    (""           "现代商务汉英大词典" "powerword2007" nil)
    (""           "英汉双解计算机词典" "powerword2007" nil)
    (""           "汉语成语词典" "chengyu" t)
    (""           "高级汉语大词典" nil nil)
    (""           "现代汉语词典" nil nil)

    (""           "Cantonese Simp-English" nil nil)
    (""           "英汉进出口商品词汇大全" nil nil)

    (""           "中国大百科全书2.0版" nil t)
    (""           "CEDICT汉英辞典" nil nil)
    (""           "英文字根字典" nil t)

    (""           "湘雅医学专业词典" nil nil)

    (""           "[七国语言]英汉化学大词典" "powerword2007" nil)
    (""           "[七国语言]英汉数学大词典" "powerword2007" nil)
    (""           "[七国语言]英汉公共大词典" "powerword2007" nil)
    (""           "[七国语言]英汉医学大词典" "powerword2007" nil)
    (""           "[七国语言]英汉信息大词典" "powerword2007" nil)
    (""           "[七国语言]英汉生物学大词典" "powerword2007" nil)

    (""           "[名词委审定]英汉铁道科技名词" "powerword2007" nil)
    (""           "[名词委审定]汉英细胞生物学名词" "powerword2007" nil)
    (""           "[名词委审定]汉英数学名词" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(七, 整形、美容、皮肤、康复)" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(四, 心血管病学等)" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(一, 妇产科学)" "powerword2007" nil)
    (""           "[名词委审定]汉英生物化学名词" "powerword2007" nil)
    (""           "[名词委审定]英汉生物化学名词" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(二, 口腔学)" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(六, 外科)" "powerword2007" nil)
    (""           "[名词委审定]汉英人体解剖学名词" "powerword2007" nil)
    (""           "[名词委审定]汉英药学名词" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(三, 遗传学等)" "powerword2007" nil)
    (""           "[名词委审定]汉英医学名词(五, 眼科学)" "powerword2007" nil))

  "
    Chinese-yasdcv 可以正确处理的 sdcv 字典。每一个字典都使用一个列表来表示。其中：

    1. 第一个字符串表示字典的代号。
    2. 第二个字符串代表 sdcv 命令 --use-dict 选项识别的字典名称。
    3. 第三个字符串通过 `yasdcv--return-output-cleaner-function'
    返回清理和美化 sdcv 输出的函数的名称。
    4. 第四个元素表示字典是否激活。"
  :group 'chinese-yasdcv
  :type 'list)

(defun yasdcv--return-output-cleaner-function (name)
  (intern (concat "yasdcv--output-cleaner:" name)))

(defun yasdcv--output-cleaner:powerword2007 ()
  "清理现代英汉综合大辞典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<\\([^><]+\\)><!\\[CDATA\\[\\([^><]+\\)\\]\\]><\\([^><]+\\)>" nil t)
    (replace-match "\\1: \\2"))

  (goto-char (point-min))
  (while (re-search-forward "\n+例句原型: *\\([^><]+\\)\n例句解释: *\\([^><]+\\)\n+" nil t)
    (replace-match "- \\1 \\2"))

  (goto-char (point-min))
  (while (re-search-forward " +索引类型='.+'" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "<例句s +" nil t)
    (replace-match "- "))

  (goto-char (point-min))
  (while (re-search-forward "<[^><]+>\\|^ *]" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "词典音标.*\n" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "单词原型: +" nil t)
    (replace-match "** "))

  (goto-char (point-min))
  (while (re-search-forward "&L{\\(.+\\)}" nil t)
    (replace-match "\\1"))

  (goto-char (point-min))
  (while (search-forward "}&L{" nil t)
    (replace-match ", "))

  (mapcar (lambda (x)
            (goto-char (point-min))
            (while (search-forward x nil t)
              (replace-match "")))
          '("单词词性: " "解释项: "
            "[.]]>" "}]]>" "ly]]>" "[F]]>"
            "]]>" "<>" "<![CDATA[" "\n[")))

(defun yasdcv--output-cleaner:oald ()
  "清理牛津高阶英汉双解词典的输出"
  (goto-char (point-min))

  (goto-char (point-min))
  (while (re-search-forward "^\\* *" nil t)
    (replace-match "**** "))

  (goto-char (point-min))
  (while (re-search-forward "/\\(.+\\)/\n+\\(.+\\)" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "\\(^[0-9]\\) +\\(([a-z])\\) +" nil t)
    (replace-match "\\1 Good good study, day day up ......\n\\2"))

  (goto-char (point-min))
  (while (re-search-forward "^[0-9] *" nil t)
    (replace-match "** "))

  (goto-char (point-min))
  (while (re-search-forward "^([a-z]) *" nil t)
    (replace-match "*** ")))

(defun yasdcv--output-cleaner:langdao ()
  "清理朗道英汉字典的输出"
  (goto-char (point-min))
  (while (search-forward "*" nil t)
    (replace-match "")))

(defun yasdcv--output-cleaner:chengyu ()
  "清理成语大词典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<[^><]+>\\|\\[[^><]+>" nil t)
    (replace-match "")))

(defun yasdcv--output-cleaner:21cen ()
  "清理21世纪英汉汉英双向词典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<<\\([^><]+\\)>>" nil t)
    (replace-match "** \\1"))

  (goto-char (point-min))
  (while (re-search-forward "\\(^[0-9]\\) +\\([a-z]\\.\\) +" nil t)
    (replace-match "\\1 Good good study, day day up ......\n\\2"))

  (goto-char (point-min))
  (while (re-search-forward "^[0-9] *" nil t)
    (replace-match "*** "))

  (goto-char (point-min))
  (while (re-search-forward "^[a-z]\\. *" nil t)
    (replace-match "**** ")))

(defun yasdcv--output-cleaner:common ()
  (goto-char (point-min))
  (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "* \\1 (\\2)"))

  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))

  (goto-char (point-min))
  (kill-line 1))

(defun yasdcv--get-sdcv-output (word dict &optional force)
  "Get sdcv translate output using dict"
  (let* ((dict-name (nth 1 dict))
         (cleaner (nth 2 dict))
         (enable (nth 3 dict))
         (command  (replace-regexp-in-string
                    "%dict" dict-name
                    (replace-regexp-in-string
                     "%word" word yasdcv-sdcv-command))))
    (when (or enable force)
      (with-temp-buffer
        (insert (shell-command-to-string command))
        (when cleaner (funcall (yasdcv--return-output-cleaner-function cleaner)))
        (funcall (yasdcv--return-output-cleaner-function "common"))
        (goto-char (point-min))
        (when (re-search-forward word nil t)
          (buffer-string))))))

(defun yasdcv--get-translate (word &optional dict-key)
  "Return sdcv translate string of `word'"
  (with-temp-buffer
    (insert (mapconcat
             (lambda (dict)
               (cond ((or (not dict-key)
                          (=  (length dict-key) 0))
                      (yasdcv--get-sdcv-output word dict))
                     ((or (string= dict-key (nth 0 dict))
                          (string-match-p dict-key (nth 1 dict)))
                      (yasdcv--get-sdcv-output word dict t))))
             yasdcv-sdcv-dicts ""))
    (buffer-string)))

(defun yasdcv--buffer-output-translation (translate-text)
  "Output sdcv translation to the temp buffer."
  (let ((buffer-name "*Stardict Output*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
        (org-mode)
        (org-indent-mode))
      (insert translate-text))))

;;;###autoload
(defun yasdcv-translate-at-point ()
  "Translate current word at point with sdcv"
  (interactive)
  (let* ((current-words (pyim-cwords-at-point))
         (word (if mark-active
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (if (> (length current-words) 1)
                     (completing-read "翻译哪一个词： " (mapcar #'car current-words))
                   (car (car current-words)))))
         (translate (when word
                      (yasdcv--get-translate word))))
    (cond
     ((not word)
      (message "Can't find word at point."))
     ((or (not translate) (string= translate ""))
      (message "Can't translate the word: \"%s\"" word))
     (t (yasdcv--buffer-output-translation translate)))))

;;;###autoload
(defun yasdcv-translate-input ()
  "Translate the input word with sdcv"
  (interactive)
  (let* ((current-words (pyim-cwords-at-point))
         (word (if mark-active
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (car (car current-words))))
         (input-word (read-string (format "Input: ")
                                  word nil nil))
         (translate (yasdcv--get-translate input-word)))

    (cond
     ((or (not translate) (string= translate ""))
      (message "Can't translate the word: \"%s\"" input-word))
     (t (yasdcv--buffer-output-translation translate)))))

(provide 'chinese-yasdcv)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-yasdcv.el ends here
