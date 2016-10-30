# 简介 #
Chinese-yasdcv 是 sdcv 的一个 emacs 前端，其工作原理是：

1. 调用 sdcv 程序，将翻译得到的结果定向到 *Stardict Output* buffer。
2. 调用对应的elisp函数，清理上述 buffer 中的内容，并将其转化为 org-mode 格式。
3. 弹出一个窗口显示上述 buffer 内容。

注：sdcv 是 StarDict 的 Console 版本，yasdcv 表示：Yet Another Sdcv。

# 安装 #
1. 安装 sdcv 程序（比如： sudo apt-get install sdcv）。
2. 从网上寻找 StarDict 字典文件，按需下载。
3. 配置melpa源，参考：http://melpa.org/#/getting-started。
4. M-x package-install RET chinese-yasdcv RET
5. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

```lisp
(require 'chinese-yasdcv)
```
# 配置 #

1. 设置 `yasdcv-sdcv-command' (具体细节见变量说明)
2. 设置 `yasdcv-sdcv-dicts'   (具体细节见变量说明)

# 使用 #

将光标移动到需要查询的单词上（点词翻译），然后运行命令 `yasdcv-translate-at-point'，
或者选择某一个单词（划词翻译），然后运行上述命令。

查询中文时，划词翻译可以正常使用，点词翻译要用到 Chinese-pyim 包中的命令
`pyim-aptwords-get', 需要用户正确安装 Chinese-pyim 并添加配置拼音词库。
具体细节请阅读 Chinese-pyim 的相关文档：http://tumashu.github.io/chinese-pyim/
