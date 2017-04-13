;;; easy-lentic.el --- Write org style comment with lentic

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu  <tumashu AT 163.com>
;; Keywords: convenience
;; Package-Version: 20170309.2143
;; Homepage: https://github.com/tumashu/easy-lentic
;; Package-Requires: ((lentic "0.10") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * easy-lentic 使用说明                                               :README:

;; 注意： 这个包作者已经不维护了，有兴趣的同学可以收养它 :-) !!!

;; easy-lentic 是基于 lentic 的一个扩展，但它不是扩展了 lentic 的功能，而是让 lentic 更加专注
;; 于一个 *特定* 的使用场合，即：

;; #+BEGIN_EXAMPLE
;; 编写 emacs-lisp 时，使用 org 格式组织 comment。
;; #+END_EXAMPLE

;; 注：[[https://github.com/phillord/lentic][lentic]] 是一个很有新意的包，引用作者的原话：

;; #+BEGIN_EXAMPLE
;; Lentic allows two buffers to share the same or similar content but otherwise operate independently.
;; This can be used for several different purposes.
;; Different buffers can be in the same mode, with different locations of point,
;; even different text sizes -- in effect, providing multiple persistent views.

;; It is also possible to have the different lentic buffers in different modes,
;; giving a form of multi-modal editing. Switching buffers effectively switches modes as well.

;; While the content of two lentic buffers must be related, it does not need to be syntactically identical.
;; This allows it to be used for a form of literate programming -- for example,
;; one buffer may contain valid LaTeX source with blocks of Lisp,
;; while in the other the LaTeX source is commented out, giving an entirely valid Lisp file.

;; ...
;; #+END_EXAMPLE


;; ** 安装

;; *** 安装 easy-lentic

;; 1. 配置 melpa 源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET easy-lentic RET

;; *** 安装 ox-gfm

;; easy-lentic 可以使用 ox-gfm (Github Flavored Markdown exporter for Org Mode) 将 org 格式转换
;; 为 github markdown 格式，但这个功能需要用户 *手动安装 ox-gfm*, 具体安装方式：

;; 1. 配置 melpa 源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET ox-gfm RET

;; 如果用户没有安装 ox-gfm, 那么，easy-lentic 将使用 ox-md 后端生成 README.md。

;; ** 配置

;; #+BEGIN_EXAMPLE
;; (require 'easy-lentic)   ;; You need install lentic and gfm
;; (easy-lentic-mode-setup) ;; Enable `easy-lentic-mode' for `emacs-lisp-mode' and `org-mode'
;; #+END_EXAMPLE

;; ** 使用

;; *** 编写 org 格式的 Comment

;; 编辑 emacs-lisp 文件时，按 'C-c ..'（`easy-lentic-switch-window'）会弹出一个
;; org-mode 窗口，这个窗口显示的内容和 emacs-lisp 文件内容在逻辑上具有高度的相似性。
;; 这样，comment 部份可以在 org buffer 中编辑，而 elisp 代码则可以到 emacs-lisp buffer中编辑，
;; 两个 buffer 中的内容实时自动的同步。

;; *** 自动生成 README 文档

;; `easy-lentic-generate-readme' 可以从当前 elisp 文件的 Commentary 部份提取相关内容，
;; 然后生成 README.md 文件，这个功能对 Commentary 的格式有一定的要求：

;; 1. 必须使用 org 格式编写。
;; 2. Head1 必须包含 README tag。

;; 比如：

;; #+BEGIN_EXAMPLE
;;; Commentary:

;; ,* This is Head1   :README:
;; ,** This is Head2
;; ,*** This is Head3
;; [[http://www.google.com][this is a link]]

;; ,* This head1 will not export
;; #+END_EXAMPLE

;;; Code:

;; * 代码                                                                 :code:

;; ** Require

;; #+BEGIN_SRC emacs-lisp
(require 'lentic)
(require 'lentic-org)
(require 'lentic-doc)
(require 'lisp-mode)
(require 'org)
(require 'ox)
(require 'cl-lib)
;; #+END_SRC

;; ** 定义一个 org-webpage 专用的 lentic el2org 转换器
;; lentic 内置了两个 el2org 转换器函数：

;; 1. `lentic-el-org-init'
;; 2. `lentic-el-orgel-init'

;; 第一个转换器太过简单，不能处理 “^;;; ”, 而第二个转换器又太复杂，稳定性不好，所以
;; 在这里，我基于 `lentic-el-org-init' 自定义了一个转换器：`easy-lentic-el2org-init',
;; 这个转换器和 `lentic-el-org-init' 转换器的功能类似，仅仅对 “^;;; ” 特殊处理。

;; #+BEGIN_SRC emacs-lisp
(defclass easy-lentic-org2el-configuration
  (lentic-unmatched-chunk-configuration
   lentic-uncommented-chunk-configuration)
  ())

(defmethod lentic-clone
  ((conf easy-lentic-org2el-configuration)
   &optional start stop length-before
   start-converted stop-converted)
  (let ((clone-return (call-next-method conf)))
    ;; replace all ';;; '  to ';;; '.
    (m-buffer-replace-match
     (m-buffer-match
      (lentic-that conf)
      "^ *;; ;;; ")
     ";;; ")
    clone-return))

(defmethod lentic-invert
  ((conf easy-lentic-org2el-configuration))
  (lentic-m-oset
   (easy-lentic-el2org-init)
   :that-buffer
   (lentic-this conf)))

(defun easy-lentic-org2el-init ()
  (lentic-org-oset
   (easy-lentic-org2el-configuration
    "easy-lentic-org2el"
    :lentic-file
    (concat
     (file-name-sans-extension
      (buffer-file-name))
     ".el"))))

(add-to-list 'lentic-init-functions
             'easy-lentic-org2el-init)

(defclass easy-lentic-el2org-configuration
  (lentic-unmatched-chunk-configuration
   lentic-commented-chunk-configuration)
  ())

(defmethod lentic-create
  ((conf easy-lentic-el2org-configuration))
  (let ((buf (call-next-method conf)))
    (with-current-buffer buf
      (show-all)
      ;; After run `show-all', the point move to
      ;; the end of buffer, reupdate the point.
      (lentic-update-point conf))
    buf))

(defmethod lentic-invert
  ((conf easy-lentic-el2org-configuration))
  (lentic-m-oset
   (easy-lentic-org2el-init)
   :delete-on-exit t
   :that-buffer (lentic-this conf)))

(defun easy-lentic-el2org-init ()
  (lentic-org-oset
   (easy-lentic-el2org-configuration
    "easy-lentic-el2org"
    ;; we don't really need a file and could cope without, but org mode assumes
    ;; that the buffer is file name bound when it exports. As it happens, this
    ;; also means that file saving is possible which in turn saves the el file
    :lentic-file
    (concat
     (file-name-sans-extension
      (buffer-file-name))
     ".org"))))

(add-to-list 'lentic-init-functions
             'easy-lentic-el2org-init)

(defun easy-lentic-switch-window ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    ;; Set buffer-local variable `lentic-init'
    (setq lentic-init '(easy-lentic-el2org-init)))
  (when (eq major-mode 'org-mode)
    ;; Set buffer-local variable `lentic-init'
    (setq lentic-init '(easy-lentic-org2el-init)))
  (lentic-mode-create-from-init)
  (lentic-mode-split-window-below)
  (let ((window
         (get-buffer-window
          (lentic-mode-find-next-visible-lentic-buffer
           (current-buffer)))))
    (when (window-live-p window)
      (select-window window))))

(defun easy-lentic-insert-begin-end ()
  (interactive)
  (easy-lentic-insert-boundary-string
   "
#++BEGIN_SRC emacs-lisp
?
#++END_SRC\n
"))

(defun easy-lentic-insert-end-begin ()
  (interactive)
  (easy-lentic-insert-boundary-string
   "
#++END_SRC
?
#++BEGIN_SRC emacs-lisp
"))

(defun easy-lentic-insert-boundary-string (str)
  (let* ((string (replace-regexp-in-string
                  "#\\+\\+"
                  (cl-case major-mode
                    (emacs-lisp-mode ";; #+")
                    (org-mode "#+"))
                  str))
         (position (cl-position ?\? string))
         (n (when position
              (- (length string) position))))
    (insert (replace-regexp-in-string "\\?" "" string))
    (when n
      (backward-char (- n 1)))))

(defvar easy-lentic-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c.." 'easy-lentic-switch-window)
    (define-key keymap "\C-c.b" 'easy-lentic-insert-begin-end)
    (define-key keymap "\C-c.e" 'easy-lentic-insert-end-begin)
    keymap)
  "Keymap for `easy-lentic-mode'")

(define-minor-mode easy-lentic-mode
  "Minor for easy-lentic."
  nil " easy-lentic" 'easy-lentic-mode-map)

(defun easy-lentic-mode-setup ()
  (interactive)
  (add-hook 'org-mode-hook 'easy-lentic-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-lentic-mode))
;; #+END_SRC

;; ** 清洗 lentic 转换得到的 org 文件

;; 在编辑 emacs-lisp 文件时，有许多 *约定俗成* 的东西，而许多相关工具，
;; 又依赖这些 *约定俗成* , 比较常见的有：

;; 1. 文档说明的开始，添加；“;;; Commentary:” 注释
;; 2. 代码部份的开始，添加：“;;; Code:” 注释

;; 这些书写习惯在 lentic 转换阶段处理起来非常麻烦，所以我将其原样输出
;; 到 org 文件，然后在 org 导出的时候用 `org-export-before-processing-hook'
;; 做处理，这样处理起来相对简单，下面是这个 hook 的定义：

;; #+BEGIN_SRC emacs-lisp
(defun easy-lentic-org-export-preprocess (backend)
  "This function delete useless strings in org files which are converted from
emacs-lisp files by lentic."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^;;; +Commentary:.*" nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (re-search-forward "^;;; +Code:.*" nil t)
      (replace-match "" nil t))))

;; #+END_SRC

;; ** 根据 elisp 文件的 Commentary，生成 README 文件

;; #+BEGIN_SRC emacs-lisp
(defun easy-lentic-orgify-if-necessary (el-file)
  (let* ((el-buffer (get-file-buffer el-file))
         (org-file (concat (file-name-sans-extension el-file) ".org"))
         (org-buffer (get-file-buffer org-file))
         (locked (or (file-locked-p el-file)
                     (file-locked-p org-file))))
    (when el-buffer
      (kill-buffer el-buffer))
    (when org-buffer
      (kill-buffer org-buffer))
    (unless locked
      (when (file-newer-than-file-p el-file org-file)
        (let ((lentic-kill-retain t))
          (lentic-batch-clone-and-save-with-config
           el-file 'easy-lentic-el2org-init))))))

(defun easy-lentic-generate-file (directory input-file-name tags backend output-file-name)
  (let (el-file org-file)
    (when (and directory input-file-name backend output-file-name)
      (let ((ext (file-name-extension input-file-name)))
        (cond ((equal ext "el") ;; elisp file convert to org file with lentic
               (setq el-file (concat directory input-file-name))
               (setq org-file (concat (file-name-sans-extension el-file) ".org"))
               (easy-lentic-orgify-if-necessary el-file))
              ((equal ext "org")
               (setq org-file (concat directory input-file-name)))))
      (if (file-exists-p org-file)
          (with-current-buffer (find-file-noselect org-file)
            (let ((org-export-before-processing-hook '(easy-lentic-org-export-preprocess))
                  (org-export-select-tags tags)
                  (org-export-with-tags nil)
                  (indent-tabs-mode nil)
                  (tab-width 4))
              (org-export-to-file backend output-file-name)))
        (message "Generate %s fail!!!" output-file-name)))))

(defun easy-lentic-generate-readme ()
  "Generate README.md from current emacs-lisp file."
  (interactive)
  (let* ((file (buffer-file-name))
         (point (point))
         (filename (when file
                     (file-name-nondirectory file)))
         (directory (when file
                      (file-name-directory file))))
    (when (and file (string-match-p "\\.el$" file)
               filename
               directory)
      (easy-lentic-generate-file
       directory filename '("README")
       (if (featurep 'ox-gfm)
           'gfm
         (message "Can't generate README.md with ox-gfm, use ox-md instead!")
         'md)
       "README.md")
      ;; Elisp buffer will be killed when generate README.md, reopen it!
      (find-file file)
      (goto-char point))))
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'easy-lentic)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; easy-lentic.el ends here
;; #+END_SRC
