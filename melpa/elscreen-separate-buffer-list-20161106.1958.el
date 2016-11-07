;;; elscreen-separate-buffer-list.el --- Separate buffer list manager for elscreen

;; Author: wamei <wamei.cho@gmail.com>
;; Keywords: elscreen
;; Package-Version: 20161106.1958
;; Version: 0.1.3
;; Package-Requires: ((emacs "24.4") (elscreen "1.4.6"))

;; License:

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

;; This makes elscreen can manage buffer list for each screen.
;;
;; To use this, add the following line somewhere in your init file:
;;
;;      (require 'elscreen-separate-buffer-list)
;;      (elscreen-separate-buffer-list-mode)
;;
;; This apply to ido-mode or something that uses ido-make-buffer-list such as helm.

;;; Code:

(eval-when-compile (require 'cl))
(require 'elscreen)

(defvar esbl-separate-buffer-list-default '("*scratch*" "*Messages*"))
(defvar esbl-separate-buffer-list '())
(defvar esbl-separate-buffer-count-list '())

(defvar ido-temp-list)

(defun esbl-save-separate-window-history (&optional screen)
  "SCREENに現在のWINDOW-HISTORYを保存する."
  (let ((screen (or screen (elscreen-get-current-screen)))
        (screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-window-history (esbl-get-all-window-history-alist))
    (elscreen-set-screen-property screen screen-property)))

(defun esbl-restore-separate-window-history (&optional screen)
  "SCREENに保存されているWINDOW-HISTORYを復元する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
        (screen-property (elscreen-get-screen-property screen)))
    (esbl-restore-all-window-history-alist (assoc-default 'separate-window-history screen-property))))

(defun esbl-save-separate-buffer-list (&optional screen)
  "SCREENに現在のSEPARATE-BUFFER-LISTを保存する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-buffer-list (esbl-get-separate-buffer-list))
    (elscreen-set-screen-property screen screen-property)))

(defun esbl-restore-separate-buffer-list (&optional screen)
  "SCREENに保存されているSEPARATE-BUFFER-LISTを復元する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (screen-property (elscreen-get-screen-property screen))
         (buffList (assoc-default 'separate-buffer-list screen-property)))
    (if buffList
        (setq esbl-separate-buffer-list buffList)
      (esbl-set-default-separate-buffer-list))))

(defun esbl-add-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTにBUFFERを加える."
  (unless (member buffer (esbl-get-separate-buffer-list))
    (setq esbl-separate-buffer-list (append (list buffer) (esbl-get-separate-buffer-list)))
    (esbl-separate-buffer-list-count-inc buffer)))

(defun esbl-remove-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTからBUFFERを取り除く."
  (esbl-separate-buffer-list-count-dec buffer)
  (setq esbl-separate-buffer-list (loop for i in (esbl-get-separate-buffer-list)
                                            unless (equal i buffer)
                                            collect i)))

(defun esbl-update-separate-buffer-list ()
  "SEPARATE-BUFFER-LISTを更新する."
  (esbl-separate-buffer-list-count-clean)
  (setq esbl-separate-buffer-list (loop for i in (esbl-get-separate-buffer-list)
                                            if (buffer-live-p i)
                                            collect i)))

(defun esbl-get-separate-buffer-list ()
  "SEPARATE-BUFFER-LISTを取得する."
  (when (equal 0 (length esbl-separate-buffer-list))
    (esbl-set-default-separate-buffer-list))
  esbl-separate-buffer-list)

(defun esbl-set-default-separate-buffer-list ()
  "デフォルトのバッファリストを設定する."
  (setq esbl-separate-buffer-list   (loop for i in esbl-separate-buffer-list-default
                                          collect (get-buffer i))))

(defun esbl-separate-buffer-list-count-inc (buffer)
  "BUFFERのカウントを上げる."
  (loop for i in esbl-separate-buffer-count-list
           if (equal (car i) buffer)
           do (setcdr i  (+ 1 (cdr i)))
           and return nil
           finally (push (cons buffer 1) esbl-separate-buffer-count-list)))

(defun esbl-separate-buffer-list-count-dec (buffer)
  "BUFFERのカウントを下げる."
  (setq esbl-separate-buffer-count-list   (loop for i in esbl-separate-buffer-count-list
                                                       if (equal (car i) buffer)
                                                       do (setcdr i  (- (cdr i) 1))
                                                       if (< 0 (cdr i))
                                                       collect i)))

(defun esbl-separate-buffer-list-count (buffer)
  "BUFFERのカウントを返す."
  (loop for i in esbl-separate-buffer-count-list
           if (equal (car i) buffer)
           return (cdr i)
           finally return 0))

(defun esbl-separate-buffer-list-count-clean ()
  "BUFFER-COUNTの掃除をする."
  (setq esbl-separate-buffer-count-list   (loop for i in esbl-separate-buffer-count-list
                                                       if (buffer-live-p (car i))
                                                       collect i)))

(defun esbl-goto:around (origin &rest args)
  "SCREENの切替時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを復元する."
  (let ((number (elscreen-get-current-screen)))
    (esbl-save-separate-window-history (elscreen-get-current-screen))
    (apply origin args)
    (esbl-restore-separate-window-history (elscreen-get-current-screen))
    (unless (eq number (elscreen-get-current-screen))
      (when (elscreen-screen-live-p (elscreen-get-previous-screen))
        (esbl-save-separate-buffer-list (elscreen-get-previous-screen)))
      (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))))

(defun esbl-swap:around (origin &rest args)
  "SCREENのswap時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを復元する."
  (esbl-save-separate-window-history (elscreen-get-current-screen))
  (apply origin args)
  (esbl-restore-separate-window-history (elscreen-get-current-screen))
  (when (elscreen-screen-live-p (elscreen-get-previous-screen))
    (esbl-save-separate-buffer-list (elscreen-get-previous-screen)))
  (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))

(defun esbl-clone:after (&rest _)
  "SCREENの複製時にSEPARATE-BUFFER-LISTも複製する."
  (esbl-restore-separate-buffer-list (elscreen-get-previous-screen))
  (loop for i in (esbl-get-separate-buffer-list)
        do (esbl-separate-buffer-list-count-inc i)))

(defvar esbl-kill-buffer-another-screen-p nil)

(defun esbl-kill:around (origin &rest args)
  "SCREENの削除時にBUFFERの削除、SEPARATE-BUFFER-LISTの復元をする."
  (let* ((screen (or (and (integerp (car args)) (car args))
                     (elscreen-get-current-screen)))
         (current-screen-p (eq screen (elscreen-get-current-screen)))
         (separate-buffer-list
          (if current-screen-p
              (esbl-get-separate-buffer-list)
            (assoc-default 'separate-buffer-list
                           (elscreen-get-screen-property screen))))
         (one-screen-p (and current-screen-p (elscreen-one-screen-p)))
         (separate-buffer-list-default
           (mapcar 'get-buffer esbl-separate-buffer-list-default))
         (origin-return (apply origin args)))
    (when (or origin-return one-screen-p)
      (mapc (lambda (buffer)
              (unless (memq buffer separate-buffer-list-default)
                (let ((esbl-kill-buffer-another-screen-p t)
                      (esbl-separate-buffer-list separate-buffer-list-default))
                  (esbl-separate-buffer-list-count-dec buffer)
                  (when elscreen-separate-buffer-list-mode
                    (kill-buffer buffer)))))
            separate-buffer-list)
      (when one-screen-p
        (esbl-set-default-separate-buffer-list)
        (esbl-save-separate-buffer-list (elscreen-get-current-screen))
        (elscreen-apply-window-configuration (elscreen-default-window-configuration)))
      (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))
    origin-return))

(defun esbl-kill-buffer-hook ()
  "BUFFER削除時にSEPARATE-BUFFER-LISTからも削除する."
  (let ((buffer (current-buffer)))
    (when (member buffer (esbl-get-separate-buffer-list))
      (esbl-remove-separate-buffer-list buffer))
    (if elscreen-separate-buffer-list-mode
        (if (> 1 (esbl-separate-buffer-list-count buffer))
            t
          (unless esbl-kill-buffer-another-screen-p
            (walk-windows
             `(lambda (win)
                (when (eq (window-buffer win) ,buffer)
                  (switch-to-prev-buffer win t)))
             nil (window-frame))
            (bury-buffer buffer))
          nil)
      t)))

(defun esbl-buffer-list-update-hook ()
  "BUFFER-LIST更新時にSEPARATE-BUFFER-LISTも更新する."
  (esbl-update-separate-buffer-list))

(defun esbl-add-separate-buffer-list:advice (buffer &rest _)
  "BUFFERをSEPARATE-BUFFER-LISTに追加するADVICE用関数."
  (esbl-add-separate-buffer-list (get-buffer buffer)))

(defun esbl-return-separate-buffer-list:buffer-list (origin &rest _)
  "BUFFER-LISTが呼ばれた際にSEPARATE-BUFFER-LISTでフィルタリングを行う."
  (loop for i in (apply origin _)
           if (member (get-buffer i) (esbl-get-separate-buffer-list))
           collect i))

(defun esbl-set-ido-separate-buffer-list ()
  "IDO-MAKE-BUFFER-LISTが呼ばれた際にSEPARATE-BUFFER-LISTでフィルタリングを行う."
  (let ((list (loop for i in ido-temp-list
                   if (member (get-buffer i) (esbl-get-separate-buffer-list))
                   collect i)))
    (setq ido-temp-list list)))

(defun esbl-switch-frame:around (origin &rest args)
  "FRAMEの切替時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを保存・復元する."
  (esbl-save-separate-window-history (elscreen-get-current-screen))
  (esbl-save-separate-buffer-list (elscreen-get-current-screen))
  (apply origin args)
  (esbl-restore-separate-window-history (elscreen-get-current-screen))
  (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))

(defun esbl-after-make-frame (frame)
  "FRAMEの作成時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを保存・復元する."
  (let ((selected-frame (selected-frame)))
    (esbl-save-separate-window-history (elscreen-get-current-screen))
    (esbl-save-separate-buffer-list (elscreen-get-current-screen))
    (save-current-buffer
      (select-frame frame)
      (esbl-restore-separate-window-history (elscreen-get-current-screen))
      (esbl-restore-separate-buffer-list (elscreen-get-current-screen))
      (select-frame selected-frame))))

(defun esbl-delete-frame-confs:before (frame)
  "FRAMEの削除時にBUFFERを削除する."
  (when (eq frame (selected-frame))
    (esbl-save-separate-buffer-list (elscreen-get-current-screen)))
  (let* ((esbl-kill-buffer-another-screen-p t)
         (separate-buffer-list-default
          (mapcar 'get-buffer esbl-separate-buffer-list-default))
         (esbl-separate-buffer-list separate-buffer-list-default))
    (loop for screen-property in (assoc-default 'screen-property
                                                (elscreen-get-frame-confs frame))
          do (dolist (buffer (assoc-default 'separate-buffer-list screen-property))
               (unless (memq buffer separate-buffer-list-default)
                 (esbl-separate-buffer-list-count-dec buffer)
                 (when elscreen-separate-buffer-list-mode
                    (kill-buffer buffer)))))))

(defvar esbl-selected-frame (selected-frame)
  "WINDOW-CONFIGURATION-CHANGE-HOOKが呼ばれる前に選択していたFRAME.")

(defun esbl-window-configuration-change-hook ()
  "FRAMEの削除後にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを復元する."
  ;; HANDLE-SWITCH-FRAMEが呼ばれない場合のために復元する.
  (unless (frame-live-p esbl-selected-frame)
    (esbl-restore-separate-window-history (elscreen-get-current-screen))
    (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))
  (setq esbl-selected-frame (selected-frame)))

;; elscreenのパッチからパクってきた
(defun esbl-window-history-supported-p ()
  "WINDOW-HISTORYに対応しているかどうか."
  (and (fboundp 'window-prev-buffers)
       (fboundp 'window-next-buffers)
       (fboundp 'set-window-prev-buffers)
       (fboundp 'set-window-next-buffers)))

(defun esbl-get-all-window-history-alist ()
  "全てのウィンドウのWINDOW-HISTORYをALISTにして取得する."
  (when (esbl-window-history-supported-p)
    (mapcar (lambda (window)
              (let ((prevs (window-prev-buffers window))
                    (nexts (window-next-buffers window)))
                (cons window (cons prevs nexts))))
            (window-list))))

(defun esbl-restore-all-window-history-alist (history-alist)
  "HISTORY-ALISTからWINDOW-HISTORYを復元する."
  (when (esbl-window-history-supported-p)
    (mapc (lambda (entry)
            (let* ((window (car entry))
                   (histories (cdr entry))
                   (prevs (car histories))
                   (nexts (cdr histories)))
              (when (window-valid-p window)
                (set-window-prev-buffers window prevs)
                (set-window-next-buffers window nexts))))
          history-alist)))

(advice-add 'elscreen-goto :around 'esbl-goto:around)
(advice-add 'elscreen-swap :around 'esbl-swap:around)
(advice-add 'elscreen-clone :after 'esbl-clone:after)
(advice-add 'elscreen-kill :around 'esbl-kill:around)
(advice-add 'switch-to-buffer :after 'esbl-add-separate-buffer-list:advice)
(advice-add 'display-buffer :after 'esbl-add-separate-buffer-list:advice)
(advice-add 'handle-switch-frame :around 'esbl-switch-frame:around)
(advice-add 'select-frame-set-input-focus :around 'esbl-switch-frame:around)
(advice-add 'elscreen-delete-frame-confs :before 'esbl-delete-frame-confs:before)
(add-hook 'kill-buffer-query-functions 'esbl-kill-buffer-hook)
(add-hook 'buffer-list-update-hook 'esbl-buffer-list-update-hook)
(add-hook 'window-configuration-change-hook 'esbl-window-configuration-change-hook)
(add-hook 'after-make-frame-functions 'esbl-after-make-frame)

;;;###autoload
(define-minor-mode elscreen-separate-buffer-list-mode
  "Toggle elscreen separate buffer list mode."
  :group 'elscreen
  :global t
  (if elscreen-separate-buffer-list-mode
        (add-hook 'ido-make-buffer-list-hook 'esbl-set-ido-separate-buffer-list)
    (remove-hook 'ido-make-buffer-list-hook 'esbl-set-ido-separate-buffer-list)))

(provide 'elscreen-separate-buffer-list)

;;; elscreen-separate-buffer-list.el ends here
