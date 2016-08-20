;;; baidu-life.el --- Use baidu api to do some interesting things

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: lisp, baidu
;; Package-Version: 20160426.519
;; Version:0.1
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; baidu-life's code can be found here:
;;   http://github.com/lujun9972/el-baidu-life

;;; Commentary:

;; There are some interesting apis in baidu api store.
;; so I write this package to encapsulates some apis and makes them easy
;; to use in emacs.


;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)
(defgroup baidu-life nil
  "爱生活,爱百度"
  :prefix "baidu-life-")

(defcustom baidu-life-api-key ""
  "apikey"
  :group 'baidu-life)

(defcustom baidu-life-timed-out 10 
  "timed out  seconds to request baidu api")

(defmacro baidu-life--with-api-key (api-key &rest body)
  (declare (indent defun))
  `(let* ((url-request-extra-headers
           (cons (cons "apikey"  (cond ((stringp ,api-key)
                                        ,api-key)
                                       ((symbolp ,api-key)
                                        (symbol-value ,api-key))
                                       (t (error "not valid api-key")))) url-request-extra-headers)))
     ,@body))

(cl-defun baidu-life--retrieve-url-synchronously (url args &optional (type 'POST ))
  (let* ((url-request-method (if (eq 'POST type)
                                 "POST"
                               "GET"))
         (url-request-data
          (mapconcat (lambda (arg)
                       (concat (url-hexify-string (format "%s" (car arg)))
                               "="
                               (url-hexify-string (format "%s" (cdr arg)))))
                     args
                     "&"))
         (url-request-extra-headers
          (cons '("Content-Type" . "application/x-www-form-urlencoded")
                url-request-extra-headers))
         (url (if (eq 'POST type)
                  url
                (concat url "?" url-request-data))))
    (url-retrieve-synchronously url)))

(cl-defun baidu-life--json-read-from-url (url args &optional (type 'POST))
  (with-timeout (baidu-life-timed-out (error "baidu life timed out"))
    (let (json-object)
      (with-current-buffer (baidu-life--with-api-key baidu-life-api-key
                             (baidu-life--retrieve-url-synchronously url args type))
        (let (charset
              json-string)
          (goto-char (point-min))
          (when (search-forward "charset=" nil t)
            (setq charset (intern (downcase (buffer-substring-no-properties (point) (progn (end-of-line)
                                                                                           (point)))))))
          (goto-char (point-min))
          (search-forward-regexp "^$")
          (setq json-string (buffer-substring (point) (point-max)))
          (when charset
            (setq json-string (decode-coding-string json-string charset)))
          (setq json-object (json-read-from-string json-string))
          (kill-buffer)))
      (let ((errNum (or (cdr (assoc 'errNum json-object))
                        0))
            (errMsg (or (cdr (assoc 'errMsg json-object))
                        "")))
        (if (= 0 errNum)
            json-object
          (error errMsg))))))


(defun baidu-life-get-weather (&optional location)
  "根据`LOCATION'获取天气信息"
  (interactive)
  (let* ((location (or location (read-string "您想查询那座城市的天气,请输入对应的拼音:")))
         (location (if (string-match-p "\\cc" location)
                       (replace-regexp-in-string "[[:blank:]]" ""  (baidu-life-to-pinyin location))
                     location))
         (weather-alist (baidu-life--json-read-from-url "http://apis.baidu.com/apistore/weatherservice/weather"
                                                        `((citypinyin . ,location)) 'GET ))
         (ret-data (cdr (assoc 'retData weather-alist))))
    (format "%s:%s" (cdr (assoc 'weather ret-data))
            (cdr (assoc 'WS ret-data)))))
;; (baidu-life-get-weather "dongguan") => 阴:3-4级(10~17m/h)
;; (baidu-life-get-weather "东莞") => 阴:3-4级(10~17m/h)

(defun baidu-life-get-mobile-location (&optional phone)
  "获取手机号码`PHONE'的开户地"
  (interactive)
  (let* ((phone (or phone (read-string "请待查询的手机号码:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/showapi_open_bus/mobile/find"
                                                       `((num . ,phone)) 'GET))
         (ret-data (cdr (assoc 'showapi_res_body result-alist))))
    (format "%s-%s%s"
            (cdr (assoc 'name ret-data))
            (cdr (assoc 'prov ret-data))
            (cdr (assoc 'city ret-data)))))
;; (baidu-life-get-mobile-location "13570494314") => 中国移动-广东广州

(defun baidu-life-ipsearch (&optional ip)
  "获取`IP'位置信息"
  (interactive)
  (let* ((ip (or ip (read-string "请待查询的IP:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/chazhao/ipsearch/ipsearch"
                                                       `((ip . ,ip)) 'GET))
         (ret-data (cdr (assoc 'data result-alist))))
    (format "%s-%s/%s/%s"
            (cdr (assoc 'operator ret-data))
            (cdr (assoc 'country ret-data))
            (cdr (assoc 'province ret-data))
            (cdr (assoc 'city ret-data)))))
;; (baidu-life-ipsearch "14.17.34.189") => 电信-China/广东省/深圳市

(defun baidu-life-idservice (&optional id)
  "获取身份证号`ID'信息"
  (interactive)
  (let* ((id (or id (read-string "请输入待查询的身份证号:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/apistore/idservice/id"
                                                       `((id . ,id)) 'GET))
         (ret-data (cdr (assoc 'retData result-alist))))
    (format "性别:%s 出生日期:%s 地址:%s"
            (cdr (assoc 'sex ret-data))
            (cdr (assoc 'birthday ret-data))
            (cdr (assoc 'address ret-data)))))
;; (baidu-life-idservice "420984198704207896") => 性别:M 出生日期:1987-04-20 地址:湖北省孝感市汉川市

(defun baidu-life-md5decode (&optional md5)
  "破解`MD5'"
  (interactive)
  (let* ((md5 (or md5 (read-string "请输入待破解的MD5:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/chazhao/md5decod/md5decod"
                                                       `((md5 . ,md5)) 'GET))
         (ret-data (cdr (assoc 'data result-alist))))
    (cdr (assoc 'md5_src ret-data))))

;; (baidu-life-md5decode "b035b895aae7ea345897cac146a9eee3369c9ef1") => fdsfejfkddl



(defun baidu-life-waybillnotrace (&optional billno expresscode)
  "查询快递信息

`EXPRESSCODE'为快递公司代码. （圆通：YT，申通：ST，中通：ZT，邮政EMS: YZEMS，天天：TT，优速：YS，快捷：KJ，全峰：QF，增益：ZY）
`BILLNO'为快递公司的订单号."
  (interactive)
  (let* ((expresscode (or expresscode (read-string "请输入快递公司代号:")))
         (billno (or billno (read-string "请输入订单号:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/ppsuda/waybillnoquery/waybillnotrace"
                                                       `((expresscode . ,expresscode)
                                                         (billno . ,billno)) 'GET))
         (result-alist (elt (cdr (assoc 'data result-alist)) 0))
         (trace-array (cdr (assoc 'wayBills result-alist)))
         (trace-list (mapcar (lambda (trace-alist)
                               (format "%s-%s"
                                       (cdr (assoc 'time trace-alist))
                                       (cdr (assoc 'processInfo trace-alist))))
                             trace-array)))
    (mapconcat 'identity trace-list "\n")))

;; (baidu-life-waybillnotrace "805121891484" "YT" )
;; =>
;; 2015-11-23 19:47:38.0 CST-广东省珠海市唐家金鼎公司 签收人: 本人签收 已签收
;; 2015-11-21 09:00:29.0 CST-广东省珠海市唐家金鼎公司 派件人: 赵凯 派件中
;; 2015-11-21 01:52:31.0 CST-广东省珠海市公司 邓亮 已发出
;; 2015-11-20 21:49:26.0 CST-江门转运中心公司 李嘉欣 已发出
;; 2015-11-19 23:33:03.0 CST-福建省漳州市公司 何美玉 已打包
;; 2015-11-19 20:38:49.0 CST-福建省漳州市公司 取件人: 欧阳慧娇 已收件

(defun baidu-life-shorten-url (&optional url_long)
  "长url转短url "
  (interactive)
  (let* ((url_long (or url_long (read-string "请输入要转换的长URL:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/3023/shorturl/shorten"
                                                       `((url_long . ,url_long)) 'GET))
         (ret-data (aref (cdr (assoc 'urls result-alist)) 0)))
    (cdr (assoc 'url_short ret-data))))

;; (baidu-life-shorten-url "http://www.github.com") => http://t.cn/aeQioO

(defun baidu-life-to-pinyin (&optional chinese)
  "查询汉字的拼音"
  (interactive)
  (let* ((chinese (or chinese (read-string "请输入要查询的中文:")))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/xiaogg/changetopinyin/topinyin"
                                                       `((str . ,chinese)
                                                         (type . "json")) 'GET)))
    (cdr (assoc 'pinyin result-alist))))

;; (baidu-life-to-pinyin "百度google") => bai du google

(defun baidu-life-translate (&optional query)
  "汉英翻译"
  (interactive)
  (let* ((query (or query (read-string "请输入要翻译的英文/中文:")))
         (from (if (string-match-p "\\cc" query)
                   "zh"
                 "en"))
         (to (if (string-match-p "\\cc" query)
                 "en"
               "zh"))
         (result-alist (baidu-life--json-read-from-url "http://apis.baidu.com/apistore/tranlateservice/translate"
                                                       `((query . ,query)
                                                         (from . ,from)
                                                         (to . ,to)) 'GET))
         (ret-data (cdr (assoc 'retData result-alist))))
    (cdr (assoc 'dst (aref (cdr (assoc 'trans_result ret-data)) 0)))))

;; (baidu-life-translate "hello world") => 你好世界
;; (baidu-life-translate "你好") => Hello


(provide 'baidu-life)

;;; baidu-life.el ends here
