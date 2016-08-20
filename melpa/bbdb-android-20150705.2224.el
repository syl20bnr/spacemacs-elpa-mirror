;;; bbdb-android.el --- Android phone contacts import/export for BBDB

;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/bbdb-android
;; Package-Version: 20150705.2224
;; Version: 0.0.1
;; Package-Requires: ((bbdb-vcard "20150705.341"))

;;; License:

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

;; ## Introduce ##
;;
;; bbdb-android is a BBDB v3 tool, which can import
;; and export from/to android phone contacts database
;; to/from BBDB (The Insidious Big Brother Database).
;;
;; NOTE: You need make sure your android phone *rooted*
;; and a android phone data line.
;;
;;
;; ## Download ##
;;
;;     https://github.com/tumashu/bbdb-android
;;
;; ## Install ##
;;
;; 1. Install adb and sqlite3.
;; 2. Config melpa: http://melpa.org/#/getting-started
;; 3. M-x package-install RET bbdb-android RET
;; 4. Add code to your emacs config file:（for example: ~/.emacs）：
;;
;; ```lisp
;; (require 'bbdb-vcard) ;; bbdb-android require bbdb-vcard
;; (require 'bbdb-android)
;; ```

;; ## Usage ##
;;
;; Import contacts to BBDB from android phone
;;
;; ```lisp
;; M-x bbdb-android-import RET
;; ```
;;
;; Export contacts to android phone from BBDB
;;
;; ```lisp
;; M-x bbdb-android-export RET
;; ```
;;
;; ## Security & Privacy ##
;;
;; By default, bbdb-android will save contacts-db-files or
;; vcard-files in two directorys:
;;
;; 1. Android phone: "/sdcard/BBDB/"
;; 2. Host:          "~/BBDB/"
;;
;; For security reason, users should clean them regularly
;; or delete them when change your phone/computer.
;;
;; ## Issues ##
;;
;; 1. adb and sqlite3 commands is hard-code,
;;    make sure add them to system PATH.
;; 2. When multi android devices are connected,
;;    bbdb-android can't work properly.
;;
;; ## Tips ##
;;
;; ### How to run adb without sudo? ###
;;
;; #### Run command: lsusb ####
;;
;; ```
;; Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
;; Bus 002 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
;; Bus 001 Device 002: ID 8087:0020 Intel Corp. Integrated Rate Matching Hub
;; Bus 002 Device 002: ID 8087:0020 Intel Corp. Integrated Rate Matching Hub
;; Bus 001 Device 003: ID 05c8:0403 Cheng Uei Precision Industry Co., Ltd (Foxlink) Webcam
;; Bus 002 Device 003: ID 093a:2510 Pixart Imaging, Inc. Optical Mouse
;; Bus 002 Device 013: ID 0bb4:0df6 HTC (High Tech Computer Corp.)
;; ```
;;
;; #### Find your android phone Vendor, for example: ####
;;
;; ```
;; Bus 002 Device 013: ID 0bb4:0df6 HTC (High Tech Computer Corp.)
;; ```
;;
;; #### Add the below code to file: "/etc/udev/rules.d/51-android.rules" ####
;;
;; ```
;; SUBSYSTEM=="usb", SYSFS{idVendor}=="0bb4", OWNER="<your-login-name>" GROUP="<your-login-name>", MODE="666"
;; ```
;;
;; #### Restart computer (Compulsory step). ####

;;; Code:
(require 'bbdb)
(require 'bbdb-vcard)
(require 'hex-util)

(defgroup bbdb-android nil
  "Customizations for bbdb-android"
  :group 'bbdb)

(defcustom bbdb-android-directory-alist
  '((host . "~/BBDB/")
    (android . "/sdcard/BBDB/"))
  "The directorys used by bbdb-android,
under which imported/exported files are stored.

Note: User should regular cleaning by hand."
  :group 'bbdb-vcard)

(defcustom bbdb-android-adb-program "adb"
  "adb program used by bbdb-android."
  :type 'string
  :group 'bbdb-android)

(defcustom bbdb-android-sqlite3-program "sqlite3"
  "sqlite3 program used by bbdb-android."
  :type 'string
  :group 'bbdb-android)

(defun bbdb-android-adb-connect-p ()
  "Test adb connect."
  (when (executable-find bbdb-android-adb-program)
    (not (= 1 (shell-command
               (format "%s devices" bbdb-android-adb-program))))))

(defun bbdb-android-export ()
  "Export BBDB contacts to vcard file, push to android phone,
then import vcard file to android phone by adb."
  (interactive)
  (let* ((records (bbdb-records))
         (vcard-file-name
          (concat "bbdb-contacts-"
                  (format-time-string "%Y%m%d" nil t) ".vcf"))
         (temp-vcard-file
          (concat (file-name-as-directory
                   (cdr (assoc 'host bbdb-android-directory-alist)))
                  vcard-file-name))
         (remote-vcard-file
          (concat (file-name-as-directory
                   (cdr (assoc 'android bbdb-android-directory-alist)))
                  vcard-file-name))
         (remote-vcard-file-2
          (concat (file-name-as-directory
                   (concat "file://"
                           (cdr (assoc 'android bbdb-android-directory-alist))))
                  vcard-file-name)))
    (with-temp-buffer
      (dolist (record records)
        (insert (bbdb-vcard-from record)))
      (bbdb-vcard-write-buffer temp-vcard-file t))
    (if (not (bbdb-android-adb-connect-p))
        (message "Can't connect android device by adb command.")

      ;; Push vcard file to android.
      (when (yes-or-no-p (format "Push temp vcard file to \"%s\" ? " remote-vcard-file))
        (shell-command (format "%s push %s %s"
                               bbdb-android-adb-program
                               temp-vcard-file
                               remote-vcard-file)))

      ;; Clean android contacts.
      (when (yes-or-no-p "Delete all android contacts before import vcard file? ")
        (shell-command (format "%s shell pm clear com.android.providers.contacts"
                               bbdb-android-adb-program)))

      ;; launch android vcard importer app by adb.
      (when (yes-or-no-p (format "Import vcard file: \"%s\" ? " remote-vcard-file))
        (shell-command (format "%s shell am start -t \"%s\" -d \"%s\" -a android.intent.action.VIEW"
                               bbdb-android-adb-program
                               "text/x-vcard"
                               remote-vcard-file-2))))))

(defun bbdb-android-import ()
  "Pull android contacts2.db to computer with adb command,
 then import to BBDB."
  (interactive)
  (let* ((time-string (format-time-string "%Y%m%d" nil t))
         (contacts-db-file-name
          (concat "android-contacts-" time-string ".db"))
         (vcard-file-name
          (concat "android-contacts-" time-string ".vcf"))
         (temp-contacts-db-1
          (concat (file-name-as-directory
                   (cdr (assoc 'android bbdb-android-directory-alist)))
                  contacts-db-file-name))
         (temp-contacts-db-2
          (concat (file-name-as-directory
                   (cdr (assoc 'host bbdb-android-directory-alist)))
                  contacts-db-file-name))
         (vcard-file
          (concat (file-name-as-directory
                   (cdr (assoc 'host bbdb-android-directory-alist)))
                  vcard-file-name)))
    (if (and (bbdb-android-adb-connect-p)
             (yes-or-no-p "Do you want to import from android phone? "))
        (progn
          ;; Copy contacts database file to /sdcard directory
          ;; NOTE: This require your android *rooted*.
          (shell-command
           (format "%s shell \"su -c 'cat %s > %s'\""
                   bbdb-android-adb-program
                   "/data/data/com.android.providers.contacts/databases/contacts2.db"
                   temp-contacts-db-1))

          ;; Pull contacts db file to computer
          (shell-command
           (format "%s pull %s %s"
                   bbdb-android-adb-program
                   temp-contacts-db-1
                   temp-contacts-db-2))

          ;; Import vcard file to BBDB
          (bbdb-android-import-contacts-db temp-contacts-db-2))
      (message "Can't connect android device by adb command."))))

(defun bbdb-android-import-contacts-db (db-file)
  "Import contacts in `db-file' to BBDB database."
  (let* ((db-file
          (if (file-exists-p db-file)
              db-file
            (progn (message "Can't find file: %S " db-file)
                   nil)))
         (command
          (when (and db-file
                     (executable-find bbdb-android-sqlite3-program))
            (format "%s %s \"%s\""
                    bbdb-android-sqlite3-program
                    db-file
                    (concat "SELECT raw_contacts._id, raw_contacts.display_name, "
                            "raw_contacts.display_name_alt, mimetypes.mimetype, "
                            "REPLACE(REPLACE(data.data1, '\r\n', '\n'), '\n', '\n'), "
                            "data.data2, "
                            "REPLACE(REPLACE(data.data4, '\r\n', '\n'), '\n', '\n'), "
                            "data.data5, data.data6, data.data7, "
                            "data.data8, data.data9, data.data10, "
                            "quote(data.data15)||'::::' " ;; concat "::::" as split-string separator
                            "FROM raw_contacts, data, mimetypes "
                            "WHERE raw_contacts.deleted = 0 "
                            "AND raw_contacts._id = data.raw_contact_id "
                            "AND data.mimetype_id = mimetypes._id "
                            "ORDER BY raw_contacts._id, mimetypes._id, data.data2"))))
         sqlite3-output contacts-list contacts-list2 scards-list)
    (when command
      ;; Extract contacts info by run sqlite3 command.
      (setq sqlite3-output (shell-command-to-string command))

      ;; Convert string to lisp, like the example:
      ;; "1|a|b::::         (("1" "a" "b")
      ;;  1|c|d::::    -->   ("1" "c" "d")
      ;;  1|e|f::::"         ("1" "e" "f"))
      (setq contacts-list
            (mapcar #'(lambda (str)
                        (split-string str "\|"))
                    ;; String "::::" at the end of data15 as separator.
                    (split-string sqlite3-output "::::\n")))

      ;; Convert list to alist, like the example:
      ;; (("1" "a" "b")       ("1" (("a" "b")
      ;;  ("1" "c" "d")   ->        ("c" "d")
      ;;  ("1" "e" "f"))            ("e" "f")))
      (dolist (contact contacts-list)
        (let* ((key (car contact))
               (value (cdr contact)))
          (if (assoc key contacts-list2)
              (push value (car (cdr (assoc key contacts-list2))))
            (push `(,key (,value)) contacts-list2))))

      ;; convert to bbdb-vcard scard format.
      (setq scards-list
            (delq 'nil (mapcar
                        #'(lambda (x)
                            (bbdb-android-scardize (car (cdr x))))
                        contacts-list2)))

      ;; import scards to bbdb database
      (mapc #'bbdb-vcard-import-vcard-internal scards-list)
      (message "Import android contacts finished."))))


;; ** Contact-list converter
;; bbdb-android will convert contact-list to bbdb-vcard's scard
;; with the help of the below function.
;;
;; This is a `contact-list' example:
;;
;; (("Hello World" "World, Hello"
;;   "vnd.android.cursor.item/note" "This is note."
;;   "" "" "" "" "" "" "" "" "NULL")
;;  ("Hello World" "World, Hello"
;;   "vnd.android.cursor.item/name" "Hello World" "Hello"
;;   "" "" "" "" "" "" "1" "NULL")
;;  ("Hello World" "World, Hello"
;;   "vnd.android.cursor.item/email_v2"
;;   "helloworld@example.com" "0" "" "" "" "" "" "" "" "NULL"))
;;
;; This is a bbdb-vcard's scard example:
;;
;; (("FN" ((("content" "Hello World"))))
;;  ("N" ((("content" ("Word" "Hello" nil nil nil)))))
;;  ("ORG" ((("content" ("Example.com Inc." nil)))))
;;  ("EMAIL" ((("type" ("pref" "work" "internet"))
;;             ("content" "helloworld@example.org"))))
;;  ("TEL" ((("type" ("pref" "work"))
;;           ("content" "123456789"))
;;          (("type" "work")
;;           ("content" "234567890"))
;;          (("type" "cell")
;;           ("content" "345678901"))
;;          (("type" "home")
;;           ("content" "456789012"))))
;;  ("ADR" ((("type" "work")
;;           ("content" (nil nil
;;                           "2 Enterprise Avenue"
;;                           "Worktown"
;;                           "NY"
;;                         "01111"
;;                         "USA")))
;;          (("type" ("pref" "home"))
;;           ("content" (nil nil
;;                           "3 Acacia Avenue"
;;                            "Hoemtown"
;;                            "MA"
;;                            "02222"
;;                            "USA")))))
;;  ("URL" ((("type" "pref")
;;           ("content" "http://www.helloworld1.com/"))
;;          (("content" "http://www.helloworld2.com/"))))
;;  ("NOTE" ((("content" "This is a note.")))))

(defun bbdb-android-scardize (contact-list)
  "Convert `contact-list' to bbdb-vcard's scard format."
  (let (scard)
    (dolist (x contact-list)
      (let* ((name (nth 0 x))
             (alt-name-string (nth 1 x))
             (alt-name
              (when (stringp alt-name-string)
                (split-string alt-name-string ", *")))
             (mimetype (nth 2 x))
             (data1 (nth 3 x))
             (data2 (nth 4 x))
             (data4 (nth 5 x))
             (data5 (nth 6 x))
             (data6 (nth 7 x))
             (data7 (nth 8 x))
             (data8 (nth 9 x))
             (data9 (nth 10 x))
             (data10 (nth 11 x))
             (data15 (nth 12 x)))
        (when (and name (string= mimetype "vnd.android.cursor.item/name"))
          (push `("FN" ((("content" ,name)))) scard))

        (when (and alt-name (string= mimetype "vnd.android.cursor.item/name"))
          (push `("N" ((("content" ,alt-name)))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/email_v2"))
          (let ((key "EMAIL")
                (value `(("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/website"))
          (let ((key "URL")
                (value `(("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/note"))
          (push `("NOTE" ((("content" ,data1)))) scard))

        ;; Photo-info in android contacts2.db is encoded by hex,
        ;; Re-encode with base64.
        (when (and data1 (string= mimetype "vnd.android.cursor.item/photo"))
          (if (and (string-match-p "^X'" data15)
                   (string-match-p "'$" data15))
              ;; Deal with inline pictures or other media,
              ;; which are encoded with hex, like: "X'FFD8FF..73FFD9'"
              (let* ((string
                      (replace-regexp-in-string
                       "'$" ""
                       (replace-regexp-in-string
                        "^X'" "" data15)))
                     (base64-string
                      (base64-encode-string
                       (decode-hex-string string) t)))
                (push `("PHOTO" ((("encoding" "b")
                                  ("content" ,base64-string)))) scard))
            (push `("PHOTO" (("content" ,data15))) scard)))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/organization"))
          (push `("ORG" ((("content" (,data1))))) scard))

        (when (and data4 (string= mimetype "vnd.android.cursor.item/organization"))
          (push `("TITLE" ((("content" (,data4))))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/nickname"))
          (push `("NICKNAME" ((("content" ,(split-string data1 ", *"))))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/phone_v2"))
          (let* ((list '(("1" "HOME" "VOICE") ("2" "CELL" "VOICE" "PREF")
                         ("3" "WORK" "VOICE") ("4" "WORK" "FAX")
                         ("5" "HOME" "FAX") ("6" "PAGER") ("7" "OTHER")
                         ("8" "CUSTOM") ("9" "CAR" "VOICE")))
                 (key "TEL")
                 (value `(("type" ,(cdr (assoc data2 list)))
                          ("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/postal-address_v2"))
          (let* ((list '(("1"  "HOME") ("2"  "WORK")))
                 (key "ADR")
                 (value (if (string= data1 "United States of America")
                            `(("type" ,(cdr (assoc data2 list)))
                              ("content" ,data1))
                          `(("type" ,(cdr (assoc data2 list)))
                            ("content" (,data4 ,data7 ,data8 ,data9 ,data10))))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/im"))
          (let* ((list `(("-1"  ,(concat "-IM-Custom-" data6))
                         ("1"  "X-MSN")
                         ("2"  "X-YAHOO")
                         ("3"  "X-SKYPE-USERNAME")
                         ("4"  "X-QQ")
                         ("5"  "X-GOOGLE-TALK")
                         ("6"  "X-ICQ")
                         ("7"  "X-JABBER")))
                 (key (or (car (cdr (assoc data2 list)))
                          "IM")))
            (push `(,key ((("content" ,data1)))) scard)))
        ))
    scard))

(defun bbdb-android-dia-with-adb (phone-number)
  "Dia phone number by adb."
  (if (bbdb-android-adb-connect-p)
      (progn
        (shell-command
         (format "%s shell am start -a android.intent.action.CALL -d tel:%s"
                 bbdb-android-adb-program
                 phone-number))
        (message "Dia phone number: %s ..." phone-number))
    (message "Can't connect android device by adb command.")))

(defun bbdb-android-import-from-radicale ()
  "Import Radicale vcard file to BBDB."
  (interactive)
  (let* ((directory (concat "~/.config/radicale/collections/"
                            (user-real-login-name) "/"))
         (files (directory-files directory t ".vcf$"))
         (file (completing-read "Import vcard file: " files)))
    (when (yes-or-no-p (format "Really import vcard file: %s? " file))
      (bbdb-vcard-import-file file))))

(provide 'bbdb-android)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; bbdb-android.el ends here
