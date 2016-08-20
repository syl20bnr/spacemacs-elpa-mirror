## Introduce ##

bbdb-android is a BBDB v3 tool, which can import
and export from/to android phone contacts database
to/from BBDB (The Insidious Big Brother Database).

NOTE: You need make sure your android phone *rooted*
and a android phone data line.


## Download ##

    https://github.com/tumashu/bbdb-android

## Install ##

1. Install adb and sqlite3.
2. Config melpa: http://melpa.org/#/getting-started
3. M-x package-install RET bbdb-android RET
4. Add code to your emacs config file:（for example: ~/.emacs）：

```lisp
(require 'bbdb-vcard) ;; bbdb-android require bbdb-vcard
(require 'bbdb-android)
```

## Usage ##

Import contacts to BBDB from android phone

```lisp
M-x bbdb-android-import RET
```

Export contacts to android phone from BBDB

```lisp
M-x bbdb-android-export RET
```

## Security & Privacy ##

By default, bbdb-android will save contacts-db-files or
vcard-files in two directorys:

1. Android phone: "/sdcard/BBDB/"
2. Host:          "~/BBDB/"

For security reason, users should clean them regularly
or delete them when change your phone/computer.

## Issues ##

1. adb and sqlite3 commands is hard-code,
   make sure add them to system PATH.
2. When multi android devices are connected,
   bbdb-android can't work properly.

## Tips ##

### How to run adb without sudo? ###

#### Run command: lsusb ####

```
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 002 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 001 Device 002: ID 8087:0020 Intel Corp. Integrated Rate Matching Hub
Bus 002 Device 002: ID 8087:0020 Intel Corp. Integrated Rate Matching Hub
Bus 001 Device 003: ID 05c8:0403 Cheng Uei Precision Industry Co., Ltd (Foxlink) Webcam
Bus 002 Device 003: ID 093a:2510 Pixart Imaging, Inc. Optical Mouse
Bus 002 Device 013: ID 0bb4:0df6 HTC (High Tech Computer Corp.)
```

#### Find your android phone Vendor, for example: ####

```
Bus 002 Device 013: ID 0bb4:0df6 HTC (High Tech Computer Corp.)
```

#### Add the below code to file: "/etc/udev/rules.d/51-android.rules" ####

```
SUBSYSTEM=="usb", SYSFS{idVendor}=="0bb4", OWNER="<your-login-name>" GROUP="<your-login-name>", MODE="666"
```

#### Restart computer (Compulsory step). ####
