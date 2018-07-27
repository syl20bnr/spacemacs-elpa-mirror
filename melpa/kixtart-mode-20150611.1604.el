;;; kixtart-mode.el --- major mode for Kixtart scripting files

;; Author:  Ryrun <https://github.com/ryrun>
;; Keywords: languages
;; Package-Version: 20150611.1604
;; Version: 20150611.1
;; Homepage: https://github.com/ryrun/kixtart-mode
;; Package-Requires: ((emacs "24"))

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

;; Add support for KiXtart scripting files. Provides basic syntax highlighting.
;; More information about KiXtart: http://www.kixtart.org/

;;; Code:

(provide 'kixtart-mode)

;syntax table
(defconst kixtart-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "\<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\* ". 23n" table)
    (modify-syntax-entry ?\/ ". 14b" table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\\ "." table)
    table))

(defun kixtart-gen-regexp-list(list &optional appendstr)
    "function to create regex for functions and keywords."
    (concat "\\(?:^\\|\\s-\\)\\("
            (regexp-opt list)
            "\\)\\>"))

(defvar kixtart-var "$[0-9a-zA-Z]+")
(defvar kixtart-type 
      (kixtart-gen-regexp-list (list "@Address" "@Build" "@CPU" "@CRLF" "@CSD" "@Color" "@Comment" "@CurDir" "@DOS" "@Date" "@Day" "@Domain" "@Error" "@FullName" "@HomeDir" "@HomeDrive" "@HomeShr" "@HostName" "@IPAddressX" "@InWin" "@KiX" "@LDomain" "@LDrive" "@LM" "@LServer" "@LanRoot" "@LogonMode" "@LongHomeDir" "@MDayNo" "@MHz" "@MSecs" "@MaxPWAge" "@Month" "@MonthNo" "@PID" "@PWAge" "@PrimaryGroup" "@Priv" "@ProductSuite" "@ProductType" "@RAS" "@RServer" "@Result" "@SError" "@SID" "@ScriptDir" "@ScriptExe" "@ScriptName" "@Site" "@StartDir" "@SysLang" "@TIME" "@Ticks" "@USERID" "@USERLANG" "@WDayNo" "@WKSTA" "@WUserID" "@YDayNo" "@Year")))
(defvar kixtart-builtin
      (kixtart-gen-regexp-list (list "AScan" "Abs" "AddKey" "AddPrinterConnection" "AddProgramGroup" "AddProgramItem" "Asc" "At" "BackupEventLog" "Beep" "Big" "Box" "Break" "CD" "CDbl" "CInt" "CLS" "CStr" "Call" "Chr" "ClearEventLog" "Close" "Color" "CompareFileTimes" "Cookie1" "Copy" "CreateObject" "DecToHex" "Del" "DelKey" "DelPrinterConnection" "DelProgramGroup" "DelProgramItem" "DelTree" "DelValue" "Dim" "Dir" "Display" "EnumGroup" "EnumIpInfo" "EnumKey" "EnumLocalGroup" "EnumValue" "Execute" "Exist" "Exit" "ExpandEnvironmentVars" "Fix" "FlushKb" "FormatNumber" "FreeFileHandle" "Get" "GetDiskSpace" "GetFileAttr" "GetFileSize" "GetFileTime" "GetFileVersion" "GetObject" "GetS" "Global" "Go" "Gosub" "Goto" "IIF" "InGroup" "InStr" "InStrRev" "Int" "IsDeclared" "Join" "KbHit" "KeyExist" "LCase" "LTrim" "Left" "Len" "LoadHive" "LoadKey" "LogEvent" "Logoff" "MD" "Macro" "MemorySize" "MessageBox" "Move" "Open" "Play" "Quit" "RD" "RTrim" "ReDim" "ReadLine" "ReadProfileString" "ReadType" "ReadValue" "RedirectOutput" "Return" "Right" "Rnd" "Round" "Run" "SRnd" "SaveKey" "Select" "Case" "EndSelect" "SendKeys" "SendMessage" "Set" "SetAscii" "SetConsole" "SetDefaultPrinter" "SetFileAttr" "SetFocus" "SetL" "SetM" "SetOption" "SetSystemState" "SetTime" "SetTitle" "SetWallpaper" "Shell" "ShowProgramGroup" "ShutDown" "SidToName" "Sleep" "Small" "Split" "Substr" "Trim" "UCase" "Ubound" "Use" "UnloadHive" "Val" "VarType" "VarTypeName" "WriteLine" "WriteProfileString" "WriteValue"))
      )
(defvar kixtart-keywords
      (kixtart-gen-regexp-list (list "Do" "Until" "Function" "EndFunction" "While" "Loop" "Debug" "On" "Off" "For" "Each" "Next" "If" "Else" "Endif" "Or" "And")))

(defconst kixtart-font-lock-defaults
  `(
    (,kixtart-var . font-lock-variable-name-face)
    (,kixtart-type . font-lock-type-face)
    (,kixtart-keywords . font-lock-keyword-face)
    (,kixtart-builtin . font-lock-builtin-face)
    ("?" . font-lock-builtin-face)
    )
  )

;;;###autoload
(define-derived-mode kixtart-mode fundamental-mode "Kixtart Mode"
  :syntax-table kixtart-mode-syntax-table
  (setq-default font-lock-keywords-case-fold-search t)
  (setq font-lock-defaults '(kixtart-font-lock-defaults))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kix\\'" . kixtart-mode))

(provide 'kixtart-mode)
;;; kixtart-mode.el ends here
