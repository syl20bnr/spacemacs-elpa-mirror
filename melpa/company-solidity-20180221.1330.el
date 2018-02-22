;;; company-solidity.el --- Company-mode back-end for solidity-mode

;; Copyright (C) 2018  Samuel Smolkin

;; Author: Samuel Smolkin <sam@future-precedent.org>
;; URL: https://github.com/ssmolkin1/company-solidity
;; Package-Version: 20180221.1330
;; Keywords: solidity, completion
;; Version: 1.1.4
;; Package-Requires: ((company "0.9.0") (cl-lib "0.5.0"))

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

;; This package provides a simple company-mode back-end for auto-completing Solidity keywords when working in solidity-mode.

;;; Code:

(require 'cl-lib)
(require 'company)

(defconst company-solidity-keywords
  '("after"
    "as"
    "assembly"
    "break"
    "constant"
    "anonymous"
    "continue"
    "contract"
    "default"
    "delete"
    "do"
    "else"
    "event"
    "external"
    "for"
    "function"
    "if"
    "import"
    "in"
    "is"
    "indexed"
    "library"
    "mapping"
    "modifier"
    "new"
    "pragma solidity "
    "private"
    "public"
    "internal"
    "pure"
    "view"
    "return"
    "returns"
    "struct"
    "switch"
    "this"
    "using"
    "var"
    "while"
    "enum"
    "throw"
    "assert"
    "require"
    "revert"
    "storage"
    "memory"
    "true"
    "false"
    "wei"
    "szabo"
    "finney"
    "ether"
    "seconds"
    "minutes"
    "hours"
    "days"
    "weeks"
    "years"
    "constant"
    "public"
    "indexed"
    "storage"
    "memory"
    "address"
    "bool"
    "bytes"
    "bytes0"
    "bytes1"
    "bytes2"
    "bytes3"
    "bytes4"
    "bytes5"
    "bytes6"
    "bytes7"
    "bytes8"
    "bytes9"
    "bytes10"
    "bytes11"
    "bytes12"
    "bytes13"
    "bytes14"
    "bytes15"
    "bytes16"
    "bytes17"
    "bytes18"
    "bytes19"
    "bytes20"
    "bytes21"
    "bytes22"
    "bytes23"
    "bytes24"
    "bytes25"
    "bytes26"
    "bytes27"
    "bytes28"
    "bytes29"
    "bytes30"
    "bytes31"
    "bytes32"
    "int"
    "int8"
    "int16"
    "int24"
    "int32"
    "int40"
    "int48"
    "int56"
    "int64"
    "int72"
    "int80"
    "int88"
    "int96"
    "int104"
    "int112"
    "int120"
    "int128"
    "int136"
    "int144"
    "int152"
    "int160"
    "int168"
    "int176"
    "int184"
    "int192"
    "int200"
    "int208"
    "int216"
    "int224"
    "int232"
    "int240"
    "int248"
    "int256"
    "mapping"
    "real"
    "string"
    "text"
    "uint"
    "uint8"
    "uint16"
    "uint24"
    "uint32"
    "uint40"
    "uint48"
    "uint56"
    "uint64"
    "uint72"
    "uint80"
    "uint88"
    "uint96"
    "uint104"
    "uint112"
    "uint120"
    "uint128"
    "uint136"
    "uint144"
    "uint152"
    "uint160"
    "uint168"
    "uint176"
    "uint184"
    "uint192"
    "uint200"
    "uint208"
    "uint216"
    "uint224"
    "uint232"
    "uint240"
    "uint248"
    "uint256"
    "ureal"
    "msg"
    "block"
    "tx"
    "addmod"
    "mulmod"
    "keccak256"
    "sha256"
    "sha3"
    "ripemd160"
    "ecrecover"
    "block.blockhash"
    "block.coinbase"
    "block.difficulty"
    "block.gaslimit"
    "block.number"
    "block.timestamp"
    "msg.data"
    "msg.gas"
    "msg.sender"
    "msg.sig"
    "msg.value"
    "now"
    "tx.gasprice" ;; tx method
    "tx.origin"   ;; tx method
    "balance"  ;; address method
    "transfer" ;; address method
    "send"     ;; address method
    "call"     ;; address method
    "callcode" ;; address method
    "delegatecall") ;; address method
)

;;;###autoload
(defun company-solidity (command &optional arg &rest ignored)
  "Autocompletion for solidity with company mode.
Argument COMMAND `company-backend` functions.
Optional argument ARG the completion target prefix.
Optional argument IGNORED Additional arguments are ingnored."
    (interactive (list 'interactive))
    (set (make-local-variable 'company-minimum-prefix-length) 2)
    (cl-case command
	(interactive (company-begin-backend 'company-solidity))
	(prefix (and (eq major-mode 'solidity-mode)
		    (company-grab-symbol)))
    (candidates
    (cl-remove-if-not
	(lambda (c) (string-prefix-p arg c))
	company-solidity-keywords))))

(add-to-list 'company-backends 'company-solidity)

(provide 'company-solidity)

;;; company-solidity.el ends here
